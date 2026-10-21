/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.storage.akubra;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;

import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_MODEL;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_ORGANIZATION;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PID;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_SOURCE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_USER;

/** Fills missing audit metadata and normalizes invalid actors against the current user list. */
public final class SolrLogBackfill {
    private static final int PAGE_SIZE = 500;
    private final SolrClient client;
    private final ModelResolver modelResolver;
    private final Map<String, String> organizations;

    @FunctionalInterface
    public interface ModelResolver { String resolve(String pid) throws IOException; }

    public record Result(long scanned, long updated, long models, long organizations,
            long unresolvedModels, long unresolvedOrganizations, long conflicts, long invalidRecords,
            long normalizedUsers, long clearedOrganizations) {
        public String summary() {
            return "Prohlédnuto: " + scanned + ", upraveno: " + updated + ", doplněno modelů: " + models
                    + ", doplněno organizací: " + organizations + ", model nedohledán: " + unresolvedModels
                    + ", organizace nedohledána: " + unresolvedOrganizations + ", souběžné změny: " + conflicts
                    + ", neplatné záznamy: " + invalidRecords
                    + ", uživatel změněn na UNKNOWN: " + normalizedUsers + ", odstraněno organizací: " + clearedOrganizations
                    + ". Doplněné údaje vycházejí ze současného modelu objektu a současné organizace uživatele.";
        }
    }

    /** organizations must contain every current username, including users with a null organization. */
    public SolrLogBackfill(SolrClient client, ModelResolver modelResolver, Map<String, String> organizations) {
        this.client = client;
        this.modelResolver = modelResolver;
        this.organizations = new LinkedHashMap<>(organizations);
    }

    public Result run(BooleanSupplier cancelled, Consumer<Result> progress) throws IOException, InterruptedException {
        long scanned = 0, updated = 0, models = 0, orgs = 0, unresolvedModels = 0, unresolvedOrgs = 0, conflicts = 0, invalid = 0;
        long normalizedUsers = 0, clearedOrganizations = 0;
        Map<String, String> modelCache = new LinkedHashMap<>(128, .75f, true) {
            @Override protected boolean removeEldestEntry(Map.Entry<String, String> eldest) { return size() > 4096; }
        };
        String cursor = "*";
        boolean dirty = false;
        try {
            while (true) {
                checkCancelled(cancelled);
                SolrQuery query = new SolrQuery("*:*");
                query.setRows(PAGE_SIZE);
                query.setSort(FIELD_SOURCE, SolrQuery.ORDER.asc);
                query.set("cursorMark", cursor);
                query.setFields(FIELD_SOURCE, FIELD_PID, FIELD_USER, FIELD_MODEL, FIELD_ORGANIZATION, "_version_");
                QueryResponse response = client.query(query);
                for (SolrDocument log : response.getResults()) {
                    if (Thread.currentThread().isInterrupted()) throw new InterruptedException("Doplňování loggingCore zastaveno");
                    scanned++;
                    String source = text(log, FIELD_SOURCE);
                    if (blank(source)) { invalid++; continue; }
                    boolean missingModel = blank(text(log, FIELD_MODEL));
                    boolean missingOrg = blank(text(log, FIELD_ORGANIZATION));
                    String user = text(log, FIELD_USER);
                    boolean unknownUser = blank(user) || SolrLogFeeder.UNKNOWN_USER.equals(user) || !organizations.containsKey(user);
                    boolean setUser = unknownUser && !SolrLogFeeder.UNKNOWN_USER.equals(user);
                    boolean clearOrg = unknownUser && log.getFieldValue(FIELD_ORGANIZATION) != null;
                    String model = null;
                    String organization = null;
                    if (missingModel) {
                        String pid = text(log, "pid");
                        if (!blank(pid)) {
                            if (!modelCache.containsKey(pid)) modelCache.put(pid, modelResolver.resolve(pid));
                            model = modelCache.get(pid);
                        }
                        if (blank(model)) unresolvedModels++;
                    }
                    if (missingOrg) {
                        organization = unknownUser ? null : organizations.get(user);
                        if (blank(organization)) unresolvedOrgs++;
                    }
                    boolean setModel = missingModel && !blank(model);
                    boolean setOrg = missingOrg && !blank(organization);
                    if (!setModel && !setOrg && !setUser && !clearOrg) continue;
                    Object version = log.getFieldValue("_version_");
                    if (!(version instanceof Number) || ((Number) version).longValue() <= 0) { invalid++; continue; }
                    SolrInputDocument patch = new SolrInputDocument();
                    patch.setField(FIELD_SOURCE, source);
                    patch.setField("_version_", ((Number) version).longValue());
                    if (setUser) patch.setField(FIELD_USER, Map.of("set", SolrLogFeeder.UNKNOWN_USER));
                    if (clearOrg) {
                        patch.setField(FIELD_ORGANIZATION, java.util.Collections.singletonMap("set", null));
                    }
                    if (setModel) {
                        patch.setField(FIELD_MODEL, Map.of("set", model));
                    }
                    if (setOrg) {
                        patch.setField(FIELD_ORGANIZATION, Map.of("set", organization));
                    }
                    try {
                        client.add(patch);
                        dirty = true;
                        updated++;
                        if (setModel) models++;
                        if (setOrg) orgs++;
                        if (setUser) normalizedUsers++;
                        if (clearOrg) clearedOrganizations++;
                    } catch (SolrException ex) {
                        // Preserve a concurrently enriched or deleted record; a later run can retry.
                        if (ex.code() != 409) throw ex;
                        conflicts++;
                    }
                }
                if (dirty) { client.commit(); dirty = false; }
                Result result = new Result(scanned, updated, models, orgs, unresolvedModels, unresolvedOrgs, conflicts, invalid,
                        normalizedUsers, clearedOrganizations);
                progress.accept(result);
                String next = response.getNextCursorMark();
                if (next == null) throw new IOException("loggingCore nevrátil cursorMark; doplňování nelze bezpečně dokončit.");
                if (cursor.equals(next)) return result;
                cursor = next;
            }
        } catch (SolrServerException ex) {
            throw new IOException("Nelze doplnit metadata loggingCore", ex);
        } finally {
            if (dirty) {
                try { client.commit(); } catch (SolrServerException ex) { throw new IOException("Nelze potvrdit změny loggingCore", ex); }
            }
        }
    }

    private static void checkCancelled(BooleanSupplier cancelled) throws InterruptedException {
        if (Thread.currentThread().isInterrupted() || cancelled.getAsBoolean()) throw new InterruptedException("Doplňování loggingCore zastaveno");
    }

    private static String text(SolrDocument document, String field) {
        Object value = document.getFieldValue(field);
        return value instanceof String ? (String) value : null;
    }

    private static boolean blank(String value) { return value == null || value.isBlank(); }
}
