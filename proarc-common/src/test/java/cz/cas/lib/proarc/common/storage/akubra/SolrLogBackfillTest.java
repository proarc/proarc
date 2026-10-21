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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import mockit.Delegate;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class SolrLogBackfillTest {
    @Mocked SolrClient client;
    @Mocked QueryResponse first;
    @Mocked QueryResponse second;

    private SolrDocument log(String source, String pid, String user, String model, String org) {
        SolrDocument log = new SolrDocument();
        log.setField("source", source);
        log.setField("pid", pid);
        log.setField("user", user);
        log.setField("_version_", 42L);
        if (model != null) log.setField("model", model);
        if (org != null) log.setField("organization", org);
        return log;
    }

    private SolrDocumentList page(SolrDocument... logs) {
        SolrDocumentList result = new SolrDocumentList();
        result.addAll(List.of(logs));
        result.setNumFound(logs.length);
        return result;
    }

    @Test
    void fillsOnlyMissingFieldsUsingOriginalIdentityAndVersionAndReportsUnresolvedValues() throws Exception {
        SolrDocumentList logs = page(
                log("a", "pidA", "alice", "historical:model", null),
                log("b", "pidB", "unknown", null, "historical:org"),
                log("c", "deleted", "unknown", null, null),
                log("d", "pidD", "alice", "existing:model", "existing:org"));
        new Expectations() {{
            client.query((SolrQuery) any); result = first;
            first.getResults(); result = logs;
            first.getNextCursorMark(); result = "*";
        }};
        SolrLogBackfill.Result result = new SolrLogBackfill(client,
                pid -> "pidB".equals(pid) ? "current:model" : null, Map.of("alice", "Library A"))
                .run(() -> false, progress -> {});
        assertEquals(4, result.scanned());
        assertEquals(3, result.updated());
        assertEquals(1, result.models());
        assertEquals(1, result.organizations());
        assertEquals(1, result.unresolvedModels());
        assertEquals(1, result.unresolvedOrganizations());
        assertEquals(2, result.normalizedUsers());
        assertEquals(1, result.clearedOrganizations());
        new Verifications() {{
            List<SolrInputDocument> patches = new ArrayList<>();
            client.add(withCapture(patches)); times = 3;
            assertEquals("a", patches.get(0).getFieldValue("source"));
            assertEquals(42L, patches.get(0).getFieldValue("_version_"));
            assertNull(patches.get(0).getField("model"));
            assertEquals(Map.of("set", "Library A"), patches.get(0).getFieldValue("organization"));
            assertEquals("b", patches.get(1).getFieldValue("source"));
            assertEquals(Map.of("set", "current:model"), patches.get(1).getFieldValue("model"));
            assertEquals(java.util.Collections.singletonMap("set", null), patches.get(1).getFieldValue("organization"));
            assertEquals(Map.of("set", "UNKNOWN"), patches.get(1).getFieldValue("user"));
            assertEquals(Map.of("set", "UNKNOWN"), patches.get(2).getFieldValue("user"));
            assertNull(patches.get(0).getField("user"));
            for (SolrInputDocument patch : patches) {
                for (String preserved : List.of("pid", "date", "operation", "stream", "device", "sigla")) {
                    assertNull(patch.getField(preserved));
                }
            }
            client.commit(); times = 1;
        }};
    }

    @Test
    void normalizesActorsEvenWithCompleteMetadataAndKeepsUsersWithoutOrganization() throws Exception {
        Map<String, String> users = new java.util.LinkedHashMap<>();
        users.put("alice", "Library A");
        users.put("no-org", null);
        new Expectations() {{
            client.query((SolrQuery) any); result = first;
            first.getResults(); result = page(
                    log("a", "pid", "Upgrade MODS metadata to 3.8", "model:page", "Wrong library"),
                    log("b", "pid", "no-org", "model:page", null),
                    log("c", "pid", null, "model:page", null),
                    log("d", "pid", "UNKNOWN", "model:page", "Wrong library"),
                    log("e", "pid", "UNKNOWN", "model:page", null),
                    log("f", "pid", "alice", "model:page", "Historical library"));
            first.getNextCursorMark(); result = "*";
        }};
        var result = new SolrLogBackfill(client, pid -> null, users).run(() -> false, progress -> {});
        assertEquals(3, result.updated());
        assertEquals(2, result.normalizedUsers());
        assertEquals(2, result.clearedOrganizations());
        new Verifications() {{
            List<SolrInputDocument> patches = new ArrayList<>();
            client.add(withCapture(patches)); times = 3;
            assertEquals(List.of("a", "c", "d"), patches.stream().map(p -> p.getFieldValue("source")).toList());
            assertEquals(java.util.Collections.singletonMap("set", null), patches.get(0).getFieldValue("organization"));
            assertEquals(java.util.Collections.singletonMap("set", null), patches.get(2).getFieldValue("organization"));
        }};
    }

    @Test
    void unknownNormalizationIsIdempotent() throws Exception {
        SolrDocument source = log("a", "pid", "Change model", "model:page", "Wrong library");
        new Expectations() {{
            client.query((SolrQuery) any); result = first;
            first.getResults(); result = page(source);
            first.getNextCursorMark(); result = "*";
            client.add((SolrInputDocument) any); result = new Delegate() {
                void update(SolrInputDocument patch) {
                    source.setField("user", "UNKNOWN");
                    source.removeFields("organization");
                }
            };
        }};
        SolrLogBackfill backfill = new SolrLogBackfill(client, pid -> null, Map.of());
        assertEquals(1, backfill.run(() -> false, progress -> {}).updated());
        assertEquals(0, backfill.run(() -> false, progress -> {}).updated());
        new Verifications() {{ client.add((SolrInputDocument) any); times = 1; }};
    }

    @Test
    void usesStableCursorAndCachesModelLookupsAcrossPages() throws Exception {
        new Expectations() {{
            client.query((SolrQuery) any); returns(first, second);
            first.getResults(); result = page(log("a", "samePid", "alice", null, null));
            first.getNextCursorMark(); result = "cursor-1";
            second.getResults(); result = page(log("b", "samePid", "alice", " ", ""));
            second.getNextCursorMark(); result = "cursor-1";
        }};
        AtomicInteger lookups = new AtomicInteger();
        SolrLogBackfill.Result result = new SolrLogBackfill(client,
                pid -> { lookups.incrementAndGet(); return "model:page"; }, Map.of("alice", "A"))
                .run(() -> false, progress -> {});
        assertEquals(2, result.updated());
        assertEquals(1, lookups.get());
        new Verifications() {{
            List<SolrQuery> queries = new ArrayList<>();
            client.query(withCapture(queries)); times = 2;
            assertEquals("*", queries.get(0).get("cursorMark"));
            assertEquals("cursor-1", queries.get(1).get("cursorMark"));
            assertEquals("source asc", queries.get(0).getSortField());
            assertEquals(500, queries.get(0).getRows());
        }};
    }

    @Test
    void concurrentChangesAreSkippedWithoutOverwritingNewerSnapshots() throws Exception {
        new Expectations() {{
            client.query((SolrQuery) any); result = first;
            first.getResults(); result = page(log("a", "pid", "alice", null, null));
            first.getNextCursorMark(); result = "*";
            client.add((SolrInputDocument) any); result = new SolrException(SolrException.ErrorCode.CONFLICT, "version changed");
        }};
        var result = new SolrLogBackfill(client, pid -> "model:page", Map.of("alice", "A"))
                .run(() -> false, progress -> {});
        assertEquals(0, result.updated());
        assertEquals(1, result.conflicts());
        new Verifications() {{ client.commit(); times = 0; }};
    }

    @Test
    void rerunDoesNotWriteAlreadyCompletedRecords() throws Exception {
        SolrDocument source = log("a", "pid", "alice", null, null);
        new Expectations() {{
            client.query((SolrQuery) any); result = first;
            first.getResults(); result = page(source);
            first.getNextCursorMark(); result = "*";
            client.add((SolrInputDocument) any); result = new Delegate() {
                void update(SolrInputDocument patch) {
                    for (String name : List.of("model", "organization")) {
                        Object value = patch.getFieldValue(name);
                        if (value instanceof Map) source.setField(name, ((Map<?, ?>) value).get("set"));
                    }
                }
            };
        }};
        SolrLogBackfill backfill = new SolrLogBackfill(client, pid -> "model:page", Map.of("alice", "A"));
        assertEquals(1, backfill.run(() -> false, progress -> {}).updated());
        assertEquals(0, backfill.run(() -> false, progress -> {}).updated());
        new Verifications() {{ client.add((SolrInputDocument) any); times = 1; }};
    }

    @Test
    void refusesUnsafeWritesWithoutOriginalVersion() throws Exception {
        SolrDocument source = log("a", "pid", "alice", null, null);
        source.removeFields("_version_");
        new Expectations() {{
            client.query((SolrQuery) any); result = first;
            first.getResults(); result = page(source);
            first.getNextCursorMark(); result = "*";
        }};
        var result = new SolrLogBackfill(client, pid -> "model:page", Map.of("alice", "A"))
                .run(() -> false, progress -> {});
        assertEquals(1, result.invalidRecords());
        new Verifications() {{ client.add((SolrInputDocument) any); times = 0; }};
    }

    @Test
    void cancelledJobDoesNotStartReadingOrWriting() throws Exception {
        assertThrows(InterruptedException.class, () -> new SolrLogBackfill(client, pid -> null, Map.of())
                .run(() -> true, progress -> {}));
        new Verifications() {{ client.query((SolrQuery) any); times = 0; }};
    }

    @Test
    void confirmsCompletedWritesEvenWhenLaterLookupFails() throws Exception {
        new Expectations() {{
            client.query((SolrQuery) any); result = first;
            first.getResults(); result = page(log("a", "first", "alice", null, null), log("b", "broken", "alice", null, null));
        }};
        assertThrows(IOException.class, () -> new SolrLogBackfill(client, pid -> {
            if ("broken".equals(pid)) throw new IOException("storage unavailable");
            return "model:page";
        }, Map.of("alice", "A")).run(() -> false, progress -> {}));
        new Verifications() {{ client.add((SolrInputDocument) any); times = 1; client.commit(); times = 1; }};
    }
}
