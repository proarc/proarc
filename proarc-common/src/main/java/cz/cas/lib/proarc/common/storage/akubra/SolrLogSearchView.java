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
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;

/** Productivity uses the actor and timestamp recorded at ingest, independently of searchCore. */
public class SolrLogSearchView {
    public static final String UNKNOWN_MODEL = "__unknown__";
    private static final DateTimeFormatter LOG_DATE = new DateTimeFormatterBuilder().appendInstant(3).toFormatter();
    private final SolrClient client;

    public SolrLogSearchView(SolrClient client) {
        this.client = client;
    }

    public List<String> getSiglas(String organization) throws IOException {
        SolrQuery query = new SolrQuery("operation:INGEST");
        query.setRows(0);
        if (organization != null && !organization.isBlank()) {
            query.addFilterQuery("organization:\"" + ClientUtils.escapeQueryChars(organization) + "\"");
        }
        query.setFacet(true);
        query.addFacetField("sigla");
        query.setFacetLimit(-1);
        query.setFacetMinCount(1);
        query.setFacetSort("index");
        try {
            FacetField facet = client.query(query).getFacetField("sigla");
            return facet == null || facet.getValues() == null ? List.of() : facet.getValues().stream()
                    .map(FacetField.Count::getName).filter(value -> value != null && !value.isBlank()).toList();
        } catch (SolrServerException ex) {
            throw new IOException("Cannot read productivity sigla options", ex);
        }
    }

    public CreatedObjectsStatistics getCreatedObjectsStatistics(List<String> users, LocalDate from, LocalDate to,
            List<String> models, String organization) throws IOException {
        return getCreatedObjectsStatistics(users, from, to, models, List.of(), organization);
    }

    public CreatedObjectsStatistics getCreatedObjectsStatistics(List<String> users, LocalDate from, LocalDate to,
            List<String> models, List<String> siglas, String organization) throws IOException {
        if (from == null || to == null || from.isAfter(to)) {
            throw new IllegalArgumentException("A valid inclusive date range is required");
        }
        SolrQuery query = new SolrQuery("*:*");
        query.setRows(0);
        query.addFilterQuery("operation:INGEST");
        ZoneId zone = ZoneId.systemDefault();
        // date is a string in loggingCore: keep fixed-width milliseconds for lexical comparisons.
        query.addFilterQuery("date:[\"" + LOG_DATE.format(from.atStartOfDay(zone).toInstant())
                + "\" TO \"" + LOG_DATE.format(to.plusDays(1).atStartOfDay(zone).toInstant()) + "\"}");
        addValues(query, "user", users);
        addValues(query, "model", models);
        addValues(query, "sigla", siglas);
        if (organization != null && !organization.isBlank()) {
            query.addFilterQuery("organization:\"" + ClientUtils.escapeQueryChars(organization) + "\"");
        }
        // Collapse before faceting so each matching PID contributes to exactly one model bucket.
        // If duplicate ingests disagree on metadata, use the earliest matching log, then source as a tie-breaker.
        query.addFilterQuery("{!collapse field=pid sort='date asc,source asc'}");
        query.setFacet(true);
        query.addFacetField("model");
        query.setFacetMinCount(1);
        query.setFacetLimit(-1);
        query.setFacetSort("count");
        query.set("facet.missing", true);
        try {
            QueryResponse response = client.query(query);
            long count = response.getResults().getNumFound();
            FacetField modelsFacet = response.getFacetField("model");
            if (count > 0 && (modelsFacet == null || modelsFacet.getValues() == null)) {
                throw new IOException("Missing productivity model counts");
            }
            Map<String, Long> counts = new LinkedHashMap<>();
            if (modelsFacet != null && modelsFacet.getValues() != null) {
                for (FacetField.Count model : modelsFacet.getValues()) {
                    if (model.getCount() > 0) {
                        String name = model.getName();
                        String bucket = name != null && name.startsWith("model:") ? name : UNKNOWN_MODEL;
                        counts.merge(bucket, model.getCount(), Long::sum);
                    }
                }
            }
            return new CreatedObjectsStatistics(count, counts);
        } catch (SolrServerException ex) {
            throw new IOException("Cannot count ingested objects in loggingCore", ex);
        }
    }

    private static void addValues(SolrQuery query, String field, List<String> values) {
        if (values == null) return;
        String terms = values.stream().filter(value -> value != null && !value.isBlank())
                .map(String::trim).distinct().map(value -> "\"" + ClientUtils.escapeQueryChars(value) + "\"")
                .collect(Collectors.joining(" OR "));
        if (!terms.isEmpty()) query.addFilterQuery(field + ":(" + terms + ")");
    }

    public static final class CreatedObjectsStatistics {
        private final long count;
        private final Map<String, Long> modelCounts;

        private CreatedObjectsStatistics(long count, Map<String, Long> modelCounts) {
            this.count = count;
            this.modelCounts = Collections.unmodifiableMap(new LinkedHashMap<>(modelCounts));
        }

        public long getCount() { return count; }
        public Map<String, Long> getModelCounts() { return modelCounts; }
    }
}
