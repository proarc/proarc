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

import cz.cas.lib.proarc.common.process.AuditLog;
import java.io.IOException;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocument;

/** Export package statistics from historical audit snapshots, without repository/searchCore lookups. */
public final class SolrExportStatistics {
    private static final DateTimeFormatter DATE = new DateTimeFormatterBuilder().appendInstant(3).toFormatter();
    private final SolrClient client;

    public SolrExportStatistics(SolrClient client) { this.client = client; }

    public record Filters(LocalDate from, LocalDate to, List<String> users, List<String> models,
            List<String> exportTypes, List<String> devices, List<String> software, List<String> siglas,
            String organization, String groupBy, boolean unique) {
        public Filters(LocalDate from, LocalDate to, List<String> users, List<String> models,
                List<String> exportTypes, List<String> devices, List<String> software,
                String organization, String groupBy, boolean unique) {
            this(from, to, users, models, exportTypes, devices, software, List.of(), organization, groupBy, unique);
        }

        public Filters {
            if (from == null || to == null || from.isAfter(to)) throw new IllegalArgumentException("Invalid date range");
            if (!List.of("model", "exportType", "device", "software", "equipment").contains(groupBy)) {
                throw new IllegalArgumentException("Invalid export grouping");
            }
            users = clean(users); models = clean(models); exportTypes = clean(exportTypes);
            devices = clean(devices); software = clean(software); siglas = clean(siglas);
        }

        public Map<String, Object> asMap() {
            Map<String, Object> values = new LinkedHashMap<>();
            values.put("dateFrom", from.toString()); values.put("dateTo", to.toString());
            values.put("usernames", users); values.put("models", models); values.put("exportTypes", exportTypes);
            values.put("devices", devices); values.put("software", software);
            values.put("siglas", siglas);
            values.put("organization", organization); values.put("groupBy", groupBy); values.put("unique", unique);
            return values;
        }
    }

    public Map<String, List<String>> options(String organization) throws IOException {
        return options(organization, false);
    }

    public Map<String, List<String>> options(String organization, boolean articles) throws IOException {
        SolrQuery query = new SolrQuery("operation:EXPORT");
        if (articles) values(query, "model", AuditLog.ARTICLE_MODELS.stream().sorted().toList());
        else excludeArticles(query);
        query.setRows(0);
        organization(query, organization);
        query.setFacet(true); query.setFacetLimit(-1); query.setFacetMinCount(1); query.setFacetSort("index");
        List<String> names = List.of("user", "model", "exportType", "device", "software", "sigla");
        names.forEach(query::addFacetField);
        try {
            QueryResponse response = client.query(query);
            Map<String, List<String>> options = new LinkedHashMap<>();
            for (String name : names) {
                FacetField facet = response.getFacetField(name);
                options.put(name, facet == null || facet.getValues() == null ? List.of() : facet.getValues().stream()
                        .map(FacetField.Count::getName).filter(value -> value != null && !value.isBlank())
                        .filter(value -> !"model".equals(name)
                                || (value.startsWith("model:") && AuditLog.ARTICLE_MODELS.contains(value) == articles)).toList());
            }
            return options;
        } catch (SolrServerException ex) { throw new IOException("Cannot read export filter options", ex); }
    }

    public Result calculate(Filters filters) throws IOException {
        SolrQuery query = query(filters, "EXPORT");
        excludeArticles(query);
        query.setRows(500);
        query.setFields("source", "pid", "date", "model", "exportType", "device", "software", "pageCount", "otherModelCount");
        query.setSort("source", SolrQuery.ORDER.asc);
        Map<GroupKey, Group> groups = new LinkedHashMap<>();
        Map<Identity, Snapshot> latest = new LinkedHashMap<>();
        String cursor = "*";
        try {
            while (true) {
                if (Thread.currentThread().isInterrupted()) throw new IOException("Export statistics interrupted");
                query.set("cursorMark", cursor);
                QueryResponse response = client.query(query);
                if (response.getResponseHeader() != null && "true".equalsIgnoreCase(String.valueOf(response.getResponseHeader().get("partialResults")))) {
                    throw new IOException("Incomplete export statistics response");
                }
                if (response.getResults() == null) throw new IOException("Missing export statistics records");
                for (SolrDocument document : response.getResults()) {
                    Snapshot snapshot = snapshot(document, filters.groupBy);
                    if (filters.unique) {
                        Identity identity = new Identity(snapshot.group, snapshot.pid);
                        latest.merge(identity, snapshot, (old, current) -> current.date.compareTo(old.date) > 0
                                || (current.date.equals(old.date) && current.source.compareTo(old.source) > 0) ? current : old);
                    } else add(groups, snapshot);
                }
                String next = response.getNextCursorMark();
                if (next == null) throw new IOException("Missing export cursor");
                if (cursor.equals(next)) break;
                cursor = next;
            }
        } catch (SolrServerException ex) { throw new IOException("Cannot calculate export statistics", ex); }
        latest.values().forEach(snapshot -> add(groups, snapshot));
        return new Result(filters, groups.values().stream().sorted(Comparator.comparingLong(Group::getPageCount).reversed()
                .thenComparing(group -> group.getModel())).toList());
    }

    private SolrQuery query(Filters filters, String operation) {
        SolrQuery query = new SolrQuery("operation:" + operation);
        ZoneId zone = ZoneId.systemDefault();
        query.addFilterQuery("date:[\"" + DATE.format(filters.from.atStartOfDay(zone).toInstant()) + "\" TO \""
                + DATE.format(filters.to.plusDays(1).atStartOfDay(zone).toInstant()) + "\"}");
        values(query, "user", filters.users); values(query, "model", filters.models);
        values(query, "exportType", filters.exportTypes); values(query, "device", filters.devices);
        values(query, "software", filters.software); organization(query, filters.organization);
        values(query, "sigla", filters.siglas);
        return query;
    }

    /** Unique articles use the latest matching export per PID, before pagination. */
    public ArticleResult articles(Filters filters, int offset, int limit) throws IOException {
        if (offset < 0 || limit < 1 || limit > 500) throw new IllegalArgumentException("Invalid article pagination");
        SolrQuery query = query(filters, "EXPORT");
        values(query, "model", AuditLog.ARTICLE_MODELS.stream().sorted().toList());
        if (filters.unique) query.addFilterQuery("{!collapse field=pid sort='date desc,source desc'}");
        query.setStart(offset); query.setRows(limit);
        query.setSort("date", SolrQuery.ORDER.desc); query.addSort("source", SolrQuery.ORDER.desc);
        query.setFields("source", "pid", "model", "doi", "exportType", "date", "user", "sigla", "exportId");
        try {
            QueryResponse response = client.query(query);
            if (response.getResponseHeader() != null && "true".equalsIgnoreCase(String.valueOf(response.getResponseHeader().get("partialResults")))) {
                throw new IOException("Incomplete article statistics response");
            }
            if (response.getResults() == null) throw new IOException("Missing article export records");
            java.util.ArrayList<Article> articles = new java.util.ArrayList<>();
            for (SolrDocument doc : response.getResults()) {
                articles.add(new Article(required(doc, "source"), required(doc, "pid"), required(doc, "model"),
                        strings(doc.getFieldValue("doi")), required(doc, "exportType"), required(doc, "date"),
                        text(doc, "user"), strings(doc.getFieldValue("sigla")), text(doc, "exportId")));
            }
            return new ArticleResult(filters, articles, response.getResults().getNumFound(), offset, limit);
        } catch (SolrServerException ex) { throw new IOException("Cannot read article export statistics", ex); }
    }

    public record Article(String source, String pid, String model, List<String> dois, String exportType,
            String date, String username, List<String> siglas, String exportId) { }

    public static final class ArticleResult {
        private final Filters filters;
        private final List<Article> articles;
        private final long count;
        private final int offset, limit;
        private ArticleResult(Filters filters, List<Article> articles, long count, int offset, int limit) {
            this.filters = filters; this.articles = List.copyOf(articles); this.count = count;
            this.offset = offset; this.limit = limit;
        }
        public Map<String, Object> getFilters() { return filters.asMap(); }
        public List<Article> getArticles() { return articles; }
        public long getCount() { return count; }
        public int getOffset() { return offset; }
        public int getLimit() { return limit; }
    }

    private static Snapshot snapshot(SolrDocument doc, String grouping) throws IOException {
        String source = required(doc, "source");
        String pid = required(doc, "pid");
        String date = required(doc, "date");
        String model = text(doc, "model");
        if (model == null || !model.startsWith("model:")) model = SolrLogSearchView.UNKNOWN_MODEL;
        GroupKey key = new GroupKey("exportType".equals(grouping) ? text(doc, "exportType") : null,
                List.of("device", "equipment").contains(grouping) ? strings(doc.getFieldValue("device")) : List.of(),
                List.of("software", "equipment").contains(grouping) ? strings(doc.getFieldValue("software")) : List.of(), model);
        Long pages = count(doc.getFieldValue("pageCount"));
        Long others = count(doc.getFieldValue("otherModelCount"));
        return new Snapshot(source, pid, date, key, pages == null ? 0 : pages, others == null ? 0 : others,
                pages == null || others == null);
    }

    private static void add(Map<GroupKey, Group> groups, Snapshot snapshot) {
        Group group = groups.computeIfAbsent(snapshot.group, Group::new);
        group.count = Math.addExact(group.count, 1);
        group.pageCount = Math.addExact(group.pageCount, snapshot.pages);
        group.otherModelCount = Math.addExact(group.otherModelCount, snapshot.others);
        if (snapshot.incomplete) group.incompleteCount++;
    }

    private static Long count(Object value) {
        return value instanceof Number && ((Number) value).longValue() >= 0 ? ((Number) value).longValue() : null;
    }
    private static String text(SolrDocument doc, String field) {
        Object value = doc.getFieldValue(field);
        return value == null || value.toString().isBlank() ? null : value.toString();
    }
    private static String required(SolrDocument doc, String field) throws IOException {
        String value = text(doc, field);
        if (value == null) throw new IOException("Export log is missing " + field);
        return value;
    }
    private static List<String> strings(Object value) {
        if (value == null) return List.of();
        Collection<?> collection = value instanceof Collection<?> ? (Collection<?>) value : List.of(value);
        return collection.stream().filter(item -> item != null && !item.toString().isBlank()).map(Object::toString)
                .distinct().sorted().toList();
    }
    private static List<String> clean(List<String> values) {
        return values == null ? List.of() : values.stream().filter(value -> value != null && !value.isBlank())
                .map(String::trim).distinct().toList();
    }
    private static void values(SolrQuery query, String field, List<String> values) {
        if (!values.isEmpty()) query.addFilterQuery(field + ":(" + values.stream()
                .map(value -> "\"" + ClientUtils.escapeQueryChars(value) + "\"").collect(java.util.stream.Collectors.joining(" OR ")) + ")");
    }
    private static void organization(SolrQuery query, String organization) {
        if (organization != null && !organization.isBlank()) values(query, "organization", List.of(organization));
    }

    private static void excludeArticles(SolrQuery query) {
        values(query, "-model", AuditLog.ARTICLE_MODELS.stream().sorted().toList());
    }

    private record GroupKey(String exportType, List<String> devices, List<String> software, String model) { }
    private record Identity(GroupKey group, String pid) { }
    private record Snapshot(String source, String pid, String date, GroupKey group, long pages, long others, boolean incomplete) { }

    public static final class Group {
        private final GroupKey key;
        private long count, pageCount, otherModelCount, incompleteCount;
        private Group(GroupKey key) { this.key = key; }
        public String getExportType() { return key.exportType; }
        public List<String> getDevices() { return key.devices; }
        public List<String> getSoftware() { return key.software; }
        public String getModel() { return key.model; }
        public long getCount() { return count; }
        public long getPageCount() { return pageCount; }
        public long getOtherModelCount() { return otherModelCount; }
        public long getIncompleteCount() { return incompleteCount; }
    }

    public static final class Result {
        private final Filters filters;
        private final List<Group> groups;
        private Result(Filters filters, List<Group> groups) { this.filters = filters; this.groups = List.copyOf(groups); }
        public Map<String, Object> getFilters() { return filters.asMap(); }
        public List<Group> getGroups() { return groups; }
        public long getCount() { return groups.stream().mapToLong(Group::getCount).reduce(0, Math::addExact); }
        public long getPageCount() { return groups.stream().mapToLong(Group::getPageCount).reduce(0, Math::addExact); }
        public long getOtherModelCount() { return groups.stream().mapToLong(Group::getOtherModelCount).reduce(0, Math::addExact); }
        public long getIncompleteCount() { return groups.stream().mapToLong(Group::getIncompleteCount).sum(); }
    }
}
