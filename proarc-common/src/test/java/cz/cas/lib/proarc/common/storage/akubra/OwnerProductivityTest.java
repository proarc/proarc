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
import java.util.Arrays;
import java.util.List;
import java.util.TimeZone;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.util.NamedList;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.client.solrj.response.FacetField;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;

class OwnerProductivityTest {
    @Mocked SolrClient client;
    @Mocked QueryResponse response;
    private TimeZone originalZone;
    private final LocalDate day = LocalDate.of(2026, 6, 1);

    @BeforeEach
    void setTimezone() {
        originalZone = TimeZone.getDefault();
        TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    }

    @AfterEach
    void restoreTimezone() { TimeZone.setDefault(originalZone); }

    private static NamedList<Object> fields(Object... entries) {
        NamedList<Object> list = new NamedList<>();
        for (int i = 0; i < entries.length; i += 2) list.add((String) entries[i], entries[i + 1]);
        return list;
    }

    private void results(long objects, List<NamedList<Object>> buckets, long missing) throws Exception {
        SolrDocumentList data = new SolrDocumentList();
        data.setNumFound(objects);
        FacetField models = new FacetField("model");
        for (NamedList<Object> bucket : buckets) {
            models.add((String) bucket.get("val"), ((Number) bucket.get("objects")).longValue());
        }
        models.add(null, missing);
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getResults(); result = data;
            response.getFacetField("model"); result = models;
        }};
    }

    @ParameterizedTest
    @CsvSource({"true,true", "true,false", "false,true", "false,false"})
    void filtersIngestEventsByActorDateModelAndOrganization(boolean filterModels, boolean filterOrganization) throws Exception {
        results(12, List.of(fields("val", "model:page", "objects", 12L)), 0);
        var statistics = new SolrLogSearchView(client).getCreatedObjectsStatistics(
                List.of("xy", "second:user", "xy"), day, day,
                filterModels ? List.of("model:page") : List.of(), filterOrganization ? "Library A" : null);
        assertEquals(12L, statistics.getCount());
        new Verifications() {{
            SolrQuery query;
            client.query(query = withCapture()); times = 1;
            assertEquals(0, query.getRows());
            assertEquals("*:*", query.getQuery());
            List<String> filters = Arrays.asList(query.getFilterQueries());
            assertTrue(filters.contains("operation:INGEST"));
            assertTrue(filters.contains("user:(\"xy\" OR \"second\\:user\")"));
            assertTrue(filters.contains("date:[\"2026-06-01T00:00:00.000Z\" TO \"2026-06-02T00:00:00.000Z\"}"));
            assertEquals(filterModels, filters.contains("model:(\"model\\:page\")"));
            assertEquals(filterOrganization, filters.contains("organization:\"Library\\ A\""));
            assertFalse(filters.stream().anyMatch(filter -> filter.startsWith("owner:") || filter.startsWith("state:") || filter.startsWith("created:")));
            assertTrue(filters.contains("{!collapse field=pid sort='date asc,source asc'}"));
            assertEquals(-1, query.getFacetLimit());
        }};
    }

    @Test
    void filtersByNormalizedSiglasAlongsideOrganizationBeforeCollapsing() throws Exception {
        results(0, List.of(), 0);
        new SolrLogSearchView(client).getCreatedObjectsStatistics(List.of(), day, day, List.of(),
                Arrays.asList(" ABA001 ", "BOA001", "ABA001", " ", null, "A:B"), "Library A");
        new Verifications() {{
            SolrQuery query;
            client.query(query = withCapture());
            List<String> filters = Arrays.asList(query.getFilterQueries());
            assertTrue(filters.contains("sigla:(\"ABA001\" OR \"BOA001\" OR \"A\\:B\")"));
            assertTrue(filters.contains("organization:\"Library\\ A\""));
            assertTrue(filters.contains("{!collapse field=pid sort='date asc,source asc'}"));
        }};
    }

    @Test
    void offersSiglasFromIngestHistoryWithOrganizationScope() throws Exception {
        FacetField siglas = new FacetField("sigla");
        siglas.add("ABA001", 2); siglas.add("BOA001", 1);
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getFacetField("sigla"); result = siglas;
        }};
        assertEquals(List.of("ABA001", "BOA001"), new SolrLogSearchView(client).getSiglas("Library A"));
        new Verifications() {{
            SolrQuery query; client.query(query = withCapture());
            assertEquals("operation:INGEST", query.getQuery());
            assertEquals(-1, query.getFacetLimit());
            assertTrue(Arrays.asList(query.getFilterQueries()).contains("organization:\"Library\\ A\""));
        }};
    }

    @Test
    void returnsLongCountsAndIncludesUnknownModelsInBreakdown() throws Exception {
        results(3_000_000_012L,
                List.of(fields("val", "model:page", "objects", 3_000_000_000L),
                        fields("val", "model:ndkmonograph", "objects", 10L)), 2);
        var statistics = new SolrLogSearchView(client).getCreatedObjectsStatistics(null, day, day, null, null);
        assertEquals(3_000_000_012L, statistics.getCount());
        assertEquals(3_000_000_000L, statistics.getModelCounts().get("model:page"));
        assertEquals(10L, statistics.getModelCounts().get("model:ndkmonograph"));
        assertEquals(2L, statistics.getModelCounts().get(SolrLogSearchView.UNKNOWN_MODEL));
        assertEquals(statistics.getCount(), statistics.getModelCounts().values().stream().mapToLong(Long::longValue).sum());
    }

    @Test
    void mergesAllValuesWithoutExactModelPrefixAndMissingValuesIntoUnknownModel() throws Exception {
        results(3_000_000_026L, List.of(
                fields("val", "model:page", "objects", 12L),
                fields("val", "model:custom", "objects", 3L),
                fields("val", "device:scanner", "objects", 3_000_000_000L),
                fields("val", "page", "objects", 2L),
                fields("val", "Model:page", "objects", 1L),
                fields("val", " model:page", "objects", 1L),
                fields("val", "", "objects", 1L),
                fields("val", "__unknown__", "objects", 1L),
                fields("val", "prefix-model:page", "objects", 1L)), 4);
        var statistics = new SolrLogSearchView(client).getCreatedObjectsStatistics(null, day, day, null, null);
        assertEquals(3, statistics.getModelCounts().size());
        assertEquals(12L, statistics.getModelCounts().get("model:page"));
        assertEquals(3L, statistics.getModelCounts().get("model:custom"));
        assertEquals(3_000_000_011L, statistics.getModelCounts().get(SolrLogSearchView.UNKNOWN_MODEL));
        assertEquals(statistics.getCount(), statistics.getModelCounts().values().stream().mapToLong(Long::longValue).sum());
    }

    @Test
    void countsAllActorsForSingleDayAcrossDaylightSavingChange() throws Exception {
        TimeZone.setDefault(TimeZone.getTimeZone("Europe/Prague"));
        results(0, List.of(), 0);
        LocalDate date = LocalDate.of(2025, 3, 30);
        var statistics = new SolrLogSearchView(client).getCreatedObjectsStatistics(List.of(" "), date, date, List.of(), null);
        assertEquals(0, statistics.getCount());
        assertTrue(statistics.getModelCounts().isEmpty());
        new Verifications() {{
            SolrQuery query;
            client.query(query = withCapture());
            List<String> filters = Arrays.asList(query.getFilterQueries());
            assertTrue(filters.contains("date:[\"2025-03-29T23:00:00.000Z\" TO \"2025-03-30T22:00:00.000Z\"}"));
            assertFalse(filters.stream().anyMatch(filter -> filter.startsWith("user:")));
        }};
    }

    @Test
    void rejectsMissingAndReversedDatesWithoutQuerying() throws Exception {
        SolrLogSearchView search = new SolrLogSearchView(client);
        assertThrows(IllegalArgumentException.class, () -> search.getCreatedObjectsStatistics(null, null, day, null, null));
        assertThrows(IllegalArgumentException.class, () -> search.getCreatedObjectsStatistics(null, day.plusDays(1), day, null, null));
        new Verifications() {{ client.query((SolrQuery) any); times = 0; }};
    }

    @Test
    void reportsLoggingFailureInsteadOfFallingBackToSearchOrReturningZero() throws Exception {
        new Expectations() {{ client.query((SolrQuery) any); result = new SolrServerException("unavailable"); }};
        assertThrows(IOException.class, () -> new SolrLogSearchView(client).getCreatedObjectsStatistics(null, day, day, null, null));
    }

    private SolrDocumentList nonEmptyResults() {
        SolrDocumentList data = new SolrDocumentList(); data.setNumFound(10); return data;
    }

    @Test
    void reportsIncompleteFacetResponseInsteadOfReturningZero() throws Exception {
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getResults(); result = nonEmptyResults();
            response.getFacetField("model"); result = null;
        }};
        assertThrows(IOException.class, () -> new SolrLogSearchView(client).getCreatedObjectsStatistics(null, day, day, null, null));
    }
}
