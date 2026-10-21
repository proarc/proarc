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
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class SolrExportStatisticsTest {
    @Mocked SolrClient client;
    @Mocked QueryResponse response;

    private SolrExportStatistics.Filters filters(String group, boolean unique) {
        return new SolrExportStatistics.Filters(LocalDate.of(2026, 1, 1), LocalDate.of(2026, 10, 20),
                List.of("actor"), List.of(), List.of(), List.of(), List.of(), "Library A", group, unique);
    }

    private SolrDocument doc(String source, String pid, String date, String type, long pages) {
        SolrDocument doc = new SolrDocument();
        doc.setField("source", source); doc.setField("pid", pid); doc.setField("date", date);
        doc.setField("exportType", type); doc.setField("model", "model:ndkmonographunit");
        doc.setField("pageCount", pages); doc.setField("otherModelCount", 2L);
        return doc;
    }

    private void data(SolrDocument... documents) throws Exception {
        SolrDocumentList list = new SolrDocumentList();
        list.addAll(Arrays.asList(documents));
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getResults(); result = list;
            response.getNextCursorMark(); result = "*";
        }};
    }

    @Test
    void listsArticleExportsWithPaginationAndHistoricalDoi() throws Exception {
        SolrDocument doc = doc("article-log", "article", "2026-01-01T00:00:00.000Z", "exportProfile.crossref", 0);
        doc.setField("model", "model:ndkarticle"); doc.setField("doi", List.of("10.1234/article"));
        SolrDocumentList documents = new SolrDocumentList(); documents.add(doc); documents.setNumFound(101);
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getResults(); result = documents;
        }};
        var result = new SolrExportStatistics(client).articles(filters("model", false), 50, 50);
        assertEquals(101, result.getCount()); assertEquals(50, result.getOffset());
        assertEquals(List.of("10.1234/article"), result.getArticles().get(0).dois());
        assertEquals("exportProfile.crossref", result.getArticles().get(0).exportType());
        new Verifications() {{
            SolrQuery query; client.query(query = withCapture());
            assertEquals("operation:EXPORT", query.getQuery());
            assertEquals(50, query.getStart()); assertEquals(50, query.getRows());
            String filters = String.join(" ", query.getFilterQueries());
            assertTrue(filters.contains("ndkarticle")); assertTrue(filters.contains("ndkearticle")); assertTrue(filters.contains("bdmarticle"));
            assertTrue(filters.contains("organization:"));
            assertFalse(filters.contains("{!collapse"));
        }};
    }

    @Test
    void uniqueArticlesCollapseByPidBeforePaginationWithinSelectedOrganization() throws Exception {
        SolrDocument article = doc("latest", "article", "2026-02-01T00:00:00.000Z", "exportProfile.crossref", 0);
        article.setField("model", "model:ndkarticle");
        SolrDocumentList documents = new SolrDocumentList(); documents.add(article); documents.setNumFound(51);
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getResults(); result = documents;
        }};
        var result = new SolrExportStatistics(client).articles(filters("model", true), 50, 50);
        assertEquals(51, result.getCount());
        assertEquals(true, result.getFilters().get("unique"));
        assertEquals("Library A", result.getFilters().get("organization"));
        new Verifications() {{
            SolrQuery query; client.query(query = withCapture());
            List<String> fq = Arrays.asList(query.getFilterQueries());
            assertTrue(fq.contains("{!collapse field=pid sort='date desc,source desc'}"));
            assertTrue(fq.contains("organization:(\"Library\\ A\")"));
            assertFalse(fq.stream().anyMatch(value -> value.startsWith("-model:")));
            assertEquals(50, query.getStart()); assertEquals(50, query.getRows());
        }};
    }

    @Test
    void excludesAllArticleModelsFromPackageStatisticsAndFilterOptions() throws Exception {
        data();
        var stats = new SolrExportStatistics(client);
        stats.calculate(filters("model", false));
        stats.options(null);
        new Verifications() {{
            List<SolrQuery> queries = new java.util.ArrayList<>(); client.query(withCapture(queries));
            assertEquals(2, queries.size());
            for (SolrQuery query : queries) {
                String exclusion = Arrays.stream(query.getFilterQueries())
                        .filter(fq -> fq.startsWith("-model:")).findFirst().orElseThrow();
                for (String model : cz.cas.lib.proarc.common.process.AuditLog.ARTICLE_MODELS) {
                    assertTrue(exclusion.contains(org.apache.solr.client.solrj.util.ClientUtils.escapeQueryChars(model)));
                }
            }
        }};
    }

    @Test
    void articleFilterOptionsUseArticleHistoryWithinOrganization() throws Exception {
        new Expectations() {{ client.query((SolrQuery) any); result = response; }};
        new SolrExportStatistics(client).options("Library A", true);
        new Verifications() {{
            SolrQuery query; client.query(query = withCapture());
            List<String> fq = Arrays.asList(query.getFilterQueries());
            assertTrue(fq.contains("organization:(\"Library\\ A\")"));
            assertFalse(fq.stream().anyMatch(value -> value.startsWith("-model:")));
            String models = fq.stream().filter(value -> value.startsWith("model:")).findFirst().orElseThrow();
            assertTrue(models.contains("ndkarticle")); assertTrue(models.contains("ndkearticle"));
            assertTrue(models.contains("bdmarticle"));
        }};
    }

    @Test
    void articleStatisticsRejectInvalidPaginationAndPartialResponses() throws Exception {
        var stats = new SolrExportStatistics(client);
        assertThrows(IllegalArgumentException.class, () -> stats.articles(filters("model", false), -1, 50));
        assertThrows(IllegalArgumentException.class, () -> stats.articles(filters("model", false), 0, 501));
        var header = new org.apache.solr.common.util.NamedList<Object>(); header.add("partialResults", true);
        new Expectations() {{ client.query((SolrQuery) any); result = response; response.getResponseHeader(); result = header; }};
        assertThrows(IOException.class, () -> stats.articles(filters("model", false), 0, 50));
    }

    @Test
    void modelGroupingCombinesExportTypesAndEquipmentAndDeduplicatesAcrossThem() throws Exception {
        SolrDocument older = doc("a", "root", "2026-01-01T00:00:00.000Z", "NDK", 10);
        older.setField("device", "device:a"); older.setField("software", "software:a");
        SolrDocument latest = doc("b", "root", "2026-02-01T00:00:00.000Z", "K4", 20);
        latest.setField("device", "device:b"); latest.setField("software", "software:b");
        SolrDocument another = doc("c", "another", "2026-01-01T00:00:00.000Z", "NDK", 5);
        data(older, latest, another);
        var unique = new SolrExportStatistics(client).calculate(filters("model", true));
        assertEquals(1, unique.getGroups().size());
        assertEquals(2, unique.getCount()); assertEquals(25, unique.getPageCount());
        assertEquals(4, unique.getOtherModelCount());
        assertNull(unique.getGroups().get(0).getExportType());
        assertTrue(unique.getGroups().get(0).getDevices().isEmpty());
        assertTrue(unique.getGroups().get(0).getSoftware().isEmpty());
        var repeated = new SolrExportStatistics(client).calculate(filters("model", false));
        assertEquals(1, repeated.getGroups().size());
        assertEquals(3, repeated.getCount()); assertEquals(35, repeated.getPageCount());
        assertEquals(6, repeated.getOtherModelCount());
    }

    @Test
    void sumsRepeatedExportsAndSeparatesTypesAndModels() throws Exception {
        SolrDocument otherModel = doc("d", "root2", "2026-02-01T00:00:00.000Z", "NDK", 5);
        otherModel.setField("model", "device:scanner");
        data(doc("a", "root", "2026-01-01T00:00:00.000Z", "NDK", 10),
                doc("b", "root", "2026-01-02T00:00:00.000Z", "NDK", 15),
                doc("c", "root", "2026-01-03T00:00:00.000Z", "K4", 20), otherModel);
        var result = new SolrExportStatistics(client).calculate(filters("exportType", false));
        assertEquals(4, result.getCount()); assertEquals(50, result.getPageCount());
        assertEquals(8, result.getOtherModelCount()); assertEquals(3, result.getGroups().size());
        assertTrue(result.getGroups().stream().anyMatch(group -> SolrLogSearchView.UNKNOWN_MODEL.equals(group.getModel())));
    }

    @Test
    void uniqueUsesLatestMatchingExportPerPidAndGroupEvenWhenSourceOrderDiffers() throws Exception {
        data(doc("a", "root", "2026-02-01T00:00:00.000Z", "NDK", 30),
                doc("b", "root", "2026-01-01T00:00:00.000Z", "NDK", 10),
                doc("c", "root", "2026-02-01T00:00:00.000Z", "NDK", 40),
                doc("d", "root", "2026-02-01T00:00:00.000Z", "K4", 20));
        var result = new SolrExportStatistics(client).calculate(filters("exportType", true));
        assertEquals(2, result.getCount()); assertEquals(60, result.getPageCount());
        assertEquals(4, result.getOtherModelCount());
    }

    @Test
    void combinedEquipmentGroupsWholeSetsOnceAndRequiresBothFilters() throws Exception {
        SolrDocument first = doc("a", "root1", "2026-01-01T00:00:00.000Z", "NDK", 10);
        first.setField("device", List.of("device:b", "device:a")); first.setField("software", List.of("software:a"));
        SolrDocument second = doc("b", "root2", "2026-01-02T00:00:00.000Z", "NDK", 15);
        second.setField("device", List.of("device:a", "device:b")); second.setField("software", "software:a");
        data(first, second);
        var f = new SolrExportStatistics.Filters(LocalDate.of(2026, 1, 1), LocalDate.of(2026, 10, 20),
                List.of("actor"), List.of("model:page"), List.of("NDK"), List.of("device:a"), List.of("software:a"), "Library A", "equipment", false);
        var result = new SolrExportStatistics(client).calculate(f);
        assertEquals(1, result.getGroups().size()); assertEquals(25, result.getPageCount());
        assertEquals(List.of("device:a", "device:b"), result.getGroups().get(0).getDevices());
        assertEquals(List.of("software:a"), result.getGroups().get(0).getSoftware());
        new Verifications() {{
            SolrQuery query; client.query(query = withCapture());
            assertEquals("operation:EXPORT", query.getQuery());
            List<String> fq = Arrays.asList(query.getFilterQueries());
            assertTrue(fq.contains("device:(\"device\\:a\")"));
            assertTrue(fq.contains("software:(\"software\\:a\")"));
            assertTrue(fq.contains("user:(\"actor\")"));
            assertTrue(fq.contains("model:(\"model\\:page\")"));
            assertTrue(fq.contains("exportType:(\"NDK\")"));
            assertTrue(fq.contains("organization:(\"Library\\ A\")"));
            assertFalse(fq.stream().anyMatch(value -> value.startsWith("owner:") || value.startsWith("created:")));
        }};
    }

    @Test
    void deviceOnlyIgnoresSoftwareForGroupingAndSoftwareOnlyIgnoresDevices() throws Exception {
        SolrDocument doc = doc("a", "root", "2026-01-01T00:00:00.000Z", "NDK", 10);
        doc.setField("device", "device:a"); doc.setField("software", "software:a"); data(doc);
        var devices = new SolrExportStatistics(client).calculate(filters("device", false));
        assertEquals(List.of("device:a"), devices.getGroups().get(0).getDevices());
        assertTrue(devices.getGroups().get(0).getSoftware().isEmpty());
        var software = new SolrExportStatistics(client).calculate(filters("software", false));
        assertEquals(List.of("software:a"), software.getGroups().get(0).getSoftware());
        assertTrue(software.getGroups().get(0).getDevices().isEmpty());
    }

    @Test
    void marksMissingHistoricalCountsAndSupportsLongSums() throws Exception {
        SolrDocument missing = doc("b", "root2", "2026-01-01T00:00:00.000Z", "NDK", 10);
        missing.removeFields("pageCount");
        data(doc("a", "root", "2026-01-01T00:00:00.000Z", "NDK", 3_000_000_000L), missing);
        var result = new SolrExportStatistics(client).calculate(filters("exportType", false));
        assertEquals(3_000_000_000L, result.getPageCount()); assertEquals(1, result.getIncompleteCount());
        assertEquals(2, result.getCount());
    }

    @Test
    void readsAllCursorPagesBeforeFinishingUniqueCounts() throws Exception {
        SolrDocumentList first = new SolrDocumentList();
        first.add(doc("a", "root", "2026-01-01T00:00:00.000Z", "NDK", 10));
        SolrDocumentList second = new SolrDocumentList();
        second.add(doc("b", "root", "2026-02-01T00:00:00.000Z", "NDK", 20));
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getResults(); returns(first, second);
            response.getNextCursorMark(); returns("next", "next");
        }};
        var result = new SolrExportStatistics(client).calculate(filters("exportType", true));
        assertEquals(1, result.getCount()); assertEquals(20, result.getPageCount());
    }

    @Test
    void failsOnUnavailableLoggingCore() throws Exception {
        new Expectations() {{ client.query((SolrQuery) any); result = new SolrServerException("offline"); }};
        assertThrows(IOException.class, () -> new SolrExportStatistics(client).calculate(filters("device", false)));
    }

    @Test
    void failsOnMissingCursorInsteadOfReturningIncompleteTotals() throws Exception {
        SolrDocumentList empty = new SolrDocumentList();
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getResults(); result = empty;
            response.getNextCursorMark(); result = null;
        }};
        assertThrows(IOException.class, () -> new SolrExportStatistics(client).calculate(filters("device", false)));
    }

    @Test
    void rejectsPartialSolrResults() throws Exception {
        org.apache.solr.common.util.NamedList<Object> header = new org.apache.solr.common.util.NamedList<>();
        header.add("partialResults", true);
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getResponseHeader(); result = header;
        }};
        assertThrows(IOException.class, () -> new SolrExportStatistics(client).calculate(filters("exportType", false)));
    }

    @Test
    void filtersByMultipleSiglasAndReturnsNormalizedSelection() throws Exception {
        data();
        var f = new SolrExportStatistics.Filters(LocalDate.of(2026, 1, 1), LocalDate.of(2026, 10, 20),
                List.of(), List.of(), List.of(), List.of(), List.of(),
                Arrays.asList(" ABA001 ", "BOA001", "ABA001", "", null, "A:B"), "Library A", "model", true);
        var result = new SolrExportStatistics(client).calculate(f);
        assertEquals(List.of("ABA001", "BOA001", "A:B"), result.getFilters().get("siglas"));
        new Verifications() {{
            SolrQuery query; client.query(query = withCapture());
            List<String> fq = Arrays.asList(query.getFilterQueries());
            assertTrue(fq.contains("sigla:(\"ABA001\" OR \"BOA001\" OR \"A\\:B\")"));
            assertTrue(fq.contains("organization:(\"Library\\ A\")"));
        }};
    }

    @Test
    void returnsEmptyStatisticsWithTheOriginalFilters() throws Exception {
        data();
        var result = new SolrExportStatistics(client).calculate(filters("equipment", true));
        assertTrue(result.getGroups().isEmpty()); assertEquals(0, result.getCount());
        assertEquals(0, result.getPageCount()); assertEquals(0, result.getOtherModelCount());
        assertEquals(true, result.getFilters().get("unique"));
        assertEquals("equipment", result.getFilters().get("groupBy"));
        assertEquals(List.of(), result.getFilters().get("siglas"));
        new Verifications() {{
            SolrQuery query; client.query(query = withCapture());
            assertFalse(Arrays.stream(query.getFilterQueries()).anyMatch(fq -> fq.startsWith("sigla:")));
        }};
    }

    @Test
    void rejectsInvalidGroupingAndDates() {
        assertThrows(IllegalArgumentException.class, () -> filters("invalid", true));
        assertThrows(IllegalArgumentException.class, () -> new SolrExportStatistics.Filters(LocalDate.of(2026, 2, 1),
                LocalDate.of(2026, 1, 1), null, null, null, null, null, null, "device", true));
    }
}
