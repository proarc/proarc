package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocumentList;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SolrResourceTypeSearchTest {
    @Mocked SolrClient client;
    @Mocked QueryResponse response;

    private SolrSearchView search() throws Exception {
        SolrDocumentList documents = new SolrDocumentList();
        documents.setNumFound(250);
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getResults(); result = documents;
        }};
        return new SolrSearchView(null, client);
    }

    @Test
    void advancedRowsAndCountsUseSameResourceFilterWithOtherRestrictions() throws Exception {
        SolrSearchView search = search();
        for (String model : Arrays.asList(null, NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_MONOGRAPHUNIT)) {
            for (String resource : List.of("text", "cartographic", "still image", "notated music")) {
                for (boolean active : List.of(true, false)) {
                    search.findAdvancedSearchItems(active, null, "title", "owner", "new", "Library", null,
                            model, "NO_PARENT", "modified", "desc", 100, 100, resource);
                    assertEquals(250, search.findAdvancedSearchCount(active, null, "title", "owner", "new",
                            "Library", null, model, "NO_PARENT", resource));
                }
            }
        }
        new Verifications() {{
            List<SolrQuery> queries = new ArrayList<>();
            client.query(withCapture(queries)); times = 48;
            for (int index = 0; index < queries.size(); index += 2) {
                SolrQuery rows = queries.get(index);
                SolrQuery count = queries.get(index + 1);
                assertArrayEquals(rows.getFilterQueries(), count.getFilterQueries());
                assertEquals(100, rows.getStart());
                assertEquals(100, rows.getRows());
                assertEquals(0, count.getRows());
                List<String> filters = Arrays.asList(rows.getFilterQueries());
                assertEquals(1, filters.stream().filter(value -> value.startsWith("typeOfResource:")).count());
                assertTrue(filters.contains("owner:\"owner\""));
                assertTrue(filters.contains("organization:\"Library\""));
                assertTrue(filters.contains("status:\"new\""));
                assertTrue(filters.contains("parentPid:\"NO_PARENT\""));
            }
        }};
    }

    @Test
    void phraseCountUsesFulltextAndIdenticalResourceFilterForPagination() throws Exception {
        SolrSearchView search = search();
        search.findPhrase("music", null, null, null, NdkPlugin.MODEL_MONOGRAPHUNIT, "modified", "desc", 100, 100, "notated music");
        assertEquals(250, search.findPhraseCount("music", null, null, null, NdkPlugin.MODEL_MONOGRAPHUNIT, "notated music"));
        new Verifications() {{
            List<SolrQuery> queries = new ArrayList<>();
            client.query(withCapture(queries)); times = 2;
            assertEquals(queries.get(0).getQuery(), queries.get(1).getQuery());
            assertArrayEquals(queries.get(0).getFilterQueries(), queries.get(1).getFilterQueries());
            assertEquals("fulltext", queries.get(1).get("df"));
            assertEquals(0, queries.get(1).getRows());
        }};
    }

    @Test
    void phraseFilterQuotesAndEscapesResourceValuesWithoutChangingFulltextQuery() throws Exception {
        SolrSearchView search = search();
        String resource = "notated music\" OR *:*";
        search.findPhrase("music", null, null, null, null, "modified", "desc", 100, 100, resource);
        new Verifications() {{
            SolrQuery query; client.query(query = withCapture()); times = 1;
            assertEquals("music", query.getQuery());
            assertEquals("fulltext", query.get("df"));
            assertTrue(Arrays.asList(query.getFilterQueries()).contains(
                    "typeOfResource:\"" + ClientUtils.escapeQueryChars(resource) + "\""));
            assertEquals(100, query.getStart());
        }};
    }

    @Test
    void missingOrBlankResourceKeepsUnfilteredSearchCompatible() throws Exception {
        SolrSearchView search = search();
        search.findAdvancedSearchItems(true, null, null, null, null, null, null, null, null, null, "desc", 0, 100, null);
        search.findAdvancedSearchItems(true, null, null, null, null, null, null, null, null, null, "desc", 0, 100, " ");
        search.findAdvancedSearchCount(true, null, null, null, null, null, null, null, null, "");
        search.findPhrase(null, null, null, null, null, null, "desc", 0, 100, null);
        new Verifications() {{
            List<SolrQuery> queries = new ArrayList<>();
            client.query(withCapture(queries)); times = 4;
            for (SolrQuery query : queries) {
                assertFalse(Arrays.stream(query.getFilterQueries()).anyMatch(value -> value.startsWith("typeOfResource:")));
            }
        }};
    }

    @Test
    void identifierBatchesRetainResourceFilter() throws Exception {
        SolrSearchView search = search();
        String identifiers = IntStream.range(0, 60).mapToObj(index -> "uuid:" + index).collect(Collectors.joining(","));
        search.findAdvancedSearchItems(true, identifiers, null, null, null, null, null,
                NdkPlugin.MODEL_MONOGRAPHUNIT, null, null, "desc", 0, 100, "cartographic");
        new Verifications() {{
            List<SolrQuery> queries = new ArrayList<>();
            client.query(withCapture(queries)); minTimes = 2;
            for (SolrQuery query : queries) {
                assertTrue(Arrays.asList(query.getFilterQueries()).contains("typeOfResource:\"cartographic\""));
            }
        }};
    }
}
