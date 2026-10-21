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
import java.util.Arrays;
import java.util.Map;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.*;

class ModelStatisticsTest {

    @Mocked SolrClient client;
    @Mocked QueryResponse response;

    @ParameterizedTest
    @CsvSource({"true,Library A", "true,", "false,Library A", "false,"})
    void countsAllModelsWithOptionalOrganizationAndState(boolean active, String organization) throws Exception {
        FacetField models = new FacetField(SolrUtils.FIELD_MODEL);
        models.add("model:page", 3_000_000_000L);
        models.add("model:ndkmonograph", 12L);
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getFacetField(SolrUtils.FIELD_MODEL); result = models;
        }};

        Map<String, Long> counts = new SolrSearchView(null, client).countObjectsByModel(organization, active);

        assertEquals(3_000_000_000L, counts.get("model:page"));
        assertEquals(12L, counts.get("model:ndkmonograph"));
        new Verifications() {{
            SolrQuery query;
            client.query(query = withCapture()); times = 1;
            assertEquals(0, query.getRows());
            assertEquals(-1, query.getFacetLimit());
            assertEquals(1, query.getFacetMinCount());
            assertTrue(Arrays.asList(query.getFilterQueries()).contains(active ? "state:\"Active\"" : "state:\"Deactive\""));
            if (organization == null) {
                assertFalse(Arrays.stream(query.getFilterQueries()).anyMatch(filter -> filter.startsWith("organization:")));
            } else {
                assertTrue(Arrays.asList(query.getFilterQueries()).contains("organization:\"Library\\ A\""));
            }
            assertTrue(Arrays.stream(query.getFilterQueries()).anyMatch(filter -> filter.startsWith("-model:")));
        }};
    }

    @Test
    void handlesEmptyIndexAndUnrestrictedVisibility() throws Exception {
        new Expectations() {{
            client.query((SolrQuery) any); result = response;
            response.getFacetField(SolrUtils.FIELD_MODEL); result = null;
        }};
        assertTrue(new SolrSearchView(null, client).countObjectsByModel(null, true).isEmpty());
        new Verifications() {{
            SolrQuery query;
            client.query(query = withCapture());
            assertFalse(Arrays.stream(query.getFilterQueries()).anyMatch(filter -> filter.startsWith("organization:")));
        }};
    }

    @Test
    void reportsSearchFailureInsteadOfReturningZeroCounts() throws Exception {
        new Expectations() {{
            client.query((SolrQuery) any); result = new SolrServerException("unavailable");
        }};
        assertThrows(IOException.class, () -> new SolrSearchView(null, client).countObjectsByModel(null, false));
    }

}
