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
package cz.cas.lib.proarc.webapp.server.rest;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.storage.akubra.SolrExportStatistics;
import java.time.LocalDate;
import java.util.List;
import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.common.util.NamedList;
import org.apache.solr.client.solrj.SolrClient;


import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class ExportStatisticResponseTest {



    @Test
    void serializesPackageRowsWithHistoricalMetadataAndPagination() throws Exception {
        SolrDocument doc = new SolrDocument();
        doc.setField("pid", "uuid:book"); doc.setField("source", "package-event");
        doc.setField("date", "2026-02-01T00:00:00.000Z"); doc.setField("model", "model:ndkmonographunit");
        doc.setField("urnNbn", List.of("urn:nbn:cz:test")); doc.setField("label", "Book");
        SolrDocumentList records = new SolrDocumentList(); records.add(doc); records.setNumFound(101);
        var filters = new SolrExportStatistics.Filters(LocalDate.of(2026, 1, 1), LocalDate.of(2026, 10, 20),
                List.of(), List.of(), List.of("exportProfile.ndk"), List.of(), List.of(), null, "model", true);
        var result = new SolrExportStatistics(client(records, new NamedList<>())).packages(filters, 50, 50);
        ObjectMapper mapper = new JacksonProvider().getContext(ProArcResponse.class);
        JsonNode value = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(result)))
                .path("response").path("data").get(0);
        assertEquals("Book", value.path("packages").get(0).path("label").asText());
        assertEquals("urn:nbn:cz:test", value.path("packages").get(0).path("urnNbns").get(0).asText());
        assertEquals(101, value.path("count").asLong()); assertEquals(50, value.path("offset").asInt());
        assertTrue(value.path("filters").path("unique").asBoolean());
    }

    @Test
    void serializesHistoricalOptionsAsOneResponseObject() throws Exception {
        org.apache.solr.client.solrj.response.FacetField model = new org.apache.solr.client.solrj.response.FacetField("model");
        model.add("model:page", 1); model.add("device:scanner", 1);
        org.apache.solr.client.solrj.response.FacetField software = new org.apache.solr.client.solrj.response.FacetField("software");
        software.add("software:deleted", 1);
        NamedList<Object> facets = new NamedList<>();
        NamedList<Object> models = new NamedList<>(); models.add("model:page", 1); models.add("device:scanner", 1);
        models.add("model:ndkarticle", 1); models.add("model:ndkearticle", 1); models.add("model:bdmarticle", 1);
        NamedList<Object> softwareValues = new NamedList<>(); softwareValues.add("software:deleted", 1);
        facets.add("model", models); facets.add("software", softwareValues);
        NamedList<Object> siglas = new NamedList<>(); siglas.add("ABA001", 2);
        facets.add("sigla", siglas);
        var options = new SolrExportStatistics(client(new SolrDocumentList(), facets)).options(null);
        ObjectMapper mapper = new JacksonProvider().getContext(ProArcResponse.class);
        JsonNode data = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(options)))
                .path("response").path("data");
        assertEquals(1, data.size());
        assertEquals(1, data.get(0).path("model").size());
        assertEquals("software:deleted", data.get(0).path("software").get(0).asText());
        assertTrue(data.get(0).path("user").isArray());
        assertEquals("ABA001", data.get(0).path("sigla").get(0).asText());
    }

    @Test
    void serializesGroupsLongTotalsAndExactFiltersForCsv() throws Exception {
        SolrDocument doc = new SolrDocument();
        doc.setField("pid", "root"); doc.setField("source", "export-42-root");
        doc.setField("date", "2026-02-01T00:00:00.000Z"); doc.setField("model", "model:page");
        doc.setField("pageCount", 3_000_000_000L); doc.setField("otherModelCount", 0L);
        doc.setField("device", List.of("device:a")); doc.setField("software", List.of("software:a"));
        SolrDocumentList data = new SolrDocumentList(); data.add(doc);
        var filters = new SolrExportStatistics.Filters(LocalDate.of(2026, 1, 1), LocalDate.of(2026, 10, 20),
                List.of("actor"), List.of(), List.of(), List.of("device:a"), List.of("software:a"), List.of("ABA001"), "Library A", "equipment", true);
        var result = new SolrExportStatistics(client(data, new NamedList<>())).calculate(filters);
        ObjectMapper mapper = new JacksonProvider().getContext(ProArcResponse.class);
        JsonNode value = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(result)))
                .path("response").path("data").get(0);
        assertEquals(3_000_000_000L, value.path("pageCount").asLong());
        assertEquals(0L, value.path("otherModelCount").asLong(-1));
        assertEquals("device:a", value.path("groups").get(0).path("devices").get(0).asText());
        assertEquals("software:a", value.path("groups").get(0).path("software").get(0).asText());
        assertTrue(value.path("filters").path("unique").asBoolean());
        assertEquals("equipment", value.path("filters").path("groupBy").asText());
        assertEquals("Library A", value.path("filters").path("organization").asText());
        assertTrue(value.path("filters").path("models").isArray());
        assertEquals("ABA001", value.path("filters").path("siglas").get(0).asText());
    }
    @Test
    void serializesArticleDoiExportTypeAndMissingDoiAsArray() throws Exception {
        SolrDocument article = new SolrDocument();
        article.setField("source", "article-42"); article.setField("pid", "uuid:article");
        article.setField("model", "model:ndkarticle"); article.setField("doi", List.of("10.1234/test"));
        article.setField("exportType", "exportProfile.crossref"); article.setField("date", "2026-01-01T00:00:00.000Z");
        SolrDocument missingDoi = new SolrDocument(); missingDoi.putAll(article);
        missingDoi.setField("source", "article-43"); missingDoi.removeFields("doi");
        SolrDocumentList documents = new SolrDocumentList(); documents.add(article); documents.add(missingDoi); documents.setNumFound(2);
        var filters = new SolrExportStatistics.Filters(LocalDate.of(2026, 1, 1), LocalDate.of(2026, 10, 20),
                List.of(), List.of(), List.of("exportProfile.crossref"), List.of(), List.of(), null, "model", false);
        var result = new SolrExportStatistics(client(documents, new NamedList<>())).articles(filters, 0, 50);
        ObjectMapper mapper = new JacksonProvider().getContext(ProArcResponse.class);
        JsonNode value = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(result)))
                .path("response").path("data").get(0);
        assertEquals(2, value.path("count").asInt());
        assertEquals("uuid:article", value.path("articles").get(0).path("pid").asText());
        assertEquals("10.1234/test", value.path("articles").get(0).path("dois").get(0).asText());
        assertEquals("exportProfile.crossref", value.path("articles").get(0).path("exportType").asText());
        assertTrue(value.path("articles").get(1).path("dois").isArray());
        assertTrue(value.path("articles").get(1).path("dois").isEmpty());
    }

    private SolrClient client(SolrDocumentList data, NamedList<Object> facets) {
        NamedList<Object> payload = new NamedList<>();
        payload.add("responseHeader", new NamedList<>());
        payload.add("response", data);
        payload.add("nextCursorMark", "*");
        NamedList<Object> counts = new NamedList<>(); counts.add("facet_fields", facets);
        payload.add("facet_counts", counts);
        return new SolrClient() {
            @Override public NamedList<Object> request(SolrRequest request, String collection) { return payload; }
            @Override public void close() { }
        };
    }}
