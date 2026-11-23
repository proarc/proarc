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
package cz.cas.lib.proarc.common.externalApp.ltp;

import cz.cas.lib.proarc.common.storage.relation.Rdf;
import cz.cas.lib.proarc.common.storage.relation.Relations;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.List;
import javax.xml.transform.stream.StreamSource;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class PreservationIndexTest {
    @Test void onlyUnfinishedStorageIsPendingRegardlessOfCatalogState() throws Exception {
        for (ArchivInfo.State state : ArchivInfo.State.values()) {
            for (String catalogId : new String[]{null, "", "verbis"}) {
                for (ArchivInfo.CatalogState catalogState : ArchivInfo.CatalogState.values()) {
                    ArchivInfo record = PreservationServiceTest.request("one");
                    record.setState(state);
                    record.setCatalogId(catalogId);
                    record.setCatalogState(catalogState);
                    SolrInputDocument doc = new SolrInputDocument();
                    PreservationIndex.addFields(doc, record, true);
                    assertEquals(state != ArchivInfo.State.STORED && state != ArchivInfo.State.FAILED,
                            doc.getFieldValue(PreservationIndex.PENDING), state + "/" + catalogId + "/" + catalogState);
                }
            }
        }
    }

    @Test void relsExtCurrentPackageRoundTripsAndProducesSearchableState() throws Exception {
        ArchivInfo current = PreservationServiceTest.request("new");
        current.setPackageId("23"); current.setVersion(2); current.setState(ArchivInfo.State.UPLOADED);
        Rdf rdf = new Rdf("uuid:unit"); rdf.getDescription().setArchivInfo(current);
        StringWriter xml = new StringWriter(); Relations.defaultMarshaller(true).marshal(rdf, xml);
        assertTrue(xml.toString().contains("parseType=\"Resource\""));
        Rdf restored = Relations.unmarshal(new StreamSource(new StringReader(xml.toString())), Rdf.class);
        assertEquals("23", restored.getDescription().getArchivInfo().getPackageId());
        SolrInputDocument doc = new SolrInputDocument();
        PreservationIndex.addFields(doc, restored.getDescription().getArchivInfo(), true);
        assertEquals(List.of("cesnet:UPLOADED"), List.copyOf(doc.getFieldValues(PreservationIndex.STATES)));
        assertEquals(true, doc.getFieldValue(PreservationIndex.PENDING));
        ArchivInfo json = PreservationIndex.read(doc.getFieldValue(PreservationIndex.RECORDS).toString());
        assertEquals("23", json.getPackageId());
        assertEquals("23", doc.getFieldValue(PreservationIndex.ID));
        assertEquals("cesnet:23", doc.getFieldValue(PreservationIndex.PROVIDER_ID));
        assertFalse(doc.getFieldValue(PreservationIndex.RECORDS).toString().contains("password"));
    }

    @Test void supersededPendingPackageDoesNotKeepCurrentPackageInCheckQueue() throws Exception {
        ArchivInfo old = PreservationServiceTest.request("old");
        old.setPackageId("12"); old.setState(ArchivInfo.State.UPLOADED);
        ArchivInfo current = PreservationServiceTest.request("new");
        current.setPackageId("23"); current.setState(ArchivInfo.State.STORED);
        current.setCatalogState(ArchivInfo.CatalogState.DONE);
        SolrInputDocument doc = new SolrInputDocument();
        PreservationIndex.addFields(doc, PreservationIndex.read(new com.fasterxml.jackson.databind.ObjectMapper().writeValueAsString(List.of(old, current))), true);
        assertEquals(false, doc.getFieldValue(PreservationIndex.PENDING));
        assertEquals("cesnet:STORED", doc.getFieldValue(PreservationIndex.STATES));
        assertEquals("23", doc.getFieldValue(PreservationIndex.ID));
    }

    @Test void atomicUpdateClearsPreviousIdWhenReplacementIsNotRegisteredYet() throws Exception {
        ArchivInfo registering = PreservationServiceTest.request("new");
        registering.setState(ArchivInfo.State.REGISTERING);
        SolrInputDocument doc = new SolrInputDocument();
        PreservationIndex.addFields(doc, registering, false);
        assertTrue(((java.util.Map<?, ?>) doc.getFieldValue(PreservationIndex.ID)).containsKey("set"));
        assertNull(((java.util.Map<?, ?>) doc.getFieldValue(PreservationIndex.ID)).get("set"));
        assertNull(((java.util.Map<?, ?>) doc.getFieldValue(PreservationIndex.PROVIDER_ID)).get("set"));
        assertEquals(true, ((java.util.Map<?, ?>) doc.getFieldValue(PreservationIndex.PENDING)).get("set"));
    }

    @Test void emptyRecordsClearCurrentSearchFields() throws Exception {
        SolrInputDocument doc = new SolrInputDocument();
        PreservationIndex.addFields(doc, null, false);
        for (String field : List.of(PreservationIndex.ID, PreservationIndex.PROVIDER_ID, PreservationIndex.STATES)) {
            assertNull(((java.util.Map<?, ?>) doc.getFieldValue(field)).get("set"));
        }
        assertEquals(false, ((java.util.Map<?, ?>) doc.getFieldValue(PreservationIndex.PENDING)).get("set"));
    }
}
