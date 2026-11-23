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

import cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo;
import cz.cas.lib.proarc.common.externalApp.ltp.PreservationIndex;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mods.ModsDefinition;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SolrObjectFeederPreservationTest {
    @Test
    void fullIndexRestoresAllLtpFieldsFromRelsExt() throws Exception {
        for (ArchivInfo.State state : new ArchivInfo.State[] {ArchivInfo.State.UPLOADED, ArchivInfo.State.STORED}) {
            ArchivInfo record = new ArchivInfo();
            record.setProvider("cesnet");
            record.setPackageId("23");
            record.setState(state);
            record.setCatalogState(ArchivInfo.CatalogState.DONE);

            SolrInputDocument document = index(record);
            assertEquals("23", document.getFieldValue(PreservationIndex.ID));
            assertEquals("cesnet:23", document.getFieldValue(PreservationIndex.PROVIDER_ID));
            assertEquals("cesnet:" + state, document.getFieldValue(PreservationIndex.STATES));
            assertEquals(state != ArchivInfo.State.STORED, document.getFieldValue(PreservationIndex.PENDING));
            ArchivInfo restored = PreservationIndex.read((String) document.getFieldValue(PreservationIndex.RECORDS));
            assertEquals("23", restored.getPackageId());
            assertEquals(state, restored.getState());
        }
    }

    @Test
    void fullIndexAcceptsObjectWithoutLtpSubmission() throws Exception {
        SolrInputDocument document = index(null);
        assertNull(document.getFieldValue(PreservationIndex.RECORDS));
        assertNull(document.getFieldValue(PreservationIndex.ID));
        assertNull(document.getFieldValue(PreservationIndex.PROVIDER_ID));
        assertNull(document.getFieldValue(PreservationIndex.STATES));
        assertEquals(false, document.getFieldValue(PreservationIndex.PENDING));
    }

    private SolrInputDocument index(ArchivInfo record) throws Exception {
        LocalStorage.LocalObject object = new LocalStorage().create("uuid:ltp-index-test");
        RelationEditor relations = new RelationEditor(object);
        relations.setModel(NdkPlugin.MODEL_MONOGRAPHUNIT);
        relations.setArchivInfo(record);
        relations.write(relations.getLastModified(), "test");
        XmlStreamEditor xml = object.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor mods = new ModsStreamEditor(xml, object);
        mods.write(new ModsDefinition(), mods.getLastModified(), "test");

        CapturingFeeder feeder = new CapturingFeeder();
        // This is the insertion path used when rebuilding the complete object index.
        feeder.insertDescriptionDocument(object.getDigitalObject(), object, false);
        return feeder.document;
    }

    private static final class CapturingFeeder extends SolrObjectFeeder {
        private SolrInputDocument document;

        CapturingFeeder() {
            super(null);
        }

        @Override
        public UpdateResponse feedDescriptionDocument(SolrInputDocument document) {
            this.document = document;
            return new UpdateResponse();
        }
    }
}
