package cz.cas.lib.proarc.common.storage.akubra;

import com.yourmediashelf.fedora.foxml.DigitalObject;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.RelatedItemDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SolrMultipartResourceTest {
    @Mocked SolrClient client;
    @Mocked ProArcObject object;
    @Mocked RelationEditor relations;
    @Mocked ModsStreamEditor editor;

    @Test
    void indexesOnlyMultipartModelsAndRootResourceTypes() {
        ModsDefinition mods = new ModsDefinition();
        for (String value : new String[]{"cartographic", " cartographic ", "still image", " ", null}) {
            mods.getTypeOfResource().add(resource(value));
        }
        RelatedItemDefinition related = new RelatedItemDefinition();
        related.getTypeOfResource().add(resource("text"));
        mods.getRelatedItem().add(related);
        for (String model : List.of(NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_MONOGRAPHUNIT)) {
            assertEquals(List.of("cartographic", "still image"), SolrObjectFeeder.getTypeOfResources(model, mods));
        }
        assertTrue(SolrObjectFeeder.getTypeOfResources(NdkPlugin.MODEL_MONOGRAPHVOLUME, mods).isEmpty());
    }

    @Test
    void apiReturnsAllIndexedValuesAndToleratesOldDocuments(@Mocked MetaModelRepository models) {
        new Expectations() {{
            MetaModelRepository.getInstance(); result = models;
            models.find(anyString); result = null;
        }};
        SolrDocument document = new SolrDocument();
        document.setField(SolrUtils.FIELD_MODEL, NdkPlugin.MODEL_MONOGRAPHUNIT);
        assertTrue(SolrUtils.createItem(document, Locale.ROOT).getTypeOfResource().isEmpty());
        document.setField(SolrUtils.FIELD_TYPE_OF_RESOURCE, List.of("cartographic", "still image"));
        assertEquals(List.of("cartographic", "still image"), SolrUtils.createItem(document, Locale.ROOT).getTypeOfResource());
        document.setField(SolrUtils.FIELD_MODEL, NdkPlugin.MODEL_MONOGRAPHVOLUME);
        assertTrue(SolrUtils.createItem(document, Locale.ROOT).getTypeOfResource().isEmpty());
    }

    @Test
    void insertStoresResourceValuesWithoutAtomicUpdateWrapper() throws Exception {
        ModsDefinition mods = new ModsDefinition();
        mods.getTypeOfResource().add(resource("cartographic"));
        new Expectations() {{
            object.getPid(); result = "uuid:multipart";
            relations.getModel(); result = NdkPlugin.MODEL_MONOGRAPHTITLE;
            editor.read(); result = mods;
        }};
        new SolrObjectFeeder(client).insertDescriptionDocument(new DigitalObject(), object, false);
        new Verifications() {{
            SolrInputDocument document;
            client.add(document = withCapture()); times = 1;
            assertEquals(List.of("cartographic"), List.copyOf(document.getFieldValues(SolrUtils.FIELD_TYPE_OF_RESOURCE)));
        }};
    }

    @Test
    void updatesExplicitlyClearResourceAfterRemovalOrModelChange() throws Exception {
        ModsDefinition mods = new ModsDefinition();
        mods.getTypeOfResource().add(resource("notated music"));
        new Expectations() {{
            object.getPid(); result = "uuid:volume";
            relations.getModel(); returns(NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHVOLUME);
            editor.read(); result = mods;
        }};
        SolrObjectFeeder feeder = new SolrObjectFeeder(client);
        feeder.updateDescriptionDocument(new DigitalObject(), object, false);
        mods.getTypeOfResource().clear();
        feeder.updateDescriptionDocument(new DigitalObject(), object, false);
        mods.getTypeOfResource().add(resource("notated music"));
        feeder.updateDescriptionDocument(new DigitalObject(), object, false);
        new Verifications() {{
            List<SolrInputDocument> documents = new java.util.ArrayList<>();
            client.add(withCapture(documents)); times = 3;
            assertEquals(Map.of("set", List.of("notated music")), documents.get(0).getFieldValue(SolrUtils.FIELD_TYPE_OF_RESOURCE));
            for (SolrInputDocument document : documents.subList(1, 3)) {
                Map<?, ?> update = (Map<?, ?>) document.getFieldValue(SolrUtils.FIELD_TYPE_OF_RESOURCE);
                assertTrue(update.containsKey("set"));
                assertNull(update.get("set"));
            }
        }};
    }

    private static TypeOfResourceDefinition resource(String value) {
        TypeOfResourceDefinition resource = new TypeOfResourceDefinition();
        resource.setValue(value);
        return resource;
    }
}
