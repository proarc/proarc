package cz.cas.lib.proarc.common.storage;

import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class AtmEditorTest {
    @Test
    void updatingDeviceAndSoftwarePreservesOmittedMetadata() throws Exception {
        LocalStorage.LocalObject object = new LocalStorage().create();
        RelationEditor relations = new RelationEditor(object);
        relations.setDevice("device:old");
        relations.setSoftware("software:old");
        relations.setDonator("norway");
        relations.setArchivalCopiesPath("/scans/original");
        relations.setOrganization("organization");
        relations.setUser("processor");
        relations.setStatus("described");
        relations.write(relations.getLastModified(), "test");

        AtmEditor editor = new AtmEditor(object, null);
        editor.write("device:new", "software:new", null, null, null, null, null, "test");
        relations = new RelationEditor(object);
        assertEquals("device:new", relations.getDevice());
        assertEquals("software:new", relations.getSoftware());
        assertEquals("norway", relations.getDonator());
        assertEquals("/scans/original", relations.getArchivalCopiesPath());
        assertEquals("organization", relations.getOrganization());
        assertEquals("processor", relations.getUser());
        assertEquals("described", relations.getStatus());

        editor.write(null, "null", null, null, null, "null", "", "test");
        relations = new RelationEditor(object);
        assertEquals("device:new", relations.getDevice());
        assertNull(relations.getSoftware());
        assertNull(relations.getDonator());
        assertNull(relations.getArchivalCopiesPath());
    }
}
