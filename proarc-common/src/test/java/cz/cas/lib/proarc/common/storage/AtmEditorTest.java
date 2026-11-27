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
package cz.cas.lib.proarc.common.storage;

import cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class AtmEditorTest {
    @Test
    void atmReturnsOnlyLatestArchiveInfoAndPreservesItOnEdit() throws Exception {
        LocalStorage.LocalObject object = new LocalStorage().create();
        RelationEditor relations = new RelationEditor(object);
        ArchivInfo old = new ArchivInfo();
        old.setPackageId("12");
        relations.setArchivInfo(old);
        ArchivInfo current = new ArchivInfo();
        current.setPackageId("23");
        current.setState(ArchivInfo.State.STORED);
        current.setStoredAt("2026-11-19T15:00:00Z");
        relations.setArchivInfo(current);
        relations.write(relations.getLastModified(), "test");

        AtmEditor editor = new AtmEditor(object, null);
        editor.write("device:new", null, null, null, null, null, null, "test");
        ArchivInfo result = editor.read().getArchivInfo();
        assertEquals("23", result.getPackageId());
        assertEquals(ArchivInfo.State.STORED, result.getState());
        assertEquals(current.getStoredAt(), result.getStoredAt());

        relations = new RelationEditor(object);
        relations.setArchivInfo(null);
        relations.write(relations.getLastModified(), "test");
        assertNull(editor.read().getArchivInfo());
    }

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
