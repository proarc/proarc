/*
 * Copyright (C) 2012 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.fedora.relation;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import java.io.File;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import org.hamcrest.core.Is;
import static org.junit.Assert.*;
import org.junit.Rule;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class RelationEditorTest {

    @Rule
    public CustomTemporaryFolder tmp = new CustomTemporaryFolder();

    @Test
    public void testRdfParse() {
        URL resource = RelationEditorTest.class.getResource("rels-ext.xml");
        String about="info:fedora/demo:999";
        assertNotNull(resource);
        Rdf unmarshal = Relations.unmarshal(resource, Rdf.class);
        RdfDescription description = unmarshal.getDescription();
        assertNotNull(description);
        assertEquals(about, description.getAbout().getResource());

        assertNotNull(description.getModel());
        assertEquals("fedora-system:ContentModel-3.0", description.getModel().getResourcePid());

        assertEquals("device:scanner", description.getDevice().getResourcePid());
        assertEquals("ABA00726009905207199800001.tif", description.getImportFile());

        assertNotNull(description.getMemberRelations());
        assertEquals(3, description.getMemberRelations().size());
        assertEquals("test:member1", description.getMemberRelations().get(0).getResourcePid());
        assertEquals("test:member2", description.getMemberRelations().get(1).getResourcePid());
        assertEquals("test:member3", description.getMemberRelations().get(2).getResourcePid());

        assertNotNull(description.getRelations());
        assertEquals(3, description.getRelations().size());

        String toXml = Relations.toXml(unmarshal, true);
        System.out.println(toXml);
    }

    @Test
    public void testReadWrite() throws Exception {
        LocalStorage storage = new LocalStorage();
        File foxml = tmp.newFile();
        LocalObject lobject = storage.create(foxml);
        RelationEditor instance = new RelationEditor(lobject);
        String model = "mode:page";
        List<String> members = Arrays.asList("test:member1", "test:member2");
        instance.setModel(model);
        String device = "device:scanner";
        instance.setDevice(device);
        String filename = "file.name";
        instance.setImportFile(filename);
        instance.setMembers(members);
        instance.write(0, null);
        lobject.flush();

        lobject = storage.load(lobject.getPid(), foxml);
        instance = new RelationEditor(lobject);
        assertEquals(model, instance.getModel());
        assertEquals(device, instance.getDevice());
        assertEquals(filename, instance.getImportFile());
        assertThat(instance.getMembers(), Is.is(members));
        long timestamp = instance.getLastModified();
        assertTrue(timestamp > 0);

        // update
        lobject = storage.load(lobject.getPid(), foxml);
        instance = new RelationEditor(lobject);
        model = "model:update";
        device = "device:update";
        members = Arrays.asList("test:member1", "test:member3");
        instance.setModel(model);
        instance.setDevice(device);
        instance.setMembers(members);
        instance.write(timestamp, null);
        lobject.flush();

        lobject = storage.load(lobject.getPid(), foxml);
        instance = new RelationEditor(lobject);
        assertEquals(model, instance.getModel());
        assertEquals(device, instance.getDevice());
        assertThat(instance.getMembers(), Is.is(members));
        assertTrue(timestamp < instance.getLastModified());
    }
}
