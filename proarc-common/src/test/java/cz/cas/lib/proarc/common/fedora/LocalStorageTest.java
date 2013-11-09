/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils.ControlGroup;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor.EditorResult;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.stream.StreamResult;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class LocalStorageTest {

    @Rule
    public CustomTemporaryFolder tmp = new CustomTemporaryFolder();

    public LocalStorageTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testLoad() throws Exception {
        File foxml = tmp.newFile();
        String pid = "PID";
        DigitalObject dobj = FoxmlUtils.createFoxml(pid);
        FoxmlUtils.marshal(new StreamResult(foxml), dobj, true);
        assertTrue(foxml.length() > 0);
        LocalStorage instance = new LocalStorage();
        LocalObject result = instance.load(pid, foxml);
        assertLocalObject(result);
        assertEquals(pid, result.getPid());
        assertEquals(pid, result.getDigitalObject().getPID());
        assertEquals(foxml, result.getFoxml());
    }

    @Test
    public void testCreate_0args() {
        LocalStorage instance = new LocalStorage();
        LocalObject result = instance.create();
        assertLocalObject(result);
        assertNull(result.getFoxml());
    }

    private void assertLocalObject(LocalObject result) {
        assertNotNull(result);
        assertNotNull(result.getPid());
        assertNotNull(result.getDigitalObject());
        assertEquals(result.getPid(), result.getDigitalObject().getPID());
        assertNotNull(result.asText());
    }

    @Test
    public void testCreate_String() {
        String pid = "PID";
        LocalStorage instance = new LocalStorage();
        LocalObject result = instance.create(pid);
        assertLocalObject(result);
        assertEquals(pid, result.getPid());
        assertEquals(pid, result.getDigitalObject().getPID());
        assertNull(result.getFoxml());
    }

    @Test
    public void testCreate_DigitalObject() {
        String pid = "PID";
        DigitalObject dobj = FoxmlUtils.createFoxml(pid);
        LocalStorage instance = new LocalStorage();
        LocalObject result = instance.create(dobj);
        assertLocalObject(result);
        assertEquals(pid, result.getPid());
        assertEquals(pid, result.getDigitalObject().getPID());
        assertNull(result.getFoxml());
    }

    @Test
    public void testCreate_File_DigitalObject() throws Exception {
        File foxml = tmp.newFile();
        String pid = "PID";
        DigitalObject dobj = FoxmlUtils.createFoxml(pid);
        LocalStorage instance = new LocalStorage();
        LocalObject result = instance.create(foxml, dobj);
        assertLocalObject(result);
        assertEquals(pid, result.getPid());
        assertEquals(pid, result.getDigitalObject().getPID());
        assertEquals(foxml, result.getFoxml());
        assertEquals(0, foxml.length());
    }

    @Test
    public void testCreate_File() throws Exception {
        File foxml = tmp.newFile();
        LocalStorage instance = new LocalStorage();
        LocalObject result = instance.create(foxml);
        assertLocalObject(result);
        assertEquals(foxml, result.getFoxml());
        assertEquals(0, foxml.length());
    }

    @Test
    public void testCreate_String_File() throws Exception {
        File foxml = tmp.newFile();
        String pid = "PID";
        LocalStorage instance = new LocalStorage();
        LocalObject result = instance.create(pid, foxml);
        assertLocalObject(result);
        assertEquals(pid, result.getPid());
        assertEquals(pid, result.getDigitalObject().getPID());
        assertEquals(foxml, result.getFoxml());
        assertEquals(0, foxml.length());
    }

    @Test
    public void testDatastreamEditorWriteBytes_ManagedAsInlined() throws Exception {
        LocalStorage storage = new LocalStorage();
        LocalObject lobject = storage.create();
        String dsID = "dsID";
        MediaType mime = MediaType.TEXT_PLAIN_TYPE;
        String label = "label";
        XmlStreamEditor editor = lobject.getEditor(FoxmlUtils.managedProfile(dsID, mime, label));
        assertNotNull(editor);
        assertEquals(mime.toString(), editor.getMimetype());
        byte[] data = "data".getBytes("UTF-8");
        editor.write(data, 0, null);
        lobject.flush();

        // is managed
        DigitalObject dobj = lobject.getDigitalObject();
        DatastreamType ds = FoxmlUtils.findDatastream(dobj, dsID);
        assertNotNull(ds);
        assertEquals(ControlGroup.MANAGED.toExternal(), ds.getCONTROLGROUP());

        // is inlined
        DatastreamVersionType dsv = FoxmlUtils.findDataStreamVersion(dobj, dsID);
        assertNotNull(dsv);
        assertEquals(mime.toString(), dsv.getMIMETYPE());
        assertEquals(label, dsv.getLABEL());
        assertArrayEquals(data, dsv.getBinaryContent());
        assertNull(dsv.getContentLocation());
    }

    @Test
    public void testDatastreamEditorWriteBytes_ManagedAsAttached() throws Exception {
        LocalStorage storage = new LocalStorage();
        LocalObject lobject = storage.create();
        String dsID = "dsID";
        MediaType mime = MediaType.TEXT_PLAIN_TYPE;
        String label = "label";
        XmlStreamEditor editor = lobject.getEditor(FoxmlUtils.managedProfile(dsID, mime, label));
        assertNotNull(editor);
        assertEquals(mime.toString(), editor.getMimetype());
        byte[] data = "data".getBytes("UTF-8");
        File attachment = tmp.newFile();
        editor.write(attachment.toURI(), 0, null);
        editor.write(data, editor.getLastModified(), null);
        lobject.flush();

        // is managed
        DigitalObject dobj = lobject.getDigitalObject();
        DatastreamType ds = FoxmlUtils.findDatastream(dobj, dsID);
        assertNotNull(ds);
        assertEquals(ControlGroup.MANAGED.toExternal(), ds.getCONTROLGROUP());

        // is attached
        DatastreamVersionType dsv = FoxmlUtils.findDataStreamVersion(dobj, dsID);
        assertNotNull(dsv);
        assertEquals(mime.toString(), dsv.getMIMETYPE());
        assertEquals(label, dsv.getLABEL());
        assertNull(dsv.getBinaryContent());
        assertNotNull(dsv.getContentLocation());
        FileInputStream fis = new FileInputStream(attachment);
        byte[] readdata = new byte[data.length];
        assertEquals(data.length, fis.read(readdata));
        fis.close();
        assertArrayEquals(data, readdata);
    }

    @Test
    public void testDatastreamEditorWriteStream_ManagedAsInlined() throws Exception {
        LocalStorage storage = new LocalStorage();
        LocalObject lobject = storage.create();
        String dsID = "dsID";
        MediaType mime = MediaType.TEXT_PLAIN_TYPE;
        String label = "label";
        XmlStreamEditor editor = lobject.getEditor(FoxmlUtils.managedProfile(dsID, mime, label));
        assertNotNull(editor);
        assertEquals(mime.toString(), editor.getMimetype());
        byte[] data = "data".getBytes("UTF-8");
        editor.write(new ByteArrayInputStream(data), 0, null);
        lobject.flush();

        // is managed
        DigitalObject dobj = lobject.getDigitalObject();
        DatastreamType ds = FoxmlUtils.findDatastream(dobj, dsID);
        assertNotNull(ds);
        assertEquals(ControlGroup.MANAGED.toExternal(), ds.getCONTROLGROUP());

        // is inlined
        DatastreamVersionType dsv = FoxmlUtils.findDataStreamVersion(dobj, dsID);
        assertNotNull(dsv);
        assertEquals(mime.toString(), dsv.getMIMETYPE());
        assertEquals(label, dsv.getLABEL());
        assertArrayEquals(data, dsv.getBinaryContent());
        assertNull(dsv.getContentLocation());
    }

    @Test
    public void testDatastreamEditorWriteStream_ManagedAsAttached() throws Exception {
        LocalStorage storage = new LocalStorage();
        LocalObject lobject = storage.create();
        String dsID = "dsID";
        MediaType mime = MediaType.TEXT_PLAIN_TYPE;
        String label = "label";
        XmlStreamEditor editor = lobject.getEditor(FoxmlUtils.managedProfile(dsID, mime, label));
        assertNotNull(editor);
        assertEquals(mime.toString(), editor.getMimetype());
        byte[] data = "data".getBytes("UTF-8");
        File attachment = tmp.newFile();
        editor.write(attachment.toURI(), 0, null);
        editor.write(new ByteArrayInputStream(data), editor.getLastModified(), null);
        lobject.flush();

        // is managed
        DigitalObject dobj = lobject.getDigitalObject();
        DatastreamType ds = FoxmlUtils.findDatastream(dobj, dsID);
        assertNotNull(ds);
        assertEquals(ControlGroup.MANAGED.toExternal(), ds.getCONTROLGROUP());

        // is attached
        DatastreamVersionType dsv = FoxmlUtils.findDataStreamVersion(dobj, dsID);
        assertNotNull(dsv);
        assertEquals(mime.toString(), dsv.getMIMETYPE());
        assertEquals(label, dsv.getLABEL());
        assertNull(dsv.getBinaryContent());
        assertNotNull(dsv.getContentLocation());
        FileInputStream fis = new FileInputStream(attachment);
        byte[] readdata = new byte[data.length];
        assertEquals(data.length, fis.read(readdata));
        fis.close();
        assertArrayEquals(data, readdata);
    }

    @Test
    public void testDatastreamEditorWriteXml_Inlined() throws Exception {
        LocalStorage storage = new LocalStorage();
        LocalObject lobject = storage.create();
        String dsID = "dsID";
        MediaType mime = MediaType.TEXT_XML_TYPE;
        String label = "label";
        String formatUri = "formatUri";
        XmlStreamEditor editor = lobject.getEditor(FoxmlUtils.inlineProfile(dsID, formatUri, label));
        assertNotNull(editor);
        assertEquals(mime.toString(), editor.getMimetype());
        XmlData xdata = new XmlData("data");
        EditorResult xmlResult = editor.createResult();
        JAXB.marshal(xdata, xmlResult);
        editor.write(xmlResult, 0, null);
        lobject.flush();

        // is inline
        DigitalObject dobj = lobject.getDigitalObject();
        DatastreamType ds = FoxmlUtils.findDatastream(dobj, dsID);
        assertNotNull(ds);
        assertEquals(ControlGroup.INLINE.toExternal(), ds.getCONTROLGROUP());

        // is inlined
        DatastreamVersionType dsv = FoxmlUtils.findDataStreamVersion(dobj, dsID);
        assertNotNull(dsv);
        assertEquals(mime.toString(), dsv.getMIMETYPE());
        assertEquals(label, dsv.getLABEL());
        assertEquals(formatUri, dsv.getFORMATURI());
        assertNotNull(dsv.getXmlContent());
        assertNull(dsv.getContentLocation());
        assertNull(dsv.getBinaryContent());
    }

    @Test
    public void testDatastreamEditorWriteXml_ManagedAsAttached() throws Exception {
        LocalStorage storage = new LocalStorage();
        LocalObject lobject = storage.create();
        String dsID = "dsID";
        MediaType mime = MediaType.TEXT_XML_TYPE;
        String label = "label";
        String formatUri = "formatUri";
        XmlStreamEditor editor = lobject.getEditor(FoxmlUtils.managedProfile(dsID, formatUri, label));
        assertNotNull(editor);
        assertEquals(mime.toString(), editor.getMimetype());
        File attachment = tmp.newFile();
        editor.write(attachment.toURI(), 0, null);
        XmlData xdata = new XmlData("data");
        EditorResult xmlResult = editor.createResult();
        JAXB.marshal(xdata, xmlResult);
        editor.write(xmlResult, editor.getLastModified(), null);
        lobject.flush();

        // is managed
        DigitalObject dobj = lobject.getDigitalObject();
        DatastreamType ds = FoxmlUtils.findDatastream(dobj, dsID);
        assertNotNull(ds);
        assertEquals(ControlGroup.MANAGED.toExternal(), ds.getCONTROLGROUP());

        // is attached
        DatastreamVersionType dsv = FoxmlUtils.findDataStreamVersion(dobj, dsID);
        assertNotNull(dsv);
        assertEquals(mime.toString(), dsv.getMIMETYPE());
        assertEquals(label, dsv.getLABEL());
        assertEquals(formatUri, dsv.getFORMATURI());
        assertNull(dsv.getBinaryContent());
        assertNull(dsv.getXmlContent());
        assertNotNull(dsv.getContentLocation());

        XmlData result = JAXB.unmarshal(attachment, XmlData.class);
        assertEquals(xdata, result);
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class XmlData {
        
        String data;

        public XmlData(String data) {
            this.data = data;
        }

        public XmlData() {
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final XmlData other = (XmlData) obj;
            if ((this.data == null) ? (other.data != null) : !this.data.equals(other.data)) {
                return false;
            }
            return true;
        }
    }

}
