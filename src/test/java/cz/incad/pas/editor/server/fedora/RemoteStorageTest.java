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
package cz.incad.pas.editor.server.fedora;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.response.ListDatastreamsResponse;
import com.yourmediashelf.fedora.generated.access.DatastreamType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.pas.editor.server.CustomTemporaryFolder;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalXmlStreamEditor;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteXmlStreamEditor;
import cz.incad.pas.editor.server.fedora.XmlStreamEditor.EditorResult;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import java.io.File;
import java.util.ConcurrentModificationException;
import java.util.List;
import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.Source;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.rules.TestName;

/**
 *
 * @author Jan Pokorsky
 */
public class RemoteStorageTest {
    
    private static FedoraClient client;
    private static FedoraTestSupport support;

    @Rule
    public TestName test = new TestName();

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Rule
    public CustomTemporaryFolder tmp = new CustomTemporaryFolder();

    public RemoteStorageTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
        support = new FedoraTestSupport();
        client = support.getClient();
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() throws Exception {
        support.cleanUp();
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testIngest() throws Exception {
        RemoteStorage fedora = new RemoteStorage(client);
        LocalObject object = new LocalStorage().create();
        String label = "testing";
        object.setLabel(label);
        fedora.ingest(object, "junit");
    }

    @Test
    public void testIngestPage() throws Exception {
        RemoteStorage fedora = new RemoteStorage(client);
//        client.debug(true);
        LocalObject local = new LocalStorage().create();
//        LocalObject local = new LocalStorage().create(new File("/tmp/failing_ingest.foxml"));
        ModsStreamEditor modsEditor = new ModsStreamEditor(local);
        ModsType mods = modsEditor.createPage(local.getPid(), "1", "[1]", "Blank");
        DcStreamEditor dcEditor = new DcStreamEditor(local);
        String model = "model:page";
        dcEditor.write(mods, model, 0);
        modsEditor.write(mods, 0);

        RelationEditor relsExt = new RelationEditor(local);
        relsExt.setModel(model);
        relsExt.write(0);

        StringEditor ocrEditor = StringEditor.ocr(local);
        ocrEditor.write("ocr", 0);

        File thumb = tmp.newFile();
        assertTrue(thumb.exists());
        BinaryEditor.dissemination(local, BinaryEditor.THUMB_ID).write(thumb, 0);
        local.flush();
        System.out.println(FoxmlUtils.toXml(local.getDigitalObject(), true));

        String label = "testing";
        local.setLabel(label);
        fedora.ingest(local, "junit");
        ListDatastreamsResponse response = FedoraClient.listDatastreams(local.getPid()).execute(client);
        List<DatastreamType> datastreams = response.getDatastreams();
        assertDatastream(DcStreamEditor.DATASTREAM_ID, datastreams);
        assertDatastream(ModsStreamEditor.DATASTREAM_ID, datastreams);
        assertDatastream(RelationEditor.DATASTREAM_ID, datastreams);
        assertDatastream(StringEditor.OCR_ID, datastreams);
        assertDatastream(BinaryEditor.THUMB_ID, datastreams);
    }

    private static void assertDatastream(String id, List<DatastreamType> datastreams) {
        for (DatastreamType ds : datastreams) {
            if (id.equals(ds.getDsid())) {
                return;
            }
        }
        fail(id);
    }

    @Test
    public void testXmlRead() throws Exception {
        String dsId = "testId";
        LocalObject local = new LocalStorage().create();
        local.setLabel(test.getMethodName());
        LocalXmlStreamEditor leditor = new LocalXmlStreamEditor(local, dsId, "testns", "label");
        EditorResult editorResult = leditor.createResult();
        TestXml content = new TestXml("test content");
        JAXB.marshal(content, editorResult);
        leditor.write(editorResult, 0);

        RemoteStorage fedora = new RemoteStorage(client);
        fedora.ingest(local, "junit");

        RemoteObject remote = fedora.find(local.getPid());
        RemoteXmlStreamEditor editor = new RemoteXmlStreamEditor(remote, dsId);
        Source src = editor.read();
        assertNotNull(src);
        TestXml resultContent = JAXB.unmarshal(src, TestXml.class);
        assertEquals(content, resultContent);
        long lastModified = editor.getLastModified();
        assertTrue(String.valueOf(lastModified), lastModified != 0 && lastModified < System.currentTimeMillis());
    }

//    @Test
    public void testXmlReadMissing() throws Exception {
        RemoteStorage fedora = new RemoteStorage(client);
        LocalObject local = new LocalStorage().create();
        local.setLabel(test.getMethodName());
        fedora.ingest(local, "junit");
        RemoteObject remote = fedora.find(local.getPid());
        RemoteXmlStreamEditor editor = new RemoteXmlStreamEditor(remote, "test");
        Source src = editor.read();
        long lastModified = editor.getLastModified();
        assertNull(src);
    }

    @Test
    public void testXmlWrite() throws Exception {
        String dsId = "testId";
        LocalObject local = new LocalStorage().create();
        local.setLabel(test.getMethodName());
        LocalXmlStreamEditor leditor = new LocalXmlStreamEditor(local, dsId, "testns", "label");
        EditorResult editorResult = leditor.createResult();
        TestXml content = new TestXml("test content");
        JAXB.marshal(content, editorResult);
        leditor.write(editorResult, 0);

        RemoteStorage fedora = new RemoteStorage(client);
        fedora.ingest(local, "junit");

        RemoteObject remote = fedora.find(local.getPid());
        RemoteXmlStreamEditor editor = new RemoteXmlStreamEditor(remote, dsId);
        Source src = editor.read();
        assertNotNull(src);
        TestXml resultContent = JAXB.unmarshal(src, TestXml.class);

        // write modification
        String expectedContent = "changed test content";
        resultContent.data = expectedContent;
        editorResult = editor.createResult();
        JAXB.marshal(resultContent, editorResult);
        long lastModified = editor.getLastModified();
        editor.write(editorResult, lastModified);
        remote.flush();

        // test current editor
        assertTrue(lastModified < editor.getLastModified());
        long expectLastModified = editor.getLastModified();
        resultContent = JAXB.unmarshal(editor.read(), TestXml.class);
        assertEquals(new TestXml(expectedContent), resultContent);

        // test new editor
        remote = fedora.find(local.getPid());
        editor = new RemoteXmlStreamEditor(remote, dsId);
        src = editor.read();
        assertNotNull(src);
        resultContent = JAXB.unmarshal(src, TestXml.class);
        assertEquals(new TestXml(expectedContent), resultContent);
        long resultLastModified = editor.getLastModified();
        assertEquals(expectLastModified, resultLastModified);
    }

    @Test
    public void testXmlWriteConcurrent() throws Exception {
        String dsId = "testId";
        LocalObject local = new LocalStorage().create();
        local.setLabel(test.getMethodName());
        LocalXmlStreamEditor leditor = new LocalXmlStreamEditor(local, dsId, "testns", "label");
        EditorResult editorResult = leditor.createResult();
        TestXml content = new TestXml("test content");
        JAXB.marshal(content, editorResult);
        leditor.write(editorResult, 0);

        RemoteStorage fedora = new RemoteStorage(client);
        fedora.ingest(local, "junit");

        RemoteObject remote = fedora.find(local.getPid());
        RemoteXmlStreamEditor editor = new RemoteXmlStreamEditor(remote, dsId);
        Source src = editor.read();
        assertNotNull(src);
        TestXml resultContent = JAXB.unmarshal(src, TestXml.class);

        // concurrent write
        RemoteObject concurrentRemote = fedora.find(local.getPid());
        RemoteXmlStreamEditor concurrentEditor = new RemoteXmlStreamEditor(concurrentRemote, dsId);
        TestXml concurrentContent = JAXB.unmarshal(concurrentEditor.read(), TestXml.class);
        concurrentContent.data = "concurrent change";
        EditorResult concurrentResult = concurrentEditor.createResult();
        JAXB.marshal(concurrentContent, concurrentResult);
        concurrentEditor.write(concurrentResult, editor.getLastModified());
        concurrentRemote.flush();

        // write out of date modification
        String expectedContent = "changed test content";
        resultContent.data = expectedContent;
        editorResult = editor.createResult();
        JAXB.marshal(resultContent, editorResult);
        long lastModified = editor.getLastModified();
        editor.write(editorResult, lastModified);

        thrown.expect(ConcurrentModificationException.class);
        remote.flush();
    }

    @XmlRootElement(namespace="testns")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class TestXml {

        String data;

        public TestXml() {
        }

        public TestXml(String data) {
            this.data = data;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final TestXml other = (TestXml) obj;
            if ((this.data == null) ? (other.data != null) : !this.data.equals(other.data)) {
                return false;
            }
            return true;
        }

        @Override
        public String toString() {
            return "TestXml{" + "data=" + data + '}';
        }

    }
}
