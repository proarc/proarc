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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.export;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraCredentials;
import com.yourmediashelf.fedora.client.response.FindObjectsResponse;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.incad.pas.editor.server.CustomTemporaryFolder;
import cz.incad.pas.editor.server.config.PasConfigurationFactory;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor;
import cz.incad.pas.editor.server.fedora.BinaryEditor;
import cz.incad.pas.editor.server.fedora.FoxmlUtils;
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.fedora.StringEditor;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import java.io.File;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import javax.xml.transform.stream.StreamSource;
import org.custommonkey.xmlunit.SimpleNamespaceContext;
import org.custommonkey.xmlunit.XMLAssert;
import org.custommonkey.xmlunit.XMLUnit;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Assume;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.xml.sax.InputSource;

/**
 * XXX needs refactoring of fedora integration stuff
 * 
 * @author Jan Pokorsky
 */
public class Kramerius4ExportTest {

    private static FedoraClient client;
    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

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
    public void testGetTarget() throws Exception {
        File output = temp.getRoot();
        String prefix = "uuid:0bcf9933-84e5-460f-9e94-d798b724d394";
        RemoteStorage storage = RemoteStorage.getInstance(PasConfigurationFactory.getInstance().defaultInstance());
        Kramerius4Export instance = new Kramerius4Export(storage);
        File expResult = new File(output, prefix);
        File result = instance.getTarget(output, prefix);
        assertEquals(expResult, result);

        expResult = new File(output, prefix + "_1");
        result = instance.getTarget(output, prefix);
        assertEquals(expResult, result);
    }

    /**
     * integration test
     */
    @Test
    public void testExport() throws Exception {
        fedoraClientSetup();
        cleanUp();
        ingest(Kramerius4ExportTest.class.getResource("Kramerius4ExportTestPage.xml"));

        File output = temp.getRoot();
        boolean hierarchy = true;
        String[] pids = {"uuid:f74f3cf3-f3be-4cac-95da-8e50331414a2"};
        RemoteStorage storage = RemoteStorage.getInstance(PasConfigurationFactory.getInstance().defaultInstance());
        Kramerius4Export instance = new Kramerius4Export(storage);
        File target = instance.export(output, hierarchy, pids);
        assertNotNull(target);

        // check datastreams with xpath
        HashMap<String, String> namespaces = new HashMap<String, String>();
        namespaces.put("f", "info:fedora/fedora-system:def/foxml#");
        XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(namespaces));
        String foxmlSystemId = Kramerius4Export.pidAsFile(target, pids[0]).toURI().toASCIIString();
        XMLAssert.assertXpathExists(streamXPath(ModsStreamEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(DcStreamEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(StringEditor.OCR_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(RelationEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath("IMG_FULL"), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath("IMG_PREVIEW"), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath("IMG_THUMBNAIL"), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathNotExists(streamXPath(BinaryEditor.RAW_ID), new InputSource(foxmlSystemId));
    }

    private static String streamXPath(String dsId) {
        return "f:digitalObject/f:datastream[@ID='" + dsId + "']";
    }

    public static void fedoraClientSetup() throws Exception {
        try {
            client = new FedoraClient(new FedoraCredentials("http://localhost:8080/fedora", "fedoraAdmin", "fedoraAdmin"));
            client.getServerVersion();
        } catch (Exception ex) {
            Assume.assumeNoException(ex);
        }
    }

    private void ingest(URL foxml) throws Exception {
        assertNotNull(foxml);
        DigitalObject dobj = FoxmlUtils.unmarshal(new StreamSource(foxml.toExternalForm()), DigitalObject.class);
        RemoteStorage fedora = new RemoteStorage(client);
        LocalObject object = new LocalStorage().create(dobj);
        fedora.ingest(object, "junit");
    }

    private void cleanUp() throws Exception {
//        client.debug(true);
        FindObjectsResponse response = FedoraClient.findObjects()
                .pid().query("ownerId='junit'")
                .maxResults(5000)
                .execute(client);

        int count = 0;
        while (true) {
            count += cleanUp(response);
            if (!response.hasNext()) {
                break;
            }
            response = FedoraClient.findObjects().sessionToken(response.getToken()).execute(client);
        }
        System.out.println("purged: " + count + " objects");
    }

    private int cleanUp(FindObjectsResponse response) throws Exception {
        List<String> pids = response.getPids();
        for (String pid : pids) {
            FedoraClient.purgeObject(pid).logMessage("junit cleanup").execute(client);
        }
        return pids.size();
    }

}
