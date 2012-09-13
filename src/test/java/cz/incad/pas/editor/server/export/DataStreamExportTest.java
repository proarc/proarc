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
import cz.incad.pas.editor.server.fedora.FoxmlUtils;
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.fedora.StringEditor;
import java.io.File;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import javax.xml.transform.stream.StreamSource;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Assume;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class DataStreamExportTest {
    
    private static FedoraClient client;
    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

    public DataStreamExportTest() {
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

    /**
     * Test of export method, of class DataStreamExport.
     */
    @Test
    public void testExport() throws Exception {
        fedoraClientSetup();
        cleanUp();
        ingest(Kramerius4ExportTest.class.getResource("Kramerius4ExportTestPage.xml"));

        File output = temp.getRoot();
        boolean hierarchy = true;
        List<String> pids = Arrays.asList("uuid:f74f3cf3-f3be-4cac-95da-8e50331414a2");
        List<String> dsIds = Arrays.asList(StringEditor.OCR_ID, "PREVIEW");
        RemoteStorage storage = RemoteStorage.getInstance(PasConfigurationFactory.getInstance().defaultInstance());
        DataStreamExport instance = new DataStreamExport(storage);
        File target = instance.export(output, hierarchy, pids, dsIds);
        assertNotNull(target);

        File ocr = new File(target, DataStreamExport.filename(pids.get(0), dsIds.get(0)));
        assertTrue(ocr.exists());
        assertTrue(ocr.length() == 3);

        File preview = new File(target, DataStreamExport.filename(pids.get(0), dsIds.get(1)));
        assertTrue(preview.exists());
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
