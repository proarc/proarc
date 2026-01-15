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
package cz.cas.lib.proarc.common.process.export;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.FedoraTestSupport;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.storage.relation.Relations;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import java.io.File;
import java.io.StringWriter;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xmlunit.xpath.JAXPXPathEngine;

import static org.easymock.EasyMock.createNiceMock;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.xmlunit.assertj.XmlAssert.assertThat;

/**
 *
 * @author Jan Pokorsky
 */
public class Kramerius4ExportTest {

    @TempDir
    File tempDir;

    private static FedoraTestSupport fedora;

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    private AppConfiguration config;
    private AkubraConfiguration akubraConfiguration;
    private JAXPXPathEngine xpathEngine;

    @BeforeEach
    public void setUp() throws Exception {
        config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, tempDir.getPath());
        }});
        if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(config.getConfigHome());
        } else {
            akubraConfiguration = null;
        }

        fedora = new FedoraTestSupport();
        fedora.cleanUp();
        MetaModelRepository.setInstance(config.getPlugins());
        DigitalObjectManager.setDefault(new DigitalObjectManager(
                config, akubraConfiguration,
                createNiceMock(BatchManager.class),
                MetaModelRepository.getInstance(),
                createNiceMock(UserManager.class)));

        // check datastreams with xpath
        HashMap<String, String> namespaces = new HashMap<>();
        namespaces.put("dc", DcConstants.NS_PURL);
        namespaces.put("f", "info:fedora/fedora-system:def/foxml#");
        namespaces.put("kramerius", Kramerius4Export.KRAMERIUS_RELATION_NS);
        namespaces.put("mods", ModsStreamEditor.DATASTREAM_FORMAT_URI);
        namespaces.put("oai", Kramerius4Export.OAI_NS);
        namespaces.put("proarc-rels", Relations.PROARC_RELS_NS);

        xpathEngine = new JAXPXPathEngine();
        xpathEngine.setNamespaceContext(namespaces);
    }


    @AfterEach
    public void tearDown() throws Exception {
    }

    @Test
    public void testEmptyOcrExport() throws Exception {
        URL url = Kramerius4ExportTest.class.getResource("Kramerius4ExportTestPage.xml");
        Path resPath = Paths.get(url.toURI());
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc = db.parse(resPath.toFile());

        XPath xpath = XPathFactory.newInstance().newXPath();
        NodeList nl = (NodeList) xpath
                .compile("//datastream[@ID='TEXT_OCR']/datastreamVersion/binaryContent")
                .evaluate(doc, XPathConstants.NODESET);

        Path emptyOcr = Files.createTempFile("proarcKramerius4ExportTest", null);
        emptyOcr.toFile().setReadable(true, false);
        emptyOcr.toFile().deleteOnExit();
        for (int i = 0; i < nl.getLength(); i++) {
            Node binaryContentNode = nl.item(i);
            Element contentLocation = doc.createElement("contentLocation");
            contentLocation.setAttribute("TYPE", "URL");
            contentLocation.setAttribute("REF", emptyOcr.toUri().toString());
            nl.item(i).getParentNode().replaceChild(contentLocation, binaryContentNode);
        }
        DOMSource domSource = new DOMSource(doc);
        StringWriter writer = new StringWriter();
        StreamResult result = new StreamResult(writer);
        TransformerFactory tf = TransformerFactory.newInstance();
        Transformer transformer = tf.newTransformer();
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        transformer.transform(domSource, result);
        writer.flush();
        String xml = writer.toString();
        fedora.ingest(xml);

        File output = tempDir;
        boolean hierarchy = true;
        String[] pids = {"uuid:f74f3cf3-f3be-4cac-95da-8e50331414a2"};
        FedoraStorage storage = fedora.getRemoteStorage();
        Kramerius4Export instance = new Kramerius4Export(storage, config, akubraConfiguration);
        Kramerius4Export.Result k4Result = instance.export(output, hierarchy, "export status", null, null, pids);
        assertNotNull(k4Result);
        File foxml = ExportUtils.pidAsXmlFile(k4Result.getFile(), pids[0]);
        String foxmlAsURI = foxml.toURI().toASCIIString();

        assertThat(
                new InputSource(foxmlAsURI)).withNamespaceContext(
                        Map.of("f", "info:fedora/fedora-system:def/foxml#")).valueByXPath(streamXPath(StringEditor.OCR_ID) + "//f:binaryContent")
                .isEqualTo(Base64.getEncoder().encodeToString(System.lineSeparator().getBytes()));


        // test ingest of exported object
        fedora.cleanUp();
        fedora.ingest(foxml.toURI().toURL());
    }

    /**
     * integration test
     */
    @Test
    public void testExport() throws Exception {
        fedora.ingest(Kramerius4ExportTest.class.getResource("Kramerius4ExportTestPage.xml"));
        File output = tempDir;
        boolean hierarchy = true;
        String[] pids = {"uuid:f74f3cf3-f3be-4cac-95da-8e50331414a2"};
        FedoraStorage storage = fedora.getRemoteStorage();
        Kramerius4Export instance = new Kramerius4Export(storage, config, akubraConfiguration);
        Kramerius4Export.Result k4Result = instance.export(output, hierarchy, "export status", null, null, pids);
        assertNotNull(k4Result);

        // check datastreams with xpath
        File foxml = ExportUtils.pidAsXmlFile(k4Result.getFile(), pids[0]);
        String foxmlSystemId = foxml.toURI().toASCIIString();

        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(ModsStreamEditor.DATASTREAM_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(DcStreamEditor.DATASTREAM_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(StringEditor.OCR_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(RelationEditor.DATASTREAM_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath("IMG_FULL"));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath("IMG_PREVIEW"));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath("IMG_THUMB"));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(BinaryEditor.RAW_ID));

        // check OAI ID
        assertThat(new InputSource(foxmlSystemId)).hasXPath("//oai:itemID");
        // check kramerius:file
        assertThat(new InputSource(foxmlSystemId)).hasXPath("//kramerius:file");
        // check exclusion of proarc-rels:hasDevice
        assertThat(new InputSource(foxmlSystemId)).hasXPath("//proarc-rels:hasDevice");
        // check MODS starts with modsCollection
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(ModsStreamEditor.DATASTREAM_ID) + "//f:xmlContent/mods:modsCollection/mods:mods");
        // check policy
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(ModsStreamEditor.DATASTREAM_ID) + "//f:xmlContent/mods:modsCollection/mods:mods");
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(DcStreamEditor.DATASTREAM_ID) + "//dc:rights");
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(DcStreamEditor.DATASTREAM_ID) + "//dc:rights");
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(RelationEditor.DATASTREAM_ID) + "//kramerius:policy");

        // test export status
        RelationEditor relationEditor = new RelationEditor(storage.find(pids[0]));
        assertNotNull(relationEditor.getExportResult());

        // test ingest of exported object
        fedora.cleanUp();
        fedora.ingest(foxml.toURI().toURL());
    }

    private static String streamXPath(String dsId) {
        return "f:digitalObject/f:datastream[@ID='" + dsId + "']";
    }

}
