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
package cz.cas.lib.proarc.common.export;

import javax.xml.bind.DatatypeConverter;
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
import java.io.File;
import java.io.StringWriter;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.FedoraTestSupport;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.StringEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.fedora.relation.Relations;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import org.custommonkey.xmlunit.SimpleNamespaceContext;
import org.custommonkey.xmlunit.XMLAssert;
import org.custommonkey.xmlunit.XMLUnit;
import org.easymock.EasyMock;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import static org.junit.Assert.assertNotNull;

/**
 * 
 * @author Jan Pokorsky
 */
public class Kramerius4ExportTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

    private static FedoraTestSupport fedora;

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }
    private AppConfiguration config;

    @Before
    public void setUp() throws Exception {
        config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, temp.getRoot().getPath());
        }});
        fedora = new FedoraTestSupport();
        fedora.cleanUp();
        MetaModelRepository.setInstance(config.getPlugins());
        DigitalObjectManager.setDefault(new DigitalObjectManager(
                config,
                EasyMock.createNiceMock(ImportBatchManager.class),
                fedora.getRemoteStorage(),
                MetaModelRepository.getInstance(),
                EasyMock.createNiceMock(UserManager.class)));

        // check datastreams with xpath
        HashMap<String, String> namespaces = new HashMap<>();
        namespaces.put("dc", DcConstants.NS_PURL);
        namespaces.put("f", "info:fedora/fedora-system:def/foxml#");
        namespaces.put("kramerius", Kramerius4Export.KRAMERIUS_RELATION_NS);
        namespaces.put("mods", ModsStreamEditor.DATASTREAM_FORMAT_URI);
        namespaces.put("oai", Kramerius4Export.OAI_NS);
        namespaces.put("proarc-rels", Relations.PROARC_RELS_NS);
        XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(namespaces));
    }


    @After
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
        NodeList nl = (NodeList)xpath
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

        File output = temp.getRoot();
        boolean hierarchy = true;
        String[] pids = {"uuid:f74f3cf3-f3be-4cac-95da-8e50331414a2"};
        RemoteStorage storage = fedora.getRemoteStorage();
        Kramerius4Export instance = new Kramerius4Export(storage, config.getKramerius4Export());
        File target = instance.export(output, hierarchy, "export status", pids);
        assertNotNull(target);
        File foxml = ExportUtils.pidAsXmlFile(target, pids[0]);
        String foxmlAsURI = foxml.toURI().toASCIIString();

        XMLAssert.assertXpathEvaluatesTo(DatatypeConverter.printBase64Binary(System.lineSeparator().getBytes()),
                streamXPath(StringEditor.OCR_ID) + "//f:binaryContent", new InputSource(foxmlAsURI));

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
        File output = temp.getRoot();
        boolean hierarchy = true;
        String[] pids = {"uuid:f74f3cf3-f3be-4cac-95da-8e50331414a2"};
        RemoteStorage storage = fedora.getRemoteStorage();
        Kramerius4Export instance = new Kramerius4Export(storage, config.getKramerius4Export());
        File target = instance.export(output, hierarchy, "export status", pids);
        assertNotNull(target);

        // check datastreams with xpath
        File foxml = ExportUtils.pidAsXmlFile(target, pids[0]);
        String foxmlSystemId = foxml.toURI().toASCIIString();
        XMLAssert.assertXpathExists(streamXPath(ModsStreamEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(DcStreamEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(StringEditor.OCR_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(RelationEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath("IMG_FULL"), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath("IMG_PREVIEW"), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath("IMG_THUMB"), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathNotExists(streamXPath(BinaryEditor.RAW_ID), new InputSource(foxmlSystemId));

        // check OAI ID
        XMLAssert.assertXpathExists("//oai:itemID", new InputSource(foxmlSystemId));
        // check kramerius:file
        XMLAssert.assertXpathExists("//kramerius:file", new InputSource(foxmlSystemId));
        // check exclusion of proarc-rels:hasDevice
        XMLAssert.assertXpathNotExists("//proarc-rels:hasDevice", new InputSource(foxmlSystemId));
        // check MODS starts with modsCollection
        XMLAssert.assertXpathExists(streamXPath(ModsStreamEditor.DATASTREAM_ID) + "//f:xmlContent/mods:modsCollection/mods:mods", new InputSource(foxmlSystemId));
        // check policy
        XMLAssert.assertXpathEvaluatesTo("policy:private", streamXPath(DcStreamEditor.DATASTREAM_ID) + "//dc:rights", new InputSource(foxmlSystemId));
        XMLAssert.assertXpathEvaluatesTo("policy:private", streamXPath(RelationEditor.DATASTREAM_ID) + "//kramerius:policy", new InputSource(foxmlSystemId));

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
