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

import cz.incad.pas.editor.server.CustomTemporaryFolder;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor;
import cz.incad.pas.editor.server.fedora.BinaryEditor;
import cz.incad.pas.editor.server.fedora.FedoraTestSupport;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.fedora.StringEditor;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.fedora.relation.Relations;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import java.io.File;
import java.util.HashMap;
import org.custommonkey.xmlunit.SimpleNamespaceContext;
import org.custommonkey.xmlunit.XMLAssert;
import org.custommonkey.xmlunit.XMLUnit;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.xml.sax.InputSource;

/**
 * 
 * @author Jan Pokorsky
 */
public class Kramerius4ExportTest {

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

    /**
     * integration test
     */
    @Test
    public void testExport() throws Exception {
        FedoraTestSupport fedora = new FedoraTestSupport();
        fedora.cleanUp();
        fedora.ingest(Kramerius4ExportTest.class.getResource("Kramerius4ExportTestPage.xml"));

        File output = temp.getRoot();
        boolean hierarchy = true;
        String[] pids = {"uuid:f74f3cf3-f3be-4cac-95da-8e50331414a2"};
        RemoteStorage storage = fedora.getRemoteStorage();
        Kramerius4Export instance = new Kramerius4Export(storage);
        File target = instance.export(output, hierarchy, pids);
        assertNotNull(target);

        // check datastreams with xpath
        HashMap<String, String> namespaces = new HashMap<String, String>();
        namespaces.put("f", "info:fedora/fedora-system:def/foxml#");
        namespaces.put("kramerius", Kramerius4Export.KRAMERIUS_RELATION_NS);
        namespaces.put("oai", Kramerius4Export.OAI_NS);
        namespaces.put("proarc-rels", Relations.PROARC_RELS_NS);
        XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(namespaces));
        File foxml = Kramerius4Export.pidAsFile(target, pids[0]);
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

        // test ingest of exported object
        fedora.cleanUp();
        fedora.ingest(foxml.toURI().toURL());
    }

    private static String streamXPath(String dsId) {
        return "f:digitalObject/f:datastream[@ID='" + dsId + "']";
    }

}
