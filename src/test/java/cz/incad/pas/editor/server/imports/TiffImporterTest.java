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
package cz.incad.pas.editor.server.imports;

import com.yourmediashelf.fedora.generated.foxml.ObjectFactory;
import cz.incad.pas.editor.server.CustomTemporaryFolder;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.imports.ImportProcess.ImportContext;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
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
public class TiffImporterTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder();
    
    private File tiff1;

    public TiffImporterTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() throws Exception {
        File root = temp.getRoot();
        System.out.println("root: " + root.toString());
        tiff1 = new File(root, "img1.tiff");

        FileOutputStream os = new FileOutputStream(tiff1);
        InputStream is = TiffImporterTest.class.getResourceAsStream("testscan-lzw.tiff");
//        InputStream is = TiffImporterTest.class.getResourceAsStream("testscan.tiff");
        try {
            byte[] buf = new byte[2048];
            for (int i = is.read(buf); i >= 0; i = is.read(buf)) {
                os.write(buf, 0, i);
            }
        } finally {
            os.flush();
            os.close();
            is.close();
        }
        assertTrue(tiff1.length() > 0);
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testConsume() throws Exception {
        temp.setDeleteOnExit(false);
        File targetFolder = temp.newFolder();
        assertTrue(targetFolder.exists());

        String mimetype = ImportProcess.findMimeType(tiff1);
        assertNotNull(mimetype);

        ImportContext ctx = new ImportContext(targetFolder, true, "junit");
        TiffImporter instance = new TiffImporter();
//        FedoraImportItem expResult = null;
        ImportItem result = instance.consume(tiff1, mimetype, ctx);
        String pid = result.getPid();
        assertTrue(pid.startsWith("uuid"));
        
        File foxml = result.getFoxmlAsFile();
        assertTrue(foxml.exists());

        File raw1 = new File(targetFolder, "img1.full.jpg");
        assertTrue(raw1.exists() && raw1.length() > 0);

        File preview1 = new File(targetFolder, "img1.preview.jpg");
        assertTrue(preview1.exists() && preview1.length() > 0);

        File thumb1 = new File(targetFolder, "img1.thumb.jpg");
        assertTrue(thumb1.exists() && thumb1.length() > 0);

        // validate FOXML
        SchemaFactory sfactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        URL foxmlXsdUrl = ObjectFactory.class.getResource("/xsd/foxml/foxml1-1.xsd");
        assertNotNull(foxmlXsdUrl);
        Schema foxmlXsd = sfactory.newSchema(foxmlXsdUrl);
        foxmlXsd.newValidator().validate(new StreamSource(foxml));

        // check datastreams with xpath
        HashMap namespaces = new HashMap();
        namespaces.put("f", "info:fedora/fedora-system:def/foxml#");
        XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(namespaces));
        String foxmlSystemId = foxml.toURI().toASCIIString();
        XMLAssert.assertXpathExists("f:digitalObject/f:datastream[@ID='BIBLIO_MODS']", new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists("f:digitalObject/f:datastream[@ID='DC']", new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists("f:digitalObject/f:datastream[@ID='TEXT_OCR']", new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists("f:digitalObject/f:datastream[@ID='RELS-EXT']", new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists("f:digitalObject/f:datastream[@ID='IMG_FULL']", new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists("f:digitalObject/f:datastream[@ID='IMG_PREVIEW']", new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists("f:digitalObject/f:datastream[@ID='IMG_THUMB']", new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists("f:digitalObject/f:datastream[@ID='IMG_RAW']", new InputSource(foxmlSystemId));
    }
}
