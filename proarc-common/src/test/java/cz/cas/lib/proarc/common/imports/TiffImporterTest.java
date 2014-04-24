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
package cz.cas.lib.proarc.common.imports;

import com.yourmediashelf.fedora.generated.foxml.ObjectFactory;
import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dao.BatchItemDao;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.StringEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.apache.commons.io.FileUtils;
import org.custommonkey.xmlunit.SimpleNamespaceContext;
import org.custommonkey.xmlunit.XMLAssert;
import org.custommonkey.xmlunit.XMLUnit;
import org.easymock.EasyMock;
import org.easymock.IAnswer;
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
    private File ocr1;
    private File alto1;
    private File ac1;
    private File uc1;
    private AppConfiguration config;
    private ArrayList<Object> toVerify = new ArrayList<Object>();;

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

        URL resource = TiffImporterTest.class.getResource("testscan-lzw.tiff");
        FileUtils.copyURLToFile(resource, tiff1);
        assertTrue(tiff1.length() > 0);

        ocr1 = new File(root, "img1.ocr.txt");
        FileUtils.writeStringToFile(ocr1, "test", "UTF-8");

        alto1 = new File(root, "img1.ocr.xml");
        FileUtils.writeStringToFile(alto1,
                "<alto xmlns=\"http://www.loc.gov/standards/alto/ns-v2#\"></alto>",
                "UTF-8");

        ac1 = new File(root, "img1.ac.jp2");
        ac1.createNewFile();
        uc1 = new File(root, "img1.uc.jp2");
        uc1.createNewFile();

        config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, temp.getRoot().getPath());
        }});
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testConsume() throws Exception {
        temp.setDeleteOnExit(true);
        File targetFolder = ImportProcess.createTargetFolder(temp.getRoot());
        assertTrue(targetFolder.exists());

        DaoFactory daos = createMockDaoFactory();
        ImportBatchManager ibm = new ImportBatchManager(config, daos);

        String mimetype = ImportProcess.findMimeType(tiff1);
        assertNotNull(mimetype);

        ImportOptions ctx = new ImportOptions(tiff1.getParentFile(), "model:page",
                "scanner:scanner1", true, "junit", config.getImportConfiguration());
        ctx.setTargetFolder(targetFolder);
        Batch batch = new Batch();
        batch.setId(1);
        batch.setFolder(ibm.relativizeBatchFile(tiff1.getParentFile()));
        ctx.setBatch(batch);
        FileSet fileSet = ImportFileScanner.getFileSets(Arrays.asList(tiff1, ocr1, alto1)).get(0);

        TiffImporter instance = new TiffImporter(ibm);
        BatchItemObject result = instance.consume(fileSet, ctx);
        String pid = result.getPid();
        assertTrue(pid.startsWith("uuid"));

        assertEquals(ObjectState.LOADED, result.getState());
        
        File foxml = result.getFile();
        assertTrue(foxml.toString(), foxml.exists());

        File rootFoxml = new File(foxml.getParent(), ImportBatchManager.ROOT_ITEM_FILENAME);
        assertTrue(rootFoxml.toString(), rootFoxml.exists());

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
        HashMap<String, String> namespaces = new HashMap<String, String>();
        namespaces.put("f", "info:fedora/fedora-system:def/foxml#");
        XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(namespaces));
        String foxmlSystemId = foxml.toURI().toASCIIString();
        XMLAssert.assertXpathExists(streamXPath(ModsStreamEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(DcStreamEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(StringEditor.OCR_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(AltoDatastream.ALTO_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(RelationEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.FULL_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.PREVIEW_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.THUMB_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.RAW_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.NDK_ARCHIVAL_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.NDK_USER_ID), new InputSource(foxmlSystemId));

        String rootSystemId = rootFoxml.toURI().toASCIIString();
        XMLAssert.assertXpathExists(streamXPath(RelationEditor.DATASTREAM_ID), new InputSource(rootSystemId));
        EasyMock.verify(toVerify.toArray());
    }

    private static String streamXPath(String dsId) {
        return "f:digitalObject/f:datastream[@ID='" + dsId + "']";
    }

    private DaoFactory createMockDaoFactory() {
        DaoFactory daos = EasyMock.createMock(DaoFactory.class);
        EasyMock.expect(daos.createTransaction()).andAnswer(new IAnswer<Transaction>() {

            @Override
            public Transaction answer() throws Throwable {
                return createMockTransaction();
            }
        }).anyTimes();
        EasyMock.expect(daos.createBatchItem()).andAnswer(new IAnswer<BatchItemDao>() {

            @Override
            public BatchItemDao answer() throws Throwable {
                return createMockBatchItemDao();
            }
        }).anyTimes();
        EasyMock.replay(daos);
        toVerify.add(daos);
        return daos;
    }

    private Transaction createMockTransaction() {
        Transaction tx = EasyMock.createMock(Transaction.class);
        tx.commit();
        EasyMock.expectLastCall().atLeastOnce();
        tx.rollback();
        EasyMock.expectLastCall().anyTimes();
        tx.close();
        EasyMock.replay(tx);
        toVerify.add(tx);
        return tx;
    }

    private BatchItemDao createMockBatchItemDao() {
        BatchItemDao dao = EasyMock.createMock(BatchItemDao.class);
        dao.update(EasyMock.<BatchItem>anyObject());
        EasyMock.expectLastCall().anyTimes();
        EasyMock.expect(dao.create()).andReturn(new BatchItem()).anyTimes();
        dao.setTransaction(EasyMock.<Transaction>anyObject());
        EasyMock.replay(dao);
        toVerify.add(dao);
        return dao;
    }

}
