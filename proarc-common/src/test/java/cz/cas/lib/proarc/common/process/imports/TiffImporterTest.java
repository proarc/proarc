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
package cz.cas.lib.proarc.common.process.imports;

import com.yourmediashelf.fedora.generated.foxml.ObjectFactory;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dao.BatchItemDao;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.BatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.process.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.process.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.process.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
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
import org.easymock.EasyMock;
import org.easymock.IAnswer;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.xml.sax.InputSource;
import org.xmlunit.xpath.JAXPXPathEngine;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.xmlunit.assertj.XmlAssert.assertThat;

/**
 *
 * @author Jan Pokorsky
 */
public class TiffImporterTest {

    @TempDir
    File tempDir;

    private File tiff1;
    private File ocr1;
    private File alto1;
    private File ac1;
    private File uc1;
    private AppConfiguration config;
    private AkubraConfiguration akubraConfiguration;
    private ArrayList<Object> toVerify = new ArrayList<Object>();
    ;
    private JhoveContext jhoveContext;
    private UserProfile junit;
    private BatchManager ibm;

    public TiffImporterTest() {
    }

    @BeforeAll
    public static void setUpClass() throws Exception {
    }

    @AfterAll
    public static void tearDownClass() throws Exception {
    }

    @BeforeEach
    public void setUp() throws Exception {
        junit = new UserProfile();
        junit.setUserName("junit");
        File root = tempDir;
        System.out.println("root: " + root.toString());
        tiff1 = new File(root, "img1.tiff");

        URL resource = TiffImporterTest.class.getResource("testscan-lzw.tiff");
        FileUtils.copyURLToFile(resource, tiff1);
        assertTrue(tiff1.length() > 0);

        ocr1 = new File(root, "img1.ocr.txt");
        FileUtils.writeStringToFile(ocr1, "test", "UTF-8");

        alto1 = new File(root, "img1.ocr.xml");
        FileUtils.writeStringToFile(alto1,
                "<alto xmlns=\"http://www.loc.gov/standards/alto/ns-v2#\">"
                        + "<Layout><Page ID=\"Page1\" PHYSICAL_IMG_NR=\"1\"/></Layout>"
                        + "</alto>",
                "UTF-8");

        ac1 = new File(root, "img1.ac.jp2");
        resource = TiffImporterTest.class.getResource("testscan.uc.jp2");
        FileUtils.copyURLToFile(resource, ac1);
        assertTrue(ac1.length() > 0);
        uc1 = new File(root, "img1.uc.jp2");
        FileUtils.copyURLToFile(resource, uc1);
        assertTrue(uc1.length() > 0);

        config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, tempDir.getPath());
        }});
        if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(config.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }

        File jhoveFolder = new File(tempDir, "jhove");
        jhoveFolder.mkdirs();
        jhoveContext = JhoveUtility.createContext(jhoveFolder);

        DaoFactory daos = createMockDaoFactory();
        ibm = new BatchManager(config, daos);

//        MetaModelRepository.setInstance(new String[]{K4Plugin.ID});
        MetaModelRepository.setInstance(new String[]{NdkPlugin.ID});
        DigitalObjectManager.setDefault(new DigitalObjectManager(config, akubraConfiguration,
                ibm,
                MetaModelRepository.getInstance(),
                EasyMock.createNiceMock(UserManager.class))
        );
    }

    @AfterEach
    public void tearDown() {
        if (jhoveContext != null) {
            jhoveContext.destroy();
        }
    }

    @Test
    public void testConsume() throws Exception {
        File targetFolder = ImportProcess.createTargetFolder(tempDir, config.getImportConfiguration(), null);
        assertTrue(targetFolder.exists());

        String mimetype = ImportProcess.findMimeType(tiff1);
        assertNotNull(mimetype);

        ImportOptions ctx = new ImportOptions(tiff1.getParentFile(),
                "scanner:scanner1", "software:objectSet", true, junit, config.getImportConfiguration(), Batch.PRIORITY_MEDIUM);
        ctx.setTargetFolder(targetFolder);
        Batch batch = new Batch();
        batch.setId(1);
        batch.setFolder(ibm.relativizeBatchFile(tiff1.getParentFile()));
        ctx.setBatch(batch);
        FileSet fileSet = ImportFileScanner.getFileSets(Arrays.asList(tiff1, ocr1, alto1, ac1, uc1)).get(0);
        ctx.setJhoveContext(jhoveContext);

        TiffImporter instance = new TiffImporter(ibm);
        BatchItemObject result = instance.consume(fileSet, ctx);
        String pid = result.getPid();
        assertTrue(pid.startsWith("uuid"));

        assertEquals(ObjectState.LOADED, result.getState());

        File foxml = result.getFile();
        assertTrue(foxml.exists(), () -> foxml.toString());

        File rootFoxml = new File(foxml.getParent(), BatchManager.ROOT_ITEM_FILENAME);
        assertTrue(rootFoxml.exists(), () -> rootFoxml.toString());

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
        JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
        xpathEngine.setNamespaceContext(namespaces);

        String foxmlSystemId = foxml.toURI().toASCIIString();
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(ModsStreamEditor.DATASTREAM_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(DcStreamEditor.DATASTREAM_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(StringEditor.OCR_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(AltoDatastream.ALTO_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(RelationEditor.DATASTREAM_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(BinaryEditor.FULL_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(BinaryEditor.PREVIEW_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(BinaryEditor.THUMB_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(BinaryEditor.RAW_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(MixEditor.RAW_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(BinaryEditor.NDK_ARCHIVAL_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(BinaryEditor.NDK_USER_ID));
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(MixEditor.NDK_ARCHIVAL_ID));

        String rootSystemId = rootFoxml.toURI().toASCIIString();
        assertThat(new InputSource(foxmlSystemId)).hasXPath(streamXPath(RelationEditor.DATASTREAM_ID));
        EasyMock.verify(toVerify.toArray());
    }

    @Test
    public void testMissingRequiredContent() throws Exception {
        assertTrue(alto1.delete());
        assertTrue(config.getImportConfiguration().getRequiredDatastreamId().contains(AltoDatastream.ALTO_ID));

        File targetFolder = ImportProcess.createTargetFolder(tempDir, config.getImportConfiguration(), null);
        assertTrue(targetFolder.exists());

        String mimetype = ImportProcess.findMimeType(tiff1);
        assertNotNull(mimetype);

        ImportOptions ctx = new ImportOptions(tiff1.getParentFile(),
                "scanner:scanner1", "software:objectSet", true, junit, config.getImportConfiguration(), Batch.PRIORITY_MEDIUM);
        ctx.setTargetFolder(targetFolder);
        Batch batch = new Batch();
        batch.setId(1);
        batch.setFolder(ibm.relativizeBatchFile(tiff1.getParentFile()));
        ctx.setBatch(batch);
        FileSet fileSet = ImportFileScanner.getFileSets(Arrays.asList(tiff1, ocr1, ac1, uc1)).get(0);
        ctx.setJhoveContext(jhoveContext);

        TiffImporter instance = new TiffImporter(ibm);
        BatchItemObject result = instance.consume(fileSet, ctx);

        assertEquals(ObjectState.LOADING_FAILED, result.getState());
        String log = result.getLog();
        assertNotNull(log);
        assertTrue(log.contains("Missing ALTO"), () -> log);
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
