/*
 * Copyright (C) 2018 Lukas Sykora
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
package cz.cas.lib.proarc.common.imports.audio;

import com.yourmediashelf.fedora.generated.foxml.ObjectFactory;
import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchItemDao;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.fedora.Aes57Editor;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.*;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import org.apache.commons.io.FileUtils;
import org.custommonkey.xmlunit.SimpleNamespaceContext;
import org.custommonkey.xmlunit.XMLAssert;
import org.custommonkey.xmlunit.XMLUnit;
import org.custommonkey.xmlunit.exceptions.XpathException;
import org.easymock.EasyMock;
import org.easymock.IAnswer;
import org.junit.*;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.*;

import static org.junit.Assert.*;

public class WaveImporterTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder();

    private File ac1;
    private File uc1;
    private AppConfiguration config;
    private ArrayList<Object> toVerify = new ArrayList<Object>();;
    private JhoveContext jhoveContext;
    private UserProfile junit;
    private ImportBatchManager ibm;

    public WaveImporterTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() throws Exception {
        junit = new UserProfile();
        junit.setUserName("junit");
        File root = temp.getRoot();
        System.out.println("root: " + root.toString());

        ac1 = new File(root, "audio1.ac.wav");
        URL resource = WaveImporterTest.class.getResource("test_wav.mc.wav");
        FileUtils.copyURLToFile(resource, ac1);
        assertTrue(ac1.length() > 0);

        uc1 = new File(root, "audio1.uc.mp3");
        resource = WaveImporterTest.class.getResource("test_mp3.uc.mp3");
        FileUtils.copyURLToFile(resource, uc1);
        assertTrue(uc1.length() > 0);

        config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, temp.getRoot().getPath());
        }});

        jhoveContext = JhoveUtility.createContext(temp.newFolder("jhove"));

        DaoFactory daos = createMockDaoFactory();
        ibm = new ImportBatchManager(config, daos);

        MetaModelRepository.setInstance(new String[]{NdkAudioPlugin.ID});
        DigitalObjectManager.setDefault(new DigitalObjectManager(config,
                ibm,
                null,
                MetaModelRepository.getInstance(),
                EasyMock.createNiceMock(UserManager.class))
        );
    }

    @After
    public void tearDown() {
        if (jhoveContext != null) {
            jhoveContext.destroy();
        }
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

    private static String streamXPath(String dsId) {
        return "f:digitalObject/f:datastream[@ID='" + dsId + "']";
    }

    @Test
    public void testConsume() throws IOException, SAXException, XpathException {
        temp.setDeleteOnExit(true);
        File targetFolder = ImportProcess.createTargetFolder(temp.getRoot());
        assertTrue(targetFolder.exists());

        String mimetype = ImportProcess.findMimeType(ac1);
        assertNotNull(mimetype);

        ImportOptions ctx = new ImportOptions(ac1.getParentFile(), "scanner:scanner1",
                true, junit, config.getImportConfiguration());
        ctx.setTargetFolder(targetFolder);
        Batch batch = new Batch();
        batch.setId(1);
        batch.setFolder(ibm.relativizeBatchFile(ac1.getParentFile()));
        ctx.setBatch(batch);
        assertNotNull(ac1);

        FileSet fileSet = ImportFileScanner.getFileSets(Arrays.asList(ac1, uc1)).get(0);
        ctx.setJhoveContext(jhoveContext);

        WaveImporter waveImporter = new WaveImporter(ibm);
        BatchItemObject result = waveImporter.consume(fileSet, ctx);

        String pid = result.getPid();
        assertTrue(pid.startsWith("uuid"));

        assertEquals(ObjectState.LOADED, result.getState());

        File foxml = result.getFile();
        assertTrue(foxml.toString(), foxml.exists());

        File rootFoxml = new File(foxml.getParent(), ImportBatchManager.ROOT_ITEM_FILENAME);
        assertTrue(rootFoxml.toString(), rootFoxml.exists());
/*
        File preview1 = new File(targetFolder, "audio1.preview.jpg");
        assertTrue(preview1.exists() && preview1.length() > 0);

        File thumb = new File(targetFolder, "audio1.thumb.jpg");
        assertTrue(thumb.exists() && thumb.length() > 0);

        File raw = new File(targetFolder, "audio1.full.jpg");
        assertTrue(raw.exists() && raw.length() > 0);
*/
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
        XMLAssert.assertXpathExists(streamXPath(RelationEditor.DATASTREAM_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.FULL_ID), new InputSource(foxmlSystemId));
        //XMLAssert.assertXpathExists(streamXPath(BinaryEditor.PREVIEW_ID), new InputSource(foxmlSystemId));
        //MLAssert.assertXpathExists(streamXPath(BinaryEditor.THUMB_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.RAW_ID), new InputSource(foxmlSystemId));
        //XMLAssert.assertXpathExists(streamXPath(Aes57Editor.RAW_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.NDK_ARCHIVAL_ID), new InputSource(foxmlSystemId));
        XMLAssert.assertXpathExists(streamXPath(BinaryEditor.NDK_USER_ID), new InputSource(foxmlSystemId));
        //XMLAssert.assertXpathExists(streamXPath(Aes57Editor.NDK_ARCHIVAL_ID), new InputSource(foxmlSystemId));

        String rootSystemId = rootFoxml.toURI().toASCIIString();
        XMLAssert.assertXpathExists(streamXPath(RelationEditor.DATASTREAM_ID), new InputSource(rootSystemId));
        EasyMock.verify(toVerify.toArray());
    }

    @Test
    public void testMissingRequiredContent() throws IOException {
        assertTrue(uc1.delete());
        assertTrue(config.getImportConfiguration().getRequiredDatastreamId().contains(BinaryEditor.NDK_ARCHIVAL_ID));

        File targetFolder = ImportProcess.createTargetFolder(temp.getRoot());
        assertTrue(targetFolder.exists());

        ImportOptions ctx = new ImportOptions(ac1.getParentFile(),
                "scanner:scanner1", true, junit, config.getImportConfiguration());
        ctx.setTargetFolder(targetFolder);
        Batch batch = new Batch();
        batch.setId(1);
        batch.setFolder(ibm.relativizeBatchFile(ac1.getParentFile()));
        ctx.setBatch(batch);
        FileSet fileSet = ImportFileScanner.getFileSets(Arrays.asList(ac1)).get(0);
        ctx.setJhoveContext(jhoveContext);

        WaveImporter waveImporter = new WaveImporter(ibm);
        BatchItemObject result = waveImporter.consume(fileSet, ctx);

        assertEquals(ObjectState.LOADING_FAILED, result.getState());
        String log = result.getLog();
        assertNotNull(log);
        assertTrue(log, log.contains("Missing audio user copy: "));
    }
}
