/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dao.empiredb.DbUnitSupport;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireBatchDaoTest;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireDaoFactory;
import cz.cas.lib.proarc.common.dao.empiredb.SqlTransaction;
import cz.cas.lib.proarc.common.fedora.FedoraTestSupport;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.process.imports.FedoraImport;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.BatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import java.io.File;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ReplacementDataSet;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class FedoraImportTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder();
    @Rule
    public TestName testName = new TestName();

    private AppConfiguration appConf;
    private static DbUnitSupport dbsupport;
    private static EmpireDaoFactory daos;
    private FedoraImport fedoraImport;
    private static FedoraTestSupport fedoraSupport;
    private BatchManager ibm;
    private LocalStorage localStorage;
    private RemoteStorage remoteStorage;
    private SearchView search;

    public FedoraImportTest() {
    }

    @BeforeClass
    public static void setUpClass() {
        fedoraSupport = new FedoraTestSupport();
        dbsupport = new DbUnitSupport();
        daos = new EmpireDaoFactory(dbsupport.getEmireCfg());
        daos.init();
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() throws Exception {
        fedoraSupport.cleanUp();
        MetaModelRepository.setInstance(new String[0]);
        localStorage = new LocalStorage();
        remoteStorage = RemoteStorage.getInstance();
        search = remoteStorage.getSearch();

        File configHome = temp.newFolder(AppConfiguration.DEFAULT_APP_HOME_NAME);
        Map<String, String> env = new HashMap<String, String>();
        env.put(AppConfiguration.PROPERTY_APP_HOME, configHome.toString());
        appConf = AppConfigurationFactory.getInstance().create(env);

        ibm = new BatchManager(appConf, daos);
        fedoraImport = new FedoraImport(appConf, remoteStorage, ibm, null, null);
    }

    @After
    public void tearDown() {
    }

    private ReplacementDataSet database(IDataSet... ds) throws Exception {
        ReplacementDataSet rds = new ReplacementDataSet(new CompositeDataSet(ds));
        rds.addReplacementObject("{$user.home}", "relative/path/");
        Timestamp dbTimestamp = new Timestamp(System.currentTimeMillis());
        rds.addReplacementObject("{$now}", dbTimestamp);
        return rds;
    }

    @Test
    public void testImportItem() throws Exception {
        File batchFolder = temp.newFolder("batch");
        // prepare foxml 1
        LocalObject lobj = localStorage.create(new File(batchFolder, "item1.foxml"));
        lobj.setOwner(fedoraSupport.getTestUser());
        lobj.setLabel("item1");
        RelationEditor locRels = new RelationEditor(lobj);
        locRels.setModel("model:page");
        locRels.write(locRels.getLastModified(), "test");
        lobj.flush();
        // prepare remote parent
        RemoteObject parent = createParent(testName.getMethodName() + "_parent");

        ReplacementDataSet db = database(
                dbsupport.loadFlatXmlDataStream(EmpireBatchDaoTest.class, "user.xml"),
                dbsupport.loadFlatXmlDataStream(getClass(), "batch.xml")
        );
        db.addReplacementObject("{$parent_pid}", parent.getPid());
        SqlTransaction tx = daos.createTransaction();
        dbsupport.cleanInsert(dbsupport.getConnection(tx), db);
        tx.commit();

        // add item 1
        Batch batch = ibm.get(1);
        assertNotNull(batch);
        BatchItemObject item1 = ibm.addLocalObject(batch, lobj);
        item1.setState(ObjectState.LOADED);
        ibm.update(item1);

        // import
        BatchItemObject result = fedoraImport.importItem(
                item1, fedoraSupport.getTestUser(), false);
        assertNotNull(result);
        assertEquals(ObjectState.INGESTED, result.getState());
        result = ibm.findBatchObject(batch.getId(), item1.getPid());
        assertEquals(ObjectState.INGESTED, result.getState());

        List<SearchViewItem> fItems = search.find(lobj.getPid());
        assertEquals(1, fItems.size());
        FedoraTestSupport.assertItem(fItems, lobj.getPid());
    }

    @Test
    public void testImportItemFailure() throws Exception {
        File batchFolder = temp.newFolder("batch");
        // prepare foxml 1
        LocalObject lobj = localStorage.create(new File(batchFolder, "item1.foxml"));
        lobj.setOwner(fedoraSupport.getTestUser());
        lobj.setLabel("item1");
        RelationEditor locRels = new RelationEditor(lobj);
        locRels.setModel("model:page");
        locRels.write(locRels.getLastModified(), "test");
        lobj.flush();
        // prepare remote parent
        RemoteObject parent = createParent(testName.getMethodName() + "_parent");

        ReplacementDataSet db = database(
                dbsupport.loadFlatXmlDataStream(EmpireBatchDaoTest.class, "user.xml"),
                dbsupport.loadFlatXmlDataStream(getClass(), "batch.xml")
        );
        db.addReplacementObject("{$parent_pid}", parent.getPid());
        SqlTransaction tx = daos.createTransaction();
        dbsupport.cleanInsert(dbsupport.getConnection(tx), db);
        tx.commit();

        // add item 1
        Batch batch = ibm.get(1);
        assertNotNull(batch);
        BatchItemObject item1 = ibm.addLocalObject(batch, lobj);
        item1.setState(ObjectState.LOADED);
        ibm.update(item1);

        // import deleted foxml
        assertTrue(lobj.getFoxml().delete());
        BatchItemObject result = fedoraImport.importItem(
                item1, fedoraSupport.getTestUser(), false);
        assertNotNull(result);
        assertEquals(ObjectState.INGESTING_FAILED, result.getState());
        assertNotNull(result.getLog());
        result = ibm.findBatchObject(batch.getId(), item1.getPid());
        assertEquals(ObjectState.INGESTING_FAILED, result.getState());
        assertNotNull(result.getLog());

        List<SearchViewItem> fItems = search.find(lobj.getPid());
        assertEquals(0, fItems.size());
    }

    @Test
    public void testImportItemRepairFailure() throws Exception {
        File batchFolder = temp.newFolder("batch");
        // prepare foxml 1
        LocalObject lobj = localStorage.create(new File(batchFolder, "item1.foxml"));
        lobj.setOwner(fedoraSupport.getTestUser());
        lobj.setLabel("item1");
        RelationEditor locRels = new RelationEditor(lobj);
        locRels.setModel("model:page");
        locRels.write(locRels.getLastModified(), "test");
        lobj.flush();
        // prepare remote parent
        RemoteObject parent = createParent(testName.getMethodName() + "_parent");

        ReplacementDataSet db = database(
                dbsupport.loadFlatXmlDataStream(EmpireBatchDaoTest.class, "user.xml"),
                dbsupport.loadFlatXmlDataStream(getClass(), "batch.xml")
        );
        db.addReplacementObject("{$parent_pid}", parent.getPid());
        SqlTransaction tx = daos.createTransaction();
        dbsupport.cleanInsert(dbsupport.getConnection(tx), db);
        tx.commit();

        // add item 1
        Batch batch = ibm.get(1);
        assertNotNull(batch);
        BatchItemObject item1 = ibm.addLocalObject(batch, lobj);
        item1.setState(ObjectState.INGESTING_FAILED);
        item1.setLog("failure");
        ibm.update(item1);

        // import
        BatchItemObject result = fedoraImport.importItem(
                item1, fedoraSupport.getTestUser(), true);
        assertNotNull(result);
        assertEquals(ObjectState.INGESTED, result.getState());
        assertNull(result.getLog());
        result = ibm.findBatchObject(batch.getId(), item1.getPid());
        assertEquals(ObjectState.INGESTED, result.getState());
        assertNull(result.getLog());

        List<SearchViewItem> fItems = search.find(lobj.getPid());
        assertEquals(1, fItems.size());
        FedoraTestSupport.assertItem(fItems, lobj.getPid());
    }

    @Test
    public void testImportItemRepairNotLinked() throws Exception {
        File batchFolder = temp.newFolder("batch");
        // prepare foxml 1
        LocalObject lobj = localStorage.create(new File(batchFolder, "item1.foxml"));
        lobj.setOwner(fedoraSupport.getTestUser());
        lobj.setLabel("item1");
        RelationEditor locRels = new RelationEditor(lobj);
        locRels.setModel("model:page");
        locRels.write(locRels.getLastModified(), "test");
        lobj.flush();
        // ingested but not linked by parent; caused by a blackout
        remoteStorage.ingest(lobj, fedoraSupport.getTestUser(), "junit");
        // prepare remote parent
        RemoteObject parent = createParent(testName.getMethodName() + "_parent");

        ReplacementDataSet db = database(
                dbsupport.loadFlatXmlDataStream(EmpireBatchDaoTest.class, "user.xml"),
                dbsupport.loadFlatXmlDataStream(getClass(), "batch.xml")
        );
        db.addReplacementObject("{$parent_pid}", parent.getPid());
        SqlTransaction tx = daos.createTransaction();
        dbsupport.cleanInsert(dbsupport.getConnection(tx), db);
        tx.commit();

        // add item 1
        Batch batch = ibm.get(1);
        assertNotNull(batch);
        BatchItemObject item1 = ibm.addLocalObject(batch, lobj);
        item1.setState(ObjectState.INGESTED);
        ibm.update(item1);

        // import
        BatchItemObject result = fedoraImport.importItem(
                item1, fedoraSupport.getTestUser(), true);
        assertNotNull(result);
        assertEquals(ObjectState.INGESTED, result.getState());
        result = ibm.findBatchObject(batch.getId(), item1.getPid());
        assertEquals(ObjectState.INGESTED, result.getState());

        List<SearchViewItem> fItems = search.find(lobj.getPid());
        assertEquals(1, fItems.size());
        FedoraTestSupport.assertItem(fItems, lobj.getPid());
    }

    @Test
    public void testImportItemRepairLinked() throws Exception {
        File batchFolder = temp.newFolder("batch");
        // prepare foxml 1
        LocalObject lobj = localStorage.create(new File(batchFolder, "item1.foxml"));
        lobj.setOwner(fedoraSupport.getTestUser());
        lobj.setLabel("item1");
        RelationEditor locRels = new RelationEditor(lobj);
        locRels.setModel("model:page");
        locRels.write(locRels.getLastModified(), "test");
        lobj.flush();
        // ingested and linked by parent; caused by a following blackout
        remoteStorage.ingest(lobj, fedoraSupport.getTestUser(), "junit");
        // prepare remote parent
        RemoteObject parent = createParent(testName.getMethodName() + "_parent", lobj.getPid());

        ReplacementDataSet db = database(
                dbsupport.loadFlatXmlDataStream(EmpireBatchDaoTest.class, "user.xml"),
                dbsupport.loadFlatXmlDataStream(getClass(), "batch.xml")
        );
        db.addReplacementObject("{$parent_pid}", parent.getPid());
        SqlTransaction tx = daos.createTransaction();
        dbsupport.cleanInsert(dbsupport.getConnection(tx), db);
        tx.commit();

        // add item 1
        Batch batch = ibm.get(1);
        assertNotNull(batch);
        BatchItemObject item1 = ibm.addLocalObject(batch, lobj);
        item1.setState(ObjectState.INGESTED);
        ibm.update(item1);

        // import
        BatchItemObject result = fedoraImport.importItem(
                item1, fedoraSupport.getTestUser(), true);
        // test skipped by repair process
        assertNull(result);
        result = ibm.findBatchObject(batch.getId(), item1.getPid());
        assertEquals(ObjectState.INGESTED, result.getState());

        List<SearchViewItem> fItems = search.find(lobj.getPid());
        assertEquals(1, fItems.size());
        FedoraTestSupport.assertItem(fItems, lobj.getPid());
    }

    private RemoteObject createParent(String label, String... childPid) throws Exception {
        LocalObject lobj = localStorage.create();
        lobj.setOwner(fedoraSupport.getTestUser());
        lobj.setLabel(label);
        RelationEditor locRels = new RelationEditor(lobj);
        locRels.setModel(NdkPlugin.MODEL_MONOGRAPHVOLUME);
        locRels.setMembers(Arrays.asList(childPid));
        locRels.write(locRels.getLastModified(), "test");
        lobj.flush();
        remoteStorage.ingest(lobj, lobj.getOwner(), "junit test");
        return remoteStorage.find(lobj.getPid());
    }

}
