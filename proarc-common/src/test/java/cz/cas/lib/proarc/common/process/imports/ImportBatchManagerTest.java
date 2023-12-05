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

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.Batch.State;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.process.BatchManager;
import java.io.File;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Map;
import org.easymock.EasyMock;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportBatchManagerTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder();
    private AppConfiguration appConf;

    public ImportBatchManagerTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() throws Exception {
        // use temporary configuration
        File configHome = temp.newFolder(AppConfiguration.DEFAULT_APP_HOME_NAME);
        Map<String, String> env = new HashMap<String, String>();
        env.put(AppConfiguration.PROPERTY_APP_HOME, configHome.toString());
        appConf = AppConfigurationFactory.getInstance().create(env);

//        System.setProperty(AppConfiguration.ENV_APP_HOME, config.toString());
//        appConf = AppConfiguration.getInstance();
//        appConf.reload();
    }

    @After
    public void tearDown() {
//        System.clearProperty(AppConfiguration.ENV_APP_HOME);
//        appConf.reload();
    }

    @Test
    public void testBatchFolderStatus() throws Exception {
//        temp.setDeleteOnExit(false);
        DaoFactory df = EasyMock.createMock(DaoFactory.class);
        BatchManager ibm = new BatchManager(appConf, df);
        File batchRoot = new File(ibm.getBatchRoot());
        File batchFolder = new File(batchRoot, "importFolder");
        batchFolder.mkdir();
        Batch b = new Batch();
        b.setId(1);
        b.setCreate(new Timestamp(System.currentTimeMillis()));
        b.setDevice("device");
        b.setFolder(ibm.relativizeBatchFile(batchFolder));
        b.setGenerateIndices(true);
        b.setState(State.LOADING);

        ibm.updateFolderStatus(b);

        ImportFolderStatus result = ibm.getFolderStatus(b);
        assertEquals(b.getId(), result.getBatchId());
    }

    @Test
    public void testMissingBatchFolderStatus() throws Exception {
//        temp.setDeleteOnExit(false);
        DaoFactory df = EasyMock.createMock(DaoFactory.class);
        BatchManager ibm = new BatchManager(appConf, df);
        File batchRoot = new File(ibm.getBatchRoot());
        File batchFolder = new File(batchRoot, "importFolder");
        batchFolder.mkdir();
        Batch b = new Batch();
        b.setId(1);
        b.setCreate(new Timestamp(System.currentTimeMillis()));
        b.setDevice("device");
        b.setFolder(ibm.relativizeBatchFile(batchFolder));
        b.setGenerateIndices(true);
        b.setState(State.LOADING);

        ImportFolderStatus result = ibm.getFolderStatus(b);
        assertNull(result);
    }

//    @Test
//    public void testGetInstance() {
//        System.out.println("getInstance");
//        ImportBatchManager expResult = null;
//        ImportBatchManager result = ImportBatchManager.getInstance();
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//
//    @Test
//    public void testFindItems() {
//        System.out.println("findItems");
//        int batchId = 0;
//        ImportBatchManager instance = new ImportBatchManager();
//        Collection expResult = null;
//        Collection result = instance.findItems(batchId);
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//
//    @Test
//    public void testFindAll() {
//        System.out.println("findAll");
//        UserProfile user = null;
//        boolean withItems = false;
//        ImportBatchManager instance = new ImportBatchManager();
//        Collection expResult = null;
//        Collection result = instance.findAll(user, withItems);
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//
//    @Test
//    public void testAdd() {
//        System.out.println("add");
//        String path = "";
//        UserProfile user = null;
//        ImportBatchManager instance = new ImportBatchManager();
//        ImportBatch expResult = null;
//        ImportBatch result = instance.add(path, user);
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//
//    @Test
//    public void testAddItem() {
//        System.out.println("addItem");
//        int batchId = 0;
//        ImportItem item = null;
//        ImportBatchManager instance = new ImportBatchManager();
//        ImportItem expResult = null;
//        ImportItem result = instance.addItem(batchId, item);
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
}
