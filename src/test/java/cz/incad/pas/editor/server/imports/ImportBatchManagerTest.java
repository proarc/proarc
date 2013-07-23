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

import cz.incad.pas.editor.server.CustomTemporaryFolder;
import cz.incad.pas.editor.server.config.AppConfiguration;
import cz.incad.pas.editor.server.config.AppConfigurationFactory;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

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
