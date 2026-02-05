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
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;


/**
 *
 * @author Jan Pokorsky
 */
public class ImportBatchManagerTest {

    @TempDir
    File tempDir;

    private AppConfiguration appConf;

    public ImportBatchManagerTest() {
    }

    @BeforeAll
    public static void setUpClass() throws Exception {
    }

    @AfterAll
    public static void tearDownClass() throws Exception {
    }

    @BeforeEach
    public void setUp() throws Exception {
        // use temporary configuration
        File configHome = new File(tempDir, AppConfiguration.DEFAULT_APP_HOME_NAME);
        configHome.mkdirs();
        Map<String, String> env = new HashMap<String, String>();
        env.put(AppConfiguration.PROPERTY_APP_HOME, configHome.toString());
        appConf = AppConfigurationFactory.getInstance().create(env);

//        System.setProperty(AppConfiguration.ENV_APP_HOME, config.toString());
//        appConf = AppConfiguration.getInstance();
//        appConf.reload();
    }

    @AfterEach
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
        b.setSoftware("software:objectSet");
        b.setFolder(ibm.relativizeBatchFile(batchFolder));
        b.setGenerateIndices(true);
        b.setState(State.LOADING);
        b.setUpdated(new Timestamp(System.currentTimeMillis()));

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
        b.setSoftware("software:objectSet");
        b.setFolder(ibm.relativizeBatchFile(batchFolder));
        b.setGenerateIndices(true);
        b.setState(State.LOADING);
        b.setUpdated(new Timestamp(System.currentTimeMillis()));

        ImportFolderStatus result = ibm.getFolderStatus(b);
        assertNull(result);
    }

}
