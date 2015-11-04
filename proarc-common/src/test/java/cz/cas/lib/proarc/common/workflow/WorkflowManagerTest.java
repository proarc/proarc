/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.workflow;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.catalog.Z3950Catalog;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.dao.empiredb.DbUnitSupport;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireDaoFactory;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.io.File;
import java.util.HashMap;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Rule;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowManagerTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireDaoFactory daos;

    public WorkflowManagerTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        support = new DbUnitSupport();
        schema = support.getEmireCfg().getSchema();
        daos = new EmpireDaoFactory(support.getEmireCfg());
        daos.init();
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testAddJob() throws Exception {
        File xmlWorkflow = temp.newFile("workflowTest.xml");
        FileUtils.copyURLToFile(WorkflowManagerTest.class.getResource("WorkflowManagerAddProfile.xml"), xmlWorkflow);
        WorkflowProfiles wp = new WorkflowProfiles(xmlWorkflow);
        JobDefinition jobProfile = wp.getProfiles().getJobs().get(0);
        AppConfiguration config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, temp.getRoot().getPath());
        }});
        UserManager users = UserUtil.createUserManagerPostgressImpl(config, null, daos);
        WorkflowManager wm = new WorkflowManager(daos, users);
        CatalogConfiguration c = new CatalogConfiguration("testCatalogId", "", new BaseConfiguration() {{
            addProperty(CatalogConfiguration.PROPERTY_URL, "tcp://localhost:9991");
            addProperty(CatalogConfiguration.PROPERTY_NAME, "test");
            addProperty(CatalogConfiguration.PROPERTY_TYPE, Z3950Catalog.TYPE);
        }});
        wm.addJob(jobProfile, "<mods></mods>", c, null);
    }

}
