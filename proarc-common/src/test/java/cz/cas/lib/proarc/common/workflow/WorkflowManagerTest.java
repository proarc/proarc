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
import cz.cas.lib.proarc.common.dao.empiredb.EmpireWorkflowJobDaoTest;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.Task.State;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterView;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.model.ValueType;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinition;
import cz.cas.lib.proarc.common.workflow.profile.ValueMapSource;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.io.File;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.dbunit.database.IDatabaseConnection;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.DefaultDataSet;
import org.dbunit.dataset.DefaultTable;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ITable;
import org.dbunit.dataset.ReplacementDataSet;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

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
    private WorkflowProfiles wp;

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

    private static IDataSet emptyBatchSet(ProarcDatabase schema) throws Exception {
        ITable[] tables = {
            new DefaultTable(schema.tableWorkflowJob.getName()),
            new DefaultTable(schema.tableWorkflowTask.getName()),
            new DefaultTable(schema.tableWorkflowParameter.getName()),
            new DefaultTable(schema.tableWorkflowMaterial.getName()),
            new DefaultTable(schema.tableWorkflowDigObj.getName()),
            new DefaultTable(schema.tableWorkflowFolder.getName()),
            new DefaultTable(schema.tableWorkflowPhysicalDoc.getName()),
            new DefaultTable(schema.tableWorkflowMaterialInTask.getName()),
        };
        return new DefaultDataSet(tables);
    }

    private IDataSet database(IDataSet... ds) throws Exception {
        ReplacementDataSet rds = new ReplacementDataSet(new CompositeDataSet(ds));
        rds.addReplacementObject("{$user.home}", "relative/path/");
        Timestamp dbTimestamp = new Timestamp(System.currentTimeMillis());
        rds.addReplacementObject("{$now}", dbTimestamp);
        return rds;
    }

    private WorkflowManager initWorkflowManager(String profilesResourceName) throws Exception {
        File xmlWorkflow = temp.newFile("workflowTest.xml");
        FileUtils.copyURLToFile(WorkflowManagerTest.class.getResource(profilesResourceName), xmlWorkflow);
        WorkflowProfiles.setInstance(new WorkflowProfiles(xmlWorkflow));
        wp = WorkflowProfiles.getInstance();
        AppConfiguration config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, temp.getRoot().getPath());
        }});
        UserManager users = UserUtil.createUserManagerPostgressImpl(config, null, daos);
        WorkflowManager wm = new WorkflowManager(wp, daos, users, config);
        return wm;
    }

    @Test
    public void testAddJob() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(EmpireWorkflowJobDaoTest.class, "user.xml"),
                emptyBatchSet(schema)
                );
        final IDatabaseConnection dbcon = support.getConnection();
        support.cleanInsert(dbcon, db);
        dbcon.getConnection().commit();
        dbcon.close();

        WorkflowManager wm = initWorkflowManager("WorkflowManagerAddProfile.xml");
        WorkflowDefinition workflow = wp.getProfiles();
        assertNotNull(workflow);
        JobDefinition jobProfile = workflow.getJobs().get(0);
        assertNotNull(jobProfile);
        CatalogConfiguration c = new CatalogConfiguration("testCatalogId", "", new BaseConfiguration() {{
            addProperty(CatalogConfiguration.PROPERTY_URL, "tcp://localhost:9991");
            addProperty(CatalogConfiguration.PROPERTY_NAME, "test");
            addProperty(CatalogConfiguration.PROPERTY_TYPE, Z3950Catalog.TYPE);
        }});
        String mods = IOUtils.toString(WorkflowManagerTest.class.getResource("rdczmods.xml"));
        Job job = wm.addJob(jobProfile, mods, c, null, null, null);
        assertNotNull(job);

        Job getJob = wm.getJob(job.getId());
        assertNotNull(getJob);

        JobFilter filter = new JobFilter();
        filter.setId(job.getId());
        Locale locale = new Locale("cs");
        filter.setLocale(locale);

        List<JobView> findJob = wm.findJob(filter);
        assertEquals(1, findJob.size());
        assertEquals("csTitle", findJob.get(0).getProfileLabel());

        TaskFilter taskFilter = new TaskFilter();
        taskFilter.setLocale(locale);
        taskFilter.setJobId(job.getId());
        List<TaskView> findTask = wm.tasks().findTask(taskFilter, workflow);
        assertEquals(2, findTask.size());
        assertEquals(job.getId(), findTask.get(0).getJobId());

        MaterialFilter materialFilter = new MaterialFilter();
        materialFilter.setJobId(job.getId());
        materialFilter.setLocale(locale);
        List<MaterialView> findMaterial = wm.findMaterial(materialFilter);
        assertEquals(2, findMaterial.size());

        TaskParameterFilter paramFilter = new TaskParameterFilter();
        paramFilter.setLocale(locale);
        paramFilter.setProfileName("param.id1");
        List<TaskParameterView> findParameter = wm.findParameter(paramFilter);
        assertEquals(2, findParameter.size());
        assertEquals("param.id1", findParameter.get(0).getParamRef());
        assertEquals("param.id1.value", findParameter.get(0).getValue());
        assertEquals(job.getId(), findParameter.get(0).getJobId());
        assertEquals("Param1", findParameter.get(0).getProfileLabel());
        // param.id2 comes just from the profile not db!
        assertEquals("param.id2", findParameter.get(1).getParamRef());
        assertNull(findParameter.get(1).getValue());
        assertEquals(job.getId(), findParameter.get(1).getJobId());
        assertEquals("param.id2", findParameter.get(1).getProfileLabel());
        assertEquals(findParameter.get(0).getTaskId(), findParameter.get(1).getTaskId());
        assertEquals("task.id1", findParameter.get(1).getTaskProfileName());
        assertEquals("proarc.devices", findParameter.get(1).getValueMapId());
        assertEquals(ValueMapSource.PROARC, findParameter.get(1).getValueMapType());
        assertEquals(Boolean.TRUE, findParameter.get(1).getRequired());
        assertEquals(ValueType.STRING, findParameter.get(1).getValueType());
    }

    @Test
    public void testUpdateTaskState() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(EmpireWorkflowJobDaoTest.class, "user.xml"),
                support.loadFlatXmlDataStream(WorkflowManagerTest.class, "WorkflowManagerUpdateTaskState.xml")
                );
        final IDatabaseConnection dbcon = support.getConnection();
        support.cleanInsert(dbcon, db);
        dbcon.getConnection().commit();
        dbcon.close();

        WorkflowManager wm = initWorkflowManager("WorkflowManagerUpdateTaskStateProfile.xml");
        TaskManager tm = wm.tasks();
        WorkflowDefinition workflow = wp.getProfiles();
        assertNotNull(workflow);
        JobDefinition jobProfile = wp.getProfile(workflow, "job1");
        assertNotNull(jobProfile);
        Map<String, Object> params = null;

        TaskFilter taskFilter = new TaskFilter();
        taskFilter.setId(new BigDecimal(3));
        taskFilter.setLocale(Locale.ENGLISH);
        List<TaskView> tasks = tm.findTask(taskFilter, workflow);
        TaskView task = tasks.get(0);
        assertEquals(Task.State.WAITING, task.getState());
        task.setState(State.READY);
        try {
            Task update = tm.updateTask(task, params, workflow);
            fail("Cannot update blocked state! " + update.getStateAsString());
        } catch (WorkflowException ex) {
            assertEquals("Task is blocked by other tasks!", ex.getMessage());
        }

        // t2 waiting->ready
        taskFilter.setId(new BigDecimal(2));
        tasks = tm.findTask(taskFilter, workflow);
        task = tasks.get(0);
        assertEquals(Task.State.WAITING, tm.findTask(taskFilter, workflow).get(0).getState());
        task.setState(State.READY);
        Task update = tm.updateTask(task, params, workflow);
        assertEquals(Task.State.READY, update.getState());
        taskFilter.setId(new BigDecimal(3));
        assertEquals(Task.State.WAITING, tm.findTask(taskFilter, workflow).get(0).getState());

        // t2 ready->finished
        taskFilter.setId(new BigDecimal(2));
        tasks = tm.findTask(taskFilter, workflow);
        task = tasks.get(0);
        assertEquals(Task.State.READY, tm.findTask(taskFilter, workflow).get(0).getState());
        task.setState(State.FINISHED);
        update = tm.updateTask(task, params, workflow);
        assertEquals(Task.State.FINISHED, update.getState());
        taskFilter.setId(new BigDecimal(4));
        assertEquals(Task.State.READY, tm.findTask(taskFilter, workflow).get(0).getState());
        taskFilter.setId(new BigDecimal(12));
        assertEquals(Task.State.WAITING, tm.findTask(taskFilter, workflow).get(0).getState());
        taskFilter.setId(new BigDecimal(13));
        assertEquals(Task.State.WAITING, tm.findTask(taskFilter, workflow).get(0).getState());
    }

}
