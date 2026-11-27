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
package cz.cas.lib.proarc.common.dao.empiredb;

import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.Job.State;
import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.Task;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ReplacementDataSet;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireWorkflowJobDaoTest {
    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireDaoFactory daos;
    private SqlTransaction tx;
    private EmpireWorkflowJobDao dao;
    private Timestamp dbTimestamp;

    @BeforeEach
    public void setUp() {
        support = new DbUnitSupport();
        schema = support.getEmireCfg().getSchema();
        daos = new EmpireDaoFactory(support.getEmireCfg());
        daos.init();
        tx = daos.createTransaction();
        dao = daos.createWorkflowJobDao();
        dao.setTransaction(tx);
    }

    @AfterEach
    public void tearDown() {
        if (tx != null) {
            tx.close();
        }
    }

    private IDataSet database(IDataSet... ds) throws Exception {
        ReplacementDataSet rds = new ReplacementDataSet(new CompositeDataSet(ds));
        rds.addReplacementObject("{$user.home}", "relative/path/");
        dbTimestamp = new Timestamp(System.currentTimeMillis());
        rds.addReplacementObject("{$now}", dbTimestamp);
        return rds;
    }

    @Test
    public void testCreate() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml")
        );
        support.cleanInsert(support.getConnection(tx), db);

        Job job = dao.create();
        job.addCreated(dbTimestamp).addFinanced("financed").addLabel("label")
                .addNote("note").addOwnerId(BigDecimal.ONE).addPriority(1)
                .addProfileName("profile").setState(State.OPEN).addTimestamp(dbTimestamp);
        dao.update(job);
        tx.commit();

        Job result = dao.find(job.getId());
        assertNotNull(result);
        assertEquals(job.getCreated(), result.getCreated());
        assertEquals(job.getFinanced(), result.getFinanced());
        assertEquals(job.getId(), result.getId());
        assertEquals(job.getLabel(), result.getLabel());
        assertEquals(job.getNote(), result.getNote());
        assertEquals(job.getOwnerId(), result.getOwnerId());
        assertEquals(job.getPriority(), result.getPriority());
        assertEquals(job.getProfileName(), result.getProfileName());
        assertEquals(job.getState(), result.getState());
        assertEquals(job.getTimestamp(), result.getTimestamp());
    }

    @Test
    public void testUpdate() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml")
        );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        Job job = dao.find(BigDecimal.ONE);
        job.setLabel("new label");
        final BigDecimal parentId = BigDecimal.valueOf(2);
        job.setParentId(parentId);
        job.setTimestamp(new Timestamp(job.getTimestamp().getTime()));
        dao.update(job);
        tx.commit();

        Job result = dao.find(BigDecimal.ONE);
        assertEquals("new label", result.getLabel());
        assertEquals(parentId, result.getParentId());
    }

    @Test
    public void testView() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml")
        );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        JobFilter filter = new JobFilter();
        filter.setId(BigDecimal.ONE);
        filter.setLabel("Monograph1");
        List<JobView> jobs = dao.view(filter);
        assertEquals(1, jobs.size());
        JobView job0 = jobs.get(0);
        assertEquals(BigDecimal.ONE, job0.getId());
        assertEquals("job.ndk", job0.getProfileName());
        assertNull(job0.getUserName());
        assertNull(job0.getTaskDoneId());
        assertNull(job0.getNextTaskId());
    }

    @ParameterizedTest
    @CsvSource(value = {
        "STARTED, 30, READY, 1, task.first",
        "READY, 1, STARTED, 30, task.second",
        "READY, 30, READY, 1, task.second",
        "STARTED, 30, STARTED, 1, task.second",
        "READY, 1, READY, 1, task.first",
        "WAITING, 0, CANCELED, 1, NULL",
        "FINISHED, 0, WAITING, 1, NULL",
        "READY, NULL, READY, 1, task.second",
        "READY, NULL, READY, NULL, task.first"
    }, nullValues = "NULL")
    public void testNextTaskSelection(Task.State firstState, Integer firstOrder,
            Task.State secondState, Integer secondOrder, String expected) throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"));
        support.cleanInsert(support.getConnection(tx), db);
        addTask(BigDecimal.ONE, "task.first", firstState, firstOrder);
        addTask(BigDecimal.ONE, "task.second", secondState, secondOrder);
        tx.commit();

        JobFilter filter = new JobFilter();
        filter.setId(BigDecimal.ONE);
        List<JobView> jobs = dao.view(filter);
        assertEquals(1, jobs.size());
        assertEquals(expected, jobs.get(0).getNextTaskId());
    }

    @Test
    public void testTaskSummaryIsPerJobAndKeepsCompletedTask() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"));
        support.cleanInsert(support.getConnection(tx), db);
        addTask(BigDecimal.ONE, "task.finished.first", Task.State.FINISHED, 2);
        addTask(BigDecimal.ONE, "task.finished.last", Task.State.FINISHED, 1);
        addTask(BigDecimal.ONE, "task.ready", Task.State.READY, 3);
        addTask(BigDecimal.valueOf(2), "task.otherJob", Task.State.STARTED, 0);
        tx.commit();

        JobFilter filter = new JobFilter();
        filter.setSortBy("id");
        List<JobView> jobs = dao.view(filter);
        assertEquals(2, jobs.size());
        JobView first = jobs.stream().filter(job -> job.getId().equals(BigDecimal.ONE)).findFirst().orElseThrow();
        JobView second = jobs.stream().filter(job -> job.getId().equals(BigDecimal.valueOf(2))).findFirst().orElseThrow();
        assertEquals("task.finished.last", first.getTaskDoneId());
        assertEquals(first.getTaskName(), first.getTaskDoneId());
        assertEquals("task.ready", first.getNextTaskId());
        assertEquals("task.otherJob", second.getNextTaskId());
        assertNull(second.getTaskDoneId());

        JobFilter page = new JobFilter();
        page.setSortBy("id");
        page.setMaxCount(1);
        page.setOffset(1);
        List<JobView> pagedJobs = dao.view(page);
        assertEquals(1, pagedJobs.size());
        JobView expected = jobs.get(1);
        assertEquals(expected.getId(), pagedJobs.get(0).getId());
        assertEquals(expected.getNextTaskId(), pagedJobs.get(0).getNextTaskId());
    }

    private void addTask(BigDecimal jobId, String profileName, Task.State state, Integer order) throws Exception {
        EmpireWorkflowTaskDao tasks = daos.createWorkflowTaskDao();
        tasks.setTransaction(tx);
        Task task = tasks.create();
        task.setJobId(jobId);
        task.setTypeRef(profileName);
        task.setState(state);
        task.setOrder(order);
        task.setPriority(1);
        task.setCreated(dbTimestamp);
        tasks.update(task);
    }

    @ParameterizedTest
    @CsvSource(value = {
        "OSN, 2",
        "osn, 2",
        "OsN, 2",
        "sn, 2",
        "' osn ', 2",
        "unmatched, 0",
        "NULL, 2",
        "'', 2",
        "'   ', 2"
    }, nullValues = "NULL")
    public void testViewFinancedFilter(String financed, int expectedCount) throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml")
        );
        support.cleanInsert(support.getConnection(tx), db);

        Job mixedCaseJob = dao.find(BigDecimal.valueOf(2));
        mixedCaseJob.setFinanced("OsN");
        dao.update(mixedCaseJob);
        tx.commit();

        JobFilter filter = new JobFilter();
        filter.setFinanced(financed);
        List<JobView> jobs = dao.view(filter);
        assertEquals(expectedCount, jobs.size());
        assertEquals("OSN", dao.find(BigDecimal.ONE).getFinanced());
        assertEquals("OsN", dao.find(BigDecimal.valueOf(2)).getFinanced());
    }

    @Test
    public void testViewDateFilter() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml")
        );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        JobFilter filter = new JobFilter();
        filter.setCreated(Arrays.asList(
                ">", "2015-10-24 23:00:00.000",
                "<", "2015-10-26 23:00:00.000"
        ));
        List<JobView> jobs = dao.view(filter);
        assertEquals(1, jobs.size());
        JobView job0 = jobs.get(0);
        assertEquals(new BigDecimal(2), job0.getId());
        assertEquals("job.stt", job0.getProfileName());
    }

    @Test
    public void testViewMaterialFilter() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_material.xml")
        );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        JobFilter filter = new JobFilter();
        filter.setLabel("Mono");
        filter.setMaterialBarcode("1234");
        filter.setMaterialDetail("detail");
        filter.setMaterialField001("001");
        filter.setMaterialIssue("11");
        filter.setMaterialSigla("ABA123");
        filter.setMaterialSignature("sig123");
        filter.setMaterialVolume("13");
        filter.setMaterialYear("1998-1999");
        List<JobView> jobs = dao.view(filter);
        assertEquals(1, jobs.size());
        JobView job0 = jobs.get(0);
        assertEquals(new BigDecimal(1), job0.getId());
        assertEquals("job.ndk", job0.getProfileName());
    }

    @Test
    public void testDeviceFromDifferentScanTasks() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_device.xml")
        );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        assertEquals("NDK scanner", dao.getDevice(BigDecimal.ONE));
        assertEquals("STT scanner", dao.getDevice(BigDecimal.valueOf(2)));
        assertEquals(Arrays.asList(BigDecimal.ONE), dao.getJobIdFromDevice("NDK scanner"));
        assertEquals(Arrays.asList(BigDecimal.valueOf(2)), dao.getJobIdFromDevice("STT scanner"));
    }

}
