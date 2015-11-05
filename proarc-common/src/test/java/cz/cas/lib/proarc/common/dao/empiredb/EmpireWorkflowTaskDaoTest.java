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

import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.Task.State;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ReplacementDataSet;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireWorkflowTaskDaoTest {

    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireDaoFactory daos;
    private SqlTransaction tx;
    private EmpireWorkflowTaskDao dao;
    private Timestamp dbTimestamp;

    public EmpireWorkflowTaskDaoTest() {
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
        tx = daos.createTransaction();
        dao = daos.createWorkflowTaskDao();
        dao.setTransaction(tx);
    }

    @After
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
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        Task task = dao.create();
        task.addCreated(dbTimestamp).addJobId(BigDecimal.ONE).addNote("note")
                .addOwnerId(BigDecimal.ONE).addPriority(1).addTypeRef("profileName")
                .setState(State.READY)
                .addTimestamp(dbTimestamp);
        dao.update(task);
        tx.commit();

        assertNotNull(task.getId());
        Task result = dao.find(task.getId());
        assertNotNull(result);
        assertEquals(result.getCreated(), task.getCreated());
        assertEquals(result.getId(), task.getId());
        assertEquals(result.getJobId(), task.getJobId());
        assertEquals(result.getNote(), task.getNote());
        assertEquals(result.getOwnerId(), task.getOwnerId());
        assertEquals(result.getPriority(), task.getPriority());
        assertEquals(result.getState(), task.getState());
        assertEquals(result.getTimestamp(), task.getTimestamp());
        assertEquals(result.getTypeRef(), task.getTypeRef());
    }

//    @Test
    public void testUpdate() {
    }

    @Test
    public void testView() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        TaskFilter filter = new TaskFilter();
        filter.setJobId(BigDecimal.ONE);
        List<TaskView> tasks = dao.view(filter);
        assertEquals(1, tasks.size());
        TaskView t = tasks.get(0);
        assertEquals(BigDecimal.ONE, t.getId());
        assertEquals(State.STARTED, t.getState());
        assertEquals("Monograph", t.getJobLabel());
        assertEquals("test", t.getUserName());
    }

}
