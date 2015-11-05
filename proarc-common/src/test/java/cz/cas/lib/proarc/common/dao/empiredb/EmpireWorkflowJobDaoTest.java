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

import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.Job.State;
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
public class EmpireWorkflowJobDaoTest {
    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireDaoFactory daos;
    private SqlTransaction tx;
    private EmpireWorkflowJobDao dao;
    private Timestamp dbTimestamp;

    public EmpireWorkflowJobDaoTest() {
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
        dao = daos.createWorkflowJobDao();
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

//    @Test
    public void testUpdate() {
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
        List<JobView> jobs = dao.view(filter);
        assertEquals(1, jobs.size());
        JobView job0 = jobs.get(0);
        assertEquals(BigDecimal.ONE, job0.getId());
        assertEquals("job.ndk", job0.getProfileName());
        assertEquals("test", job0.getUserName());
    }

}
