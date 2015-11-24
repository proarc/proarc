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
import cz.cas.lib.proarc.common.workflow.model.TaskParameter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterView;
import cz.cas.lib.proarc.common.workflow.model.ValueType;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ReplacementDataSet;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireWorkflowParameterDaoTest {

    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireDaoFactory daos;
    private SqlTransaction tx;
    private EmpireWorkflowParameterDao dao;
    private Timestamp dbTimestamp;

    @Before
    public void setUp() {
        support = new DbUnitSupport();
        schema = support.getEmireCfg().getSchema();
        daos = new EmpireDaoFactory(support.getEmireCfg());
        daos.init();
        tx = daos.createTransaction();
        dao = daos.createWorkflowParameterDao();
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
    public void testAdd() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        Task task = new Task().addId(BigDecimal.ONE);
        List<TaskParameter> params = Arrays.asList(dao.create().addParamRef("param.p1").addValue(ValueType.STRING, "p1Val"),
                dao.create().addParamRef("param.p2").addValue(ValueType.NUMBER, "1"),
                dao.create().addParamRef("param.p3").addValue(ValueType.NUMBER, null),
                dao.create().addParamRef("param.p4").addValue(ValueType.DATETIME, "2011-12-31T15:05:50+0100"),
                dao.create().addParamRef("param.p5").addValue(ValueType.NUMBER, "1.01"),
                dao.create().addParamRef("param.p6").addValue(ValueType.NUMBER, "0")
        );
        dao.add(task.getId(), params);
        tx.commit();
        assertEquals(task.getId(), params.get(0).getTaskId());

        Map<String, String> map = dao.findAsMap(task.getId());
        assertEquals(params.size(), map.size());
        assertTrue(map.containsKey("param.p1"));
        assertEquals("p1Val", map.get("param.p1"));
        assertTrue(map.containsKey("param.p2"));
        assertEquals("1", map.get("param.p2"));
        assertTrue(map.containsKey("param.p3"));
        assertEquals(null, map.get("param.p3"));
        assertTrue(map.containsKey("param.p4"));
        assertEquals("2011-12-31T14:05:50.000Z", map.get("param.p4"));
        assertEquals("1.01", map.get("param.p5"));
        assertEquals("0", map.get("param.p6"));

        TaskParameterFilter filter = new TaskParameterFilter();
        filter.setTaskId(task.getId());
        filter.setLocale(Locale.ENGLISH);
        filter.setProfileName("param.p6");
        List<TaskParameterView> view = dao.view(filter);
        assertEquals(1, view.size());
        assertEquals("0", view.get(0).getValue());
        assertEquals(0, BigDecimal.ZERO.compareTo(view.get(0).getValueNumber()));

        List<TaskParameter> result = dao.find(task.getId());
        assertEquals(task.getId(), result.get(0).getTaskId());
    }

    @Test
    public void testRemove() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_param.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        Task task = new Task().addId(BigDecimal.ONE);
        List<TaskParameter> params = dao.find(task.getId());
        assertEquals(2, params.size());

        dao.remove(task.getId());
        tx.commit();

        params = dao.find(task.getId());
        assertEquals(0, params.size());
    }

    @Test
    public void testView() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_param.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        Task task = new Task().addId(BigDecimal.ONE);
        TaskParameterFilter filter = new TaskParameterFilter();
        filter.setTaskId(task.getId());
        List<TaskParameterView> params = dao.view(filter);
        assertEquals(2, params.size());

        filter = new TaskParameterFilter();
        filter.setTaskId(BigDecimal.TEN);
        params = dao.view(filter);
        assertEquals(1, params.size());

        filter = new TaskParameterFilter();
        filter.setTaskId(BigDecimal.ZERO);
        params = dao.view(filter);
        assertEquals(0, params.size());

        filter = new TaskParameterFilter();
        filter.setJobId(BigDecimal.ONE);
        params = dao.view(filter);
        assertEquals(2, params.size());

        filter = new TaskParameterFilter();
        filter.setProfileName("param.t10.p1");
        params = dao.view(filter);
        assertEquals(1, params.size());
        assertEquals(BigDecimal.TEN, params.get(0).getTaskId());
        assertEquals("task.id2", params.get(0).getTaskProfileName());
        assertEquals(new BigDecimal(2), params.get(0).getJobId());
        assertEquals(ValueType.NUMBER, params.get(0).getValueType());
        assertEquals("10.1", params.get(0).getValue());
    }

}
