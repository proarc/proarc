/*
 * Copyright (C) 2013 Jan Pokorsky
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

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.Batch.State;
import cz.cas.lib.proarc.common.dao.BatchDao;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import java.sql.Timestamp;
import java.util.EnumSet;
import java.util.List;
import org.dbunit.Assertion;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.DefaultDataSet;
import org.dbunit.dataset.DefaultTable;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ITable;
import org.dbunit.dataset.ReplacementDataSet;
import org.dbunit.dataset.ReplacementTable;
import org.dbunit.operation.DatabaseOperation;
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
public class EmpireBatchDaoTest {

    private DaoFactory daos;
    private DbUnitSupport support;
    private BatchDao dao;
    private Transaction tx;
    private ProarcDatabase schema;
    private Timestamp dbTimestamp;

    public EmpireBatchDaoTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() throws Exception {
        support = new DbUnitSupport();
        schema = support.getEmireCfg().getSchema();
        daos = new EmpireDaoFactory(support.getEmireCfg());
        daos.init();
        tx = daos.createTransaction();
        dao = daos.createBatch();
        dao.setTransaction(tx);
    }

    @After
    public void tearDown() {
        if (tx != null) {
            tx.close();
        }
    }

    public static IDataSet emptyBatchSet(ProarcDatabase schema) throws Exception {
        ITable[] tables = {
            new DefaultTable(schema.tableBatch.getName()),
            new DefaultTable(schema.tableBatchItem.getName()),
        };
        return new DefaultDataSet(tables);
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
                emptyBatchSet(schema)
                );
        DatabaseOperation.CLEAN_INSERT.execute(support.getConnection(tx), db);
        support.initSequences(tx, 1,
                schema.tableBatch.id.getSequenceName());
        tx.commit();

        Batch batch = dao.create();
        batch.setCreate(Timestamp.valueOf("2013-01-17 12:12:12.000"));
        batch.setDevice("device:scanner");
        batch.setEstimateItemNumber(2);
        batch.setFolder("folder/");
        batch.setGenerateIndices(true);
        batch.setLog("log");
        batch.setParentPid("uuid:0eaa6730-9068-11dd-97de-000d606f5dc6");
        batch.setState(State.LOADING);
        batch.setTitle("title_folder/");
        batch.setUserId(1);
        dao.update(batch);
        tx.commit();

        ReplacementTable expected = new ReplacementTable(
                support.loadFlatXmlDataStream(getClass(), "batch.xml")
                        .getTable(schema.tableBatch.getName()));
        expected.addReplacementObject("{$now}", batch.getTimestamp());
        Assertion.assertEquals(expected, support.getConnection().createTable(schema.tableBatch.getName()));
    }

    @Test
    public void testUpdate() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml")
                );
        DatabaseOperation.CLEAN_INSERT.execute(support.getConnection(tx), db);
        tx.commit();

        Batch batch = dao.find(1);
        State expectedState = State.INGESTING;
        batch.setState(expectedState);
        String expectedLog = "updated log";
        batch.setLog(expectedLog);
        Timestamp timestamp = batch.getTimestamp();
        dao.update(batch);
        tx.commit();

        batch = dao.find(1);
        assertEquals(expectedState, batch.getState());
        assertEquals(expectedLog, batch.getLog());
        assertTrue(timestamp.before(batch.getTimestamp()));
    }

    @Test(expected = ConcurrentModificationException.class)
    public void testUpdateConcurrently() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml")
                );
        DatabaseOperation.CLEAN_INSERT.execute(support.getConnection(tx), db);
        tx.commit();

        Batch batch = dao.find(1);
        State expectedState = State.INGESTING;
        batch.setState(expectedState);
        String expectedLog = "updated log";
        batch.setLog(expectedLog);
        Timestamp timestamp = batch.getTimestamp();

        Transaction tx2 = daos.createTransaction();
        BatchDao dao2 = daos.createBatch();
        dao2.setTransaction(tx2);
        try {
            Batch batch2 = dao2.find(1);
            batch2.setState(State.INGESTING_FAILED);
            batch2.setLog("concurrent update");
            dao2.update(batch2);
            tx2.commit();
        } finally {
            tx2.close();
        }

        dao.update(batch);
        tx.commit();
    }

    @Test
    public void testFind() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml")
                );
        DatabaseOperation.CLEAN_INSERT.execute(support.getConnection(tx), db);
        tx.commit();

        Batch result = dao.find(1);
        assertNotNull(result);
        assertEquals(1, (int) result.getId());
    }

    @Test
    public void testFindForPid() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch_item.xml")
                );
        DatabaseOperation.CLEAN_INSERT.execute(support.getConnection(tx), db);
        tx.commit();

        Batch batch = dao.findForPid("uuid:4a7c2e50-af36-11dd-9643-000d606f5dc6");
        assertNotNull(batch);
    }

    @Test
    public void testFindLoadingBatches() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml")
                );
        DatabaseOperation.CLEAN_INSERT.execute(support.getConnection(tx), db);
        tx.commit();

        List<Batch> result = dao.findLoadingBatches();
        assertEquals(1, result.size());

        Batch batch = dao.find(1);
        batch.setState(State.LOADED);
        dao.update(batch);

        result = dao.findLoadingBatches();
        assertEquals(0, result.size());
    }

    @Test
    public void testView() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml")
                );
        DatabaseOperation.CLEAN_INSERT.execute(support.getConnection(tx), db);
        tx.commit();

        List<BatchView> view = dao.view(1, null, null, 0);
        assertEquals(1, view.size());
        BatchView bv = view.get(0);
        assertEquals((Integer) 1, bv.getId());
        assertEquals(Timestamp.valueOf("2013-01-17 12:12:12.000"), bv.getCreate());
        assertEquals("folder/", bv.getFolder());
        assertEquals("uuid:0eaa6730-9068-11dd-97de-000d606f5dc6", bv.getParentPid());
        assertEquals("LOADING", bv.getState());
        assertEquals(dbTimestamp, bv.getTimestamp());
        assertEquals("title_folder/", bv.getTitle());
        assertEquals((Integer) 1, bv.getUserId());
        assertEquals("test", bv.getUsername());

        view = dao.view(1, 1, null, 0);
        assertEquals(1, view.size());
        view = dao.view(1, null, State.LOADING, 0);
        assertEquals(1, view.size());
        view = dao.view(1, null, State.LOADING_FAILED, 0);
        assertEquals(0, view.size());
        view = dao.view(2, null, null, 0);
        assertEquals(0, view.size());
    }

    @Test
    public void testView2() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch_with_items.xml")
                );
        DatabaseOperation.CLEAN_INSERT.execute(support.getConnection(tx), db);
        tx.commit();

        List<BatchView> view = dao.view(1, null, null, 0);
        assertEquals(2, view.size());

        view = dao.view(1, 1, null, 0);
        assertEquals(1, view.size());
        view = dao.view(1, null, State.LOADING, 0);
        assertEquals(1, view.size());
        view = dao.view(20, null, null, 0);
        assertEquals(0, view.size());

        // test paging
        view = dao.view(1, null, null, null, null, 0, 1, null);
        assertEquals(1, view.size());
        assertEquals((Integer) 2, view.get(0).getId());
        view = dao.view(1, null, null, null, null, 1, 1, null);
        assertEquals(1, view.size());
        assertEquals((Integer) 1, view.get(0).getId());
        view = dao.view(1, null, null, null, null, 2, 1, null);
        assertEquals(0, view.size());
    }

    @Test
    public void testViewSort() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch_with_items.xml")
                );
        DatabaseOperation.CLEAN_INSERT.execute(support.getConnection(tx), db);
        tx.commit();

        List<BatchView> view = dao.view(null, null, null, null, null, 0, 100, "-create");
        assertEquals(2, view.size());
        assertEquals((Integer) 2, view.get(0).getId());
        assertEquals((Integer) 1, view.get(1).getId());

        view = dao.view(null, null, null, null, null, 0, 100, "create");
        assertEquals(2, view.size());
        assertEquals((Integer) 1, view.get(0).getId());
        assertEquals((Integer) 2, view.get(1).getId());

        view = dao.view(null, null, EnumSet.of(State.LOADING, State.LOADED), null, null, 0, 100, "-estimateItemNumber");
        assertEquals(2, view.size());
        assertEquals((Integer) 2, view.get(0).getId());
        assertEquals((Integer) 1, view.get(1).getId());
    }
}