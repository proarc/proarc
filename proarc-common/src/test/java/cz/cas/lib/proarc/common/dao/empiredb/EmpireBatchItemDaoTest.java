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

import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dao.BatchItem.Type;
import java.sql.Timestamp;
import java.util.List;
import org.dbunit.Assertion;
import org.dbunit.database.IDatabaseConnection;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ReplacementDataSet;
import org.dbunit.dataset.ReplacementTable;
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
public class EmpireBatchItemDaoTest {
    private DbUnitSupport support;
    private EmpireDaoFactory daos;
    private SqlTransaction tx;
    private EmpireBatchItemDao dao;
    private ProarcDatabase schema;

    public EmpireBatchItemDaoTest() {
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
        daos = new EmpireDaoFactory(support.getEmireCfg());
        daos.init();
        tx = daos.createTransaction();
        dao = daos.createBatchItem();
        dao.setTransaction(tx);
        schema = support.getEmireCfg().getSchema();
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
        rds.addReplacementObject("{$now}", new Timestamp(System.currentTimeMillis()));
        return rds;
    }

    @Test
    public void testCreate() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml")
                );
        final IDatabaseConnection dbcon = support.getConnection(tx);
        support.cleanInsert(dbcon, db);
        support.initSequences(tx, 1,
                schema.tableBatchItem.id.getSequenceName()
                );
        tx.commit();

        int batchId = 1;
        BatchItem item = dao.create();
        item.setBatchId(batchId);
        item.setFile("file1.xml");
        item.setLog("log");
        item.setPid("uuid:4a7c2e50-af36-11dd-9643-000d606f5dc6");
        item.setState(ObjectState.LOADED.name());
        item.setType(Type.OBJECT);
        dao.update(item);
        tx.commit();

        ReplacementTable expected = new ReplacementTable(
                support.loadFlatXmlDataStream(getClass(), "batch_item.xml")
                        .getTable(schema.tableBatchItem.getName()));
        expected.addReplacementObject("{$now}", item.getTimestamp());
        Assertion.assertEquals(expected, dbcon.createTable(schema.tableBatchItem.getName()));
    }

    @Test
    public void testCreateDevice() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml")
                );
        final IDatabaseConnection dbcon = support.getConnection(tx);
        support.cleanInsert(dbcon, db);
        support.initSequences(tx, 1,
                schema.tableBatchItem.id.getSequenceName()
                );
        tx.commit();

        int batchId = 1;
        BatchItem item = dao.create();
        item.setBatchId(batchId);
        item.setFile("file1.xml");
        item.setLog("log");
        item.setPid("device:4a7c2e50-af36-11dd-9643-000d606f5dc6");
        item.setState(ObjectState.LOADING.name());
        item.setType(Type.OBJECT);
        dao.update(item);
        tx.commit();

        BatchItem result = dao.find(batchId);
        assertEquals(item.getPid(), result.getPid());
        assertEquals(item.getState(), result.getState());
        assertEquals(item.getType(), result.getType());
    }

    @Test
    public void testUpdate() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch_item.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        BatchItem item = dao.find(1);
        String expectedState = ObjectState.EXCLUDED.name();
        item.setState(expectedState);
        item.setLog(null);
        dao.update(item);
        tx.commit();

        item = dao.find(1);
        assertEquals(expectedState, item.getState());
        assertNull(item.getLog());
    }

    @Test
    public void testFind() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch_item.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        BatchItem item = dao.find(1);
        assertNotNull(item);
        assertEquals(1, (int) item.getId());
    }

    @Test
    public void testFind_Items() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch_with_items.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        // find objects of the batch
        List<BatchItem> items = dao.find(2, null, null, null, null, Type.OBJECT.name());
        assertEquals(4, items.size());

        // find excluded objects of the batch
        items = dao.find(2, null, null, null, ObjectState.EXCLUDED.name(), Type.OBJECT.name());
        assertEquals(1, items.size());

        // find files of the object
        items = dao.find(2, "pid:item:2", null, null, null, Type.FILE.name());
        assertEquals(2, items.size());
    }

    @Test
    public void testRemoveItems() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "batch_with_items.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        dao.removeItems(2);
        tx.commit();
        List<BatchItem> items = dao.find(2, null, null, null, null, null);
        assertTrue(items.isEmpty());
    }
}
