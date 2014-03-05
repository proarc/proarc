/*
 * Copyright (C) 2014 Jan Pokorsky
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

import cz.cas.lib.proarc.common.dao.GroupDao;
import cz.cas.lib.proarc.common.user.Group;
import java.sql.Timestamp;
import java.util.List;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ReplacementDataSet;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireGroupDaoTest {
    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireDaoFactory daos;
    private SqlTransaction tx;
    private GroupDao dao;
    private Timestamp dbTimestamp;

    public EmpireGroupDaoTest() {
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
        dao = daos.createUserGroup();
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
//        rds.addReplacementObject("{$user.home}", "relative/path/");
        dbTimestamp = new Timestamp(System.currentTimeMillis());
        rds.addReplacementObject("{$now}", dbTimestamp);
        return rds;
    }

    @Test
    public void testFind() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "group.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        Integer id = 1;
        Group result = dao.find(id);
        assertNotNull(result);
        assertEquals(Timestamp.valueOf("2013-01-12 00:00:00"), result.getCreated());
        assertEquals(id, result.getId());
        assertEquals("group:testgrp", result.getName());
        assertEquals("GroupTitle", result.getTitle());
        assertEquals(dbTimestamp, result.getTimestamp());
    }

    @Test
    public void testFindName() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "group.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        List<Group> find = dao.find(null, "group:testgrp", null, null);
        assertNotNull(find);
        assertEquals(1, find.size());
        Group result = find.get(0);
        assertNotNull(result);
        assertEquals(Timestamp.valueOf("2013-01-12 00:00:00"), result.getCreated());
        assertEquals(Integer.valueOf(1), result.getId());
        assertEquals("group:testgrp", result.getName());
        assertEquals("GroupTitle", result.getTitle());
        assertEquals(dbTimestamp, result.getTimestamp());

        // find remote
        find = dao.find(null, null, "DESA_GROUP", "DESA");
        assertNotNull(find);
        assertEquals(1, find.size());
    }

    @Test
    public void testCreate() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "group.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        final Integer id = 10;
        support.initSequences(tx, id, schema.tableUserGroup.id.getSequenceName());
        tx.commit();

        Group group = Group.create("createName", "createTitle");
        group.setCreated(new Timestamp(System.currentTimeMillis()));
        dao.update(group);

        assertEquals(id, group.getId());
        Group result = dao.find(group.getId());
        assertEquals(group.getId(), result.getId());
        assertEquals(group.getCreated(), result.getCreated());
        assertEquals(group.getName(), result.getName());
        assertEquals(group.getRemoteName(), result.getRemoteName());
        assertEquals(group.getTimestamp(), result.getTimestamp());
        assertEquals(group.getTitle(), result.getTitle());
    }

    @Test
    public void testUpdate() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "group.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);

        tx.commit();
        Group group = dao.find(1);
        group.setRemoteName("updateRemoteName");
        group.setRemoteType("updateRemoteType");
        group.setTitle("updateTitle");
        dao.update(group);

        List<Group> find = dao.find(null, null, "updateRemoteName", "updateRemoteType");
        assertNotNull(find);
        assertEquals(1, find.size());
        Group result = find.get(0);
        assertEquals(group.getId(), result.getId());
        assertEquals(group.getCreated(), result.getCreated());
        assertEquals(group.getName(), result.getName());
        assertEquals(group.getRemoteName(), result.getRemoteName());
        assertEquals(group.getRemoteType(), result.getRemoteType());
        assertEquals(group.getTimestamp(), result.getTimestamp());
        assertEquals(group.getTitle(), result.getTitle());
    }

}