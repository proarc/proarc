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

import cz.cas.lib.proarc.common.dao.UserDao;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ReplacementDataSet;
import org.dbunit.operation.DatabaseOperation;
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
public class EmpireUserDaoTest {

    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireDaoFactory daos;
    private SqlTransaction tx;
    private UserDao dao;

    public EmpireUserDaoTest() {
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
        dao = daos.createUser();
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
        rds.addReplacementObject("{$now}", new Timestamp(System.currentTimeMillis()));
        return rds;
    }

    @Test
    public void testFindId() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        Integer userId = 1;
        UserProfile result = dao.find(userId);
        assertNotNull(result);
        assertEquals(userId, result.getId());
    }

    @Test
    public void testFindQuery() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        String username = "test";
        List<UserProfile> result = dao.find(username, null, null, null);
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(username, result.get(0).getUserName());

        result = dao.find("UnknownUser", null, null, null);
        assertEquals(Arrays.asList(), result);
    }

    @Test
    public void testCreate() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();
        support.initSequences(tx, 10, schema.tableUser.id.getSequenceName());

        UserProfile create = dao.create();
        create.setEmail("email");
        create.setForename("forename");
        create.setUserHome("home");
        create.setRemoteName("remoteName");
        create.setRemoteType("remoteType");
        create.setSurname("surname");
        create.setUserName("create");
        create.setUserPasswordDigest("digist");
        dao.update(create);
        List<UserProfile> find = dao.find(create.getUserName(), null, create.getRemoteName(), create.getRemoteType());
        assertEquals(1, find.size());
        UserProfile result = find.get(0);
        assertNotNull(result.getCreated());
        assertEquals(create.getEmail(), result.getEmail());
        assertEquals(create.getForename(), result.getForename());
        assertEquals(create.getId(), result.getId());
        assertEquals(create.getRemoteName(), result.getRemoteName());
        assertEquals(create.getRemoteType(), result.getRemoteType());
        assertEquals(create.getSurname(), result.getSurname());
        assertEquals(create.getUserHome(), result.getUserHome());
        assertEquals(create.getUserPasswordDigest(), result.getUserPasswordDigest());
    }

    @Test
    public void testUpdate() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "group.xml"),
                support.loadFlatXmlDataStream(getClass(), "user.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        UserProfile user = dao.find(1);
        user.setDefaultGroup(1);
        user.setEmail("email2");
        user.setForename("forename2");
        user.setRemoteName("remotename2");
        user.setRemoteType("remotetype2");
        user.setSurname("surname2");
        user.setUserHome("home2");
        user.setUserPasswordDigest("digest2");
        dao.update(user);

        UserProfile result = dao.find(1);
        assertNotNull(result.getCreated());
        assertEquals((Integer) 1, result.getDefaultGroup());
        assertEquals("email2", result.getEmail());
        assertEquals("forename2", result.getForename());
        assertEquals((Integer) 1, result.getId());
        assertEquals("remotename2", result.getRemoteName());
        assertEquals("remotetype2", result.getRemoteType());
        assertEquals("surname2", result.getSurname());
        assertEquals("home2", result.getUserHome());
        assertEquals("digest2", result.getUserPasswordDigest());
    }

}