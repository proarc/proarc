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
package cz.cas.lib.proarc.common.user;

import cz.cas.lib.proarc.common.fedora.FedoraTestSupport;
import cz.cas.lib.proarc.common.fedora.FedoraTransaction;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import java.util.Arrays;
import java.util.Collection;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 *
 * @author Jan Pokorsky
 */
public class FedoraUserDaoTest {

    @Rule
    public TestName testName = new TestName();
    private FedoraTestSupport support;
    private FedoraTransaction tx;

    public FedoraUserDaoTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() throws Exception {
        support = new FedoraTestSupport();
        support.cleanUp();
        tx = new FedoraTransaction(support.getRemoteStorage());
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testCreateUser() throws Exception {
        FedoraUserDao dao = new FedoraUserDao();
        dao.setTransaction(tx);
        UserProfile user = new UserProfile();
        user.setUserName("testUser");
        dao.add(user, support.getTestUser(), testName.getMethodName());
        assertEquals("testUser", user.getUserName());
        final String userPid = user.getUserNameAsPid();
        assertTrue(tx.getRemoteStorage().exist(userPid));

        try {
            dao.add(user, support.getTestUser(), testName.getMethodName());
            fail("Created the duplicate of user! " + userPid);
        } catch (Exception e) {
        }
    }

    @Test
    public void testCreateRemoteUser() throws Exception {
        FedoraUserDao dao = new FedoraUserDao();
        dao.setTransaction(tx);
        UserProfile user = new UserProfile();
        user.setUserName("testRemoteUser");
        user.setRemoteName("remoteName");
        dao.add(user, support.getTestUser(), testName.getMethodName());
        assertEquals("testRemoteUser", user.getUserName());
        String userPid = user.getUserNameAsPid();
        assertTrue(tx.getRemoteStorage().exist(userPid));

        dao.add(user, support.getTestUser(), testName.getMethodName());
        assertEquals("testRemoteUser_1", user.getUserName());
        userPid = user.getUserNameAsPid();
        assertTrue(tx.getRemoteStorage().exist(userPid));
    }

    @Test
    public void testSetMembership() throws Exception {
        FedoraUserDao dao = new FedoraUserDao();
        dao.setTransaction(tx);
        UserProfile user = new UserProfile();
        user.setUserName("testUser");
        dao.add(user, support.getTestUser(), testName.getMethodName());

        Group g = Group.create(testName.getMethodName(), testName.getMethodName() + "Title");
        FedoraGroupDao groupDao = new FedoraGroupDao();
        groupDao.setTransaction(tx);
        groupDao.addGroup(g, support.getTestUser(), testName.getMethodName());

        dao.setMembership(user, Arrays.asList(g), testName.getMethodName());

        String userPid = user.getUserNameAsPid();
        RemoteObject fobject = tx.getRemoteStorage().find(userPid);
        RelationEditor relationEditor = new RelationEditor(fobject);
        Collection<String> membership = relationEditor.getMembership();
        assertEquals(Arrays.asList(UserUtil.toGroupPid(g)), membership);
    }

    @Test
    public void testRollback() throws Exception {
        FedoraUserDao dao = new FedoraUserDao();
        dao.setTransaction(tx);
        UserProfile user = new UserProfile();
        user.setUserName(testName.getMethodName() + "_1");
        dao.add(user, support.getTestUser(), testName.getMethodName());
        String userPid1 = user.getUserNameAsPid();

        user = new UserProfile();
        user.setUserName(testName.getMethodName() + "_2");
        dao.add(user, support.getTestUser(), testName.getMethodName());
        String userPid2 = user.getUserNameAsPid();
        assertTrue(tx.getRemoteStorage().exist(userPid1));
        assertTrue(tx.getRemoteStorage().exist(userPid2));
        tx.rollback();
        assertFalse(tx.getRemoteStorage().exist(userPid1));
        assertFalse(tx.getRemoteStorage().exist(userPid2));
    }

}