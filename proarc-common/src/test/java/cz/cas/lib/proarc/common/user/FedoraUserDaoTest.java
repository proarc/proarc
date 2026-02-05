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

import cz.cas.lib.proarc.common.storage.FedoraTestSupport;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.fedora.FedoraTransaction;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 *
 * @author Jan Pokorsky
 */
public class FedoraUserDaoTest {

    @TempDir
    File tempDir;

    private FedoraTestSupport support;
    private FedoraTransaction tx;
    private String testValue = "testValue";

    public FedoraUserDaoTest() {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() throws Exception {
        support = new FedoraTestSupport();
        support.cleanUp();
        tx = new FedoraTransaction(support.getRemoteStorage());
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testCreateUser() throws Exception {
        FedoraUserDao dao = new FedoraUserDao();
        dao.setTransaction(tx);
        UserProfile user = new UserProfile();
        user.setUserName("testUser");
        dao.add(user, support.getTestUser(), testValue);
        assertEquals("testUser", user.getUserName());
        final String userPid = user.getUserNameAsPid();
        assertTrue(tx.getRemoteStorage().exist(userPid));

        try {
            dao.add(user, support.getTestUser(), testValue);
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
        dao.add(user, support.getTestUser(), testValue);
        assertEquals("testRemoteUser", user.getUserName());
        String userPid = user.getUserNameAsPid();
        assertTrue(tx.getRemoteStorage().exist(userPid));

        dao.add(user, support.getTestUser(), testValue);
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
        dao.add(user, support.getTestUser(), testValue);

        Group g = Group.create(testValue, testValue + "Title");
        FedoraGroupDao groupDao = new FedoraGroupDao();
        groupDao.setTransaction(tx);
        groupDao.addGroup(g, support.getTestUser(), testValue);

        dao.setMembership(user, Arrays.asList(g), testValue);

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
        user.setUserName(testValue + "_1");
        dao.add(user, support.getTestUser(), testValue);
        String userPid1 = user.getUserNameAsPid();

        user = new UserProfile();
        user.setUserName(testValue + "_2");
        dao.add(user, support.getTestUser(), testValue);
        String userPid2 = user.getUserNameAsPid();
        assertTrue(tx.getRemoteStorage().exist(userPid1));
        assertTrue(tx.getRemoteStorage().exist(userPid2));
        tx.rollback();
        assertFalse(tx.getRemoteStorage().exist(userPid1));
        assertFalse(tx.getRemoteStorage().exist(userPid2));
    }

}