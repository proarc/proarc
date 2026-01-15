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
import cz.cas.lib.proarc.common.storage.fedora.FedoraTransaction;
import java.io.File;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 *
 * @author Jan Pokorsky
 */
public class FedoraGroupDaoTest {

    @TempDir
    File tempDir;


    private FedoraTestSupport support;
    private FedoraTransaction tx;
    private String testValue = "testValue";

    public FedoraGroupDaoTest() {
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
    public void testAddGroup() throws Exception {
        Group g = Group.create(testValue, testValue + "Title");
        FedoraGroupDao dao = new FedoraGroupDao();
        dao.setTransaction(tx);
        dao.addGroup(g, support.getTestUser(), testValue);
        assertEquals(UserUtil.toGroupPid(testValue), g.getName());
        String pid = g.getName();
        assertTrue(tx.getRemoteStorage().exist(pid));

        try {
            dao.addGroup(g, support.getTestUser(), testValue);
            fail("Created the duplicate of group! " + pid);
        } catch (Exception e) {
        }
    }

    @Test
    public void testAddNewGroup() throws Exception {
        Group g = Group.create(testValue, testValue + "Title");
        FedoraGroupDao dao = new FedoraGroupDao();
        dao.setTransaction(tx);
        dao.addNewGroup(g, support.getTestUser(), testValue);
        assertEquals(UserUtil.toGroupPid(testValue), g.getName());
        String pid = g.getName();
        assertTrue(tx.getRemoteStorage().exist(pid));

        dao.addNewGroup(g, support.getTestUser(), testValue);
        assertNotEquals(UserUtil.toGroupPid(testValue), g.getName());
        pid = g.getName();
        assertTrue(tx.getRemoteStorage().exist(pid));
    }

}