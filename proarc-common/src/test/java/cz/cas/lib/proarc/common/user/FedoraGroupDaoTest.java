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
public class FedoraGroupDaoTest {

    @Rule
    public TestName testName = new TestName();
    private FedoraTestSupport support;
    private FedoraTransaction tx;

    public FedoraGroupDaoTest() {
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
    public void testAddGroup() throws Exception {
        Group g = Group.create(testName.getMethodName(), testName.getMethodName() + "Title");
        FedoraGroupDao dao = new FedoraGroupDao();
        dao.setTransaction(tx);
        dao.addGroup(g, support.getTestUser(), testName.getMethodName());
        assertEquals(UserUtil.toGroupPid(testName.getMethodName()), g.getName());
        String pid = g.getName();
        assertTrue(tx.getRemoteStorage().exist(pid));

        try {
            dao.addGroup(g, support.getTestUser(), testName.getMethodName());
            fail("Created the duplicate of group! " + pid);
        } catch (Exception e) {
        }
    }

    @Test
    public void testAddNewGroup() throws Exception {
        Group g = Group.create(testName.getMethodName(), testName.getMethodName() + "Title");
        FedoraGroupDao dao = new FedoraGroupDao();
        dao.setTransaction(tx);
        dao.addNewGroup(g, support.getTestUser(), testName.getMethodName());
        assertEquals(UserUtil.toGroupPid(testName.getMethodName()), g.getName());
        String pid = g.getName();
        assertTrue(tx.getRemoteStorage().exist(pid));

        dao.addNewGroup(g, support.getTestUser(), testName.getMethodName());
        assertNotEquals(UserUtil.toGroupPid(testName.getMethodName()), g.getName());
        pid = g.getName();
        assertTrue(tx.getRemoteStorage().exist(pid));
    }

}