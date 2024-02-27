/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.cas.lib.proarc.common.storage;

import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorageInitializer;
import cz.cas.lib.proarc.common.storage.fedora.PurgeFedoraObject;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class PurgeProArcObjectTest {

    private FedoraTestSupport support;

    public PurgeProArcObjectTest() {
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
        MetaModelRepository.setInstance(new String[]{NdkPlugin.ID});
        new FedoraStorageInitializer(support.getRemoteStorage()).init();
        support.cleanUp();
        support.ingest(
                getClass().getResource("tree1.xml"),
                getClass().getResource("tree1-child1.xml"),
                getClass().getResource("tree1-child1-child1.xml"),
                getClass().getResource("tree1-child1-child1-child1.xml"),
                getClass().getResource("tree1-child1-child1-child2.xml"),
                getClass().getResource("tree1-child2.xml"),
                getClass().getResource("tree1-child2-child1.xml"),
                getClass().getResource("tree1-child2-child1-child1.xml")
        );
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testPurgeTreeLeaf() throws Exception {
        String pid = "uuid:tree1-child2-child1-child1";
        String parentPid = "uuid:tree1-child2-child1";
        boolean hierarchy = true;
        PurgeFedoraObject purge = new PurgeFedoraObject(support.getRemoteStorage());
        purge.purge(pid, hierarchy, "junit");

        List<String> pids = Arrays.asList(
                "uuid:tree1",
                "uuid:tree1-child1",
                "uuid:tree1-child1-child1",
                "uuid:tree1-child1-child1-child1",
                "uuid:tree1-child1-child1-child2",
                "uuid:tree1-child2",
                "uuid:tree1-child2-child1",
                "uuid:tree1-child2-child1-child1"
        );
        ArrayList<String> shouldRemain = new ArrayList<String>(pids);
        shouldRemain.remove(pid);
        List<SearchViewItem> found = support.getRemoteStorage().getSearch().find(false, pids);
        FedoraTestSupport.assertItem(found, shouldRemain);
        FedoraTestSupport.assertNoItem(found, pid);
        List<SearchViewItem> children = support.getRemoteStorage().getSearch().findChildren(parentPid);
        FedoraTestSupport.assertNoItem(children, pid);
    }

    @Test
    public void testPurgeTree() throws Exception {
        String pid = "uuid:tree1-child2";
        String parentPid = "uuid:tree1";
        boolean hierarchy = true;
        PurgeFedoraObject purge = new PurgeFedoraObject(support.getRemoteStorage());
        purge.purge(pid, hierarchy, "junit");

        List<String> pids = Arrays.asList(
                "uuid:tree1",
                "uuid:tree1-child1",
                "uuid:tree1-child1-child1",
                "uuid:tree1-child1-child1-child1",
                "uuid:tree1-child1-child1-child2",
                "uuid:tree1-child2",
                "uuid:tree1-child2-child1",
                "uuid:tree1-child2-child1-child1"
        );
        List<String> removed = Arrays.asList(pid, "uuid:tree1-child2-child1", "uuid:tree1-child2-child1-child1");
        ArrayList<String> shouldRemain = new ArrayList<String>(pids);
        shouldRemain.removeAll(removed);
        List<SearchViewItem> found = support.getRemoteStorage().getSearch().find(false, pids);
        FedoraTestSupport.assertItem(found, shouldRemain);
        FedoraTestSupport.assertNoItem(found, removed);
        List<SearchViewItem> children = support.getRemoteStorage().getSearch().findChildren(parentPid);
        FedoraTestSupport.assertNoItem(children, pid);
    }

    @Test
    public void testPurgeTreeRoot() throws Exception {
        String pid = "uuid:tree1-child2";
        String parentPid = "uuid:tree1";
        boolean hierarchy = false;
        PurgeFedoraObject purge = new PurgeFedoraObject(support.getRemoteStorage());
        purge.purge(pid, hierarchy, "junit");

        List<String> pids = Arrays.asList(
                "uuid:tree1",
                "uuid:tree1-child1",
                "uuid:tree1-child1-child1",
                "uuid:tree1-child1-child1-child1",
                "uuid:tree1-child1-child1-child2",
                "uuid:tree1-child2",
                "uuid:tree1-child2-child1",
                "uuid:tree1-child2-child1-child1"
        );
        List<String> removed = Arrays.asList(pid);
        ArrayList<String> shouldRemain = new ArrayList<String>(pids);
        shouldRemain.removeAll(removed);
        List<SearchViewItem> found = support.getRemoteStorage().getSearch().find(false, pids);
        FedoraTestSupport.assertItem(found, shouldRemain);
        FedoraTestSupport.assertNoItem(found, removed);
        List<SearchViewItem> children = support.getRemoteStorage().getSearch().findChildren(parentPid);
        FedoraTestSupport.assertNoItem(children, pid);
    }

    @Test
    public void testPurgeTheWholeTree() throws Exception {
        String pid = "uuid:tree1";
        boolean hierarchy = true;
        PurgeFedoraObject purge = new PurgeFedoraObject(support.getRemoteStorage());
        purge.purge(pid, hierarchy, "junit");

        String[] pids = {
                "uuid:tree1",
                "uuid:tree1-child1",
                "uuid:tree1-child1-child1",
                "uuid:tree1-child1-child1-child1",
                "uuid:tree1-child1-child1-child2",
                "uuid:tree1-child2",
                "uuid:tree1-child2-child1",
                "uuid:tree1-child2-child1-child1"
        };
        List<SearchViewItem> found = support.getRemoteStorage().getSearch().find(false, pids);
        FedoraTestSupport.assertNoItem(found, pids);
    }

    @Test
    public void testDeleteTheWholeTree() throws Exception {
        String pid = "uuid:tree1";
        boolean hierarchy = true;
        PurgeFedoraObject purge = new PurgeFedoraObject(support.getRemoteStorage());
        purge.delete(pid, hierarchy, "junit");

        String[] pids = {
                "uuid:tree1",
                "uuid:tree1-child1",
                "uuid:tree1-child1-child1",
                "uuid:tree1-child1-child1-child1",
                "uuid:tree1-child1-child1-child2",
                "uuid:tree1-child2",
                "uuid:tree1-child2-child1",
                "uuid:tree1-child2-child1-child1"
        };
        List<SearchViewItem> found = support.getRemoteStorage().getSearch().find(false, pids);
        FedoraTestSupport.assertItem(found, pids);
        for (SearchViewItem item : found) {
            assertEquals(item.getPid(), "fedora-system:def/model#Deleted", item.getState());
        }

        found = support.getRemoteStorage().getSearch().find(pids);
        assertTrue(found.isEmpty());
    }
}
