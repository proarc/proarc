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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.fedora;

import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.SearchView.Result;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.user.FedoraGroupDao;
import cz.cas.lib.proarc.common.user.FedoraUserDao;
import cz.cas.lib.proarc.common.user.Group;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import static cz.cas.lib.proarc.common.fedora.FedoraTestSupport.assertItem;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class SearchViewTest {

    @Rule
    public TestName testName = new TestName();
    private RemoteStorage storage;
    private FedoraTestSupport fedora;

    public SearchViewTest() {
    }

    @Before
    public void setUp() throws Exception {
        fedora = new FedoraTestSupport();
        storage = fedora.getRemoteStorage();
        MetaModelRepository.setInstance(new String[]{NdkPlugin.ID});
        new FedoraStorageInitializer(storage).init();
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testReadJsonResult() throws Exception {
        SearchView instance = new SearchView(storage);
        String json = "{\"results\":[{\"pid\" : \"p1\", \"k0\" : \"1\"}]}";
        Result result = instance.readResponse(json);
        assertNotNull(result);
        assertNotNull(result.getResults());
        assertEquals("p1", result.getResults().get(0).getPid());
        assertEquals("1", result.getResults().get(0).getK0());
        assertEquals((Integer) 1, result.getResults().get(0).getHasExport());
    }

    @Test
    public void testResolveObjectLabel() {
        // TODO! Quarantine (Lukas will fix it later)
        //https://github.com/proarc/proarc/pull/749
        Assume.assumeTrue(System.getenv("TRAVIS") == null);
        Assume.assumeTrue(LocalDate.of(2018, 6, 28).isAfter(LocalDate.now()));

        SearchView instance = new SearchView(storage);
        // model:page
        Item item = new Item("pid");
        item.setModel("model:page");
        item.setLabel("1");
        instance.resolveObjectLabel(item);
        assertEquals("1", item.getLabel());

        item.setLabel("1, ");
        instance.resolveObjectLabel(item);
        assertEquals("1, ", item.getLabel());

        item.setLabel("1, TitlePage");
        instance.resolveObjectLabel(item);
        assertEquals("1, Title Page", item.getLabel());

        item.setLabel("1, UnknownPageType");
        instance.resolveObjectLabel(item);
        assertEquals("1, UnknownPageType", item.getLabel());

        instance.setLocale(new Locale("cs"));

        item.setLabel("[1], 1, TitlePage");
        instance.resolveObjectLabel(item);
        assertEquals("[1], 1, Titulní strana (TitlePage)", item.getLabel());

        // model:periodical
        item.setModel("model:periodical");
        item.setLabel("[1], 1, TitlePage");
        instance.resolveObjectLabel(item);
        assertEquals("[1], 1, TitlePage", item.getLabel());
    }

    @Test
    public void testFindLastCreated() throws Exception {
        // XXX needs some assertions; it tests no exception now
//        client.debug(true);
        String user = NdkPlugin.MODEL_PERIODICAL;
//        String user = null;
        SearchView instance = new SearchView(storage);
        List<Item> result = instance.findLastCreated(0, user, null, false, "desc");
        System.out.println(result);
    }

    @Test
    public void testFind_HasOwner() throws Exception {
        String modelId = NdkPlugin.MODEL_PERIODICAL;
        UserProfile user = UserProfile.create(testName.getMethodName(), testName.getMethodName(), testName.getMethodName());
        fedora.cleanUp();
        // prepare fedora user and group
        FedoraTransaction tx = new FedoraTransaction(storage);
        FedoraGroupDao groups = new FedoraGroupDao();
        groups.setTransaction(tx);
        Group group = Group.create(testName.getMethodName(), null);
        groups.addNewGroup(group, fedora.getTestUser(), testName.getMethodName());
        FedoraUserDao users = new FedoraUserDao();
        users.setTransaction(tx);
        users.add(user, fedora.getTestUser(), testName.getMethodName());
        users.addMembership(user, Arrays.asList(group), testName.getMethodName());
        tx.commit();

        // prepare digital object
        LocalObject lobject = new LocalStorage().create();
        DigitalObjectHandler handler = new DigitalObjectHandler(lobject, null);
        lobject.setLabel(testName.getMethodName());
        lobject.setOwner(fedora.getTestUser());
        RelationEditor relations = handler.relations();
        relations.setModel(modelId);
        relations.setOwners(Arrays.asList(group.getName()));
        relations.write(relations.getLastModified(), testName.getMethodName());
        DcStreamEditor adminEditor = handler.objectMetadata();
        DublinCoreRecord dcr = adminEditor.read();
        adminEditor.write(handler, dcr, testName.getMethodName());
        handler.commit();
        storage.ingest(lobject, fedora.getTestUser(), testName.getMethodName());

//        fedora.getClient().debug(true);
        SearchView instance = new SearchView(storage);
        List<Item> result = instance.findLastCreated(0, modelId, user.getUserNameAsPid(), false, "desc");
        System.out.println(result);
        assertNotNull(result);
        assertEquals(1, result.size());
        final String pid = lobject.getPid();
        FedoraTestSupport.assertItem(result, pid);

        result = instance.findQuery(null, null, null, null, modelId, Arrays.asList(UserUtil.toGroupPid(group)));
        assertNotNull(result);
        assertEquals(1, result.size());
        FedoraTestSupport.assertItem(result, pid);
    }

    @Test
    public void testBuildQuery() {
        assertEquals("title~'*test*'", SearchView.buildQuery(new StringBuilder(), "title", "test").toString());
        assertEquals("title~'*test*'", SearchView.buildQuery(new StringBuilder(), "title", "  test  ").toString());
        // issue 220
        assertEquals("title~'*test test*'", SearchView.buildQuery(new StringBuilder(), "title", "  test test  ").toString());
        assertEquals("title~'*test?s test???*'", SearchView.buildQuery(new StringBuilder(), "title", "test's test???").toString());
        assertEquals("", SearchView.buildQuery(new StringBuilder(), "title", "").toString());
        assertEquals("", SearchView.buildQuery(new StringBuilder(), "title", "***").toString());

        assertEquals("label~'*test1*' title~'*test2*'", SearchView.buildQuery(
                SearchView.buildQuery(new StringBuilder(), "label", "test1"),
                "title", "test2").toString());
    }

    @Test
    public void testNormalizePhrase() {
        assertEquals("*query*", SearchView.normalizePhrase("*query*"));
        assertEquals("*query*", SearchView.normalizePhrase("*query"));
        assertEquals("*query*", SearchView.normalizePhrase("query*"));
        assertEquals("*query*", SearchView.normalizePhrase(" * query* **"));
        assertEquals("*", SearchView.normalizePhrase(" ***"));
        assertEquals("*", SearchView.normalizePhrase(""));
        assertEquals("*", SearchView.normalizePhrase(null));
        assertEquals("*Stráž pokroku*", SearchView.normalizePhrase("Stráž pokroku"));
    }

    @Test
    public void testFindQuery() throws Exception {
//        fedora.getClient().debug(true);
        fedora.cleanUp();
        fedora.ingest(
                getClass().getResource("tree1.xml")
        );
        SearchView instance = new SearchView(storage);
        List<Item> result = instance.findQuery("tree", "tree", "u", "u", NdkPlugin.MODEL_PERIODICAL, Collections.<String>emptyList());
        assertFalse(result.isEmpty());
    }

    @Test
    public void testFindModelQuery() throws Exception {
//        fedora.getClient().debug(true);
        fedora.cleanUp();
        fedora.ingest(
                getClass().getResource("tree1.xml")
        );
        SearchView instance = new SearchView(storage);
        List<Item> result = instance.findQuery(null, null, null, null, NdkPlugin.MODEL_PERIODICAL, Collections.<String>emptyList());
        assertFalse(result.isEmpty());
    }

    @Test
    public void testFindPid() throws Exception {
//        fedora.getClient().debug(true);
        fedora.cleanUp();
        fedora.ingest(
                getClass().getResource("tree1-child1-child1-child1.xml"),
                getClass().getResource("tree1-child1-child1-child2.xml")
        );
        SearchView instance = new SearchView(storage);
        String[] pids = {"uuid:tree1-child1-child1-child1", "uuid:tree1-child1-child1-child2"};
        List<Item> result = instance.find(pids);
        assertItem(result, pids);
    }

    @Test
    public void testFindChildrenHierarchy() throws Exception {
        fedora.cleanUp();
        fedora.ingest(
                getClass().getResource("tree1.xml"),
                getClass().getResource("tree1-child1.xml"),
                getClass().getResource("tree1-child1-child1.xml"),
                getClass().getResource("tree1-child1-child1-child1.xml"),
                getClass().getResource("tree1-child1-child1-child2.xml"),
                getClass().getResource("tree1-child2.xml"),
                getClass().getResource("tree1-child2-child1.xml"),
                getClass().getResource("tree1-child2-child1-child1.xml")
                );
        SearchView instance = new SearchView(storage);
        List<Item> result = instance.findChildrenHierarchy("uuid:tree1");
        assertEquals(7, result.size());
        assertItem(result, "uuid:tree1-child1",
                "uuid:tree1-child1-child1",
                "uuid:tree1-child1-child1-child1",
                "uuid:tree1-child1-child1-child2",
                "uuid:tree1-child2",
                "uuid:tree1-child2-child1",
                "uuid:tree1-child2-child1-child1");
    }

    @Test
    public void testFindReferrers() throws Exception {
        fedora.cleanUp();
        fedora.ingest(
                getClass().getResource("tree1-child1-child1.xml"),
                getClass().getResource("tree1-child1-child1-child1.xml")
                );
        SearchView instance = new SearchView(storage);
        List<Item> result = instance.findReferrers("uuid:tree1-child1-child1-child1");
        assertEquals(1, result.size());
        assertItem(result, "uuid:tree1-child1-child1");
    }

    @Test
    public void testIsDeviceInUse() throws Exception {
        fedora.cleanUp();
        SearchView instance = new SearchView(storage);
        assertFalse(instance.isDeviceInUse("device:testDevice"));
        fedora.ingest(
                getClass().getResource("device.xml"),
                getClass().getResource("tree1-child1-child1-child1.xml")
                );
        assertTrue(instance.isDeviceInUse("device:testDevice"));
    }

}
