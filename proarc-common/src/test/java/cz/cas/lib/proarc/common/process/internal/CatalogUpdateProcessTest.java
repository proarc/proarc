package cz.cas.lib.proarc.common.process.internal;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class CatalogUpdateProcessTest {
    private static final String TITLE = "model:ndkperiodical";
    private static final String VOLUME = "model:ndkperiodicalvolume";
    private static final String ISSUE = "model:ndkperiodicalissue";

    @Test
    void volumeIncludesAllDescendantsAndOnlyItsDirectAncestors() throws Exception {
        Tree tree = tree();
        assertEquals(List.of("volume", "issue", "article", "page", "title"),
                CatalogUpdateProcess.selectTargets(tree, List.of("volume"), List.of(TITLE, VOLUME, ISSUE, "article", "page")));
        assertFalse(tree.childrenRead.contains("title"));
    }

    @Test
    void issueIncludesItsSubtreeButNotSiblingIssueOrOtherVolumes() throws Exception {
        Tree tree = tree();
        tree.add("sibling", ISSUE, "volume");
        assertEquals(List.of("issue", "article", "page", "volume", "title"),
                CatalogUpdateProcess.selectTargets(tree, List.of("issue"), List.of(TITLE, VOLUME, ISSUE, "article", "page")));
        assertFalse(tree.childrenRead.contains("volume"));
        assertFalse(tree.childrenRead.contains("title"));
    }

    @Test
    void sharedAncestorsAndOverlappingSelectionsAreDeduplicatedAndModelsAreFiltered() throws Exception {
        assertEquals(List.of("issue", "title"), CatalogUpdateProcess.selectTargets(tree(),
                List.of("volume", "issue", "issue"), List.of(ISSUE, TITLE)));
        assertTrue(CatalogUpdateProcess.selectTargets(tree(), List.of("volume"), List.of("unsupported")).isEmpty());
    }

    @Test
    void ambiguousOrCyclicAncestorPathFailsRatherThanFollowingAnotherBranch() {
        Tree tree = tree();
        tree.parents.put("volume", List.of(tree.items.get("title"), tree.items.get("otherVolume")));
        assertThrows(IOException.class, () -> CatalogUpdateProcess.selectTargets(tree, List.of("issue"), List.of(TITLE)));
        tree.parents.put("volume", List.of(tree.items.get("issue")));
        assertThrows(IOException.class, () -> CatalogUpdateProcess.selectTargets(tree, List.of("issue"), List.of(TITLE)));
    }

    @Test
    void catalogParametersSurviveBatchSerializationAndMissingOptInRemainsDisabled() {
        BatchParams params = new BatchParams(List.of("uuid:one"));
        params.setUpdateCatalog(true);
        params.setCatalogId("library");
        Batch batch = new Batch();
        batch.setParamsFromObject(params);
        BatchParams restored = batch.getParamsAsObject();
        assertEquals(Boolean.TRUE, restored.isUpdateCatalog());
        assertEquals("library", restored.getCatalogId());
        batch.setParamsFromObject(new BatchParams(List.of("uuid:old")));
        assertFalse(Boolean.TRUE.equals(batch.getParamsAsObject().isUpdateCatalog()));
    }

    private Tree tree() {
        Tree tree = new Tree();
        tree.add("title", TITLE, null);
        tree.add("volume", VOLUME, "title");
        tree.add("otherVolume", VOLUME, "title");
        tree.add("issue", ISSUE, "volume");
        tree.add("article", "article", "issue");
        tree.add("page", "page", "article");
        tree.add("otherIssue", ISSUE, "otherVolume");
        return tree;
    }

    private static class Tree extends SearchView {
        final Map<String, SearchViewItem> items = new LinkedHashMap<>();
        final Map<String, List<SearchViewItem>> children = new LinkedHashMap<>();
        final Map<String, List<SearchViewItem>> parents = new LinkedHashMap<>();
        final List<String> childrenRead = new ArrayList<>();
        void add(String pid, String model, String parent) {
            SearchViewItem item = new SearchViewItem(pid);
            item.setModel(model);
            items.put(pid, item);
            if (parent != null) {
                children.computeIfAbsent(parent, key -> new ArrayList<>()).add(item);
                parents.put(pid, List.of(items.get(parent)));
            }
        }
        @Override public List<SearchViewItem> find(List<String> pids) {
            return pids.stream().map(items::get).toList();
        }
        @Override public List<SearchViewItem> findSortedChildren(String pid) {
            childrenRead.add(pid);
            return children.getOrDefault(pid, List.of());
        }
        @Override public List<SearchViewItem> findReferrers(String pid) {
            return parents.getOrDefault(pid, List.of());
        }
    }
}
