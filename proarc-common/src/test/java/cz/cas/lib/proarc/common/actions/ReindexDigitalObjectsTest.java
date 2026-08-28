package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ReindexDigitalObjectsTest {

    @Test
    void findsAllPageParentsRecursively() throws Exception {
        Map<String, List<ReindexDigitalObjects.HierarchyMember>> hierarchy = new HashMap<>();
        hierarchy.put("periodical", members(member("volume", "model:periodicalvolume")));
        hierarchy.put("volume", members(
                member("issue", "model:periodicalitem"),
                member("volume-supplement", "model:supplement")));
        hierarchy.put("issue", members(
                member("page-1", NdkPlugin.MODEL_PAGE),
                member("page-2", NdkPlugin.MODEL_PAGE),
                member("issue-supplement", "model:supplement")));
        hierarchy.put("issue-supplement", members(member("page-3", NdkPlugin.MODEL_NDK_PAGE)));
        hierarchy.put("volume-supplement", members(member("page-4", NdkPlugin.MODEL_PAGE)));

        List<String> result = ReindexDigitalObjects.findPageParents(
                "periodical", pid -> hierarchy.getOrDefault(pid, List.of()));

        assertEquals(List.of("issue-supplement", "issue", "volume-supplement"), result);
    }

    @Test
    void visitsSharedObjectsAndCyclesOnlyOnce() throws Exception {
        Map<String, List<ReindexDigitalObjects.HierarchyMember>> hierarchy = new HashMap<>();
        hierarchy.put("root", members(member("first", "model:container"), member("second", "model:container")));
        hierarchy.put("first", members(member("shared", "model:supplement")));
        hierarchy.put("second", members(member("shared", "model:supplement")));
        hierarchy.put("shared", members(
                member("root", "model:container"),
                member("page", NdkPlugin.MODEL_PAGE)));

        List<String> result = ReindexDigitalObjects.findPageParents(
                "root", pid -> hierarchy.getOrDefault(pid, List.of()));

        assertEquals(List.of("shared"), result);
    }

    private static List<ReindexDigitalObjects.HierarchyMember> members(
            ReindexDigitalObjects.HierarchyMember... members) {
        return List.of(members);
    }

    private static ReindexDigitalObjects.HierarchyMember member(String pid, String model) {
        return new ReindexDigitalObjects.HierarchyMember(pid, model);
    }
}
