/*
 * Copyright (C) 2026 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.export;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.AuditLog;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.storage.akubra.SolrLogFeeder;
import cz.cas.lib.proarc.common.storage.akubra.SolrLogMetadata;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import static org.junit.jupiter.api.Assertions.*;

class ExportAuditTest {
    private final CapturingFeeder feeder = new CapturingFeeder();
    private final UserProfile actor = UserProfile.create("actor", "password", "Actor");

    private Batch batch(Batch.State state) {
        Batch batch = new Batch();
        batch.setId(42);
        batch.setState(state);
        batch.setProfileId(Batch.EXPORT_NDK);
        return batch;
    }

    @Test
    void logsAllThreeArticleModelsOncePerBatchWithTheirOwnDois() throws Exception {
        feeder.snapshots.put("root", new SolrLogMetadata("model:ndkperiodicalissue", Set.of(), Set.of()));
        int index = 0;
        for (String model : AuditLog.ARTICLE_MODELS) {
            feeder.snapshots.put("article" + index, new SolrLogMetadata(model, Set.of(), Set.of("ABA001"), Set.of(), Set.of("10.1234/" + index)));
            index++;
        }
        try (AuditLog audit = AuditLog.begin()) {
            AuditLog.record("root", List.of("root", "article0", "article1", "article2"));
            AuditLog.record("article0", List.of("article0"));
            audit.complete(batch(Batch.State.EXPORT_DONE), actor, feeder, feeder::exportMetadata);
        }
        assertEquals(2, feeder.exports.size());
        assertEquals(3, feeder.articles.size());
        for (CapturedExport article : feeder.articles) {
            assertEquals(Set.of("10.1234/" + article.pid().substring(7)), article.metadata().dois());
            assertEquals(Batch.EXPORT_NDK, article.type());
        }
    }

    @Test
    void countsActualMembersOnceAcrossFragmentsAndKeepsDeviceAndSiglaSnapshots() throws Exception {
        feeder.snapshots.put("root", new SolrLogMetadata("model:ndkmonographunit", Set.of(), Set.of("ABA001"),
                Set.of("software:root"), Set.of(), Set.of("urn:nbn:cz:test"), "Historical book title"));
        feeder.snapshots.put("page1", new SolrLogMetadata(NdkPlugin.MODEL_NDK_PAGE, Set.of("device:first"), Set.of(), Set.of("software:page")));
        feeder.snapshots.put("page2", new SolrLogMetadata(NdkPlugin.MODEL_PAGE, Set.of("device:second"), Set.of(), Set.of("software:page")));
        feeder.snapshots.put("chapter", new SolrLogMetadata("model:ndkchapter", Set.of(), Set.of()));
        try (AuditLog audit = AuditLog.begin()) {
            AuditLog.record("root", List.of("root", "page1", "page1"));
            AuditLog.record("root", List.of("page1", "page2", "chapter"));
            audit.complete(batch(Batch.State.EXPORT_DONE), actor, feeder, feeder::exportMetadata);
        }
        assertEquals(1, feeder.exports.size());
        CapturedExport log = feeder.exports.get(0);
        assertEquals("root", log.pid());
        assertSame(actor, log.user());
        assertEquals(2, log.pages());
        assertEquals(2, log.others());
        assertEquals(Batch.EXPORT_NDK, log.type());
        assertEquals("model:ndkmonographunit", log.metadata().model());
        assertEquals("Historical book title", log.metadata().label());
        assertEquals(Set.of("urn:nbn:cz:test"), log.metadata().urnNbns());
        assertEquals(Set.of("ABA001"), log.metadata().siglas());
        assertEquals(Set.of("device:first", "device:second"), log.metadata().devices());
        assertEquals(Set.of("software:root", "software:page"), log.metadata().software());
        assertEquals(1, feeder.reads.get("page1"));
    }

    @ParameterizedTest
    @EnumSource(value = Batch.State.class, names = {"EXPORT_FAILED", "STOPPED", "EXPORTING"})
    void neverLogsFailedStoppedOrIncompleteExports(Batch.State state) throws Exception {
        try (AuditLog audit = AuditLog.begin()) {
            AuditLog.record("root", List.of("root"));
            audit.complete(batch(state), actor, feeder, feeder::exportMetadata);
        }
        assertTrue(feeder.exports.isEmpty());
        assertTrue(feeder.articles.isEmpty());
        assertTrue(feeder.reads.isEmpty());
    }

    @Test
    void logsCompletedPackageWhenBatchHasWarning() throws Exception {
        feeder.snapshots.put("page", new SolrLogMetadata(NdkPlugin.MODEL_NDK_PAGE, Set.of(), Set.of()));
        try (AuditLog audit = AuditLog.begin()) {
            AuditLog.record("page", List.of("page"));
            audit.complete(batch(Batch.State.EXPORT_WARNING), actor, feeder, feeder::exportMetadata);
        }
        assertEquals(1, feeder.exports.size());
        assertEquals(1, feeder.exports.get(0).pages());
        assertEquals(0, feeder.exports.get(0).others());
    }

    @Test
    void closesBatchScopeWithoutLeakingMembersIntoNextExport() throws Exception {
        try (AuditLog first = AuditLog.begin()) { AuditLog.record("old", List.of("old")); }
        AuditLog.record("outside", List.of("outside"));
        try (AuditLog second = AuditLog.begin()) { second.complete(batch(Batch.State.EXPORT_DONE), actor, feeder, feeder::exportMetadata); }
        assertTrue(feeder.exports.isEmpty());
        assertTrue(feeder.reads.isEmpty());
    }

    @Test
    void metsPackageCountsOnlyItsTreeAndHandlesSharedChildrenAndCycles() throws Exception {
        Map<String, List<String>> tree = Map.of("root", List.of("chapter", "page"),
                "chapter", List.of("page", "root"), "page", List.of());
        feeder.snapshots.put("root", new SolrLogMetadata("model:ndkmonographunit", Set.of(), Set.of()));
        feeder.snapshots.put("chapter", new SolrLogMetadata("model:ndkchapter", Set.of(), Set.of()));
        feeder.snapshots.put("page", new SolrLogMetadata(NdkPlugin.MODEL_NDK_PAGE, Set.of(), Set.of()));
        try (AuditLog audit = AuditLog.begin()) {
            AuditLog.recordTree(element("root", tree));
            audit.complete(batch(Batch.State.EXPORT_DONE), actor, feeder, feeder::exportMetadata);
        }
        assertEquals(1, feeder.exports.get(0).pages());
        assertEquals(2, feeder.exports.get(0).others());
        assertEquals(Set.of("root", "chapter", "page"), feeder.reads.keySet());
    }

    private IMetsElement element(String pid, Map<String, List<String>> tree) {
        return (IMetsElement) java.lang.reflect.Proxy.newProxyInstance(IMetsElement.class.getClassLoader(),
                new Class<?>[] {IMetsElement.class}, (proxy, method, args) -> switch (method.getName()) {
                    case "getOriginalPid" -> pid;
                    case "getChildren" -> tree.getOrDefault(pid, List.of()).stream().map(child -> element(child, tree)).toList();
                    default -> throw new UnsupportedOperationException(method.getName());
                });
    }

    private record CapturedExport(String pid, UserProfile user, SolrLogMetadata metadata, long pages, long others, String type) { }

    private static final class CapturingFeeder extends SolrLogFeeder {
        final Map<String, SolrLogMetadata> snapshots = new HashMap<>();
        final Map<String, Integer> reads = new HashMap<>();
        final List<CapturedExport> exports = new ArrayList<>();
        final List<CapturedExport> articles = new ArrayList<>();

        CapturingFeeder() { super(null); }

        @Override public SolrLogMetadata metadataFor(String pid) {
            throw new AssertionError("Export audit must use the supplied metadata resolver");
        }

        SolrLogMetadata exportMetadata(String pid) {
            reads.merge(pid, 1, Integer::sum);
            return snapshots.get(pid);
        }

        @Override public void feedExportLog(String pid, UserProfile user, SolrLogMetadata metadata,
                long pages, long others, String type) {
            exports.add(new CapturedExport(pid, user, metadata, pages, others, type));
        }

        @Override public void feedArticleExportLog(String pid, UserProfile user, SolrLogMetadata metadata, String type) {
            articles.add(new CapturedExport(pid, user, metadata, 0, 0, type));
        }
    }
}
