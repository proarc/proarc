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
package cz.cas.lib.proarc.common.process;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.akubra.SolrLogFeeder;
import cz.cas.lib.proarc.common.storage.akubra.SolrLogMetadata;
import cz.cas.lib.proarc.common.storage.akubra.SolrUtils;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.util.Collection;
import java.util.ArrayDeque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/** Collects actual package members; failed/stopped batches never become EXPORT events. */
public final class AuditLog implements AutoCloseable {
    public static final Set<String> ARTICLE_MODELS = Set.of(NdkPlugin.MODEL_ARTICLE,
            NdkEbornPlugin.MODEL_EARTICLE, BornDigitalModsPlugin.MODEL_ARTICLE);
    private static final ThreadLocal<AuditLog> CURRENT = new ThreadLocal<>();
    private final Map<String, Set<String>> packages = new LinkedHashMap<>();

    private AuditLog() { }

    public static AuditLog begin() {
        if (CURRENT.get() != null) throw new IllegalStateException("Export audit already active");
        AuditLog audit = new AuditLog();
        CURRENT.set(audit);
        return audit;
    }

    public static void record(String rootPid, Collection<String> exportedPids) {
        AuditLog audit = CURRENT.get();
        if (audit != null && !exportedPids.isEmpty()) {
            audit.packages.computeIfAbsent(rootPid, key -> new LinkedHashSet<>()).addAll(exportedPids);
        }
    }

    /** Restrict a METS package to its own tree; the context cache can contain other packages. */
    public static void recordTree(IMetsElement root) {
        if (CURRENT.get() == null) return;
        Set<String> pids = new LinkedHashSet<>();
        ArrayDeque<IMetsElement> pending = new ArrayDeque<>();
        pending.add(root);
        while (!pending.isEmpty()) {
            IMetsElement element = pending.removeFirst();
            if (pids.add(element.getOriginalPid()) && element.getChildren() != null) pending.addAll(element.getChildren());
        }
        record(root.getOriginalPid(), pids);
    }

    public void complete(Batch batch, UserProfile user, SolrLogFeeder feeder) throws DigitalObjectException {
        if (batch == null || (batch.getState() != Batch.State.EXPORT_DONE
                && batch.getState() != Batch.State.EXPORT_WARNING)) return;
        Map<String, SolrLogMetadata> snapshots = new LinkedHashMap<>();
        Set<String> articles = new LinkedHashSet<>();
        for (Map.Entry<String, Set<String>> entry : packages.entrySet()) {
            long pages = 0;
            long others = 0;
            Set<String> devices = new LinkedHashSet<>();
            Set<String> software = new LinkedHashSet<>();
            Set<String> siglas = new LinkedHashSet<>();
            for (String pid : entry.getValue()) {
                SolrLogMetadata metadata = snapshot(pid, feeder, snapshots);
                if (ARTICLE_MODELS.contains(metadata.model())) articles.add(pid);
                if (SolrUtils.isPage(metadata.model())) pages++; else others++;
                devices.addAll(metadata.devices());
                software.addAll(metadata.software());
                siglas.addAll(metadata.siglas());
            }
            SolrLogMetadata root = snapshot(entry.getKey(), feeder, snapshots);
            devices.addAll(root.devices());
            software.addAll(root.software());
            siglas.addAll(root.siglas());
            feeder.feedExportLog(entry.getKey(), user, new SolrLogMetadata(root.model(), devices, siglas, software),
                    pages, others, batch.getProfileId());
        }
        for (String pid : articles) {
            feeder.feedArticleExportLog(pid, user, snapshots.get(pid), batch.getProfileId());
        }
    }

    private SolrLogMetadata snapshot(String pid, SolrLogFeeder feeder, Map<String, SolrLogMetadata> snapshots)
            throws DigitalObjectException {
        SolrLogMetadata metadata = snapshots.get(pid);
        if (metadata == null) {
            metadata = feeder.metadataFor(pid);
            if (metadata.model() == null) throw new DigitalObjectException(pid, "Missing model for export audit");
            snapshots.put(pid, metadata);
        }
        return metadata;
    }

    @Override
    public void close() { CURRENT.remove(); }
}
