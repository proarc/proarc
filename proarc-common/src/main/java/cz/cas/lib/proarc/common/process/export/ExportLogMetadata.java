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

import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.AuditLog.MetadataResolver;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.akubra.SolrLogMetadata;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/** Prepares export snapshots before handing them to the loggingCore writer. */
final class ExportLogMetadata implements MetadataResolver {
    private static final Set<String> PERIODICAL_MODELS = Set.of(K4Plugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICAL, NdkEbornPlugin.MODEL_EPERIODICAL);
    private final MetadataResolver objects;
    private final SearchView search;
    private final Map<String, SolrLogMetadata> snapshots = new HashMap<>();

    ExportLogMetadata(MetadataResolver objects, SearchView search) {
        this.objects = objects;
        this.search = search;
    }

    @Override
    public SolrLogMetadata resolve(String pid) throws DigitalObjectException {
        SolrLogMetadata metadata = snapshot(pid);
        if (!SolrLogMetadata.isIssue(metadata.model())) return metadata;
        return new SolrLogMetadata(metadata.model(), metadata.devices(), metadata.siglas(), metadata.software(),
                metadata.dois(), metadata.urnNbns(), label(pid, metadata.label()));
    }

    private SolrLogMetadata snapshot(String pid) throws DigitalObjectException {
        SolrLogMetadata metadata = snapshots.get(pid);
        if (metadata == null) {
            metadata = objects.resolve(pid);
            snapshots.put(pid, metadata);
        }
        return metadata;
    }

    private String label(String pid, String issueTitle) throws DigitalObjectException {
        Set<String> visited = new HashSet<>();
        ArrayDeque<String> pending = new ArrayDeque<>();
        pending.add(pid);
        try {
            while (!pending.isEmpty()) {
                String current = pending.removeFirst();
                if (!visited.add(current)) continue;
                for (var parent : search.findReferrers(current)) {
                    String model = parent.getModel();
                    if (PERIODICAL_MODELS.contains(model == null ? "" : model)) {
                        String title = snapshot(parent.getPid()).label();
                        return title == null || title.isBlank() ? issueTitle
                                : title + (issueTitle == null || issueTitle.isBlank() ? "" : ", " + issueTitle);
                    }
                    if (model != null && model.contains("periodicalvolume")) pending.add(parent.getPid());
                }
            }
            return issueTitle;
        } catch (IOException ex) {
            throw new DigitalObjectException(pid, "Cannot read export package title", ex);
        }
    }
}
