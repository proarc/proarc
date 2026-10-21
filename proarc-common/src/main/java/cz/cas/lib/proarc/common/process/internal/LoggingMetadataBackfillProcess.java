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
package cz.cas.lib.proarc.common.process.internal;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.SolrLogBackfill;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import org.apache.solr.client.solrj.impl.HttpSolrClient;

/** Admin job; progress and unresolved records are summarized in process management. */
public final class LoggingMetadataBackfillProcess {
    private static final AtomicBoolean RUNNING = new AtomicBoolean();

    public static Batch run(AkubraConfiguration config, BatchManager manager, Batch batch) {
        if (!RUNNING.compareAndSet(false, true)) {
            return BatchUtils.finishedInternalWithError(manager, batch, batch.getFolder(), new IOException("Doplňování loggingCore již probíhá."));
        }
        try {
            synchronized (LoggingMetadataBackfillProcess.class) {
                Batch current = manager.get(batch.getId());
                if (current == null) throw new IOException("Dávka neexistuje.");
                if (current.getState() != Batch.State.INTERNAL_PLANNED) return current;
                batch = BatchUtils.startWaitingInternalBatch(manager, current);
            }
            AkubraStorage storage = AkubraStorage.getInstance(config);
            Map<String, String> organizations = new LinkedHashMap<>();
            for (UserProfile profile : UserUtil.getDefaultManger().findAll()) {
                // Keep users without an organization too: they are still valid audit actors.
                organizations.put(profile.getUserName(), profile.getOrganization());
            }
            Batch running = batch;
            try (HttpSolrClient client = new HttpSolrClient.Builder(config.getSolrLoggingHost()).build()) {
                SolrLogBackfill backfill = new SolrLogBackfill(client, pid -> {
                    try {
                        return storage.exist(pid) ? new RelationEditor(storage.find(pid)).getModel() : null;
                    } catch (DigitalObjectException ex) { throw new IOException("Nelze přečíst model objektu " + pid, ex); }
                }, organizations);
                SolrLogBackfill.Result result = backfill.run(() -> {
                    Batch current = manager.get(running.getId());
                    return current == null || current.getState() == Batch.State.STOPPED;
                }, progress -> {
                    Batch current = manager.get(running.getId());
                    if (current != null && current.getState() == Batch.State.INTERNAL_RUNNING) {
                        current.setLog(progress.summary());
                        manager.update(current);
                    }
                });
                Batch current = manager.get(running.getId());
                if (current == null || current.getState() == Batch.State.STOPPED) return current;
                return BatchUtils.finishedSuccessfully(manager, running, running.getFolder(), result.summary(), Batch.State.INTERNAL_DONE);
            }
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            Batch current = manager.get(batch.getId());
            if (current != null && current.getState() == Batch.State.STOPPED) return current;
            return BatchUtils.finishedInternalWithError(manager, batch, batch.getFolder(), ex);
        } catch (Exception ex) {
            Batch current = manager.get(batch.getId());
            if (current != null && current.getState() == Batch.State.STOPPED) return current;
            return BatchUtils.finishedInternalWithError(manager, batch, batch.getFolder(), ex);
        } finally { RUNNING.set(false); }
    }
}
