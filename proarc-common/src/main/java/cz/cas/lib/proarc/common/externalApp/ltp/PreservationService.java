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
package cz.cas.lib.proarc.common.externalApp.ltp;

import cz.cas.lib.proarc.common.catalog.updateCatalog.UpdateCatalog;
import cz.cas.lib.proarc.common.catalog.updateCatalog.VerbisUpdateCatalog;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.LtpCesnetClient;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.io.File;
import java.io.IOException;
import java.time.Instant;
import java.util.Objects;
import org.json.JSONObject;

/** Durable provider-independent workflow. API clients do not depend on RELS-EXT or Verbis. */
public final class PreservationService {
    private static final Object[] LOCKS = new Object[64];
    static { for (int i = 0; i < LOCKS.length; i++) LOCKS[i] = new Object(); }

    public interface Repository {
        ArchivInfo read(String pid) throws IOException;
        void save(String pid, ArchivInfo record) throws IOException;
    }
    @FunctionalInterface public interface ProviderFactory {
        PreservationProvider open(ArchivInfo record) throws IOException;
    }
    @FunctionalInterface public interface CatalogWriter {
        void write(String pid, ArchivInfo record) throws IOException;
    }

    private final Repository repository;
    private final ProviderFactory providers;
    private final CatalogWriter catalog;

    public PreservationService(Repository repository, ProviderFactory providers, CatalogWriter catalog) {
        this.repository = repository;
        this.providers = providers;
        this.catalog = catalog;
    }

    public static PreservationService cesnet(AppConfiguration config) {
        return cesnet(config, null);
    }

    public static PreservationService cesnet(AppConfiguration config, Integer credentialOwnerId) {
        return new PreservationService(new ObjectRepository(), record -> {
            if (!LtpCesnetClient.ID.equals(record.getProvider())) throw new IOException("Unsupported LTP provider");
            // Old submissions retain their destination even if the current upload configuration changes.
            org.apache.commons.configuration2.BaseConfiguration options = new org.apache.commons.configuration2.BaseConfiguration();
            options.copy(config.getLtpCesnetConfiguration());
            options.setProperty("apiUrl", record.getEndpoint());
            options.setProperty("groupToken", record.getGroup());
            return cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenStore.getInstance()
                    .open(config, credentialOwnerId == null ? record.getCredentialOwnerId() : credentialOwnerId, options);
        }, (pid, record) -> {
            if (record.getCatalogId() == null || record.getCatalogId().isBlank()) return;
            CatalogConfiguration target = config.getCatalogs().findConfiguration(record.getCatalogId());
            if (target == null || !VerbisUpdateCatalog.ID.equals(target.getCatalogUpdateType())) {
                throw new IOException("LTP catalog update requires a configured Verbis catalog");
            }
            try {
                String field001 = UpdateCatalog.getObjectField001(pid);
                if (field001 == null || field001.isBlank()) throw new IOException("Missing catalog record identifier (001)");
                new VerbisUpdateCatalog(config, null).processLtp(target, field001, record.getPackageId());
            } catch (cz.cas.lib.proarc.common.storage.DigitalObjectException ex) {
                throw new IOException("Cannot read catalog identifier for " + pid, ex);
            }
        });
    }

    private static Object lock(String pid) { return LOCKS[(pid.hashCode() & Integer.MAX_VALUE) % LOCKS.length]; }

    public void submit(String pid, File archive, JSONObject metadata, ArchivInfo request) throws IOException {
        submit(pid, archive, metadata, request, false);
    }

    public ArchivInfo archivInfo(String pid) throws IOException {
        synchronized (lock(pid)) { return repository.read(pid); }
    }

    public void submit(String pid, File archive, JSONObject metadata, ArchivInfo request, boolean confirmed) throws IOException {
        synchronized (lock(pid)) {
            ArchivInfo record = repository.read(pid);
            ArchivInfo current = record != null && Objects.equals(record.getOperationId(), request.getOperationId())
                    && sameTarget(record, request) ? record : null;
            if (current != null && (current.getState() == ArchivInfo.State.UPLOADED
                    || current.getState() == ArchivInfo.State.STORED)) {
                // Also repairs an index update that failed after RELS-EXT had already been committed.
                repository.save(pid, current);
                return;
            }
            if (current == null) {
                if (record != null && !confirmed) throw new IOException("LTP upload requires confirmation for " + pid);
                current = request;
                current.setPreviousPackageId(null);
                current.setSubmittedAt(Instant.now().toString());
                current.setState(ArchivInfo.State.REGISTERING);
                repository.save(pid, current);
            }
            final ArchivInfo submission = current;
            if (request.getCredentialOwnerId() != null
                    && !Objects.equals(submission.getCredentialOwnerId(), request.getCredentialOwnerId())) {
                submission.setCredentialOwnerId(request.getCredentialOwnerId());
                repository.save(pid, current);
            }
            try (PreservationProvider provider = providers.open(submission)) {
                PreservationProvider.RemotePackage result = provider.submit(archive, metadata, submission, remote -> {
                    apply(submission, remote);
                    repository.save(pid, submission);
                });
                apply(submission, result);
                if (submission.getState() == ArchivInfo.State.UPLOADING) submission.setState(ArchivInfo.State.UPLOADED);
                repository.save(pid, current);
                // The status check schedules a separate catalog synchronization when configured.
            }
        }
    }

    public void check(String pid) throws IOException {
        check(pid, null, false);
    }

    /** Use the catalog configured at check time, independently of the upload configuration. */
    public void check(String pid, String catalogId) throws IOException {
        check(pid, catalogId, true);
    }

    private void check(String pid, String catalogId, boolean configureCatalog) throws IOException {
        synchronized (lock(pid)) {
            ArchivInfo record = repository.read(pid);
            if (record == null || record.getState() == ArchivInfo.State.FAILED) return;
            if (record.getPackageId() == null) {
                try (PreservationProvider provider = providers.open(record)) {
                    PreservationProvider.RemotePackage recovered = provider.recover(record.getOperationId());
                    if (recovered == null) return;
                    apply(record, recovered);
                    repository.save(pid, record);
                }
            }
            if (record.getState() != ArchivInfo.State.STORED) {
                try (PreservationProvider provider = providers.open(record)) {
                    PreservationProvider.RemotePackage remote = provider.check(record.getPackageId());
                    if (remote.state() != ArchivInfo.State.STORED && remote.state() != ArchivInfo.State.FAILED) return;
                    apply(record, remote);
                }
            }
            if (record.getState() == ArchivInfo.State.STORED && configureCatalog) {
                record.setCatalogId(catalogId);
            }
            // Also clear stale ltpPending flags for packages indexed under the former catalog retry policy.
            repository.save(pid, record);
        }
    }

    /** Catalog synchronization is a separate, permission-checked process. */
    public void updateCatalog(String pid) throws IOException {
        synchronized (lock(pid)) {
            ArchivInfo record = repository.read(pid);
            if (needsCatalogUpdate(record)) {
                catalog.write(pid, record);
                record.setCatalogState(ArchivInfo.CatalogState.DONE);
                repository.save(pid, record);
            }
        }
    }

    public static boolean needsCatalogUpdate(ArchivInfo record) {
        return record != null && record.getState() == ArchivInfo.State.STORED
                && record.getCatalogId() != null && !record.getCatalogId().isBlank()
                && record.getCatalogState() != ArchivInfo.CatalogState.DONE;
    }

    private static boolean sameTarget(ArchivInfo a, ArchivInfo b) {
        return Objects.equals(a.getProvider(), b.getProvider()) && Objects.equals(a.getEndpoint(), b.getEndpoint())
                && Objects.equals(a.getGroup(), b.getGroup());
    }

    private static void apply(ArchivInfo record, PreservationProvider.RemotePackage remote) {
        record.setPackageId(remote.id());
        record.setVersion(remote.version());
        record.setState(remote.state());
        record.setIntegrityStatus(remote.integrityStatus());
        record.setUploadStatus(remote.uploadStatus());
        if (remote.state() == ArchivInfo.State.STORED && record.getStoredAt() == null) record.setStoredAt(Instant.now().toString());
    }

    private static class ObjectRepository implements Repository {
        private final java.util.Map<String, Long> versions = new java.util.HashMap<>();
        @Override public ArchivInfo read(String pid) throws IOException {
            try {
                RelationEditor editor = new RelationEditor(DigitalObjectManager.getDefault().find(pid, null));
                versions.put(pid, editor.getLastModified());
                return editor.getArchivInfo();
            }
            catch (Exception ex) { throw new IOException("Cannot read LTP records for " + pid, ex); }
        }
        @Override public void save(String pid, ArchivInfo record) throws IOException {
            try {
                ProArcObject object = DigitalObjectManager.getDefault().find(pid, null);
                RelationEditor editor = new RelationEditor(object);
                long timestamp = editor.getLastModified();
                if (!Objects.equals(versions.get(pid), timestamp)) throw new IOException("Object changed during LTP processing; retry " + pid);
                editor.setArchivInfo(record);
                editor.write(timestamp, "LTP archivInfo status");
                object.flush();
                versions.put(pid, new RelationEditor(DigitalObjectManager.getDefault().find(pid, null)).getLastModified());
            } catch (Exception ex) { throw new IOException("Cannot save/index LTP records for " + pid, ex); }
        }
    }
}
