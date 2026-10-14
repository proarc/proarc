package cz.cas.lib.proarc.common.process.internal;

import cz.cas.lib.proarc.common.actions.CatalogRecord;
import cz.cas.lib.proarc.common.catalog.updateCatalog.CatalogUpdateResult;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.InternalExternalDispatcher;
import cz.cas.lib.proarc.common.process.InternalExternalProcess;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/** Shared, queued catalog operation for manual requests and completed exports. */
public final class CatalogUpdateProcess {
    private CatalogUpdateProcess() { }

    public static synchronized void retry(AppConfiguration config, AkubraConfiguration akubra, BatchManager manager,
            UserProfile user, int batchId, String log, Locale locale) throws IOException {
        Batch batch = manager.get(batchId);
        if (batch == null || !Batch.INTERNAL_UPDATE_CATALOG_RECORDS.equals(batch.getProfileId())
                || !(batch.getState() == Batch.State.INTERNAL_FAILED || batch.getState() == Batch.State.STOPPED)) {
            throw new IOException("Lze opakovat pouze neúspěšný nebo zastavený zápis do katalogu.");
        }
        batch.setState(Batch.State.INTERNAL_PLANNED);
        batch.setUserId(user.getId());
        batch.setUpdated(new java.sql.Timestamp(System.currentTimeMillis()));
        batch = manager.update(batch);
        InternalExternalDispatcher.getDefault().addInternalExternalProcess(
                InternalExternalProcess.prepare(config, akubra, batch, manager, user, log, locale));
    }

    public static List<String> selectTargets(SearchView search, List<String> roots, List<String> models)
            throws Exception {
        Map<String, SearchViewItem> objects = new LinkedHashMap<>();
        Set<String> descendants = new LinkedHashSet<>();
        ArrayDeque<String> queue = new ArrayDeque<>(roots);
        while (!queue.isEmpty()) {
            String pid = queue.removeFirst();
            if (!descendants.add(pid)) {
                continue;
            }
            List<SearchViewItem> found = search.find(List.of(pid));
            if (found.isEmpty()) {
                throw new IOException("Objekt nenalezen: " + pid);
            }
            objects.put(pid, found.get(0));
            for (SearchViewItem child : search.findSortedChildren(pid)) {
                queue.addLast(child.getPid());
            }
        }
        for (String root : roots) {
            Set<String> path = new LinkedHashSet<>();
            String pid = root;
            while (true) {
                if (!path.add(pid)) {
                    throw new IOException("Cyklus v cestě předků: " + pid);
                }
                List<SearchViewItem> parents = search.findReferrers(pid);
                if (parents.isEmpty()) {
                    break;
                }
                if (parents.size() != 1) {
                    throw new IOException("Objekt má více rodičů: " + pid);
                }
                SearchViewItem parent = parents.get(0);
                objects.putIfAbsent(parent.getPid(), parent);
                pid = parent.getPid();
            }
        }
        return objects.values().stream().filter(item -> models.contains(item.getModel()))
                .map(SearchViewItem::getPid).toList();
    }

    public static void schedule(AppConfiguration config, AkubraConfiguration akubra,
            BatchManager manager, UserProfile user, List<String> pids, String catalogId,
            boolean nightOnly, String priority, String log, Locale locale) throws IOException {
        for (String pid : new LinkedHashSet<>(pids)) {
            BatchParams params = new BatchParams(List.of(pid));
            params.setCatalogId(catalogId);
            Batch batch = manager.add(pid, user, Batch.INTERNAL_UPDATE_CATALOG_RECORDS,
                    Batch.State.INTERNAL_PLANNED, nightOnly, priority, params);
            InternalExternalProcess process = InternalExternalProcess.prepare(config, akubra, batch, manager, user, log, locale);
            InternalExternalDispatcher.getDefault().addInternalExternalProcess(process);
        }
    }

    public static Batch run(AppConfiguration config, AkubraConfiguration akubra, BatchManager manager,
            UserProfile user, Batch batch, BatchParams params, Locale locale) {
        String context = "Katalog: " + params.getCatalogId() + "\nObjekt PID: " + params.getPids();
        try {
            // Claim the persisted job, so cancelled or duplicate queued tasks cannot perform a write.
            synchronized (CatalogUpdateProcess.class) {
                Batch current = manager.get(batch.getId());
                if (current == null) throw new IOException("Katalogová dávka neexistuje.");
                if (current.getState() != Batch.State.INTERNAL_PLANNED) return current;
                batch = BatchUtils.startWaitingInternalBatch(manager, current);
            }
            user = cz.cas.lib.proarc.common.user.UserUtil.getDefaultManger().find(batch.getUserId());
            if (user == null || !user.hasImportToCatalogFunction()) {
                throw new IOException("Uživatel nemá oprávnění k zápisu do katalogu.");
            }
            if (params.getCatalogId() == null || params.getCatalogId().isBlank()
                    || params.getPids() == null || params.getPids().size() != 1) {
                throw new IOException("Chybí katalog nebo jednoznačný cílový objekt.");
            }
            String pid = params.getPids().get(0);
            List<SearchViewItem> found = AkubraStorage.getInstance(akubra).getSearch(locale).find(List.of(pid));
            if (found.isEmpty()) throw new IOException("Objekt nenalezen: " + pid);
            SearchViewItem item = found.get(0);
            if (!config.getCatalogUpdateModels().contains(item.getModel())) {
                throw new IOException("Model není povolen pro zápis do katalogu: " + item.getModel());
            }
            ValidationProcess.Result validation = new ValidationProcess(config, akubra, List.of(pid), locale)
                    .validate(ValidationProcess.Type.UPDATE_CATALOG_RECORD);
            if (!validation.isStatusOk(true)) {
                throw new IOException(validation.getMessages());
            }
            CatalogUpdateResult result = new CatalogRecord(config, akubra).updateWithResult(params.getCatalogId(), pid);
            return BatchUtils.finishedSuccessfully(manager, batch, batch.getFolder(), context + "\n" + result.message(),
                    result.warning() ? Batch.State.INTERNAL_WARNING : Batch.State.INTERNAL_DONE);
        } catch (Exception ex) {
            return BatchUtils.finishedInternalWithError(manager, batch, batch.getFolder(), context + "\n" + ex.getMessage());
        }
    }
}
