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
package cz.cas.lib.proarc.common.process.external.cesnet;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.externalApp.ltp.PreservationService;
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenStore;
import cz.cas.lib.proarc.common.jobs.PreservationCheckJob;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Reconciliation and catalog writes have independent batches and logs.
 */
public final class CesnetCheckProcess {
    private final BatchManager manager;
    private final UserProfile user;
    private final Checker checker;
    private final CesnetProcess.TokenValidator validator;
    private final CatalogUpdater catalogUpdater;

    @FunctionalInterface
    interface Checker {
        int check(Consumer<String> log, Consumer<String> catalogCandidate) throws IOException;
    }

    @FunctionalInterface
    interface CatalogUpdater {
        void update(String pid) throws IOException;
    }

    public CesnetCheckProcess(AppConfiguration config, AkubraConfiguration akubra, BatchManager manager, UserProfile user) {
        this(config, akubra, manager, user, List.of());
    }

    public CesnetCheckProcess(AppConfiguration config, AkubraConfiguration akubra, BatchManager manager, UserProfile user,
                              List<String> selectedPids) {
        this(manager, user,
                (log, candidate) -> PreservationCheckJob.checkAll(config, akubra, user.getId(), log, candidate, selectedPids),
                token -> CesnetTokenStore.getInstance().validate(config, user.getId(), token),
                pid -> PreservationService.cesnet(config, user.getId()).updateCatalog(pid));
    }

    CesnetCheckProcess(BatchManager manager, UserProfile user, Checker checker,
                       CesnetProcess.TokenValidator validator, CatalogUpdater catalogUpdater) {
        this.manager = manager;
        this.user = user;
        this.checker = checker;
        this.validator = validator;
        this.catalogUpdater = catalogUpdater;
    }

    public Batch plan() throws IOException {
        if (user == null || !user.hasAllObjectsFunction())
            throw new IOException("Přístup odepřen: je vyžadováno oprávnění allObjectsFunction");
        return manager.add("CESNET check", user, Batch.EXTERNAL_CESNET_CHECK, Batch.State.EXTERNAL_PLANNED,
                false, Batch.PRIORITY_MEDIUM, new BatchParams(List.of()));
    }

    public Batch run(Batch batch, CesnetQueue.Submitter submitter) {
        List<String> catalogPids = new ArrayList<>();
        boolean[] failed = {false};
        batch.setLog("Kontrola archivace v CESNETu byla zahájena.");
        manager.update(batch);
        try {
            requireActive(batch);
            List<String> pids = batch.getParamsAsObject().getPids();
            if (user == null || (!user.hasAllObjectsFunction() && (pids == null || pids.isEmpty()))) {
                throw new IOException("Přístup odepřen: je vyžadováno oprávnění allObjectsFunction");
            }
            validator.validate(null);
            int count = checker.check(message -> {
                if (message.startsWith("CHYBA:")) failed[0] = true;
                append(batch, message);
            }, catalogPids::add);
            requireActive(batch);
            append(batch, "Kontrola archivace v CESNETu byla dokončena. Počet zkontrolovaných objektů: " + count + ".");
        } catch (Exception ex) {
            failed[0] = true;
            append(batch, "CHYBA: " + ex.getMessage());
        }
        try {
            requireActive(batch);
            if (!catalogPids.isEmpty()) {
                BatchParams params = new BatchParams(catalogPids);
                params.setSourceBatchId(batch.getId());
                Batch next = manager.add(catalogPids.get(0), user, Batch.EXTERNAL_CESNET_CATALOG,
                        Batch.State.EXTERNAL_PLANNED, false, batch.getPriority(), params);
                BatchParams checkParams = batch.getParamsAsObject();
                checkParams.setNextBatchId(next.getId());
                batch.setParamsFromObject(checkParams);
                append(batch, "Proces aktualizace katalogu: " + next.getId());
                try {
                    submitter.enqueue(next);
                } catch (Exception ex) {
                    BatchUtils.finishedExternalWithError(manager, next, next.getFolder(), ex);
                    throw ex;
                }
            }
        } catch (Exception ex) {
            failed[0] = true;
            append(batch, "CHYBA: Aktualizaci katalogu se nepodařilo naplánovat: " + ex.getMessage());
        }
        return finish(batch, failed[0]);
    }

    public Batch updateCatalog(Batch batch) {
        batch.setLog("Aktualizace katalogu podle archivace v CESNETu byla zahájena.");
        manager.update(batch);
        boolean failed = false;
        try {
            if (user == null || !user.hasImportToCatalogFunction()) {
                throw new IOException("Přístup odepřen: je vyžadováno oprávnění importToCatalogFunction");
            }
            for (String pid : batch.getParamsAsObject().getPids()) {
                requireActive(batch);
                try {
                    catalogUpdater.update(pid);
                    append(batch, pid + ": synchronizace katalogu byla dokončena.");
                } catch (Exception ex) {
                    failed = true;
                    append(batch, "CHYBA: " + pid + ": " + ex.getMessage());
                }
            }
        } catch (Exception ex) {
            failed = true;
            append(batch, "CHYBA: " + ex.getMessage());
        }
        return finish(batch, failed);
    }

    private void requireActive(Batch batch) throws InterruptedIOException {
        Batch current = manager.get(batch.getId());
        if (Thread.currentThread().isInterrupted() || (current != null && current.getState() == Batch.State.STOPPED)) {
            throw new InterruptedIOException("Proces CESNET byl zastaven");
        }
    }

    private void append(Batch batch, String message) {
        batch.setLog(batch.getLog() + "\n" + message);
        Batch current = manager.get(batch.getId());
        if (current != null && current.getState() == Batch.State.STOPPED) batch.setState(Batch.State.STOPPED);
        manager.update(batch);
    }

    private Batch finish(Batch batch, boolean failed) {
        Batch current = manager.get(batch.getId());
        if (current != null && current.getState() == Batch.State.STOPPED) return current;
        return BatchUtils.finishedSuccessfully(manager, batch, batch.getFolder(), batch.getLog(),
                failed ? Batch.State.EXTERNAL_FAILED : Batch.State.EXTERNAL_DONE);
    }
}
