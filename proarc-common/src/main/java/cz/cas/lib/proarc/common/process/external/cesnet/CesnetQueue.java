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
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenStore;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

/** Explicit, privileged release of waiting uploads, retaining their original owners and checkpoints. */
public final class CesnetQueue {
    private static final Object RESUME_LOCK = new Object();
    @FunctionalInterface public interface Submitter { void enqueue(Batch batch) throws IOException; }
    public record Failure(int batchId, String message) { }
    public record Result(List<Integer> resumedBatchIds, List<Failure> failures) { }

    private final BatchManager manager;
    private final CesnetProcess.TokenValidator validator;

    public CesnetQueue(AppConfiguration config, BatchManager manager, UserProfile operator) {
        this(manager, token -> CesnetTokenStore.getInstance().validate(config, operator.getId(), token));
    }

    CesnetQueue(BatchManager manager, CesnetProcess.TokenValidator validator) {
        this.manager = manager;
        this.validator = validator;
    }

    public static boolean mayResume(UserProfile operator) {
        return operator != null && operator.getId() != null
                && operator.hasAllObjectsFunction() && operator.hasImportToCatalogFunction();
    }

    public Result resume(UserProfile operator, String token, Submitter submitter) throws IOException {
        if (!mayResume(operator)) throw new IOException("Access denied: CESNET queue operator permission required");
        // Validate once before changing any batch; neither a bad token nor a network failure releases work.
        validator.validate(token);
        List<Integer> resumed = new ArrayList<>();
        List<Failure> failures = new ArrayList<>();
        synchronized (RESUME_LOCK) {
            for (Batch candidate : manager.findWaitingCesnetBatches()) {
                Batch batch = manager.get(candidate.getId());
                if (batch == null || !Batch.EXTERNAL_CESNET.equals(batch.getProfileId())
                        || batch.getState() != Batch.State.WAITING_CESNET_TOKEN) continue;
                BatchParams params = batch.getParamsAsObject();
                if (params == null) {
                    failures.add(new Failure(batch.getId(), "CESNET batch has no parameters"));
                    continue;
                }
                Integer previousCredentialOwner = params.getCesnetCredentialOwnerId();
                boolean claimed = false;
                try {
                    params.setCesnetCredentialOwnerId(operator.getId());
                    batch.setParamsFromObject(params);
                    batch.setState(Batch.State.EXTERNAL_PLANNED);
                    batch.setUpdated(new Timestamp(System.currentTimeMillis()));
                    batch.setLog("CESNET upload released by user " + operator.getId());
                    manager.update(batch);
                    claimed = true;
                    submitter.enqueue(batch);
                    resumed.add(batch.getId());
                } catch (Exception ex) {
                    // Restore waiting only when work has not started in another worker meanwhile.
                    Batch current = manager.get(batch.getId());
                    if (claimed && current != null && current.getState() == Batch.State.EXTERNAL_PLANNED) {
                        BatchParams currentParams = current.getParamsAsObject();
                        currentParams.setCesnetCredentialOwnerId(previousCredentialOwner);
                        current.setParamsFromObject(currentParams);
                        current.setState(Batch.State.WAITING_CESNET_TOKEN);
                        current.setLog("Čeká na token CESNET. Upload se nepodařilo zařadit do fronty.");
                        manager.update(current);
                    }
                    // Do not expose callback exception text, which may contain request data.
                    failures.add(new Failure(batch.getId(), "CESNET upload could not be scheduled; retry resume"));
                }
            }
        }
        return new Result(List.copyOf(resumed), List.copyOf(failures));
    }
}
