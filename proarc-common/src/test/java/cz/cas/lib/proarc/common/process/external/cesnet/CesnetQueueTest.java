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
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenException;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.junit.jupiter.api.Assertions.*;

class CesnetQueueTest {
    @TempDir Path temp;
    AppConfiguration config;
    MemoryManager manager;
    UserProfile operator;

    @BeforeEach void setup() throws Exception {
        Files.writeString(temp.resolve("proarc.cfg"), "");
        config = AppConfigurationFactory.getInstance().create(new HashMap<>(Map.of(AppConfiguration.PROPERTY_APP_HOME, temp.toString())));
        manager = new MemoryManager(config);
        operator = user(99);
        operator.setAllObjectsFunction(true);
        operator.setImportToCatalogFunction(true);
    }

    static UserProfile user(int id) { UserProfile user = new UserProfile(); user.setId(id); return user; }

    Batch waiting(int userId) throws IOException {
        BatchParams params = new BatchParams(List.of("uuid:" + userId));
        params.setCesnetOperationId("operation-" + userId);
        params.setCompletedUploads(new ArrayList<>(List.of("completed-" + userId)));
        Batch batch = manager.add("uuid:" + userId, user(userId), Batch.EXTERNAL_CESNET,
                Batch.State.WAITING_CESNET_TOKEN, false, Batch.PRIORITY_MEDIUM, params);
        batch.setFolder(temp.toString());
        return batch;
    }

    @Test void oneValidatedTokenReleasesAllOwnersWithoutChangingOwnershipOrCheckpointsAndCannotReleaseTwice() throws Exception {
        Batch first = waiting(1), second = waiting(2);
        AtomicInteger validations = new AtomicInteger();
        CesnetQueue queue = new CesnetQueue(manager, token -> {
            assertEquals("secret-token", token);
            validations.incrementAndGet();
        });
        List<Integer> enqueued = new ArrayList<>();
        var result = queue.resume(operator, "secret-token", batch -> enqueued.add(batch.getId()));
        assertEquals(List.of(first.getId(), second.getId()), result.resumedBatchIds());
        assertEquals(result.resumedBatchIds(), enqueued);
        assertTrue(result.failures().isEmpty());
        assertEquals(1, validations.get());
        for (Batch batch : List.of(first, second)) {
            BatchParams params = batch.getParamsAsObject();
            assertEquals(Batch.State.EXTERNAL_PLANNED, batch.getState());
            assertEquals(99, params.getCesnetCredentialOwnerId());
            assertEquals("operation-" + batch.getUserId(), params.getCesnetOperationId());
            assertEquals(List.of("completed-" + batch.getUserId()), params.getCompletedUploads());
            assertFalse(batch.getParams().contains("secret-token"));
        }
        assertEquals(1, first.getUserId());
        assertEquals(2, second.getUserId());
        assertTrue(queue.resume(operator, "secret-token", batch -> fail("Duplicate enqueue")).resumedBatchIds().isEmpty());
    }

    @Test void unauthorizedUserCannotSupplyCredentialsOrChangeAnyBatch() throws Exception {
        Batch batch = waiting(1);
        CesnetQueue queue = new CesnetQueue(manager, token -> fail("Must check permission first"));
        assertThrows(IOException.class, () -> queue.resume(user(1), "secret", b -> fail("Must not enqueue")));
        assertEquals(Batch.State.WAITING_CESNET_TOKEN, batch.getState());
    }

    @Test void invalidTokenOrConnectionFailureDoesNotReleaseAnyWork() throws Exception {
        Batch batch = waiting(1);
        for (IOException failure : List.of(new CesnetTokenException(CesnetTokenException.Reason.REJECTED),
                new IOException("Connection failed"))) {
            CesnetQueue queue = new CesnetQueue(manager, token -> { throw failure; });
            assertThrows(IOException.class, () -> queue.resume(operator, "secret", b -> fail("Must not enqueue")));
            assertEquals(Batch.State.WAITING_CESNET_TOKEN, batch.getState());
            assertNull(batch.getParamsAsObject().getCesnetCredentialOwnerId());
        }
    }

    @Test void enqueueFailureRestoresWaitingAndDoesNotPreventOtherBatchesFromContinuing() throws Exception {
        Batch first = waiting(1), second = waiting(2);
        CesnetQueue queue = new CesnetQueue(manager, token -> { });
        var result = queue.resume(operator, "secret", batch -> {
            if (batch.getId().equals(first.getId())) throw new IOException("secret callback data");
        });
        assertEquals(List.of(second.getId()), result.resumedBatchIds());
        assertEquals(first.getId().intValue(), result.failures().get(0).batchId());
        assertFalse(result.failures().get(0).message().contains("secret callback data"));
        assertEquals(Batch.State.WAITING_CESNET_TOKEN, first.getState());
        assertNull(first.getParamsAsObject().getCesnetCredentialOwnerId());
        assertEquals(Batch.State.EXTERNAL_PLANNED, second.getState());
    }

    @Test void partiallyUploadedBatchWaitsAndResumesSameOperationUsingOperatorsTokenOnlyForRemainingArchives() throws Exception {
        UserProfile owner = user(1);
        Path first = Files.writeString(temp.resolve("archive_a.zip"), "archive-a");
        Path second = Files.writeString(temp.resolve("archive_b.zip"), "archive-b");
        BatchParams sourceParams = new BatchParams(List.of("uuid:a", "uuid:b"));
        sourceParams.setBagitArchives(List.of(first.toString(), second.toString()));
        Batch source = manager.add("uuid:a", owner, Batch.EXTERNAL_BAGIT, Batch.State.EXTERNAL_DONE,
                false, Batch.PRIORITY_MEDIUM, sourceParams);
        source.setFolder(temp.toString());
        Map<String, String> operations = new HashMap<>();
        CesnetProcess initial = new CesnetProcess(config, manager, owner, (archive, operation, pid, confirmed, credentials) -> {
            assertEquals(1, credentials);
            operations.put(archive.getName(), operation);
            if (archive.getName().equals(second.getFileName().toString())) {
                throw new CesnetTokenException(CesnetTokenException.Reason.REJECTED);
            }
        }, pid -> null);
        Batch batch = initial.plan(source, null);
        initial.run(batch, batch.getParamsAsObject());
        assertEquals(Batch.State.WAITING_CESNET_TOKEN, batch.getState());
        assertEquals(List.of(first.toString()), batch.getParamsAsObject().getCompletedUploads());
        String operation = batch.getParamsAsObject().getCesnetOperationId();
        new CesnetQueue(manager, token -> { }).resume(operator, "secret", b -> { });
        List<String> uploaded = new ArrayList<>();
        CesnetProcess resumed = new CesnetProcess(config, manager, owner, (archive, op, pid, confirmed, credentials) -> {
            assertEquals(99, credentials);
            assertEquals(operations.get(archive.getName()), op);
            uploaded.add(archive.toString());
        }, pid -> null);
        resumed.run(batch, batch.getParamsAsObject());
        assertEquals(Batch.State.EXTERNAL_DONE, batch.getState());
        assertEquals(List.of(second.toString()), uploaded);
        assertEquals(operation, batch.getParamsAsObject().getCesnetOperationId());
        assertEquals(1, batch.getUserId());
        assertTrue(Files.exists(first));
        assertTrue(Files.exists(second));
    }

    @Test void everyTokenFailureReturnsToWaitingButOtherIoErrorsStillFailNormally() throws Exception {
        Path archive = Files.writeString(temp.resolve("archive_unit.zip"), "archive");
        BatchParams sourceParams = new BatchParams(List.of("uuid:unit"));
        sourceParams.setBagitArchives(List.of(archive.toString()));
        Batch source = manager.add("uuid:unit", user(1), Batch.EXTERNAL_BAGIT, Batch.State.EXTERNAL_DONE,
                false, Batch.PRIORITY_MEDIUM, sourceParams);
        source.setFolder(temp.toString());
        for (CesnetTokenException.Reason reason : CesnetTokenException.Reason.values()) {
            CesnetProcess stage = new CesnetProcess(config, manager, user(1), (file, operation, pid, confirmed, credentials) -> {
                throw new CesnetTokenException(reason);
            }, pid -> null);
            Batch batch = stage.plan(source, null);
            stage.run(batch, batch.getParamsAsObject());
            assertEquals(Batch.State.WAITING_CESNET_TOKEN, batch.getState());
            assertNull(batch.getParamsAsObject().getLtpCesnetToken());
            assertTrue(Files.exists(archive));
        }
        CesnetProcess networkFailure = new CesnetProcess(config, manager, user(1), (file, operation, pid, confirmed, credentials) -> {
            throw new IOException("Network failure");
        }, pid -> null);
        Batch failed = networkFailure.plan(source, null);
        assertThrows(IOException.class, () -> networkFailure.run(failed, failed.getParamsAsObject()));
        assertNotEquals(Batch.State.WAITING_CESNET_TOKEN, failed.getState());
    }

    @Test void cancelledOrFailedBatchesAreNotReleasedByTokenSubmission() throws Exception {
        Batch cancelled = waiting(1), failed = waiting(2);
        cancelled.setState(Batch.State.STOPPED);
        failed.setState(Batch.State.EXTERNAL_FAILED);
        var result = new CesnetQueue(manager, token -> { }).resume(operator, "secret", b -> fail("Not waiting"));
        assertTrue(result.resumedBatchIds().isEmpty());
        assertEquals(Batch.State.STOPPED, cancelled.getState());
        assertEquals(Batch.State.EXTERNAL_FAILED, failed.getState());
    }

    @Test void simultaneousResumeCallsDoNotEnqueueTheSameBatchTwice() throws Exception {
        Batch first = waiting(1), second = waiting(2);
        var calls = new java.util.concurrent.ConcurrentLinkedQueue<Integer>();
        var start = new java.util.concurrent.CountDownLatch(1);
        CesnetQueue queue = new CesnetQueue(manager, token -> { });
        try (var executor = java.util.concurrent.Executors.newFixedThreadPool(2)) {
            java.util.concurrent.Callable<CesnetQueue.Result> resume = () -> {
                start.await();
                return queue.resume(operator, "secret", b -> calls.add(b.getId()));
            };
            var a = executor.submit(resume);
            var b = executor.submit(resume);
            start.countDown();
            var firstResult = a.get(5, java.util.concurrent.TimeUnit.SECONDS);
            var secondResult = b.get(5, java.util.concurrent.TimeUnit.SECONDS);
            assertEquals(2, firstResult.resumedBatchIds().size() + secondResult.resumedBatchIds().size());
            assertEquals(List.of(first.getId(), second.getId()), new ArrayList<>(calls));
        }
    }

    @Test void failedClaimDoesNotRollBackAnotherWorkersClaim() throws Exception {
        var rejectNext = new java.util.concurrent.atomic.AtomicBoolean();
        manager = new MemoryManager(config) {
            @Override public Batch update(Batch batch) {
                if (rejectNext.compareAndSet(true, false)) {
                    BatchParams winner = batch.getParamsAsObject();
                    winner.setCesnetCredentialOwnerId(42);
                    batch.setParamsFromObject(winner);
                    super.update(batch);
                    throw new IllegalStateException("Concurrent update");
                }
                return super.update(batch);
            }
        };
        Batch batch = waiting(1);
        rejectNext.set(true);
        var result = new CesnetQueue(manager, token -> { }).resume(operator, "secret", b -> fail("Claim failed"));
        assertTrue(result.resumedBatchIds().isEmpty());
        assertEquals(1, result.failures().size());
        assertEquals(Batch.State.EXTERNAL_PLANNED, batch.getState());
        assertEquals(42, batch.getParamsAsObject().getCesnetCredentialOwnerId());
    }

    private static class MemoryManager extends BatchManager {
        final Map<Integer, Batch> batches = new LinkedHashMap<>();
        MemoryManager(AppConfiguration config) {
            super(config, (DaoFactory) Proxy.newProxyInstance(DaoFactory.class.getClassLoader(), new Class<?>[]{DaoFactory.class},
                    (proxy, method, args) -> { throw new AssertionError("Unexpected database access"); }));
        }
        @Override public Batch update(Batch batch) {
            if (batch.getId() == null) batch.setId(batches.size() + 1);
            batches.put(batch.getId(), batch);
            return batch;
        }
        @Override public Batch get(int id) { return batches.get(id); }
        @Override public List<Batch> findWaitingCesnetBatches() {
            return batches.values().stream().filter(b -> b.getState() == Batch.State.WAITING_CESNET_TOKEN
                    && Batch.EXTERNAL_CESNET.equals(b.getProfileId())).toList();
        }
    }
}
