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
package cz.cas.lib.proarc.common.process.external;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo;
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenException;
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.FolderUploadRepository;
import cz.cas.lib.proarc.common.process.external.bagit.BagitProcess;
import cz.cas.lib.proarc.common.process.external.cesnet.CesnetProcess;
import cz.cas.lib.proarc.common.process.external.bagit.MissingSourceDirectoryException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.InternalExternalProcess;
import cz.cas.lib.proarc.common.process.InternalExternalDispatcher;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.junit.jupiter.api.Assertions.*;

class PackageProcessesTest {
    @TempDir Path temp;
    AppConfiguration config;
    MemoryManager manager;
    UserProfile user;

    @BeforeEach
    void setup() throws Exception {
        Path home = Files.createDirectory(temp.resolve("config"));
        Files.writeString(home.resolve(AppConfiguration.CONFIG_FILE_NAME), "");
        config = AppConfigurationFactory.getInstance().create(new HashMap<>(Map.of(AppConfiguration.PROPERTY_APP_HOME, home.toString())));
        user = new UserProfile();
        user.setId(7);
        user.setUserHome(temp.toUri());
        manager = new MemoryManager(config);
    }

    @Test
    void externalRunnerUsesExternalStatesAndPreservesFinishedExport() throws Exception {
        Batch source = export();
        Batch bagit = new BagitProcess(config, manager, user).plan(source);
        assertEquals(Batch.State.EXTERNAL_PLANNED, bagit.getState());
        assertEquals("externalProfile.bagit", bagit.getProfileId());
        Batch result = InternalExternalProcess.prepare(config, null, bagit, manager, user, "test", java.util.Locale.ROOT).start();
        assertEquals(Batch.State.EXTERNAL_DONE, result.getState());
        assertTrue(manager.states.contains(Batch.State.EXTERNAL_RUNNING));
        assertFalse(manager.states.contains(Batch.State.EXPORTING));
        assertEquals(Batch.State.EXPORT_DONE, source.getState());
        Batch upload = uploadProcess().plan(result, "fresh");
        assertEquals("externalProfile.cesnet", upload.getProfileId());
        assertEquals(Batch.State.EXTERNAL_PLANNED, upload.getState());
    }

    @Test
    void externalRunnerSchedulesCesnetAfterBagitAndWaitsForMissingToken() throws Exception {
        Batch source = export();
        BatchParams params = source.getParamsAsObject();
        params.setLtpCesnet(true);
        params.setConfirmCesnetUpload(true);
        source.setParamsFromObject(params);
        Batch bagit = new BagitProcess(config, manager, user).plan(source);
        BatchParams bagitParams = bagit.getParamsAsObject();
        bagitParams.setLtpCesnet(true);
        bagitParams.setConfirmCesnetUpload(true);
        bagit.setParamsFromObject(bagitParams);
        InternalExternalDispatcher previous = InternalExternalDispatcher.getDefault();
        InternalExternalDispatcher dispatcher = new InternalExternalDispatcher();
        dispatcher.init();
        InternalExternalDispatcher.setDefault(dispatcher);
        try {
            Batch result = InternalExternalProcess.prepare(config, null, bagit, manager, user,
                    "test", java.util.Locale.ROOT).start();
            dispatcher.stop(5, java.util.concurrent.TimeUnit.SECONDS);

            assertEquals(Batch.State.EXTERNAL_DONE, result.getState());
            Integer nextId = result.getParamsAsObject().getNextBatchId();
            assertNotNull(nextId);
            Batch upload = manager.get(nextId);
            assertEquals(Batch.EXTERNAL_CESNET, upload.getProfileId());
            assertEquals(bagit.getId(), upload.getParamsAsObject().getSourceBatchId());
            assertEquals(Batch.State.WAITING_CESNET_TOKEN, upload.getState());
            assertTrue(result.getLog().contains("Navazující proces: " + Batch.EXTERNAL_CESNET + ", dávka " + nextId));
            assertEquals(bagit.getParamsAsObject().getBagitArchives(), upload.getParamsAsObject().getBagitArchives());
            assertFalse(Files.exists(Path.of(source.getFolder())));
            for (String archive : upload.getParamsAsObject().getBagitArchives()) {
                assertTrue(Files.isRegularFile(Path.of(archive)));
            }
        } finally {
            dispatcher.stop(5, java.util.concurrent.TimeUnit.SECONDS);
            InternalExternalDispatcher.setDefault(previous);
        }
    }

    @Test
    void externalRunnerReportsExternalFailureAndSkipsCancelledProcess() throws Exception {
        Batch source = export();
        Batch bagit = new BagitProcess(config, manager, user).plan(source);
        source.setFolder(temp.resolve("missing-export").toString());
        Batch result = InternalExternalProcess.prepare(config, null, bagit, manager, user, "test", java.util.Locale.ROOT).start();
        assertEquals(Batch.State.EXTERNAL_FAILED, result.getState());
        String expectedMessage = "Zdrojová složka " + Path.of(source.getFolder()).toAbsolutePath().normalize() + " již neexistuje.";
        assertEquals(expectedMessage, result.getLog());
        var error = assertThrows(MissingSourceDirectoryException.class,
                () -> new BagitProcess(config, manager, user).plan(source));
        assertEquals(expectedMessage, error.getMessage());
        assertEquals(Batch.State.EXPORT_DONE, source.getState());
        bagit.setState(Batch.State.STOPPED);
        result = InternalExternalProcess.prepare(config, null, bagit, manager, user, "test", java.util.Locale.ROOT).start();
        assertEquals(Batch.State.STOPPED, result.getState());
    }

    @Test
    void startupMarksInterruptedExternalProcessesFailedWithoutDeletingArchives() throws Exception {
        Batch source = export();
        Batch bagit = new BagitProcess(config, manager, user).plan(source);
        Path retained = Files.writeString(temp.resolve("retained.zip"), "keep");
        bagit.setFolder(retained.getParent().toString());
        bagit.setState(Batch.State.EXTERNAL_RUNNING);
        BatchUtils.finishedInternalExternalRunningBatch(manager, config);
        assertEquals(Batch.State.EXTERNAL_FAILED, bagit.getState());
        assertEquals(Batch.State.EXPORT_DONE, source.getState());
        assertEquals("keep", Files.readString(retained));
    }

    @Test
    void retryUploadsOnlyRemainingArchiveAndKeepsOperationIdAfterSourceExportIsDeleted() throws Exception {
        Batch source = export();
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(source);
        packaging.run(bagit, bagit.getParamsAsObject());
        assertEquals(Batch.State.EXPORT_DONE, source.getState());
        assertEquals(Batch.State.EXTERNAL_DONE, bagit.getState());
        List<String> archives = bagit.getParamsAsObject().getBagitArchives();
        assertEquals(2, archives.size());
        assertFalse(Files.exists(Path.of(source.getFolder())));
        assertEquals(Boolean.TRUE, bagit.getParamsAsObject().getBagitComplete());
        List<String> calls = new ArrayList<>();
        java.util.concurrent.atomic.AtomicBoolean fail = new java.util.concurrent.atomic.AtomicBoolean(true);
        Map<String, String> operations = new HashMap<>();
        CesnetProcess uploading = new CesnetProcess(config, manager, user, (file, operation, pid, confirmed, credentialOwnerId) -> {
            calls.add(file.getAbsolutePath());
            String previous = operations.put(file.getAbsolutePath(), operation);
            if (previous != null) assertEquals(previous, operation);
            assertEquals(file.getName().equals("archive_a.zip") ? "uuid:a" : "uuid:b", pid);
            if (file.getAbsolutePath().equals(archives.get(1)) && fail.getAndSet(false)) throw new IOException("Connection lost");
        }, pid -> null);
        Batch failed = uploading.plan(bagit, "expired");
        IOException error = assertThrows(IOException.class, () -> uploading.run(failed, failed.getParamsAsObject()));
        BatchUtils.finishedExternalWithError(manager, failed, failed.getFolder(), error);
        assertEquals(List.of(archives.get(0)), failed.getParamsAsObject().getCompletedUploads());
        assertNull(failed.getParamsAsObject().getLtpCesnetToken());
        Batch retry = uploading.plan(failed, "fresh");
        assertNotEquals(failed.getId(), retry.getId());
        uploading.run(retry, retry.getParamsAsObject());

        assertEquals(List.of(archives.get(0), archives.get(1), archives.get(1)), calls);
        assertEquals(Batch.State.EXTERNAL_DONE, retry.getState());
        assertNull(retry.getParamsAsObject().getLtpCesnetToken());
        for (String archive : archives) assertFalse(Files.exists(Path.of(archive)));
        assertFalse(Files.exists(Path.of(bagit.getFolder())));
        assertFalse(Files.exists(Path.of(source.getFolder())));
        assertEquals(archives, bagit.getParamsAsObject().getBagitArchives());
    }

    @Test
    void exportIncludesPackagingInSameBatchAndDoesNotRepeatIt() throws Exception {
        Batch source = export();
        BatchParams params = source.getParamsAsObject();
        params.setBagit(true);
        params.setLtpCesnet(true);
        params.setLtpCesnetToken("initial-token");
        params.setConfirmCesnetUpload(true);
        source.setParamsFromObject(params);
        BagitProcess packaging = new BagitProcess(config, manager, user);
        CesnetProcess uploading = uploadProcess();
        source.setState(Batch.State.EXPORTING);
        Path original = Path.of(source.getFolder());
        Batch bagit = packaging.runInExport(source);
        assertSame(source, bagit);
        assertEquals(1, manager.batches.size());
        assertEquals(Batch.EXPORT_ARCHIVE, bagit.getProfileId());
        assertEquals(Batch.State.EXPORT_DONE, bagit.getState());
        assertFalse(Files.exists(original));
        assertNull(bagit.getParamsAsObject().getNextBatchId());
        List<String> archives = bagit.getParamsAsObject().getBagitArchives();
        packaging.runInExport(bagit);
        assertEquals(archives, bagit.getParamsAsObject().getBagitArchives());
        Batch upload = uploading.planAfterBagit(bagit);
        assertEquals(Batch.EXTERNAL_CESNET, upload.getProfileId());
        assertEquals(bagit.getId(), upload.getParamsAsObject().getSourceBatchId());
        assertNull(upload.getParamsAsObject().getLtpCesnetToken());
        assertNull(source.getParamsAsObject().getLtpCesnetToken());
        assertNull(bagit.getParamsAsObject().getLtpCesnetToken());
        assertEquals(Boolean.TRUE, bagit.getParamsAsObject().getConfirmCesnetUpload());
        assertEquals(Boolean.TRUE, upload.getParamsAsObject().getConfirmCesnetUpload());
    }

    @Test
    void failedPackagingRetainsSourceScansAndCompletedArchivesForRetry() throws Exception {
        Batch source = export();
        Path sourceRoot = Path.of(source.getFolder());
        Path scans = Files.createDirectory(temp.resolve("raw"));
        Files.writeString(scans.resolve("scan"), "raw");
        BatchParams sourceParams = source.getParamsAsObject();
        sourceParams.setBagit(true);
        sourceParams.setRawScanFolders(List.of(scans.toString()));
        source.setParamsFromObject(sourceParams);
        Path invalid = Files.writeString(Path.of(source.getFolder(), "b/bagit.txt"), "already bagged");
        BagitProcess packaging = new BagitProcess(config, manager, user);
        source.setState(Batch.State.EXPORTING);
        Batch bagit = source;
        assertThrows(IOException.class, () -> packaging.runInExport(bagit));
        assertEquals(Batch.State.EXPORTING, bagit.getState());
        assertEquals(1, manager.batches.size());
        List<String> completed = bagit.getParamsAsObject().getBagitArchives();
        assertEquals(1, completed.size());
        var modified = Files.getLastModifiedTime(Path.of(completed.get(0)));
        assertTrue(Files.exists(scans.resolve("scan")));
        assertTrue(Files.exists(sourceRoot.resolve("a/scan.tif")));
        Files.delete(invalid);
        packaging.runInExport(bagit);
        assertEquals(Batch.State.EXPORT_DONE, bagit.getState());
        assertFalse(Files.exists(sourceRoot));
        assertEquals(2, bagit.getParamsAsObject().getBagitArchives().size());
        assertEquals(modified, Files.getLastModifiedTime(Path.of(completed.get(0))));
        assertFalse(Files.exists(scans));
    }

    @Test
    void manualBagitDoesNotUploadImplicitlyAndFoxmlIsPackedAsOneWholeExport() throws Exception {
        Batch source = export();
        source.setProfileId(Batch.EXPORT_KRAMERIUS);
        BatchParams params = source.getParamsAsObject();
        params.setLtpCesnet(true);
        params.setLtpCesnetToken("old-token");
        source.setParamsFromObject(params);
        BagitProcess packaging = new BagitProcess(config, manager, user);
        CesnetProcess uploading = uploadProcess();
        Batch bagit = packaging.plan(source);
        packaging.run(bagit, bagit.getParamsAsObject());
        assertEquals(1, bagit.getParamsAsObject().getBagitArchives().size());
        assertNull(uploading.planAfterBagit(bagit));
        try (var zip = new java.util.zip.ZipFile(bagit.getParamsAsObject().getBagitArchives().get(0))) {
            assertEquals(2, zip.stream().filter(entry -> entry.getName().endsWith("/scan.tif")).count());
        }
        bagit.setNightOnly(true);
        assertFalse(uploading.plan(bagit, "fresh-token").isNightOnly());
    }

    @Test
    void configuredAuthenticationDoesNotRequireTokenToChainUpload() throws Exception {
        Batch source = export();
        BatchParams params = source.getParamsAsObject();
        params.setLtpCesnet(true);
        source.setParamsFromObject(params);
        BagitProcess packaging = new BagitProcess(config, manager, user);
        CesnetProcess uploading = uploadProcess();
        Batch bagit = packaging.runInExport(source);
        assertNotNull(uploading.planAfterBagit(bagit));
        assertEquals(Batch.State.EXPORT_DONE, bagit.getState());
        assertNotNull(uploading.plan(bagit, "new-token"));
    }

    @Test
    void ownershipStateAndMissingArtifactsAreCheckedBeforeScheduling() throws Exception {
        Batch source = export();
        BagitProcess packaging = new BagitProcess(config, manager, user);
        CesnetProcess uploading = uploadProcess();
        source.setUserId(8);
        assertThrows(IOException.class, () -> packaging.plan(source));
        source.setUserId(7);
        source.setState(Batch.State.EXPORTING);
        assertThrows(IOException.class, () -> packaging.plan(source));
        source.setState(Batch.State.EXPORT_DONE);
        assertThrows(IOException.class, () -> uploading.plan(source, "token"));
        Batch bagit = packaging.plan(source);
        packaging.run(bagit, bagit.getParamsAsObject());
        assertNotNull(uploading.plan(bagit, ""));
        Files.delete(Path.of(bagit.getParamsAsObject().getBagitArchives().get(0)));
        assertThrows(IOException.class, () -> uploading.plan(bagit, "token"));
        assertFalse(Files.exists(Path.of(source.getFolder())));
    }

    @Test
    void destinationInsideSourceIsRejectedBeforePackaging() throws Exception {
        Batch source = export();
        Path root = Path.of(source.getFolder());
        // A misconfigured destination inside the source must never be deleted with it.
        Files.writeString(config.getConfigHome().toPath().resolve(AppConfiguration.CONFIG_FILE_NAME),
                "export.bagit.path=" + root.toString().replace('\\', '/') + "\n");
        AppConfiguration nested = AppConfigurationFactory.getInstance().create(new HashMap<>(
                Map.of(AppConfiguration.PROPERTY_APP_HOME, config.getConfigHome().toString())));
        BagitProcess packaging = new BagitProcess(nested, manager, user);
        CesnetProcess uploading = uploadProcess();
        Batch bagit = packaging.plan(source);
        assertThrows(IOException.class, () -> packaging.run(bagit, bagit.getParamsAsObject()));
        assertTrue(Files.exists(root.resolve("a/scan.tif")));
        assertTrue(Files.exists(root.resolve("b/scan.tif")));
        assertNull(bagit.getParamsAsObject().getBagitComplete());
        assertThrows(IOException.class, () -> packaging.run(bagit, bagit.getParamsAsObject()));
    }

    @Test
    void usesDedicatedFallbackFolderAndPreservesPreviousExports() throws Exception {
        Batch source = export();
        Path original = Path.of(source.getFolder());
        Path archive = original.resolveSibling("archive_1234");
        Files.move(original, archive);
        source.setFolder(archive.toString());
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(source);
        Path expected = archive.resolveSibling("bagit_1234");
        Files.createDirectory(expected);
        Path existing = Files.writeString(expected.resolve("existing.zip"), "keep");
        Files.writeString(Files.createDirectory(expected.resolve("old-content")).resolve("old.sums"), "old");

        packaging.run(bagit, bagit.getParamsAsObject());
        assertTrue(Files.exists(existing));
        assertTrue(Files.exists(expected.resolve("old-content")));
        expected = config.getConfigHome().toPath().resolve("temp/ltpCesnet/bagit_" + bagit.getId() + "_1234");
        assertEquals(expected.toString(), bagit.getFolder());
        assertFalse(Files.exists(archive));
        assertFalse(Files.exists(Path.of(user.getExportFolder()).resolve("stages")));
        assertFalse(Files.exists(expected.resolve("work")));
        try (var files = Files.list(expected)) {
            assertEquals(java.util.Set.of("archive_a.zip", "archive_a.sums", "archive_b.zip", "archive_b.sums"),
                    files.map(path -> path.getFileName().toString()).collect(java.util.stream.Collectors.toSet()));
        }
        for (String filename : bagit.getParamsAsObject().getBagitArchives()) {
            assertEquals(expected, Path.of(filename).getParent());
        }
    }

    @Test
    void missingSourceDoesNotErasePreviousBagitOutput() throws Exception {
        Batch source = export();
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(source);
        Path root = Path.of(source.getFolder());
        Path output = Files.createDirectory(root.resolveSibling("bagit_" + root.getFileName()));
        Path existing = Files.writeString(output.resolve("existing.zip"), "keep");
        Files.move(root, root.resolveSibling("moved-source"));
        assertThrows(IOException.class, () -> packaging.run(bagit, bagit.getParamsAsObject()));
        assertEquals("keep", Files.readString(existing));
    }

    @Test
    void configuredArchiveDestinationRemainsAvailableAfterWorkCleanup() throws Exception {
        Batch source = export();
        Path destination = Files.createDirectory(temp.resolve("bagit-destination"));
        Files.writeString(config.getConfigHome().toPath().resolve(AppConfiguration.CONFIG_FILE_NAME),
                "export.bagit.path=" + destination.toString().replace('\\', '/') + "\n");
        AppConfiguration relocated = AppConfigurationFactory.getInstance().create(new HashMap<>(
                Map.of(AppConfiguration.PROPERTY_APP_HOME, config.getConfigHome().toString())));
        BagitProcess packaging = new BagitProcess(relocated, manager, user);
        CesnetProcess uploading = uploadProcess();
        Batch bagit = packaging.plan(source);
        packaging.run(bagit, bagit.getParamsAsObject());
        for (String archive : bagit.getParamsAsObject().getBagitArchives()) {
            assertEquals(destination, Path.of(archive).getParent().getParent());
            assertEquals(Path.of(bagit.getFolder()), Path.of(archive).getParent());
            assertTrue(Files.isRegularFile(Path.of(archive)));
            assertTrue(Files.isRegularFile(Path.of(archive.replaceFirst("\\.zip$", ".sums"))));
        }
        assertNotNull(uploading.plan(bagit, "fresh"));
    }

    @Test
    void scansAreDeletedOnlyAfterSeparateBagitProcessCompletes() throws Exception {
        Batch source = export();
        Path scans = Files.createDirectory(temp.resolve("raw-scans"));
        Files.writeString(scans.resolve("scan"), "raw");
        BatchParams params = source.getParamsAsObject();
        params.setRawScanFolders(List.of(scans.toString()));
        source.setParamsFromObject(params);
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(source);
        assertTrue(Files.exists(scans));
        packaging.run(bagit, bagit.getParamsAsObject());
        assertFalse(Files.exists(scans));
        assertFalse(Files.exists(Path.of(source.getFolder())));
    }

    @Test
    void automaticRepeatRetainsCesnetBatchForConfirmedRetryWithoutRepeatingExport() throws Exception {
        Batch source = export();
        BatchParams params = source.getParamsAsObject();
        params.setLtpCesnet(true);
        source.setParamsFromObject(params);
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.runInExport(source);
        List<String> archives = bagit.getParamsAsObject().getBagitArchives();
        var existing = new cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo();
        existing.setPackageId("12");
        existing.setState(cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo.State.STORED);
        List<String> uploaded = new ArrayList<>();
        CesnetProcess cesnet = new CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, credentialOwnerId) -> {
                    assertTrue(confirmed);
                    uploaded.add(file.toString());
                }, pid -> existing);

        Batch blocked = cesnet.planAfterBagit(bagit);
        assertEquals(Batch.State.EXTERNAL_FAILED, blocked.getState());
        assertEquals(Batch.EXTERNAL_CESNET, blocked.getProfileId());
        assertEquals(bagit.getId(), blocked.getParamsAsObject().getSourceBatchId());
        assertEquals(archives, blocked.getParamsAsObject().getBagitArchives());
        assertEquals(Boolean.FALSE, blocked.getParamsAsObject().getConfirmCesnetUpload());
        assertEquals(blocked.getId(), bagit.getParamsAsObject().getNextBatchId());
        assertEquals(bagit.getFolder(), blocked.getFolder());
        assertTrue(blocked.getLog().contains("vyžaduje potvrzení"));
        assertTrue(blocked.getLog().contains("uuid:a"));
        assertTrue(blocked.getLog().contains("uuid:b"));
        assertNull(cesnet.planAfterBagit(bagit));
        assertTrue(uploaded.isEmpty());
        assertThrows(CesnetProcess.ConfirmationRequiredException.class, () -> cesnet.plan(blocked, null));

        Batch retry = cesnet.plan(blocked, null, true);
        assertEquals(blocked.getParamsAsObject().getCesnetOperationId(), retry.getParamsAsObject().getCesnetOperationId());
        cesnet.run(retry, retry.getParamsAsObject());
        assertEquals(Batch.State.EXTERNAL_DONE, retry.getState());
        assertEquals(archives, uploaded);
        assertEquals(Batch.State.EXPORT_DONE, bagit.getState());
        assertFalse(Files.exists(Path.of(source.getFolder())));
        for (String archive : archives) assertFalse(Files.exists(Path.of(archive)));
    }

    @Test
    void repeatRequiresConfirmationAndSuccessfulUploadRequiresNewExportForFurtherUploads() throws Exception {
        Batch source = export();
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(source);
        packaging.run(bagit, bagit.getParamsAsObject());
        var existing = new cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo();
        existing.setPackageId("12");
        List<String> operations = new ArrayList<>();
        var cesnet = new cz.cas.lib.proarc.common.process.external.cesnet.CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, credentialOwnerId) -> {
                    assertTrue(confirmed);
                    operations.add(operation);
                }, pid -> existing);
        for (var state : List.of(cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo.State.STORED,
                cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo.State.UPLOADED)) {
            existing.setState(state);
            int batches = manager.batches.size();
            var error = assertThrows(cz.cas.lib.proarc.common.process.external.cesnet.CesnetProcess.ConfirmationRequiredException.class,
                    () -> cesnet.plan(bagit, null));
            assertEquals(batches, manager.batches.size());
            assertEquals(2, error.getWarnings().size());
            assertEquals(state, error.getWarnings().get(0).state());
            assertEquals("12", error.getWarnings().get(0).packageId());
            assertTrue(error.getWarnings().get(0).message().contains(state ==
                    cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo.State.STORED
                    ? "a zvalidován" : "čeká na zvalidování"));
        }
        Batch first = cesnet.plan(bagit, null, true);
        cesnet.run(first, first.getParamsAsObject());
        // Successful upload removes local packages; another upload requires a new export.
        assertThrows(IOException.class, () -> cesnet.plan(first, null, true));
        assertEquals(2, operations.size());
    }

    @Test
    void rejectedTokenPreventsSchedulingAndFreshTokenNeverReachesBatchParameters() throws Exception {
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(export());
        packaging.run(bagit, bagit.getParamsAsObject());
        int batches = manager.batches.size();
        var rejected = new cz.cas.lib.proarc.common.process.external.cesnet.CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, credentialOwnerId) -> fail("Must not upload"), pid -> null,
                token -> { throw new cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenException(
                        cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenException.Reason.REJECTED); });
        assertThrows(cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenException.class,
                () -> rejected.plan(bagit, "invalid-secret"));
        assertEquals(batches, manager.batches.size());
        for (String archive : bagit.getParamsAsObject().getBagitArchives()) assertTrue(Files.exists(Path.of(archive)));

        var accepted = new cz.cas.lib.proarc.common.process.external.cesnet.CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, credentialOwnerId) -> { }, pid -> null, token -> assertEquals("fresh-secret", token));
        Batch upload = accepted.plan(bagit, "fresh-secret");
        assertNull(upload.getParamsAsObject().getLtpCesnetToken());
        assertFalse(upload.getParams().contains("fresh-secret"));
    }

    @Test
    void exportAndBagitFinishWithoutCredentialsAndUploadWaitsWithoutFailingOrDeletingArchives() throws Exception {
        Batch source = export();
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(source);
        packaging.run(bagit, bagit.getParamsAsObject());
        BatchParams params = bagit.getParamsAsObject();
        params.setLtpCesnet(true);
        params.setConfirmCesnetUpload(true);
        bagit.setParamsFromObject(params);
        CesnetProcess production = new CesnetProcess(config, manager, user);
        Batch upload = production.planAfterBagit(bagit);
        assertNotNull(upload);
        assertEquals(upload.getId(), bagit.getParamsAsObject().getNextBatchId());
        Batch result = InternalExternalProcess.prepare(config, null, upload, manager, user, "test", java.util.Locale.ROOT).start();
        assertEquals(Batch.State.WAITING_CESNET_TOKEN, result.getState());
        assertEquals(Batch.State.EXTERNAL_DONE, bagit.getState());
        assertEquals(Batch.State.EXPORT_DONE, source.getState());
        assertTrue(result.getLog().contains("Čeká na token CESNET"));
        assertNull(result.getParamsAsObject().getLtpCesnetToken());
        for (String archive : params.getBagitArchives()) assertTrue(Files.exists(Path.of(archive)));
        BatchUtils.finishedInternalExternalRunningBatch(manager, config);
        assertEquals(Batch.State.WAITING_CESNET_TOKEN, result.getState());
        assertEquals(Batch.State.WAITING_CESNET_TOKEN,
                InternalExternalProcess.prepare(config, null, upload, manager, user, "test", java.util.Locale.ROOT).start().getState());
        assertNull(production.planAfterBagit(bagit));
    }

    @Test
    void bagitWithoutParametersFailsAsExternalProcessAndRetainsSource() throws Exception {
        Batch source = export();
        Batch bagit = new BagitProcess(config, manager, user).plan(source);
        bagit.setParams(null);

        Batch result = InternalExternalProcess.prepare(config, null, bagit, manager, user,
                "test", java.util.Locale.ROOT).start();

        assertEquals(Batch.State.EXTERNAL_FAILED, result.getState());
        assertTrue(result.getLog().contains("Batch params are null."));
        assertTrue(Files.exists(Path.of(source.getFolder()).resolve("a/scan.tif")));
        assertEquals(Batch.State.EXPORT_DONE, source.getState());
        assertFalse(manager.states.contains(Batch.State.INTERNAL_FAILED));
    }

    @Test
    void urnNamedArchiveUsesObjectPidForBothPlanningAndUpload() throws Exception {
        String objectPid = "uuid:91e24ccc-4292-49a4-9674-95192edef5ee";
        Path folder = Files.createDirectory(temp.resolve("bagit_91e24ccc-4292-49a4-9674-95192edef5ee"));
        Path archive = Files.writeString(folder.resolve("archive_aba007-000k1w.zip"), "archive");
        BatchParams params = new BatchParams(List.of(objectPid));
        params.setBagitArchives(List.of(archive.toString()));
        Batch bagit = manager.add(objectPid, user, Batch.EXTERNAL_BAGIT, Batch.State.EXTERNAL_DONE,
                false, Batch.PRIORITY_MEDIUM, params);
        bagit.setFolder(folder.toString());
        List<String> read = new ArrayList<>();
        List<String> uploaded = new ArrayList<>();
        CesnetProcess cesnet = new CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, credentialOwnerId) -> uploaded.add(pid),
                pid -> { read.add(pid); return null; });

        Batch upload = cesnet.plan(bagit, null);
        cesnet.run(upload, upload.getParamsAsObject());

        assertEquals(List.of(objectPid), read);
        assertEquals(List.of(objectPid), uploaded);
    }

    private CesnetProcess uploadProcess() {
        return new CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, credentialOwnerId) -> { }, pid -> null);
    }

    @org.junit.jupiter.params.ParameterizedTest
    @org.junit.jupiter.params.provider.ValueSource(booleans = {false, true})
    void waitingUploadPersistsDestinationAndSuccessfulResumeCleansOnlyItsBatch(boolean configured) throws Exception {
        Path destination = configured ? temp.resolve("bagit-destination")
                : config.getConfigHome().toPath().resolve("temp/ltpCesnet");
        if (configured) {
            Files.writeString(config.getConfigHome().toPath().resolve(AppConfiguration.CONFIG_FILE_NAME),
                    "export.bagit.path=" + destination.toString().replace('\\', '/') + "\n");
            config = AppConfigurationFactory.getInstance().create(new HashMap<>(
                    Map.of(AppConfiguration.PROPERTY_APP_HOME, config.getConfigHome().toString())));
        }
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(export());
        packaging.run(bagit, bagit.getParamsAsObject());
        Path folder = Path.of(bagit.getFolder());
        assertEquals(destination, folder.getParent());
        Path unrelated = Files.writeString(destination.resolve("other-batch.zip"), "keep");
        CesnetProcess waiting = new CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, owner) -> {
                    throw new cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenException(
                            cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenException.Reason.REJECTED);
                }, pid -> null);
        Batch upload = waiting.plan(bagit, null);
        assertEquals(bagit.getFolder(), upload.getFolder());
        waiting.run(upload, upload.getParamsAsObject());
        assertEquals(Batch.State.WAITING_CESNET_TOKEN, upload.getState());
        for (String archive : upload.getParamsAsObject().getBagitArchives()) {
            assertEquals(folder, Path.of(archive).getParent());
            assertTrue(Files.isRegularFile(Path.of(archive)));
            assertTrue(Files.isRegularFile(Path.of(archive.replaceFirst("\\.zip$", ".sums"))));
        }
        uploadProcess().run(upload, upload.getParamsAsObject());
        assertEquals(Batch.State.EXTERNAL_DONE, upload.getState());
        assertFalse(Files.exists(folder));
        assertEquals("keep", Files.readString(unrelated));
    }

    @Test
    void retryAfterInterruptedCleanupDoesNotUploadCompletedArchivesAgain() throws Exception {
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(export());
        packaging.run(bagit, bagit.getParamsAsObject());
        CesnetProcess uploading = uploadProcess();
        Batch upload = uploading.plan(bagit, null);
        BatchParams params = upload.getParamsAsObject();
        params.setCompletedUploads(params.getBagitArchives());
        upload.setParamsFromObject(params);
        upload.setState(Batch.State.EXTERNAL_FAILED);
        Files.delete(Path.of(params.getBagitArchives().get(0)));
        CesnetProcess cleanup = new CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, owner) -> fail("Already uploaded"), pid -> null);
        Batch retry = cleanup.plan(upload, null);
        cleanup.run(retry, retry.getParamsAsObject());
        assertEquals(Batch.State.EXTERNAL_DONE, retry.getState());
        assertFalse(Files.exists(Path.of(bagit.getFolder())));
    }

    private Batch export() throws IOException {
        Path root = Files.createDirectory(temp.resolve("source-export"));
        for (String name : List.of("a", "b")) {
            Files.writeString(Files.createDirectory(root.resolve(name)).resolve("scan.tif"), name);
        }
        Batch source = manager.add("uuid:test", user, Batch.EXPORT_ARCHIVE, Batch.State.EXPORT_DONE,
                false, Batch.PRIORITY_MEDIUM, new BatchParams(List.of("uuid:test")));
        source.setFolder(root.toString());
        return source;
    }

    private void configureFolderUploads(boolean enabled) throws Exception {
        Files.writeString(config.getConfigHome().toPath().resolve(AppConfiguration.CONFIG_FILE_NAME),
                "export.ltpCesnet.enable=" + enabled + "\n");
        config = AppConfigurationFactory.getInstance().create(new HashMap<>(Map.of(
                AppConfiguration.PROPERTY_APP_HOME, config.getConfigHome().toString())));
    }
    private Path importFolder(String name) throws Exception {
        Path root = Path.of(user.getImportFolder());
        Files.createDirectories(root);
        Path folder = Files.createDirectory(root.resolve(name));
        Files.writeString(folder.resolve("scan.tif"), "original payload");
        configureFolderUploads(true);
        return folder;
    }

    @Test
    void importFoldersHaveIndependentJobsAndKeepTheirPayloads() throws Exception {
        configureFolderUploads(true);
        var process = new BagitProcess(config, manager, user);
        List<Integer> jobs = new ArrayList<>();
        for (int i = 0; i < 20; i++) {
            Path folder = importFolder("folder-" + i);
            Batch batch = process.planImportFolder("folder-" + i, false, false, Batch.PRIORITY_MEDIUM);
            jobs.add(batch.getId());
            assertEquals(folder.toRealPath().toString(), batch.getParamsAsObject().getImportFolder());
        }
        assertEquals(20, new HashSet<>(jobs).size());
        Batch bagit = manager.get(jobs.get(0));
        process.run(bagit, bagit.getParamsAsObject());
        assertEquals("original payload", Files.readString(Path.of(user.getImportFolder()).resolve("folder-0/scan.tif")));
        Batch upload = uploadProcess().planAfterBagit(bagit);
        assertNotNull(upload.getParamsAsObject().getImportFolder());
        List<String> identities = new ArrayList<>();
        var cesnet = new CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, owner) -> identities.add(pid), pid -> null);
        cesnet.run(upload, upload.getParamsAsObject());
        assertEquals(List.of(Path.of(user.getImportFolder()).resolve("folder-0").toRealPath().toUri().toString()), identities);
        assertEquals(Batch.State.EXTERNAL_DONE, upload.getState());
        assertTrue(Files.exists(Path.of(user.getImportFolder()).resolve("folder-0/scan.tif")));
        assertEquals(Batch.State.EXTERNAL_PLANNED, manager.get(jobs.get(1)).getState());
        Files.delete(Path.of(user.getImportFolder()).resolve("folder-1/scan.tif"));
        Files.delete(Path.of(user.getImportFolder()).resolve("folder-1"));
        Batch missing = manager.get(jobs.get(1));
        assertThrows(IOException.class, () -> process.run(missing, missing.getParamsAsObject()));
        Batch independent = manager.get(jobs.get(2));
        process.run(independent, independent.getParamsAsObject());
        assertEquals(Batch.State.EXTERNAL_DONE, independent.getState());
        assertTrue(Files.exists(Path.of(user.getImportFolder()).resolve("folder-2/scan.tif")));
    }

    @Test
    void folderPackagingExcludesHistoriesAndTokenWaitRetainsTheArchive() throws Exception {
        Path folder = importFolder("nested");
        Files.writeString(folder.resolve("cesnet-upload.json"), "{\"schemaVersion\":1,\"uploads\":[]}");
        Files.writeString(folder.resolve(".cesnet-upload-interrupted.json"), "temporary history");
        assertEquals(List.of(folder.resolve("scan.tif").toFile()),
                new ImportFileScanner().findDigitalContent(folder.toFile()));
        Files.writeString(Files.createDirectory(folder.resolve("child")).resolve("file.txt"), "child payload");
        var process = new BagitProcess(config, manager, user);
        Batch bagit = process.planImportFolder("nested", false, false, Batch.PRIORITY_MEDIUM);
        process.run(bagit, bagit.getParamsAsObject());
        Path archive = Path.of(bagit.getParamsAsObject().getBagitArchives().get(0));
        try (var zip = new ZipFile(archive.toFile())) {
            var names = zip.stream().map(ZipEntry::getName).toList();
            assertTrue(names.stream().anyMatch(n -> n.endsWith("/data/child/file.txt")));
            assertFalse(names.stream().anyMatch(n -> n.endsWith("cesnet-upload.json")));
        }
        var cesnet = new CesnetProcess(config, manager, user, (file, operation, pid, confirmed, owner) -> {
            throw new CesnetTokenException(
                    CesnetTokenException.Reason.MISSING);
        }, pid -> null);
        Batch upload = cesnet.planAfterBagit(bagit);
        cesnet.run(upload, upload.getParamsAsObject());
        assertEquals(Batch.State.WAITING_CESNET_TOKEN, upload.getState());
        assertTrue(Files.exists(archive));
        assertTrue(Files.exists(folder.resolve("scan.tif")));
        // Resume uses the packaged snapshot; it must not recopy changed source data.
        Files.writeString(folder.resolve("scan.tif"), "changed after packaging");
        var resumed = new CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, owner) -> assertEquals(archive.toFile(), file), pid -> null);
        resumed.run(upload, upload.getParamsAsObject());
        assertEquals(Batch.State.EXTERNAL_DONE, upload.getState());
        assertFalse(Files.exists(archive));
    }

    @Test
    void folderUploadRequiresFeatureFlagAndConfirmsExistingHistory() throws Exception {
        Path folder = importFolder("repeat");
        configureFolderUploads(false);
        var disabled = new BagitProcess(config, manager, user);
        assertThrows(IOException.class, () -> disabled.planImportFolder("repeat", false, false, Batch.PRIORITY_MEDIUM));
        configureFolderUploads(true);
        var process = new BagitProcess(config, manager, user);
        var record = new ArchivInfo();
        record.setOperationId("old-operation");
        record.setPackageId("123");
        record.setState(ArchivInfo.State.UPLOADED);
        new FolderUploadRepository(folder, null).save("folder", record);
        assertThrows(CesnetProcess.ConfirmationRequiredException.class,
                () -> process.planImportFolder("repeat", false, false, Batch.PRIORITY_MEDIUM));
        assertNotNull(process.planImportFolder("repeat", true, false, Batch.PRIORITY_MEDIUM));
        assertThrows(IOException.class, () -> process.planImportFolder("../outside", true, false, Batch.PRIORITY_MEDIUM));
        assertThrows(IOException.class, () -> process.planImportFolder(".", true, false, Batch.PRIORITY_MEDIUM));
    }

    private static class MemoryManager extends BatchManager {
        final Map<Integer, Batch> batches = new HashMap<>();
        final List<Batch.State> states = new ArrayList<>();
        MemoryManager(AppConfiguration config) {
            super(config, (DaoFactory) Proxy.newProxyInstance(DaoFactory.class.getClassLoader(), new Class<?>[]{DaoFactory.class},
                    (proxy, method, args) -> { throw new AssertionError("Unexpected database access"); }));
        }
        @Override public Batch update(Batch batch) {
            if (batch.getId() == null) batch.setId(batches.size() + 1);
            batches.put(batch.getId(), batch);
            states.add(batch.getState());
            return batch;
        }
        @Override public Batch get(int id) { return batches.get(id); }
        @Override public List<Batch> findInternalExternalRunningBatches() {
            return batches.values().stream().filter(batch -> batch.getState() == Batch.State.INTERNAL_RUNNING
                    || (batch.hasExternalProfile() && batch.getState() == Batch.State.EXTERNAL_RUNNING)).toList();
        }
    }
}
