package cz.cas.lib.proarc.common.process.external;

import cz.cas.lib.proarc.common.config.AppConfiguration;
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
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
        var modificationTimes = archives.stream().map(file -> {
            try { return Files.getLastModifiedTime(Path.of(file)); } catch (IOException ex) { throw new RuntimeException(ex); }
        }).toList();

        Batch retry = uploading.plan(failed, "fresh");
        assertNotEquals(failed.getId(), retry.getId());
        uploading.run(retry, retry.getParamsAsObject());

        assertEquals(List.of(archives.get(0), archives.get(1), archives.get(1)), calls);
        assertEquals(Batch.State.EXTERNAL_DONE, retry.getState());
        assertNull(retry.getParamsAsObject().getLtpCesnetToken());
        for (int i = 0; i < archives.size(); i++) {
            assertEquals(modificationTimes.get(i), Files.getLastModifiedTime(Path.of(archives.get(i))));
        }
        assertFalse(Files.exists(Path.of(source.getFolder())));
        assertEquals(archives, bagit.getParamsAsObject().getBagitArchives());
    }

    @Test
    void automaticChainHasSeparatePersistentBatchesAndDoesNotRepeatPackaging() throws Exception {
        Batch source = export();
        BatchParams params = source.getParamsAsObject();
        params.setBagit(true);
        params.setLtpCesnet(true);
        params.setLtpCesnetToken("initial-token");
        params.setConfirmCesnetUpload(true);
        source.setParamsFromObject(params);
        BagitProcess packaging = new BagitProcess(config, manager, user);
        CesnetProcess uploading = uploadProcess();
        Batch bagit = packaging.planAfterExport(source);
        assertEquals(Batch.EXTERNAL_BAGIT, bagit.getProfileId());
        assertEquals(source.getId(), bagit.getParamsAsObject().getSourceBatchId());
        assertEquals(bagit.getId(), source.getParamsAsObject().getNextBatchId());
        assertNull(packaging.planAfterExport(source));
        packaging.run(bagit, bagit.getParamsAsObject());
        List<String> archives = bagit.getParamsAsObject().getBagitArchives();
        packaging.run(bagit, bagit.getParamsAsObject());
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
        Path scans = Files.createDirectory(temp.resolve("raw"));
        Files.writeString(scans.resolve("scan"), "raw");
        BatchParams sourceParams = source.getParamsAsObject();
        sourceParams.setRawScanFolders(List.of(scans.toString()));
        source.setParamsFromObject(sourceParams);
        Path invalid = Files.writeString(Path.of(source.getFolder(), "b/bagit.txt"), "already bagged");
        BagitProcess packaging = new BagitProcess(config, manager, user);
        Batch bagit = packaging.plan(source);
        assertThrows(IOException.class, () -> packaging.run(bagit, bagit.getParamsAsObject()));
        List<String> completed = bagit.getParamsAsObject().getBagitArchives();
        assertEquals(1, completed.size());
        var modified = Files.getLastModifiedTime(Path.of(completed.get(0)));
        assertTrue(Files.exists(scans.resolve("scan")));
        assertTrue(Files.exists(Path.of(source.getFolder(), "a/scan.tif")));
        Files.delete(invalid);
        packaging.run(bagit, bagit.getParamsAsObject());
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
        Batch bagit = packaging.planAfterExport(source);
        packaging.run(bagit, bagit.getParamsAsObject());
        assertNotNull(uploading.planAfterBagit(bagit));
        assertEquals(Batch.State.EXTERNAL_DONE, bagit.getState());
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
    void cleanupRetryChecksZipAndSumsAndDoesNotRebuildCompletedPackages() throws Exception {
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
        assertEquals(Boolean.TRUE, bagit.getParamsAsObject().getBagitComplete());
        List<String> archives = bagit.getParamsAsObject().getBagitArchives();
        assertEquals(2, archives.size());
        Path checksum = Path.of(archives.get(0).replaceFirst("\\.zip$", ".sums"));
        String contents = Files.readString(checksum);
        Files.delete(checksum);
        assertThrows(IOException.class, () -> packaging.run(bagit, bagit.getParamsAsObject()));
        assertTrue(Files.exists(root.resolve("b/scan.tif")));
        Files.writeString(checksum, contents);

        Path safeDestination = Files.createDirectory(temp.resolve("completed-bags"));
        List<String> relocated = new ArrayList<>();
        for (String archive : archives) {
            Path zip = Path.of(archive);
            Path sums = Path.of(archive.replaceFirst("\\.zip$", ".sums"));
            relocated.add(Files.move(zip, safeDestination.resolve(zip.getFileName())).toString());
            Files.move(sums, safeDestination.resolve(sums.getFileName()));
        }
        BatchParams params = bagit.getParamsAsObject();
        params.setBagitArchives(relocated);
        bagit.setParamsFromObject(params);
        packaging.run(bagit, bagit.getParamsAsObject());
        assertFalse(Files.exists(root));
        assertEquals(relocated, bagit.getParamsAsObject().getBagitArchives());
        // Also covers a restart after source deletion but before the final DONE state was persisted.
        bagit.setState(Batch.State.EXTERNAL_PLANNED);
        InternalExternalProcess.prepare(config, null, bagit, manager, user,
                "retry", java.util.Locale.ROOT).start();
        assertEquals(Batch.State.EXTERNAL_DONE, bagit.getState());
        assertNotNull(uploading.plan(bagit, "fresh"));
    }

    @Test
    void usesSiblingBagitFolderWithoutIntermediateFoldersAndReplacesExistingDestination() throws Exception {
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
        assertFalse(Files.exists(existing));
        assertFalse(Files.exists(expected.resolve("old-content")));
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
            assertEquals(destination, Path.of(archive).getParent());
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
    void repeatRequiresConfirmationBeforeSchedulingAndUsesNewOperations() throws Exception {
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
        // A finished CESNET batch can also be explicitly submitted again.
        Batch repeated = cesnet.plan(first, null, true);
        assertTrue(repeated.getParamsAsObject().getCompletedUploads() == null
                || repeated.getParamsAsObject().getCompletedUploads().isEmpty());
        cesnet.run(repeated, repeated.getParamsAsObject());
        assertEquals(4, operations.size());
        assertNotEquals(operations.get(0), operations.get(2));
        assertNotEquals(first.getParamsAsObject().getCesnetOperationId(), repeated.getParamsAsObject().getCesnetOperationId());
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
        assertEquals(Batch.State.WAITING_FOR_TOKEN, result.getState());
        assertEquals(Batch.State.EXTERNAL_DONE, bagit.getState());
        assertEquals(Batch.State.EXPORT_DONE, source.getState());
        assertTrue(result.getLog().contains("Čeká na token CESNET"));
        assertNull(result.getParamsAsObject().getLtpCesnetToken());
        for (String archive : params.getBagitArchives()) assertTrue(Files.exists(Path.of(archive)));
        BatchUtils.finishedInternalExternalRunningBatch(manager, config);
        assertEquals(Batch.State.WAITING_FOR_TOKEN, result.getState());
        assertEquals(Batch.State.WAITING_FOR_TOKEN,
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

    private CesnetProcess uploadProcess() {
        return new CesnetProcess(config, manager, user,
                (file, operation, pid, confirmed, credentialOwnerId) -> { }, pid -> null);
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
