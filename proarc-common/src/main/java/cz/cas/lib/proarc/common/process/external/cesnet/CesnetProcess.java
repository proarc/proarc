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
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.FolderUploadRepository;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetUploader;
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenException;
import cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo;
import cz.cas.lib.proarc.common.externalApp.ltp.PreservationService;
import cz.cas.lib.proarc.common.process.external.bagit.ImportFolderSource;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/** Plans and resumes CESNET uploads, retaining local archives until the entire upload succeeds. */
public final class CesnetProcess {
    private final BatchManager manager;
    private final UserProfile user;
    private final ArchiveUploader uploader;
    private final RecordReader reader;
    private final TokenValidator tokenValidator;

    @FunctionalInterface public interface TokenValidator { void validate(String token) throws IOException; }

    public CesnetProcess(AppConfiguration config, BatchManager manager, UserProfile user) {
        this(config, manager, user, (archive, operationId, pid, confirmed, credentialOwnerId) -> {
                    CesnetUploader uploader = new CesnetUploader(config, credentialOwnerId);
                    if (pid.startsWith("file:")) {
                        Path folder = ImportFolderSource.validate(
                                user, Path.of(URI.create(pid)));
                        uploader.uploadFolder(archive, operationId, folder, confirmed);
                    } else {
                        uploader.upload(archive, operationId, pid, confirmed);
                    }
                },
                pid -> pid.startsWith("file:")
                        ? new FolderUploadRepository(
                                Path.of(URI.create(pid)), null).read(pid)
                        : PreservationService.cesnet(config).archivInfo(pid),
                token -> cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenStore.getInstance()
                        .validate(config, user.getId(), token));
    }

    public CesnetProcess(AppConfiguration config, BatchManager manager, UserProfile user, ArchiveUploader uploader, RecordReader reader) {
        this(config, manager, user, uploader, reader, token -> { });
    }

    public CesnetProcess(AppConfiguration config, BatchManager manager, UserProfile user, ArchiveUploader uploader,
                       RecordReader reader, TokenValidator tokenValidator) {
        this.manager = manager;
        this.user = user;
        this.uploader = uploader;
        this.reader = reader;
        this.tokenValidator = tokenValidator;
    }

    @FunctionalInterface
    public interface ArchiveUploader {
        void upload(File archive, String operationId, String pid, boolean confirmed, Integer credentialOwnerId) throws Exception;
    }

    @FunctionalInterface public interface RecordReader {
        ArchivInfo read(String pid) throws IOException;
    }

    public record UploadWarning(String pid, String packageId, ArchivInfo.State state, String message) { }

    public static UploadWarning warning(String pid, ArchivInfo record) {
        String message = switch (record.getState()) {
            case STORED -> "Záznam již byl nahrán do CESNETu a zvalidován.";
            case UPLOADED -> "Záznam již byl nahrán do CESNETu a čeká na zvalidování.";
            default -> "Záznam již má balíček v CESNETu (" + record.getState() + ").";
        };
        return new UploadWarning(pid, record.getPackageId(), record.getState(),
                message + " Chcete jej nahrát jako nový balíček a nahradit evidované LTP ID?");
    }

    public static final class ConfirmationRequiredException extends IOException {
        private final List<UploadWarning> warnings;
        public ConfirmationRequiredException(List<UploadWarning> warnings) {
            super("Nahrání do CESNETu vyžaduje potvrzení");
            this.warnings = List.copyOf(warnings);
        }
        public List<UploadWarning> getWarnings() { return warnings; }
    }

    public static UploadWarning folderWarning(String folder, ArchivInfo record) {
        return new UploadWarning(folder, record.getPackageId(), record.getState(),
                "Složka již má záznam nahrání do CESNETu. Chcete vytvořit nový balíček? Historie bude zachována.");
    }

    public Batch plan(Batch source, String token) throws IOException {
        return plan(source, token, false);
    }

    public Batch plan(Batch source, String token, boolean confirmed) throws IOException {
        return plan(source, token, confirmed, false);
    }

    private Batch plan(Batch source, String token, boolean confirmed, boolean retainUnconfirmed) throws IOException {
        requireOwned(source);
        BatchParams previous = source.getParamsAsObject();
        List<String> completed = new ArrayList<>();
        String operationId = java.util.UUID.randomUUID().toString();
        if (Batch.EXTERNAL_CESNET.equals(source.getProfileId())) {
            if (source.getState() == Batch.State.EXTERNAL_RUNNING || source.getState() == Batch.State.EXTERNAL_PLANNED
                    || source.getState() == Batch.State.WAITING_CESNET_TOKEN) {
                throw new IOException("Nahrávání do CESNETu již probíhá, je naplánováno nebo čeká na token; pro čekající nahrávání použijte cesnet/resume");
            }
            if (source.getState() != Batch.State.EXTERNAL_DONE) {
                completed.addAll(list(previous.getCompletedUploads()));
                operationId = previous.getCesnetOperationId();
                // Retain the operation scheme of batches created before operation IDs were persisted.
                if (operationId == null) operationId = String.valueOf(previous.getSourceBatchId());
                confirmed = confirmed || Boolean.TRUE.equals(previous.getConfirmCesnetUpload());
            }
            source = manager.get(previous.getSourceBatchId());
        }
        requireOwnedFinished(source);
        if (!Batch.EXTERNAL_BAGIT.equals(source.getProfileId())) throw new IOException("Zdrojem musí být dokončená dávka BagIt");
        List<String> archives = list(source.getParamsAsObject().getBagitArchives());
        requirePendingArchives(archives, completed);
        // Preparing an archive does not require CESNET access. Credentials are checked at upload time.
        if (token != null && !token.isBlank()) tokenValidator.validate(token);
        List<UploadWarning> warnings = new ArrayList<>();
        if (!confirmed) {
            for (String archive : archives) {
                if (completed.contains(archive)) continue;
                String pid = packagePid(new File(archive), source);
                String operation = operation(operationId, new File(archive));
                ArchivInfo record = reader.read(pid);
                if (record != null && !operation.equals(record.getOperationId())) {
                    warnings.add(previous.getImportFolder() != null ? folderWarning(pid, record) : warning(pid, record));
                }
            }
            if (!warnings.isEmpty() && !retainUnconfirmed) throw new ConfirmationRequiredException(warnings);
        }
        BatchParams params = new BatchParams(source.getParamsAsObject().getPids());
        params.setImportFolder(source.getParamsAsObject().getImportFolder());
        params.setSourceBatchId(source.getId());
        params.setBagitArchives(new ArrayList<>(archives));
        params.setCompletedUploads(completed);
        params.setConfirmCesnetUpload(confirmed);
        params.setCesnetOperationId(operationId);
        Batch batch = manager.add(params.getPids().get(0), user, Batch.EXTERNAL_CESNET,
                warnings.isEmpty() ? Batch.State.EXTERNAL_PLANNED : Batch.State.EXTERNAL_FAILED,
                false, source.getPriority(), params);
        batch.setFolder(source.getFolder());
        save(batch, params);
        if (!warnings.isEmpty()) {
            StringBuilder log = new StringBuilder("Nahrání do CESNETu vyžaduje potvrzení. "
                    + "Spusťte tuto dávku CESNET znovu s parametrem confirmCesnetUpload=true. Místní archivy byly zachovány.");
            for (UploadWarning warning : warnings) {
                log.append('\n').append(warning.pid()).append(": ").append(warning.message());
            }
            return BatchUtils.finishedExternalWithError(manager, batch, source.getFolder(), log.toString());
        }
        return batch;
    }

    /** Plans the optional upload once BagIt has completed. */
    public Batch planAfterBagit(Batch source) throws IOException {
        BatchParams params = source.getParamsAsObject();
        if (params.getNextBatchId() != null || !Batch.EXTERNAL_BAGIT.equals(source.getProfileId())
                || !Boolean.TRUE.equals(params.isLtpCesnet())) return null;
        Batch next = plan(source, null, Boolean.TRUE.equals(params.getConfirmCesnetUpload()), true);
        params.setNextBatchId(next.getId());
        params.setLtpCesnetToken(null);
        save(source, params);
        return next;
    }

    public Batch run(Batch batch, BatchParams params) throws Exception {
        Batch source = manager.get(params.getSourceBatchId());
        requireOwnedFinished(source);
        if (!Batch.EXTERNAL_BAGIT.equals(source.getProfileId())) throw new IOException("Neplatný profil zdrojové dávky pro CESNET");
        List<String> archives = list(params.getBagitArchives());
        requirePendingArchives(archives, list(params.getCompletedUploads()));
        if (!list(source.getParamsAsObject().getBagitArchives()).containsAll(archives)) throw new IOException("Archiv nepatří do zdrojové dávky");
        List<String> completed = new ArrayList<>(list(params.getCompletedUploads()));
        batch.setFolder(source.getFolder());
        save(batch, params);
        try {
            for (String archive : archives) {
                if (completed.contains(archive)) continue;
                requireActive(batch);
                File file = new File(archive);
                String operation = operation(params.getCesnetOperationId() == null
                        ? String.valueOf(params.getSourceBatchId()) : params.getCesnetOperationId(), file);
                Integer credentialOwnerId = params.getCesnetCredentialOwnerId() == null
                        ? user.getId() : params.getCesnetCredentialOwnerId();
                uploader.upload(file, operation, packagePid(file, source),
                        Boolean.TRUE.equals(params.getConfirmCesnetUpload()), credentialOwnerId);
                completed.add(archive);
                params.setCompletedUploads(completed);
                save(batch, params);
            }
        } catch (CesnetTokenException ex) {
            requireActive(batch);
            batch.setState(Batch.State.WAITING_CESNET_TOKEN);
            batch.setLog("Čeká na token CESNET. " + ex.getMessage());
            batch.setUpdated(new java.sql.Timestamp(System.currentTimeMillis()));
            return batch;
        } finally {
            // Remove legacy persisted tokens; the client obtains credentials only when it runs.
            params.setLtpCesnetToken(null);
            save(batch, params);
        }
        requireActive(batch);
        deleteUploadedArchives(archives, batch.getFolder());
        return BatchUtils.finishedSuccessfully(manager, batch, batch.getFolder(),
                "Nahrávání do CESNETu bylo dokončeno. Místní archivy byly smazány.", Batch.State.EXTERNAL_DONE);
    }

    private static void deleteUploadedArchives(List<String> archives, String folder) throws IOException {
        java.util.Set<Path> directories = new java.util.HashSet<>();
        for (String archive : archives) {
            Path zip = Path.of(archive);
            Files.deleteIfExists(zip);
            Files.deleteIfExists(zip.resolveSibling(zip.getFileName().toString().replaceFirst("\\.zip$", ".sums")));
            directories.add(zip.toAbsolutePath().getParent());
        }
        if (folder != null) directories.add(Path.of(folder).toAbsolutePath());
        for (Path directory : directories) {
            try {
                Files.deleteIfExists(directory);
            } catch (DirectoryNotEmptyException ex) {
                // Older batches may use a shared export directory. Keep unrelated files.
            }
        }
    }

    static String packagePid(File archive, Batch source) throws IOException {
        if (source.getParamsAsObject().getImportFolder() != null) {
            return Path.of(source.getParamsAsObject().getImportFolder()).toUri().toString();
        }
        // NDK package names can be URN:NBN suffixes rather than object UUIDs.
        // A single archive for a single PID has an explicit identity in the source batch.
        String name = archive.getName();
        if (!name.startsWith("archive_") || !name.endsWith(".zip")) throw new IOException("Nelze určit PID balíčku BagIt");
        String uuid = name.substring("archive_".length(), name.length() - ".zip".length());
        if (source.getParamsAsObject().getBagitArchives().size() == 1
                && source.getParamsAsObject().getPids().size() == 1) {
            return source.getParamsAsObject().getPids().get(0);
        }
        return uuid.startsWith("uuid:") ? uuid : "uuid:" + uuid;
    }

    private static String operation(String id, File archive) {
        return java.util.UUID.nameUUIDFromBytes((id + ":" + archive.getName())
                .getBytes(java.nio.charset.StandardCharsets.UTF_8)).toString();
    }

    private void save(Batch batch, BatchParams params) {
        batch.setParamsFromObject(params);
        Batch latest = manager.get(batch.getId());
        if (latest != null && latest.getState() == Batch.State.STOPPED) batch.setState(Batch.State.STOPPED);
        manager.update(batch);
    }

    private void requireActive(Batch batch) throws IOException {
        Batch latest = manager.get(batch.getId());
        if (Thread.currentThread().isInterrupted() || (latest != null && latest.getState() == Batch.State.STOPPED)) {
            throw new java.io.InterruptedIOException("Proces CESNET byl zastaven");
        }
    }

    private void requireOwned(Batch source) throws IOException {
        if (source == null || !Objects.equals(user.getId(), source.getUserId())) throw new IOException("Zdrojová dávka nebyla nalezena nebo byl odepřen přístup");
        if (source.getParamsAsObject() == null) throw new IOException("Zdrojová dávka nemá parametry");
    }

    private void requireOwnedFinished(Batch source) throws IOException {
        requireOwned(source);
        boolean finished = source.getState() == Batch.State.EXTERNAL_DONE;
        if (!finished) {
            throw new IOException("Zdrojová dávka musí být dokončena");
        }
    }

    private static void requirePendingArchives(List<String> archives, List<String> completed) throws IOException {
        if (archives.isEmpty()) throw new IOException("Zdrojová dávka neobsahuje archivy BagIt");
        requireArchivesIfPresent(archives.stream().filter(archive -> !completed.contains(archive)).toList());
    }

    private static void requireArchivesIfPresent(List<String> archives) throws IOException {
        for (String archive : archives) {
            if (!new File(archive).isFile()) throw new IOException("Archiv BagIt již neexistuje: " + archive);
        }
    }

    private static List<String> list(List<String> values) { return values == null ? List.of() : values; }
}
