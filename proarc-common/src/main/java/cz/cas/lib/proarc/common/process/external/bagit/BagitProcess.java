package cz.cas.lib.proarc.common.process.external.bagit;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import org.apache.commons.io.FileUtils;

/** Creates retryable BagIt archives and cleans up completed source exports. */
public final class BagitProcess {
    private static final Set<String> EXPORTS = Set.of(Batch.EXPORT_NDK, Batch.EXPORT_ARCHIVE, Batch.EXPORT_KRAMERIUS);
    private final AppConfiguration config;
    private final BatchManager manager;
    private final UserProfile user;

    public BagitProcess(AppConfiguration config, BatchManager manager, UserProfile user) {
        this.config = config;
        this.manager = manager;
        this.user = user;
    }

    public Batch plan(Batch source) throws IOException {
        requireOwnedFinished(source);
        if (!EXPORTS.contains(source.getProfileId())) throw new IOException("Zdrojem musí být export NDK, archivní export nebo export FOXML");
        if (source.getFolder() == null) throw new IOException("Zdrojová složka není určena.");
        Path directory = Path.of(source.getFolder());
        if (!Files.isDirectory(directory)) throw new MissingSourceDirectoryException(directory);
        BatchParams params = new BatchParams(source.getParamsAsObject().getPids());
        params.setSourceBatchId(source.getId());
        return manager.add(params.getPids().get(0), user, Batch.EXTERNAL_BAGIT, Batch.State.EXTERNAL_PLANNED,
                source.isNightOnly(), source.getPriority(), params);
    }

    /** Plans optional packaging after a completed export, retaining the automatic upload options. */
    public Batch planAfterExport(Batch source) throws IOException {
        BatchParams params = source.getParamsAsObject();
        if (params.getNextBatchId() != null || !EXPORTS.contains(source.getProfileId())
                || !(Boolean.TRUE.equals(params.isBagit()) || Boolean.TRUE.equals(params.isLtpCesnet()))
                || (params.getKrameriusInstanceId() != null && !params.getKrameriusInstanceId().isEmpty()
                    && !"local".equals(params.getKrameriusInstanceId()))) return null;
        Batch next = plan(source);
        BatchParams nextParams = next.getParamsAsObject();
        nextParams.setLtpCesnet(params.isLtpCesnet());
        nextParams.setConfirmCesnetUpload(params.getConfirmCesnetUpload());
        save(next, nextParams);
        params.setNextBatchId(next.getId());
        params.setLtpCesnetToken(null);
        save(source, params);
        return next;
    }

    public Batch run(Batch batch, BatchParams params) throws Exception {
        Batch source = manager.get(params.getSourceBatchId());
        requireOwnedFinished(source);
        if (!EXPORTS.contains(source.getProfileId())) throw new IOException("Neplatný profil zdrojové dávky pro BagIt");
        Path sourceRoot = Path.of(source.getFolder()).toAbsolutePath().normalize();
        String sourceName = sourceRoot.getFileName().toString();
        String packageName = sourceName.startsWith("archive_") ? sourceName.substring("archive_".length()) : sourceName;
        String configuredPath = Batch.EXPORT_KRAMERIUS.equals(source.getProfileId())
                ? config.getBagitFoxmlExportPath() : config.getBagitExportPath();
        if (configuredPath == null || configuredPath.isBlank()) configuredPath = config.getBagitExportPath();
        Path destination = configuredPath == null || configuredPath.isBlank()
                ? config.getConfigHome().toPath().resolve("temp/ltpCesnet") : Path.of(configuredPath);
        // Each batch owns a separate directory, even when object/package names are identical.
        Path output = batch.getFolder() == null || params.getPids().contains(batch.getFolder())
                ? destination.resolve("bagit_" + batch.getId() + "_" + packageName).toAbsolutePath().normalize()
                : Path.of(batch.getFolder()).toAbsolutePath().normalize();
        if (Files.isDirectory(sourceRoot)) {
            // Resolve existing parents too, so a configured symlink cannot hide nesting.
            Path existing = output;
            while (!Files.exists(existing)) existing = existing.getParent();
            if (output.startsWith(sourceRoot) || existing.toRealPath().startsWith(sourceRoot.toRealPath())) {
                throw new IOException("Cílová složka BagIt je uvnitř zdrojového exportu: " + output);
            }
        }
        Files.createDirectories(output);
        batch.setFolder(output.toString());
        save(batch, params);
        if (!Boolean.TRUE.equals(params.getBagitComplete())) {
            createArchives(batch, params, source, sourceRoot, output);
            requireCompleteArchives(list(params.getBagitArchives()));
            params.setBagitComplete(true);
            // Persist completion before deleting anything, so a restart retries cleanup only.
            save(batch, params);
        }
        requireCompleteArchives(list(params.getBagitArchives()));
        requireActive(batch);
        deleteSourceExport(sourceRoot, output, list(params.getBagitArchives()));
        // Raw scan cleanup remains controlled by the original export's deleteRawScans option.
        try {
            for (String folder : list(source.getParamsAsObject().getRawScanFolders())) {
                FileUtils.deleteDirectory(new File(folder));
            }
        } catch (IOException ex) {
            return BatchUtils.finishedSuccessfully(manager, batch, batch.getFolder(),
                    "Vytvoření balíčku BagIt bylo dokončeno; zdrojový export byl smazán; smazání původních skenů selhalo: " + ex.getMessage(), Batch.State.EXTERNAL_DONE);
        }
        requireActive(batch);
        return BatchUtils.finishedSuccessfully(manager, batch, batch.getFolder(),
                "Vytvoření balíčku BagIt bylo dokončeno. Zdrojový export byl smazán. Archivy byly zachovány pro nahrání do CESNETu.", Batch.State.EXTERNAL_DONE);
    }

    private void createArchives(Batch batch, BatchParams params, Batch source, Path sourceRoot, Path output) throws Exception {
        requireSourceDirectory(sourceRoot);
        List<Path> packages;
        boolean foxml = Batch.EXPORT_KRAMERIUS.equals(source.getProfileId());
        if (foxml) {
            packages = List.of(sourceRoot);
        } else {
            try (var children = Files.list(sourceRoot)) {
                packages = children.filter(Files::isDirectory).sorted().toList();
            }
        }
        if (packages.isEmpty()) throw new IOException("Zdrojový export neobsahuje žádné balíčky");
        List<String> completed = new ArrayList<>(list(params.getCompletedPackageSources()));
        List<String> archives = new ArrayList<>(list(params.getBagitArchives()));
        requireArchivesIfPresent(archives);
        for (Path sourcePackage : packages) {
            requireActive(batch);
            if (completed.contains(sourcePackage.toString())) continue;
            Path work = output.resolve("work");
            if (Files.exists(work)) FileUtils.deleteDirectory(work.toFile());
            Path copy = work.resolve(sourcePackage.getFileName());
            FileUtils.copyDirectory(sourcePackage.toFile(), copy.toFile());
            requireActive(batch);
            Bagit export = new Bagit(config, copy.toFile());
            export.prepare();
            export.bagit();
            export.zip();
            export.moveToBagitFolder();
            export.createMd5File();
            requireActive(batch);
            export.moveToSpecifiedDirectories(null);
            File archive = export.getDestinationFile();
            if (archive == null || !archive.isFile()) throw new IOException("Archiv BagIt nebyl vytvořen");
            Path stable = output.resolve(archive.getName());
            Path sums = archive.toPath().resolveSibling(archive.getName().replaceFirst("\\.zip$", ".sums"));
            // Local output is durable even when work is cleared for the next package or retry.
            if (archive.toPath().startsWith(work)) {
                Files.move(archive.toPath(), stable);
                Files.move(sums, stable.resolveSibling(sums.getFileName()));
                archive = stable.toFile();
            }
            archives.add(archive.getAbsolutePath());
            completed.add(sourcePackage.toString());
            params.setBagitArchives(archives);
            params.setCompletedPackageSources(completed);
            save(batch, params);
            export.deleteExportFolder();
            FileUtils.deleteDirectory(work.toFile());
        }
    }

    private void deleteSourceExport(Path sourceRoot, Path output, List<String> archives) throws IOException {
        if (!Files.exists(sourceRoot, java.nio.file.LinkOption.NOFOLLOW_LINKS)) return;
        if (Files.isSymbolicLink(sourceRoot)) throw new IOException("Nelze smazat zdrojový export tvořený symbolickým odkazem: " + sourceRoot);
        Path target = sourceRoot.toRealPath();
        if (target.getParent() == null || output.toRealPath().startsWith(target)) {
            throw new IOException("Zdrojový export obsahuje pracovní složku BagIt: " + target);
        }
        for (String archive : archives) {
            if (Path.of(archive).toRealPath().startsWith(target) || checksumFile(archive).toRealPath().startsWith(target)) {
                throw new IOException("Zdrojový export obsahuje dokončený archiv BagIt: " + target);
            }
        }
        FileUtils.deleteDirectory(target.toFile());
    }

    private static Path checksumFile(String archive) {
        Path path = Path.of(archive);
        return path.resolveSibling(path.getFileName().toString().replaceFirst("\\.zip$", ".sums"));
    }

    private static void requireSourceDirectory(Path directory) throws MissingSourceDirectoryException {
        if (!Files.isDirectory(directory)) throw new MissingSourceDirectoryException(directory);
    }

    private static void requireCompleteArchives(List<String> archives) throws IOException {
        requireArchives(archives);
        for (String archive : archives) {
            if (!Files.isRegularFile(checksumFile(archive))) {
                throw new IOException("Soubor s kontrolním součtem BagIt již neexistuje: " + checksumFile(archive));
            }
        }
    }

    private void requireOwned(Batch source) throws IOException {
        if (source == null || !Objects.equals(user.getId(), source.getUserId())) throw new IOException("Zdrojová dávka nebyla nalezena nebo byl odepřen přístup");
        if (source.getParamsAsObject() == null) throw new IOException("Zdrojová dávka nemá parametry");
    }

    private void requireOwnedFinished(Batch source) throws IOException {
        requireOwned(source);
        boolean finished = source.getState() == Batch.State.EXPORT_DONE || source.getState() == Batch.State.EXPORT_WARNING;
        if (!finished) {
            throw new IOException("Zdrojová dávka musí být dokončena");
        }
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
            throw new java.io.InterruptedIOException("Proces BagIt byl zastaven");
        }
    }

    private static void requireArchives(List<String> archives) throws IOException {
        if (archives.isEmpty()) throw new IOException("Zdrojová dávka neobsahuje archivy BagIt");
        requireArchivesIfPresent(archives);
    }

    private static void requireArchivesIfPresent(List<String> archives) throws IOException {
        for (String archive : archives) {
            if (!new File(archive).isFile()) throw new IOException("Archiv BagIt již neexistuje: " + archive);
        }
    }

    private static List<String> list(List<String> values) { return values == null ? List.of() : values; }
}
