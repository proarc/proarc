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

    public Batch run(Batch batch, BatchParams params) throws Exception {
        Batch source = manager.get(params.getSourceBatchId());
        if (source == null || !Objects.equals(user.getId(), source.getUserId())) {
            throw new IOException("Source batch not found or access denied");
        }
        if (source.getParamsAsObject() == null) throw new IOException("Source batch has no parameters");
        if (source.getState() != Batch.State.EXPORT_DONE && source.getState() != Batch.State.EXPORT_WARNING) {
            throw new IOException("Source batch must be completed");
        }
        if (!EXPORTS.contains(source.getProfileId())) throw new IOException("Invalid BagIt source profile");
        Path sourceRoot = Path.of(source.getFolder()).toAbsolutePath().normalize();
        String sourceName = sourceRoot.getFileName().toString();
        String packageName = sourceName.startsWith("archive_") ? sourceName.substring("archive_".length()) : sourceName;
        Path output = sourceRoot.resolveSibling("bagit_" + packageName);
        if (Files.exists(output, java.nio.file.LinkOption.NOFOLLOW_LINKS) && !output.toString().equals(batch.getFolder())) {
            requireActive(batch);
            requireSourceDirectory(sourceRoot);
            if (Files.isSymbolicLink(output)) {
                Files.delete(output);
            } else {
                Path target = output.toRealPath();
                Path resolvedSource = sourceRoot.toRealPath();
                if (!target.getParent().equals(sourceRoot.getParent().toRealPath())
                        || !target.getFileName().equals(output.getFileName()) || resolvedSource.startsWith(target)) {
                    throw new IOException("Refusing to replace an unsafe BagIt destination: " + target);
                }
                FileUtils.deleteDirectory(target.toFile());
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
                    "BagIt completed; source export deleted; raw scan cleanup failed: " + ex.getMessage(), Batch.State.EXTERNAL_DONE);
        }
        requireActive(batch);
        return BatchUtils.finishedSuccessfully(manager, batch, batch.getFolder(),
                "BagIt completed. Source export deleted. Archives retained for CESNET upload.", Batch.State.EXTERNAL_DONE);
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
        if (packages.isEmpty()) throw new IOException("Source export contains no packages");
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
            // Existing configured destination folders remain supported.
            export.moveToSpecifiedDirectories(foxml ? config.getBagitFoxmlExportPath() : config.getBagitExportPath());
            File archive = export.getDestinationFile();
            if (archive == null || !archive.isFile()) throw new IOException("BagIt archive was not created");
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
        if (Files.isSymbolicLink(sourceRoot)) throw new IOException("Refusing to delete a linked source export: " + sourceRoot);
        Path target = sourceRoot.toRealPath();
        if (target.getParent() == null || output.toRealPath().startsWith(target)) {
            throw new IOException("Source export contains the BagIt working directory: " + target);
        }
        for (String archive : archives) {
            if (Path.of(archive).toRealPath().startsWith(target) || checksumFile(archive).toRealPath().startsWith(target)) {
                throw new IOException("Source export contains a completed BagIt archive: " + target);
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
                throw new IOException("BagIt checksum file no longer exists: " + checksumFile(archive));
            }
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
            throw new java.io.InterruptedIOException("BagIt process stopped");
        }
    }

    private static void requireArchives(List<String> archives) throws IOException {
        if (archives.isEmpty()) throw new IOException("Source batch has no BagIt archives");
        requireArchivesIfPresent(archives);
    }

    private static void requireArchivesIfPresent(List<String> archives) throws IOException {
        for (String archive : archives) {
            if (!new File(archive).isFile()) throw new IOException("BagIt archive no longer exists: " + archive);
        }
    }

    private static List<String> list(List<String> values) { return values == null ? List.of() : values; }
}
