package cz.cas.lib.proarc.common.process.export;

import java.io.IOException;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.Set;
import org.apache.commons.io.FileUtils;

/** Tracks successfully copied source folders, for cleanup after BAGIT succeeds. */
final class RawScanCleanup {

    private final Set<Path> copiedFolders = new LinkedHashSet<>();

    void recordCopy(Path source) {
        copiedFolders.add(source);
    }

    void deleteCopiedFolders() throws IOException {
        for (Path source : copiedFolders) {
            FileUtils.deleteDirectory(source.toFile());
        }
    }
}
