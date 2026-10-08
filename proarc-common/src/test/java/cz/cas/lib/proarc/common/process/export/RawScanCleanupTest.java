package cz.cas.lib.proarc.common.process.export;

import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RawScanCleanupTest {

    @TempDir
    Path temp;

    @Test
    void deletesEntireCopiedSourceFolderAndKeepsArchive() throws Exception {
        Path source = Files.createDirectory(temp.resolve("source"));
        Path destination = Files.createDirectory(temp.resolve("archive"));
        Path scan = Files.writeString(source.resolve("scan.tif"), "scan");
        Files.copy(scan, destination.resolve("scan.tif"));
        RawScanCleanup cleanup = new RawScanCleanup();
        cleanup.recordCopy(source);
        Files.writeString(scan, "changed");
        Path nested = Files.createDirectory(source.resolve("nested"));
        Files.writeString(nested.resolve("new.tif"), "new scan");
        Path otherSource = Files.createDirectory(temp.resolve("other-source"));
        Files.writeString(otherSource.resolve("scan.tif"), "other scan");

        cleanup.deleteCopiedFolders();

        assertFalse(Files.exists(source));
        assertTrue(Files.exists(destination.resolve("scan.tif")));
        assertTrue(Files.exists(otherSource.resolve("scan.tif")));
    }

    @Test
    void retainsSourceUntilCleanupAfterSuccessfulBagit() throws Exception {
        Path source = Files.createDirectory(temp.resolve("source"));
        Path scan = Files.writeString(source.resolve("scan.tif"), "scan");

        new RawScanCleanup().recordCopy(source);

        assertTrue(Files.exists(scan));
        assertTrue(Files.isDirectory(source));
    }
}
