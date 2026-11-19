package cz.cas.lib.proarc.common.process.external.bagit;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class BagitJavaTest {
    @TempDir
    Path temp;

    @Test
    void createsCompleteBagWithVerifiableChecksumsAndUtf8Paths() throws Exception {
        Path root = Files.createDirectory(temp.resolve("export"));
        Files.writeString(root.resolve("abc.txt"), "abc");
        Path nested = Files.createDirectories(root.resolve("data/prilis žluťoučký"));
        Files.write(nested.resolve("scan 100%.tif"), new byte[]{0, 1, 2, (byte) 255});
        Files.write(root.resolve(".empty"), new byte[0]);
        Files.createDirectory(root.resolve("empty-directory"));

        new BagitJava().create(root);

        assertEquals("BagIt-Version: 1.0\nTag-File-Character-Encoding: UTF-8\n", Files.readString(root.resolve("bagit.txt")));
        assertEquals("abc", Files.readString(root.resolve("data/abc.txt")));
        assertArrayEquals(new byte[]{0, 1, 2, (byte) 255}, Files.readAllBytes(root.resolve("data/data/prilis žluťoučký/scan 100%.tif")));
        assertTrue(Files.isDirectory(root.resolve("data/empty-directory")));
        assertTrue(Files.readString(root.resolve("bag-info.txt")).contains("Payload-Oxum: 7.3\n"));
        List<String> payload = Files.readAllLines(root.resolve("manifest-md5.txt"), StandardCharsets.UTF_8);
        assertEquals(3, payload.size());
        assertTrue(payload.contains("900150983cd24fb0d6963f7d28e17f72  data/abc.txt"));
        assertTrue(payload.stream().anyMatch(line -> line.endsWith("data/data/prilis žluťoučký/scan 100%25.tif")));
        verifyManifest(root, payload);
        List<String> tags = Files.readAllLines(root.resolve("tagmanifest-md5.txt"));
        assertEquals(3, tags.size());
        verifyManifest(root, tags);
        try (var entries = Files.list(temp)) {
            assertEquals(1, entries.count());
        }
    }

    @Test
    void streamsLargePayloadAndArchiveChecksum() throws Exception {
        Path root = Files.createDirectory(temp.resolve("export"));
        byte[] buffer = new byte[8192];
        MessageDigest expected = MessageDigest.getInstance("MD5");
        long length = 128L * 1024 * 1024;
        try (var output = Files.newOutputStream(root.resolve("large.tif"))) {
            for (long written = 0; written < length; written += buffer.length) {
                output.write(buffer);
                expected.update(buffer);
            }
        }
        String checksum = HexFormat.of().formatHex(expected.digest());
        new BagitJava().create(root);
        assertEquals(checksum + "  data/large.tif\n", Files.readString(root.resolve("manifest-md5.txt")));
        assertTrue(Files.readString(root.resolve("bag-info.txt")).contains("Payload-Oxum: " + length + ".1\n"));
        assertEquals(checksum, BagitJava.md5(root.resolve("data/large.tif")));
    }

    @Test
    void supportsEmptyPayload() throws Exception {
        Path root = Files.createDirectory(temp.resolve("export"));
        new BagitJava().create(root);
        assertTrue(Files.isDirectory(root.resolve("data")));
        assertEquals("", Files.readString(root.resolve("manifest-md5.txt")));
        assertTrue(Files.readString(root.resolve("bag-info.txt")).contains("Payload-Oxum: 0.0\n"));
        verifyManifest(root, Files.readAllLines(root.resolve("tagmanifest-md5.txt")));
    }

    @Test
    void refusesRebaggingWithoutChangingPayload() throws Exception {
        Path root = Files.createDirectory(temp.resolve("export"));
        Files.writeString(root.resolve("original"), "keep");
        BagitJava writer = new BagitJava();
        writer.create(root);
        String manifest = Files.readString(root.resolve("manifest-md5.txt"));
        assertThrows(IOException.class, () -> writer.create(root));
        assertEquals(manifest, Files.readString(root.resolve("manifest-md5.txt")));
        assertEquals("keep", Files.readString(root.resolve("data/original")));
    }

    @Test
    void refusesMissingFolder() {
        assertThrows(IOException.class, () -> new BagitJava().create(temp.resolve("missing")));
    }

    @Test
    void escapesNewlinesInUnixFilenames() throws Exception {
        assumeTrue("/".equals(temp.getFileSystem().getSeparator()));
        Path root = Files.createDirectory(temp.resolve("export"));
        Files.writeString(root.resolve("a\r\nb%0A"), "abc");
        new BagitJava().create(root);
        assertEquals("900150983cd24fb0d6963f7d28e17f72  data/a%0D%0Ab%250A\n",
                Files.readString(root.resolve("manifest-md5.txt")));
    }

    @Test
    void refusesLinksBeforeMovingAnyFiles() throws Exception {
        Path root = Files.createDirectory(temp.resolve("export"));
        Path original = Files.writeString(root.resolve("original"), "keep");
        try {
            Files.createSymbolicLink(root.resolve("link"), original);
        } catch (IOException | UnsupportedOperationException | SecurityException ex) {
            assumeTrue(false, "Symbolic links unavailable: " + ex);
        }
        assertThrows(IOException.class, () -> new BagitJava().create(root));
        assertEquals("keep", Files.readString(original));
        assertFalse(Files.exists(root.resolve("data")));
    }

    private static void verifyManifest(Path root, List<String> lines) throws Exception {
        for (String line : lines) {
            String[] entry = line.split("  ", 2);
            String filename = entry[1].replace("%0D", "\r").replace("%0A", "\n").replace("%25", "%");
            byte[] bytes = Files.readAllBytes(root.resolve(filename));
            assertEquals(HexFormat.of().formatHex(MessageDigest.getInstance("MD5").digest(bytes)), entry[0]);
        }
    }
}
