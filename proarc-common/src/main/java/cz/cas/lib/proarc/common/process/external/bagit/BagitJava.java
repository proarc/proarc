package cz.cas.lib.proarc.common.process.external.bagit;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HexFormat;
import java.util.List;
import java.util.stream.Stream;

/** Creates local BagIt 1.0 bags with MD5 manifests, compatible with the existing export. */
final class BagitJava {

    private static final List<String> TAG_FILES = List.of("bagit.txt", "bag-info.txt", "manifest-md5.txt");

    void create(Path folder) throws IOException {
        Path root = folder.toAbsolutePath().normalize();
        if (!Files.isDirectory(root, LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Složka exportu BagIt není adresář: " + root);
        }
        if (Files.exists(root.resolve("bagit.txt"), LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Export již obsahuje bagit.txt: " + root);
        }
        // Validate before changing the export. Links and special files are not portable payloads.
        List<Path> payload = new ArrayList<>();
        Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                if (!attrs.isRegularFile()) {
                    throw new IOException("Nepodporovaný datový soubor BagIt (odkaz nebo speciální soubor): " + file);
                }
                payload.add(file);
                return FileVisitResult.CONTINUE;
            }
        });
        payload.sort(Comparator.comparing(path -> manifestPath(root.relativize(path))));
        List<Path> entries;
        try (Stream<Path> children = Files.list(root)) {
            entries = children.sorted().toList();
        }

        // Stage on the same filesystem: payloads are moved, never copied into memory or duplicated.
        Path staging = Files.createTempDirectory(root.getParent(), ".proarc-bagit-");
        Path data = Files.createDirectory(staging.resolve("data"));
        List<Path> moved = new ArrayList<>();
        List<Path> published = new ArrayList<>();
        try {
            writeMetadata(root, staging, payload);
            for (Path entry : entries) {
                Files.move(entry, data.resolve(entry.getFileName()));
                moved.add(entry);
            }
            List<String> outputs = new ArrayList<>(TAG_FILES);
            outputs.add("tagmanifest-md5.txt");
            outputs.add("data");
            for (String output : outputs) {
                Path target = root.resolve(output);
                Files.move(staging.resolve(output), target);
                published.add(target);
            }
        } catch (IOException ex) {
            // Restore the original layout on failure. If restoration fails, retain staging for recovery.
            try {
                for (int i = published.size() - 1; i >= 0; i--) {
                    Path target = published.get(i);
                    Files.move(target, staging.resolve(target.getFileName()));
                }
                for (int i = moved.size() - 1; i >= 0; i--) {
                    Path original = moved.get(i);
                    Files.move(data.resolve(original.getFileName()), original);
                }
                cleanup(staging);
            } catch (IOException recovery) {
                ex.addSuppressed(new IOException("Obnovení po chybě BagIt selhalo; soubory byly zachovány v " + staging, recovery));
            }
            throw ex;
        }
        Files.delete(staging);
    }

    private void writeMetadata(Path root, Path staging, List<Path> payload) throws IOException {
        long bytes = 0;
        try (BufferedWriter manifest = Files.newBufferedWriter(staging.resolve("manifest-md5.txt"), StandardCharsets.UTF_8)) {
            for (Path file : payload) {
                Checksum checksum = checksum(file);
                bytes += checksum.bytes;
                manifest.write(checksum.hex + "  data/" + manifestPath(root.relativize(file)) + "\n");
            }
        }
        Files.writeString(staging.resolve("bagit.txt"),
                "BagIt-Version: 1.0\nTag-File-Character-Encoding: UTF-8\n", StandardCharsets.UTF_8);
        Files.writeString(staging.resolve("bag-info.txt"),
                "Bagging-Date: " + LocalDate.now() + "\nBag-Software-Agent: ProArc\nPayload-Oxum: "
                        + bytes + "." + payload.size() + "\n", StandardCharsets.UTF_8);
        try (BufferedWriter tags = Files.newBufferedWriter(staging.resolve("tagmanifest-md5.txt"), StandardCharsets.UTF_8)) {
            for (String name : TAG_FILES) {
                tags.write(md5(staging.resolve(name)) + "  " + name + "\n");
            }
        }
    }

    private void cleanup(Path staging) throws IOException {
        for (String name : TAG_FILES) {
            Files.deleteIfExists(staging.resolve(name));
        }
        Files.deleteIfExists(staging.resolve("tagmanifest-md5.txt"));
        Files.delete(staging.resolve("data"));
        Files.delete(staging);
    }

    private static String manifestPath(Path relative) {
        String path = relative.toString().replace(relative.getFileSystem().getSeparator(), "/");
        return path.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A");
    }

    static String md5(Path file) throws IOException {
        return checksum(file).hex;
    }

    private static Checksum checksum(Path file) throws IOException {
        MessageDigest digest;
        try {
            digest = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException ex) {
            throw new IOException("MD5 není k dispozici", ex);
        }
        long bytes = 0;
        byte[] buffer = new byte[64 * 1024];
        try (InputStream input = Files.newInputStream(file)) {
            int count;
            while ((count = input.read(buffer)) != -1) {
                if (Thread.currentThread().isInterrupted()) {
                    throw new java.io.InterruptedIOException("Vytváření balíčku BagIt bylo přerušeno");
                }
                digest.update(buffer, 0, count);
                bytes += count;
            }
        }
        return new Checksum(HexFormat.of().formatHex(digest.digest()), bytes);
    }

    private record Checksum(String hex, long bytes) {
    }
}
