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
package cz.cas.lib.proarc.common.process.external.bagit;

import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import cz.cas.lib.proarc.common.externalApp.ltpCesnet.FolderUploadRepository;

/** Resolves sources against the owner's import root and copies payloads without following links. */
public final class ImportFolderSource {

    private ImportFolderSource() {
    }

    public static Path resolve(UserProfile user, String relative) throws IOException {
        if (relative == null || relative.isBlank()) {
            throw new IOException("Vyberte podsložku importu.");
        }
        Path path = Path.of(relative);
        if (path.isAbsolute()) {
            throw new IOException("Očekávána relativní cesta importu.");
        }
        for (Path part : path) {
            if ("..".equals(part.toString())) {
                throw new IOException("Neplatná cesta importu.");
            }
        }
        return validate(user, Path.of(user.getImportFolder()).resolve(path));
    }

    public static Path validate(UserProfile user, Path directory) throws IOException {
        Path root = Path.of(user.getImportFolder()).toRealPath();
        Path source = directory.toRealPath();
        if (source.equals(root) || !source.startsWith(root) || !Files.isDirectory(source)) {
            throw new IOException("Zdroj musí být podsložkou importního adresáře uživatele.");
        }
        return source;
    }

    public static void copy(Path source, Path destination) throws IOException {
        Files.walkFileTree(source, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                checkInterrupted();
                Files.createDirectories(destination.resolve(source.relativize(dir)));
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                checkInterrupted();
                if (!attrs.isRegularFile()) {
                    throw new IOException("Odkazy a speciální soubory nelze zabalit: " + file);
                }
                String name = file.getFileName().toString();
                if (!name.equals(FolderUploadRepository.FILE_NAME) && !name.startsWith(FolderUploadRepository.TEMP_PREFIX)) {
                    Files.copy(file, destination.resolve(source.relativize(file)), StandardCopyOption.COPY_ATTRIBUTES);
                }
                return FileVisitResult.CONTINUE;
            }
        });
    }

    private static void checkInterrupted() throws InterruptedIOException {
        if (Thread.currentThread().isInterrupted()) {
            throw new InterruptedIOException("Kopírování bylo zastaveno.");
        }
    }
}
