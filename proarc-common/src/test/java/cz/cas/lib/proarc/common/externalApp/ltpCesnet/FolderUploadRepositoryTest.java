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
package cz.cas.lib.proarc.common.externalApp.ltpCesnet;

import cz.cas.lib.proarc.common.externalApp.ltp.*;
import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.junit.jupiter.api.Assertions.*;

class FolderUploadRepositoryTest {
    @TempDir
    Path folder;

    @Test
    void historyRetainsIndependentSubmissionsAndUpdatesRetriesWithoutDuplicates() throws Exception {
        Path archive = Files.writeString(folder.resolve("test.zip"), "archive bytes");
        var repository = new FolderUploadRepository(folder, archive);
        for (String id : new String[] {"first", "second"}) {
            var record = new ArchivInfo();
            record.setOperationId(id);
            record.setProvider("cesnet");
            record.setState(ArchivInfo.State.UPLOADING);
            repository.save("folder", record);
            record.setState(ArchivInfo.State.UPLOADED);
            record.setPackageId(id + "-package");
            repository.save("folder", record);
        }
        JSONObject history = new JSONObject(Files.readString(folder.resolve(FolderUploadRepository.FILE_NAME)));
        var uploads = history.getJSONArray("uploads");
        assertEquals(2, uploads.length());
        assertEquals("first-package", uploads.getJSONObject(0).getJSONObject("record").getString("packageId"));
        assertEquals("UPLOADED", uploads.getJSONObject(1).getJSONObject("record").getString("state"));
        assertEquals(64, uploads.getJSONObject(0).getString("sha256").length());
        assertEquals("second-package", repository.read("folder").getPackageId());
        assertFalse(history.toString().contains("token"));
        try (var files = Files.list(folder)) {
            assertEquals(2, files.count());
        }
    }

    @Test
    void servicePersistsRegistrationBeforeUploadAndDoesNotUploadAgainAfterHistoryCommit() throws Exception {
        Path archive = Files.writeString(folder.resolve("test.zip"), "archive");
        var repository = new FolderUploadRepository(folder, archive);
        int[] submitted = {0};
        var service = new PreservationService(repository, record -> new PreservationProvider() {
            public RemotePackage submit(File file, JSONObject metadata, ArchivInfo current, RegistrationListener listener) throws IOException {
                submitted[0]++;
                listener.registered(new RemotePackage("remote-1", 1, ArchivInfo.State.UPLOADING, null, null));
                assertEquals("remote-1", repository.read("folder").getPackageId());
                return new RemotePackage("remote-1", 1, ArchivInfo.State.UPLOADED, null, null);
            }
            public RemotePackage check(String id) {
                throw new AssertionError();
            }
            public void close() { }
        }, (pid, record) -> fail("Folder uploads must not access a catalog"));
        var request = new ArchivInfo();
        request.setOperationId("operation");
        request.setProvider("cesnet");
        service.submit("folder", archive.toFile(), new JSONObject(), request);
        service.submit("folder", archive.toFile(), new JSONObject(), request);
        assertEquals(1, submitted[0]);
        assertEquals(1, new JSONObject(Files.readString(folder.resolve(FolderUploadRepository.FILE_NAME)))
                .getJSONArray("uploads").length());
    }

    @Test
    void corruptHistoryIsNeverOverwritten() throws Exception {
        Path history = Files.writeString(folder.resolve(FolderUploadRepository.FILE_NAME), "not json");
        var repository = new FolderUploadRepository(folder, null);
        var record = new ArchivInfo();
        record.setOperationId("operation");
        assertThrows(IOException.class, () -> repository.save("folder", record));
        assertEquals("not json", Files.readString(history));
    }
}
