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

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo;
import cz.cas.lib.proarc.common.externalApp.ltp.PreservationService;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Instant;
import org.apache.commons.codec.digest.DigestUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/** Atomic, credential-free history stored beside an import folder's payload. */
public final class FolderUploadRepository implements PreservationService.Repository {

    public static final String FILE_NAME = "cesnet-upload.json";
    public static final String TEMP_PREFIX = ".cesnet-upload-";
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private final Path folder;
    private final Path archive;

    public FolderUploadRepository(Path folder, Path archive) {
        this.folder = folder;
        this.archive = archive;
    }

    private JSONObject load() throws IOException {
        Path file = folder.resolve(FILE_NAME);
        if (Files.isSymbolicLink(file)) {
            throw new IOException("Historie CESNET nesmí být odkaz.");
        }
        if (!Files.exists(file)) {
            return new JSONObject().put("schemaVersion", 1).put("uploads", new JSONArray());
        }
        try {
            JSONObject history = new JSONObject(Files.readString(file));
            if (history.getInt("schemaVersion") != 1) {
                throw new IOException("Nepodporovaná verze historie CESNET.");
            }
            history.getJSONArray("uploads");
            return history;
        } catch (JSONException ex) {
            throw new IOException("Nelze načíst historii CESNET.", ex);
        }
    }

    @Override
    public ArchivInfo read(String identity) throws IOException {
        JSONArray uploads = load().getJSONArray("uploads");
        return uploads.isEmpty() ? null : MAPPER.readValue(uploads.getJSONObject(uploads.length() - 1)
                .getJSONObject("record").toString(), ArchivInfo.class);
    }

    @Override
    public void save(String identity, ArchivInfo record) throws IOException {
        JSONObject history = load();
        JSONArray uploads = history.getJSONArray("uploads");
        JSONObject entry = null;
        for (int i = 0; i < uploads.length(); i++) {
            JSONObject candidate = uploads.getJSONObject(i);
            if (record.getOperationId().equals(candidate.getJSONObject("record").optString("operationId"))) {
                entry = candidate;
            }
        }
        if (entry == null) {
            entry = new JSONObject();
            if (archive != null) {
                entry.put("archive", archive.getFileName().toString());
                try (var input = Files.newInputStream(archive)) {
                    entry.put("sha256", DigestUtils.sha256Hex(input));
                }
                entry.put("size", Files.size(archive));
            }
            uploads.put(entry);
        }
        entry.put("record", new JSONObject(MAPPER.writeValueAsString(record)));
        entry.put("updatedAt", Instant.now().toString());
        if ((record.getState() == ArchivInfo.State.UPLOADED || record.getState() == ArchivInfo.State.STORED)
                && !entry.has("uploadedAt")) {
            entry.put("uploadedAt", Instant.now().toString());
        }
        Path temporary = Files.createTempFile(folder, TEMP_PREFIX, ".json");
        try {
            Files.writeString(temporary, history.toString(2) + "\n");
            Files.move(temporary, folder.resolve(FILE_NAME), StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }
}
