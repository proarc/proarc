/*
 * Copyright (C) 2023 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.imports.replaceStream;

import cz.cas.lib.proarc.common.storage.AesEditor;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.CodingHistoryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.logging.Logger;
import javax.ws.rs.core.MediaType;

/**
 * The class that ingests files into storage
 *
 * @author Lukas Sykora
 */
public class FileIngest {

    private final FileReader.ImportSession iSession;
    private static final Logger LOG = Logger.getLogger(FileReader.class.getName());

    public FileIngest(FileReader.ImportSession iSession) {
        this.iSession = iSession;
    }

    public void ingest(File file, ImportProcess.ImportOptions context) {
        try {
            ingestImp(file, context);
        } catch (Exception ex) {
            throw new IllegalStateException(file.getAbsolutePath(), ex);
        }
    }

    private void ingestImp(File file, ImportProcess.ImportOptions context) throws DigitalObjectException, IOException {
        String pid = FileReader.getPid(file);

        ProArcObject fo = null;
        if (Storage.FEDORA.equals(iSession.getTypeOfStorage())) {
            fo = iSession.getRemotes().find(pid);
        } else if (Storage.AKUBRA.equals(iSession.getTypeOfStorage())) {
            fo = iSession.getAkubraStorage().find(pid);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + iSession.getTypeOfStorage());
        }

        boolean updated = updateDatastream(fo, file, context);
        if (updated) {
            fo.flush();
        } else {
            throw new IllegalStateException("Unimplemented type of dsId.");
        }
    }

    private boolean updateDatastream(ProArcObject fo, File file, ImportProcess.ImportOptions context) throws DigitalObjectException, IOException {
        String dsId = FileReader.toValidDsId(file, context);

        if (AltoDatastream.ALTO_ID.equals(dsId)) {
            AltoDatastream.importAlto(fo, file.toURI(), null);
            return true;
        } else if (StringEditor.OCR_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf(Files.probeContentType(file.toPath()));
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(StringEditor.OCR_ID, mime, StringEditor.OCR_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.RAW_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("image/tiff");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.RAW_ID, mime, BinaryEditor.RAW_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            MixEditor mixEditor = MixEditor.raw(fo);
            mixEditor.write(file, context.getJhoveContext(), mixEditor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.FULL_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("image/jpeg");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.FULL_ID, mime, BinaryEditor.FULL_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.NDK_ARCHIVAL_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("image/jp2");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.NDK_ARCHIVAL_ID, mime, BinaryEditor.NDK_ARCHIVAL_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            MixEditor mixEditor = MixEditor.raw(fo);
            mixEditor.write(file, context.getJhoveContext(), mixEditor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.NDK_USER_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("image/jp2");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.NDK_USER_ID, mime, BinaryEditor.NDK_USER_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.PREVIEW_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("image/jpeg");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.PREVIEW_ID, mime, BinaryEditor.PREVIEW_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.THUMB_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("image/jpeg");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.THUMB_ID, mime, BinaryEditor.THUMB_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.RAW_AUDIO_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("audio/wave");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.RAW_AUDIO_ID, mime, BinaryEditor.RAW_AUDIO_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            AesEditor aesEditor = AesEditor.raw(fo);
            aesEditor.write(file, context.getJhoveContext(), aesEditor.getLastModified(), null);
            CodingHistoryEditor codingHistoryEditor = CodingHistoryEditor.raw(fo);
            codingHistoryEditor.write(file, context.getJhoveContext(), codingHistoryEditor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.NDK_AUDIO_ARCHIVAL_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("audio/wave");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.NDK_AUDIO_ARCHIVAL_ID, mime, BinaryEditor.NDK_AUDIO_ARCHIVAL_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            AesEditor aesEditor = AesEditor.ndkArchival(fo);
            aesEditor.write(file, context.getJhoveContext(), aesEditor.getLastModified(), null);
            CodingHistoryEditor codingHistoryEditor = CodingHistoryEditor.ndkArchival(fo);
            codingHistoryEditor.write(file, context.getJhoveContext(), codingHistoryEditor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.NDK_AUDIO_ARCHIVAL_FLAC_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("audio/flac");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.NDK_AUDIO_ARCHIVAL_FLAC_ID, mime, BinaryEditor.NDK_AUDIO_ARCHIVAL_FLAC_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.NDK_AUDIO_USER_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("audio/mp3");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.NDK_AUDIO_USER_ID, mime, BinaryEditor.NDK_AUDIO_USER_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            return true;
        } else if (BinaryEditor.NDK_AUDIO_USER_OGG_ID.equals(dsId)) {
            MediaType mime = MediaType.valueOf("audio/ogg");
            BinaryEditor editor = BinaryEditor.dissemination(fo, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(fo, FoxmlUtils.managedProfile(BinaryEditor.NDK_AUDIO_USER_OGG_ID, mime, BinaryEditor.NDK_AUDIO_USER_OGG_LABEL));
            }
            editor.write(file, editor.getLastModified(), null);
            return true;
        }
        return false;
    }
}
