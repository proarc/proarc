/*
 * Copyright (C) 2012 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import javax.ws.rs.core.MediaType;

/**
 * Supports plaint text as Fedora data stream binary content.
 *
 * <p>Non XML content can be stored as managed or external but not inline.
 *
 * @author Jan Pokorsky
 */
public final class StringEditor {

    public static final String OCR_ID = "TEXT_OCR";
    public static final String OCR_LABEL = "OCR for this object";
    public static final String PRIVATE_NOTE_ID = "PRIVATE_NOTE";
    public static final String PRIVATE_NOTE_LABEL = "Private note for this object";
    public static final String OCR_ALTO_GEN_ID = "OCR_ALTO_GEN";

    private final XmlStreamEditor editor;
    private final FedoraObject object;

    public static StringEditor ocr(FedoraObject object) {
        return ocr(object, false);
    }

    public static StringEditor ocr(FedoraObject object, boolean storeExternally) {
        // inlined binary content ingest fails!
        return new StringEditor(object, ocrProfile());
    }

    public static DatastreamProfile ocrProfile() {
        return FoxmlUtils.managedProfile(OCR_ID, MediaType.TEXT_PLAIN_TYPE, OCR_LABEL);
    }

    public static StringEditor technical(FedoraObject object) {
        return technical(object, false);
    }

    public static StringEditor technical(FedoraObject object, boolean storeExternally) {
        return new StringEditor(object, technicalProfile());
    }

    public static DatastreamProfile technicalProfile() {
        return FoxmlUtils.managedProfile(OCR_ID, MediaType.TEXT_PLAIN_TYPE, OCR_LABEL);
    }

    public static StringEditor privateNote(FedoraObject object) {
        return new StringEditor(object, privateNoteProfile());
    }

    public static DatastreamProfile privateNoteProfile() {
        return FoxmlUtils.managedProfile(PRIVATE_NOTE_ID, MediaType.TEXT_PLAIN_TYPE, PRIVATE_NOTE_LABEL);
    }

    StringEditor(FedoraObject object, DatastreamProfile profile) {
        this(object.getEditor(profile), object);
    }

    StringEditor(XmlStreamEditor editor, FedoraObject object) {
        this.editor = editor;
        this.object = object;
    }

    public String read() throws DigitalObjectException {
        StringRecord r = readRecord();
        return r.getContent();
    }

    public StringRecord readRecord() throws DigitalObjectException {
        InputStream source = editor.readStream();
        StringRecord result;
        if (source != null) {
            try {
                String content = read(source);
                result = new StringRecord(content, getLastModified(), object.getPid());
            } catch (IOException ex) {
                throw new DigitalObjectException(object.getPid(), ex);
            }
        } else {
            result = new StringRecord("", -1, object.getPid());
        }
        return result;
    }

    public void write(String data, long timestamp, String message) throws DigitalObjectException {
        try {
            editor.write(data.getBytes("UTF-8"), timestamp, message);
        } catch (IOException ex) {
            throw new DigitalObjectException(object.getPid(), ex);
        }
    }

    /**
     * Copies file and normalizes line endings.
     */
    public static void copy(File source, String sourceCharset, File target, String targetCharset) throws IOException {
        BufferedReader reader = null;
        BufferedWriter writer = null;
        try {
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(source), sourceCharset));
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(target), targetCharset));
            for (String line; (line = reader.readLine()) != null; ) {
                writer.write(line);
                writer.write('\n');
            }
        } finally {
            FoxmlUtils.closeQuietly(reader, source.toString());
            writer.close();
        }
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    private static String read(Reader reader) throws IOException {
        if (reader == null) {
            return null;
        }
        StringBuilder sb = new StringBuilder(2048);
        try {
            for (int c; (c = reader.read()) != -1;) {
                sb.append((char) c);
            }
        } finally {
            reader.close();
        }
        return sb.toString();
    }

    static String read(InputStream stream) throws IOException {
        if (stream == null) {
            return null;
        }
        return read(new InputStreamReader(stream, "UTF-8"));
    }

    public static class StringRecord {

        private String pid;
        private Integer batchId;
        private long timestamp;
        private String content;

        public StringRecord() {
        }

        public StringRecord(String content, long timestamp, String pid) {
            this.content = content;
            this.timestamp = timestamp;
            this.pid = pid;
        }

        public Integer getBatchId() {
            return batchId;
        }

        public void setBatchId(Integer batchId) {
            this.batchId = batchId;
        }

        public String getContent() {
            return content;
        }

        public void setContent(String content) {
            this.content = content;
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
        }

        public long getTimestamp() {
            return timestamp;
        }

        public void setTimestamp(long timestamp) {
            this.timestamp = timestamp;
        }
    }

}
