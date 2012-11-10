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
package cz.incad.pas.editor.server.fedora;

import cz.incad.pas.editor.server.fedora.FoxmlUtils.ControlGroup;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalXmlStreamEditor;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteXmlStreamEditor;
import cz.incad.pas.editor.server.fedora.XmlStreamEditor.EditorResult;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

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

    private final XmlStreamEditor editor;
    private final FedoraObject object;

    public static StringEditor ocr(FedoraObject object) {
//        return new StringEditor(object, OCR_ID, MediaType.TEXT_PLAIN_TYPE, OCR_LABEL, ControlGroup.INLINE);
        // inlined binary content ingest fails!
        return new StringEditor(object, OCR_ID, MediaType.TEXT_PLAIN_TYPE, OCR_LABEL, ControlGroup.MANAGED);
    }

    public static StringEditor privateNote(FedoraObject object) {
        return new StringEditor(object, PRIVATE_NOTE_ID, MediaType.TEXT_PLAIN_TYPE, PRIVATE_NOTE_LABEL, ControlGroup.MANAGED);
    }

    private static XmlStreamEditor createEditor(FedoraObject object, String dsId,
                MediaType mimetype, String label, ControlGroup control) {

        XmlStreamEditor editor;
        if (object instanceof LocalStorage.LocalObject) {
            editor = new LocalXmlStreamEditor((LocalObject) object, dsId, mimetype, label, control);
        } else if (object instanceof RemoteObject) {
            editor = new RemoteXmlStreamEditor(
                    (RemoteObject) object,
                    RemoteXmlStreamEditor.managedProfile(dsId, mimetype, label));
        } else {
            throw new IllegalArgumentException("Unsupported fedora object: " + object.getClass());
        }
        return editor;
    }
    
    public StringEditor(FedoraObject object, String dsId,
                MediaType mimetype, String label, ControlGroup control) {
        this(createEditor(object, dsId, mimetype, label, control), object);
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
        Source source = editor.read();
        if (source != null && !(source instanceof StreamSource)) {
            throw new IllegalStateException("Unsupported: " + source.getClass());
        }
        StringRecord result;
        if (source != null) {
            try {
                StreamSource stream = (StreamSource) source;
                String content = read(stream);
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
        EditorResult result = editor.createResult();
        if (!(result instanceof StreamResult)) {
            throw new DigitalObjectException(object.getPid(), "Unsupported: " + result.getClass());
        }
        try {
            write((StreamResult) result, data);
            editor.write(result, timestamp, message);
        } catch (IOException ex) {
            throw new DigitalObjectException(object.getPid());
        }
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    private void write(StreamResult result, String data) throws IOException, DigitalObjectException {
        if (!write(result.getOutputStream(), data)) {
            if (!write(result.getWriter(), data)) {
                throw new DigitalObjectException(object.getPid(), "Data not written: " + this.object.getPid());
            }
        }
    }

    private static boolean write(OutputStream stream, String data) throws IOException {
        if (stream != null) {
            return write(new OutputStreamWriter(stream, "UTF-8"), data);
        }
        return false;
    }

    private static boolean write(Writer writer, String data) throws IOException {
        if (writer != null) {
            try {
                writer.write(data);
                return true;
            } finally {
                writer.close();
            }
        }
        return false;
    }

    static String read(StreamSource source) throws IOException {
        String result;
        result = read(source.getInputStream());
        if (result == null) {
            result = read(source.getReader());
        }
        if (result == null) {
            result = read(source.getSystemId());
        }
        return result;
    }

    private static String read(Reader reader) throws IOException {
        if (reader == null) {
            return null;
        }
        StringBuilder sb = new StringBuilder(2048);
        try {
            for (int c = 0; (c = reader.read()) != -1;) {
                sb.append((char) c);
            }
        } finally {
            reader.close();
        }
        return sb.toString();
    }

    private static String read(InputStream stream) throws IOException {
        if (stream == null) {
            return null;
        }
        return read(new InputStreamReader(stream, "UTF-8"));
    }

    private static String read(String systemId) throws IOException {
        if (systemId == null) {
            return null;
        }
        URL url = new URL(systemId);
        InputStream is = url.openStream();
        return read(new BufferedInputStream(is));
    }

    @XmlRootElement(name = DigitalObjectResourceApi.STRINGRECORD_ELEMENT)
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class StringRecord {

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_PID)
        private String pid;
        
        @XmlElement(name = DigitalObjectResourceApi.BATCHID_PARAM)
        private Integer batchId;

        @XmlElement(name = DigitalObjectResourceApi.TIMESTAMP_PARAM)
        private long timestamp;

        @XmlElement(name = DigitalObjectResourceApi.STRINGRECORD_CONTENT)
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
