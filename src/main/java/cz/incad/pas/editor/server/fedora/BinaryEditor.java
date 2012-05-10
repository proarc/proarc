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
import cz.incad.pas.editor.server.fedora.XmlStreamEditor.EditorResult;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import javax.ws.rs.core.MediaType;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 * Editor for managed binary content.
 * 
 * XXX implement RemoteObject support
 *
 * @author Jan Pokorsky
 */
public final class BinaryEditor {

    public static final String PREVIEW_ID = "IMG_PREVIEW";
    public static final String PREVIEW_LABEL = "Preview of this object";
    public static final String THUMB_ID = "IMG_THUMB";
    public static final String THUMB_LABEL = "Thumbnail of this object";
    public static final String FULL_ID = "IMG_FULL";
    public static final String FULL_LABEL = "Presentable version of RAW";
    public static final String RAW_ID = "IMG_RAW";
    public static final String RAW_LABEL = "Original digital content of this object";
    public static final MediaType IMAGE_JPEG = new MediaType("image", "jpeg");
    public static final MediaType IMAGE_TIFF = new MediaType("image", "tiff");

    private final XmlStreamEditor editor;
    private final FedoraObject object;

    public static BinaryEditor preview(FedoraObject object) {
        return new BinaryEditor(object, PREVIEW_ID, IMAGE_JPEG, PREVIEW_LABEL, ControlGroup.MANAGED);
    }

    public static BinaryEditor dissemination(FedoraObject object, String dsId) {
        return dissemination(object, dsId, IMAGE_JPEG);
    }

    public static BinaryEditor dissemination(FedoraObject object, String dsId, MediaType mime) {
        if (THUMB_ID.equals(dsId)) {
            return new BinaryEditor(object, dsId, mime, THUMB_LABEL, ControlGroup.MANAGED);
        } else if (PREVIEW_ID.equals(dsId)) {
            return new BinaryEditor(object, dsId, mime, PREVIEW_LABEL, ControlGroup.MANAGED);
        } else if (FULL_ID.equals(dsId)) {
            return new BinaryEditor(object, dsId, mime, FULL_LABEL, ControlGroup.MANAGED);
        } else if (RAW_ID.equals(dsId)) {
            return new BinaryEditor(object, dsId, mime, RAW_LABEL, ControlGroup.MANAGED);
        }
        return null;
    }

    public static BinaryEditor image(FedoraObject object, String dsId, String label) {
        return new BinaryEditor(object, dsId, IMAGE_JPEG, label, ControlGroup.MANAGED);
    }
    
    private static XmlStreamEditor createEditor(FedoraObject object, String dsId,
                MediaType mimetype, String label, ControlGroup control) {

        XmlStreamEditor editor;
        if (object instanceof LocalObject) {
            editor = new LocalXmlStreamEditor((LocalObject) object, dsId, mimetype, label, control, true);
        } else {
            throw new IllegalArgumentException("Unsupported fedora object: " + object.getClass());
        }
        return editor;
    }

    public BinaryEditor(FedoraObject object, String dsId,
                MediaType mimetype, String label, ControlGroup control) {
        this(createEditor(object, dsId, mimetype, label, control), object);
    }

    private BinaryEditor(XmlStreamEditor editor, FedoraObject object) {
        this.editor = editor;
        this.object = object;
    }

    public long getLastModified() {
        return editor.getLastModified();
    }

    public String getMimetype() {
        return editor.getMimetype();
    }

    public File read() {
        Source source = editor.read();
        if (source != null && !(source instanceof StreamSource)) {
            throw new IllegalStateException("Unsupported: " + source.getClass());
        }
        if (source != null) {
            StreamSource stream = (StreamSource) source;
            String systemId = stream.getSystemId();
            if (systemId != null) {
                URI uri = URI.create(systemId);
                return new File(uri);
            }
        }

        return null;
    }

    public void write(File data, long timestamp) throws IOException {
        EditorResult result = editor.createResult();
        if (!(result instanceof StreamResult)) {
            throw new IllegalStateException("Unsupported: " + result.getClass());
        }
        write((StreamResult) result, data);
        editor.write(result, timestamp);
    }
    private void write(StreamResult result, File data) throws IOException {
        result.setSystemId(data);
    }

    private static boolean write(OutputStream stream, InputStream data) throws IOException {
        throw new UnsupportedOperationException();
    }

}
