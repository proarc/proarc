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
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import java.io.File;
import java.net.URI;
import javax.ws.rs.core.MediaType;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

/**
 * Editor for managed binary content.
 * 
 * XXX implement RemoteObject support
 *
 * @author Jan Pokorsky
 */
public final class BinaryEditor {

    public static final String PREVIEW_ID = "PREVIEW";
    public static final String PREVIEW_LABEL = "Preview of this object";
    public static final String THUMB_ID = "THUMBNAIL";
    public static final String THUMB_LABEL = "Thumbnail of this object";
    public static final String FULL_ID = "FULL";
    public static final String FULL_LABEL = "Presentable version of RAW";
    public static final String RAW_ID = "RAW";
    public static final String RAW_LABEL = "Original digital content of this object";
    public static final MediaType IMAGE_JPEG = new MediaType("image", "jpeg");
    public static final MediaType IMAGE_TIFF = new MediaType("image", "tiff");

    private final XmlStreamEditor editor;
    private final FedoraObject object;

    public static BinaryEditor preview(FedoraObject object) {
        DatastreamProfile dp = FoxmlUtils.managedProfile(PREVIEW_ID, IMAGE_JPEG, PREVIEW_LABEL);
        return new BinaryEditor(object, dp);
    }

    public static BinaryEditor dissemination(FedoraObject object, String dsId) {
        return dissemination(object, dsId, IMAGE_JPEG);
    }

    public static BinaryEditor dissemination(FedoraObject object, String dsId, MediaType mime) {
        DatastreamProfile dp;
        if (THUMB_ID.equals(dsId)) {
            dp = FoxmlUtils.managedProfile(dsId, mime, THUMB_LABEL);
        } else if (PREVIEW_ID.equals(dsId)) {
            dp = FoxmlUtils.managedProfile(dsId, mime, PREVIEW_LABEL);
        } else if (FULL_ID.equals(dsId)) {
            dp = FoxmlUtils.managedProfile(dsId, mime, FULL_LABEL);
        } else if (RAW_ID.equals(dsId)) {
            dp = FoxmlUtils.managedProfile(dsId, mime, RAW_LABEL);
        } else {
            return null;
        }
        return new BinaryEditor(object, dp);
    }

    public BinaryEditor(FedoraObject object, DatastreamProfile profile) {
        if (!(object instanceof LocalObject)) {
            throw new IllegalArgumentException("Unsupported fedora object: " + object.getClass());
        }
        this.editor = object.getEditor(profile);
        this.object = object;
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    public DatastreamProfile getProfile() throws DigitalObjectException {
        return editor.getProfile();
    }

    public void setProfile(DatastreamProfile profile) throws DigitalObjectException {
        editor.setProfile(profile);
    }

    public File read() throws DigitalObjectException {
        Source source = editor.read();
        if (source != null && !(source instanceof StreamSource)) {
            throw new DigitalObjectException(object.getPid(), "Unsupported: " + source.getClass());
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

    public void write(File data, long timestamp, String message) throws DigitalObjectException {
        editor.write(data.toURI(), timestamp, message);
    }

}
