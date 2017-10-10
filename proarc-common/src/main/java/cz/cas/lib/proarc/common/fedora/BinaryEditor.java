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
import java.io.File;
import java.net.URI;
import java.util.Arrays;
import java.util.List;
import javax.ws.rs.core.MediaType;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

/**
 * Editor for managed binary content.
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
    /** ID of the optional data stream to hold a NDK archive copy of RAW. E.g. lossless JPEG 2000. */
    public static final String NDK_ARCHIVAL_ID = "NDK_ARCHIVAL";
    public static final String NDK_ARCHIVAL_LABEL = "NDK archive copy of RAW";
    /** ID of the optional data stream to hold a NDK production copy of RAW. E.g. lossy JPEG 2000. */
    public static final String NDK_USER_ID = "NDK_USER";
    public static final String NDK_USER_LABEL = "NDK user copy of RAW";

    public static final MediaType IMAGE_JP2 = new MediaType("image", "jp2");
    public static final MediaType IMAGE_JPEG = new MediaType("image", "jpeg");
    public static final MediaType IMAGE_TIFF = new MediaType("image", "tiff");
    /**
     * Data stream IDs with binary contents.
     */
    private static final List<String> MEDIA_DS_IDS = Arrays.asList(
            BinaryEditor.FULL_ID, BinaryEditor.PREVIEW_ID, BinaryEditor.RAW_ID,
            BinaryEditor.THUMB_ID, BinaryEditor.NDK_ARCHIVAL_ID, BinaryEditor.NDK_USER_ID);

    private final XmlStreamEditor editor;
    private final FedoraObject object;
    private final String dsId;

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
        } else if (NDK_ARCHIVAL_ID.equals(dsId)) {
            dp = FoxmlUtils.managedProfile(dsId, mime, NDK_ARCHIVAL_LABEL);
        } else if (NDK_USER_ID.equals(dsId)) {
            dp = FoxmlUtils.managedProfile(dsId, mime, NDK_USER_LABEL);
        } else {
            return null;
        }
        return new BinaryEditor(object, dp);
    }

    /**
     * Is the data stream holding media content?
     */
    public static boolean isMediaStream(String dsId) {
        return MEDIA_DS_IDS.contains(dsId);
    }

    public BinaryEditor(FedoraObject object, DatastreamProfile profile) {
        this.editor = object.getEditor(profile);
        this.object = object;
        this.dsId = profile.getDsID();
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
            throw new DigitalObjectException(object.getPid(), null, dsId,
                    "Unsupported: " + source.getClass(), null);
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

    /**
     * Writes source contents or just a location according to the stream profile
     * according to the actual profile control value (external/managed).
     *
     * @param source a contents location. The contents must be accessible
     *              even in case of an external link!
     * @param timestamp
     * @param message
     * @throws DigitalObjectException failure
     */
    public void write(URI source, long timestamp, String message) throws DigitalObjectException {
        editor.write(source, timestamp, message);
    }
}
