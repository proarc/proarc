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
package cz.cas.lib.proarc.common.storage;

import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import java.io.InputStream;
import java.net.URI;
import javax.xml.transform.Result;
import javax.xml.transform.Source;

/**
 * Editor for FOXML streams.
 *
 * @author Jan Pokorsky
 */
public interface XmlStreamEditor {

    /**
     * Creates a result for {@link #write write}.
     *
     * @return
     */
    EditorResult createResult();

    long getLastModified() throws DigitalObjectException;

    /**
     * Gets subset of {@link DatastreamProfile} properties for
     * {@link #setProfile setProfile}. dsId, dsLabel, dsCreateDate,
     * dsFormatURI and dsMIME are guaranteed.
     *
     * @return the profile
     * @throws DigitalObjectException failure
     */
    DatastreamProfile getProfile() throws DigitalObjectException;

    /**
     * Sets label, format or MIME. At least MIME requires to change also
     * the stream contents.
     *
     * @param profile {@link #getProfile}
     * @throws DigitalObjectException failure
     */
    void setProfile(DatastreamProfile profile) throws DigitalObjectException;

    /**
     * Provides content of the stream.
     *
     * @return source or {@code null} if stream not exist yet
     * @throws DigitalObjectNotFoundException object that should contain the stream not found
     * @throws DigitalObjectException         general failure
     */
    Source read() throws DigitalObjectException;

    /**
     * Provides content of the data stream as InputStream.
     *
     * @return stream or {@code null} if stream not exist yet
     * @throws DigitalObjectNotFoundException object that should contain the stream not found
     * @throws DigitalObjectException         general failure
     */
    InputStream readStream() throws DigitalObjectException;

    /**
     * Writes XML content to stream.
     *
     * @param data      content {@link #createResult() holder}
     * @param timestamp time stamp
     * @param message   log message
     * @throws DigitalObjectConcurrentModificationException stream has already changed. Reload and try again.
     * @throws DigitalObjectNotFoundException               object that should contain the stream not found
     * @throws DigitalObjectException                       general failure
     * @see #createResult() to
     */
    void write(EditorResult data, long timestamp, String message) throws DigitalObjectException;

    /**
     * Writes binary contents to stream.
     *
     * @see #write(cz.cas.lib.proarc.common.storage.XmlStreamEditor.EditorResult, long, java.lang.String)
     */
    void write(byte[] data, long timestamp, String message) throws DigitalObjectException;

    /**
     * Writes reference to contents to stream.
     *
     * @see #write(cz.cas.lib.proarc.common.storage.XmlStreamEditor.EditorResult, long, java.lang.String)
     */
    void write(URI data, long timestamp, String message) throws DigitalObjectException;

    /**
     * Writes binary contents to stream.
     *
     * @see #write(cz.cas.lib.proarc.common.storage.XmlStreamEditor.EditorResult, long, java.lang.String)
     */
    void write(InputStream data, long timestamp, String message) throws DigitalObjectException;

    /**
     * Makes modifications persistent. Use {@link ProArcObject#flush() }
     * in case of several editors.
     *
     * @throws DigitalObjectConcurrentModificationException stream has already changed. Reload and try again.
     * @throws DigitalObjectNotFoundException               object that should contain the stream not found
     * @throws DigitalObjectException                       general failure
     */
    void flush() throws DigitalObjectException;

    /**
     * Helper interface for {@link XmlStreamEditor#write} to enforce compatible result.
     */
    public interface EditorResult extends Result {
    }

}
