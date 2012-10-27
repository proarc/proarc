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
     * @return
     */
    EditorResult createResult();

    long getLastModified() throws DigitalObjectException;

    String getMimetype() throws DigitalObjectException;

    /**
     * Provides content of the stream.
     *
     * @return source or {@code null} if stream not exist yet
     * @throws DigitalObjectNotFoundException object that should contain the stream not found
     * @throws DigitalObjectException general failure
     */
    Source read() throws DigitalObjectException;

    /**
     * Writes content to stream.
     *
     * @param data content {@link #createResult() holder}
     * @param timestamp time stamp
     * @throws DigitalObjectConcurrentModificationException
     *      stream has already changed. Reload and try again.
     * @throws DigitalObjectNotFoundException
     *      object that should contain the stream not found
     * @throws DigitalObjectException general failure
     * @see #createResult() to
     */
    void write(EditorResult data, long timestamp) throws DigitalObjectException;

    /**
     * Makes modifications persistent. Use {@link FedoraObject#flush() }
     * in case of several editors.
     * @throws DigitalObjectConcurrentModificationException
     *      stream has already changed. Reload and try again.
     * @throws DigitalObjectNotFoundException
     *      object that should contain the stream not found
     * @throws DigitalObjectException general failure
     */
    void flush() throws DigitalObjectException;

    /**
     * Helper interface for {@link XmlStreamEditor#write} to enforce compatible result.
     */
    public interface EditorResult extends Result {
    }

}
