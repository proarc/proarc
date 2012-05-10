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
 * Editor for FOXML streams with XML content.
 *
 * @author Jan Pokorsky
 */
public interface XmlStreamEditor {

    /**
     * Creates a result for {@link #write write}.
     * @return
     */
    EditorResult createResult();

    long getLastModified();

    String getMimetype();

    Source read();

    void write(EditorResult data, long timestamp);

    /**
     * Makes modifications persistent. Use {@link FedoraObject#flush() }
     * in case of several editors.
     */
    void flush();

    /**
     * Helper interface for {@link XmlStreamEditor#write} to enforce compatible result.
     */
    public interface EditorResult extends Result {
    }

}
