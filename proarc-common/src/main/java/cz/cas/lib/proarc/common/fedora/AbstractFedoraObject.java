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

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Default implementation of {@link FedoraObject}.
 *
 * @author Jan Pokorsky
 */
public abstract class AbstractFedoraObject implements FedoraObject {

    private final String pid;
//    private final Map<Class<?>, XmlStreamEditor> editors = new HashMap<Class<?>, XmlStreamEditor>();
    private final Set<XmlStreamEditor> editors = new LinkedHashSet<XmlStreamEditor>();

    public AbstractFedoraObject(String pid) {
        this.pid = pid;
    }

    @Override
    public final String getPid() {
        return pid;
    }

    @Override
    public final void register(XmlStreamEditor editor) {
        editors.add(editor);
    }

//        public <T extends XmlStreamEditor> T getEditor(Class<T> type) {
//            return (T) editors.get(type);
//        }

    @Override
    public void flush() throws DigitalObjectException {
        // write changes
        for (XmlStreamEditor editor : editors) {
            editor.flush();
        }
    }

    @Override
    public void purgeDatastream(String datastream, String logMessage) throws DigitalObjectException {
        RemoteStorage.getInstance().find(pid).purgeDatastream(datastream, logMessage);
    }
}
