/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.xml;

import java.util.HashMap;
import java.util.Iterator;
import javax.xml.namespace.NamespaceContext;

/**
 * The simple implementation of XML namespace mappings. Initialize the mapping
 * using {@link #add}.
 *
 * @author Jan Pokorsky
 */
public class SimpleNamespaceContext implements NamespaceContext {

    private final HashMap<String, String> map = new HashMap<String, String>();

    public SimpleNamespaceContext add(String prefix, String namespaceURI) {
        map.put(prefix, namespaceURI);
        return this;
    }

    @Override
    public String getNamespaceURI(String prefix) {
        return map.get(prefix);
    }

    @Override
    public String getPrefix(String namespaceURI) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Iterator<?> getPrefixes(String namespaceURI) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
