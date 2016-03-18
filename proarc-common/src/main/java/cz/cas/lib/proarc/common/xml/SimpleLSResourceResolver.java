/*
 * Copyright (C) 2016 Jan Pokorsky
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

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSInput;
import org.w3c.dom.ls.LSResourceResolver;

/**
 * The simple implementation of the resource mapping. It tries to resolve
 * systemId of a resource against {@link #map(java.lang.String, java.lang.String) mappings}
 * and {@link #base(java.lang.Class) class package resources}.
 *
 * @author Jan Pokorsky
 */
public class SimpleLSResourceResolver implements LSResourceResolver {

    private final DOMImplementationLS dls;
    private final Map<String, String> map;
    private ArrayList<Class> bases;

    public SimpleLSResourceResolver() {
        try {
            dls = (DOMImplementationLS) DOMImplementationRegistry.newInstance().getDOMImplementation("LS");
            map = new HashMap<String, String>();
            bases = new ArrayList<Class>();
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
    }

    public SimpleLSResourceResolver base(Class<?> clazz) {
        bases.add(clazz);
        return this;
    }

    public SimpleLSResourceResolver map(String systemId, Class<?> clazz, String resourcePath) {
        return map(systemId, clazz.getResource(resourcePath).toExternalForm());
    }

    public SimpleLSResourceResolver map(String systemId, String resourcePath) {
        map.put(systemId, resourcePath);
        return this;
    }

    @Override
    public LSInput resolveResource(String type, String namespaceURI, String publicId, String systemId, String baseURI) {
        String location = map.get(systemId);
//        System.out.printf("\ntype: %s\nnsURI: %s\npublicId: %s\nsystemId: %s\nbaseURI: %s\n",
//                type, namespaceURI, publicId, systemId, baseURI);
        if (location == null) {
            for (Class<?> base : bases) {
                URL resource = base.getResource(systemId);
                if (resource != null) {
                    systemId = resource.toExternalForm();
                    break;
                }
            }
        }
        LSInput input = dls.createLSInput();
        input.setSystemId(systemId);
//        System.out.println("Real system ID: " + systemId);
        return input;
    }

}
