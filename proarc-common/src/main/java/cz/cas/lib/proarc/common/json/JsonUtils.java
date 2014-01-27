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
package cz.cas.lib.proarc.common.json;

import java.util.logging.Logger;
import org.codehaus.jackson.map.AnnotationIntrospector;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.xc.JaxbAnnotationIntrospector;

/**
 *
 * @author Jan Pokorsky
 */
public final class JsonUtils {

    private static final Logger LOG = Logger.getLogger(JsonUtils.class.getName());
    private static ObjectMapper MAPPER;

    public static ObjectMapper defaultObjectMapper() {
        if (MAPPER != null) {
            return MAPPER;
        }
        LOG.warning("Using default JSON ObjectMapper!");
        ObjectMapper om = new ObjectMapper();
        // JaxbAnnotationIntrospector failes to handle enums.
        // It requires the itnrospector pair (JSON, JAXB) to mimic JacksonJaxbJsonProvider.
        // Use setDefaultObjectMapper in webapp tests!
        AnnotationIntrospector introspector = new JaxbAnnotationIntrospector();
        om.getSerializationConfig().setAnnotationIntrospector(introspector);
        om.getDeserializationConfig().setAnnotationIntrospector(introspector);
        setDefaultObjectMapper(om);
        return om;
    }

    public static void setDefaultObjectMapper(ObjectMapper om) {
        om.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        MAPPER = om;
    }

}
