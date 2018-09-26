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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.introspect.AnnotationIntrospectorPair;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public final class JsonUtils {

    private static final Logger LOG = Logger.getLogger(JsonUtils.class.getName());
    private static ObjectMapper MAPPER;

    /**
     * Create a new instance of the mapper without mix in annotation registrations.
     * @return the new mapper
     */
    public static ObjectMapper createObjectMapper() {
        return createObjectMapper(new ObjectMapper());
    }

    /**
     * Configures an existing mapper with common settings.
     * @param om
     * @return
     */
    public static ObjectMapper createObjectMapper(ObjectMapper om) {
        // reuses XmlRootElement.name to adhere to XML structure
        om.configure(SerializationFeature.WRAP_ROOT_VALUE, true);
        om.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        om.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        om.configure(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY, true);
        om.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        om.configure(JsonParser.Feature.ALLOW_UNQUOTED_CONTROL_CHARS, true);
        return om;
    }

    /**
     * Creates a configured mapper supporting JAXB.
     * @see #createObjectMapper(com.fasterxml.jackson.databind.ObjectMapper)
     */
    public static ObjectMapper createJaxbMapper() {
        ObjectMapper om = createObjectMapper(createObjectMapper());
        JaxbAnnotationIntrospector jaxbIntr = new JaxbAnnotationIntrospector(om.getTypeFactory());
        JacksonAnnotationIntrospector jsonIntr = new JacksonAnnotationIntrospector();
        om.setAnnotationIntrospector(new AnnotationIntrospectorPair(jsonIntr, jaxbIntr));
        return om;
    }

    /**
     * Gets an object mapper that should be used preferably. It can contain registered
     * mix in annotation from other modules (see {@link #setDefaultObjectMapper} ).
     * @return the mapper
     */
    public static ObjectMapper defaultObjectMapper() {
        if (MAPPER != null) {
            return MAPPER;
        }
        LOG.warning("Using default JSON ObjectMapper!");
        ObjectMapper om = createJaxbMapper();
        // Use setDefaultObjectMapper in webapp tests!
        setDefaultObjectMapper(om);
        return om;
    }

    /**
     * Allows to inject the webapp mapper.
     * @param om mapper
     */
    public static void setDefaultObjectMapper(ObjectMapper om) {
        MAPPER = om;
    }

}
