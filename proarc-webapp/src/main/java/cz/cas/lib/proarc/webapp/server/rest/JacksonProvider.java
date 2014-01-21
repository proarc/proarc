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
package cz.cas.lib.proarc.webapp.server.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.ext.Provider;
import org.codehaus.jackson.jaxrs.JacksonJaxbJsonProvider;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;

/**
 * {@link JacksonJaxbJsonProvider} enables real JSON with JAXB annotation support.
 * Jackson handles arrays properly like {@code "items":[{"propName":"propValue"}]}
 * instead of {@code "items":{"propName":"propValue"}}
 * It is not necessary to declare arrays in context provider.
 *
 * @author Jan Pokorsky
 */
@Provider
@Consumes({MediaType.APPLICATION_JSON, "text/json"})
@Produces({MediaType.APPLICATION_JSON, "text/json"})
public final class JacksonProvider extends JacksonJaxbJsonProvider {

    private final SerializationConfig serializationConfig;
    private final DeserializationConfig deserializationConfig;

    public JacksonProvider() {
        // reuses XmlRootElement.name to adhere to XML structure
        configure(SerializationConfig.Feature.WRAP_ROOT_VALUE, true);
        configure(SerializationConfig.Feature.WRITE_NULL_PROPERTIES, false);
        configure(SerializationConfig.Feature.WRITE_DATES_AS_TIMESTAMPS, false);
        ObjectMapper mapper = _mapperConfig.getConfiguredMapper();
        serializationConfig = mapper.getSerializationConfig();
        deserializationConfig = mapper.getDeserializationConfig();

        registerAnnotatedSuperclass(AnnotatedAtmItem.class);
        registerAnnotatedSuperclass(AnnotatedBatchView.class);
        registerAnnotatedSuperclass(AnnotatedDescriptionMetadata.class);
        registerAnnotatedSuperclass(AnnotatedDevice.class);
        registerAnnotatedSuperclass(AnnotatedDublinCoreRecord.class);
//        registerAnnotatedSuperclass(AnnotatedMetaModel.class);
        registerAnnotatedSuperclass(AnnotatedPageViewItem.class);
        registerAnnotatedSuperclass(AnnotatedSearchViewItem.class);
        registerAnnotatedSuperclass(AnnotatedStringRecord.class);
    }

    private void register(Class<?> target, Class<?> mixinSource) {
        serializationConfig.addMixInAnnotations(target, mixinSource);
        deserializationConfig.addMixInAnnotations(target, mixinSource);
    }

    private void registerAnnotatedSuperclass(Class<?> mixinSource) {
        register(mixinSource.getSuperclass(), mixinSource);
    }

}
