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

import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse.AnnotatedSmartGwtResponse;
import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.ext.Provider;
import javax.xml.bind.annotation.adapters.XmlAdapter;
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
        ObjectMapper mapper = _mapperConfig.getDefaultMapper();
        mapper = JsonUtils.createObjectMapper(mapper);
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
        registerAnnotatedSuperclass(AnnotatedSmartGwtResponse.class);
        registerAnnotatedSuperclass(AnnotatedStringRecord.class);
        registerAnnotatedSuperclass(AnnotatedUser.class);
        registerAnnotatedSuperclass(AnnotatedValueMap.class);

        registerAnnotatedSuperclass(AnnotatedMods.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedAbstractDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedAccessConditionDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedCopyInformationDefinitionNote.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedLanguageDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedLocationDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedNameDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedNoteDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedOriginInfoDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedPartDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedPhysicalDescriptionDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedPhysicalDescriptionNote.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedPhysicalLocationDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedRecordInfoDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedRelatedItemDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedStringPlusLanguage.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedSubjectDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedSubjectNameDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedSubjectTitleInfoDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedTableOfContentsDefinition.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedText.class);
        registerAnnotatedSuperclass(AnnotatedMods.AnnotatedTitleInfoDefinition.class);
        JsonUtils.setDefaultObjectMapper(mapper);
    }

    private void register(Class<?> target, Class<?> mixinSource) {
        serializationConfig.addMixInAnnotations(target, mixinSource);
        deserializationConfig.addMixInAnnotations(target, mixinSource);
    }

    private void registerAnnotatedSuperclass(Class<?> mixinSource) {
        register(mixinSource.getSuperclass(), mixinSource);
    }

    /**
     * The helper class that can override a declared XML adapter with the default JAXB mapping.
     */
    public static class DefaultAdapter extends XmlAdapter<Object, Object> {

        @Override
        public Object unmarshal(Object v) throws Exception {
            return v;
        }

        @Override
        public Object marshal(Object v) throws Exception {
            return v;
        }

    }

}
