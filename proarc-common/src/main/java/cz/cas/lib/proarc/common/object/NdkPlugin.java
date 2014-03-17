/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.object;

import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.Mapping;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.codehaus.jackson.map.ObjectMapper;

/**
 * The plugin to support NDK digital objects.
 *
 * @author Jan Pokorsky
 */
public class NdkPlugin implements DigitalObjectPlugin, HasMetadataHandler<ModsType> {

    /**
     * The plugin ID.
     */
    public static final String ID = "ndk";

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public <T extends HasDataHandler> T getHandlerProvider(Class<T> type) {
        return type.isInstance(this) ? type.cast(this): null;
    }

    @Override
    public Collection<MetaModel> getModel() {
        // for now it is read only repository
        List<MetaModel> models = new ArrayList<MetaModel>();
        models.add(new MetaModel(
                "model:periodical", true, null,
                Arrays.asList(new ElementType("Periodical", "en"), new ElementType("Periodikum", "cs")),
                ModsConstants.NS,
                ModsCutomEditorType.EDITOR_PERIODICAL,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:periodicalvolume", null, null,
                Arrays.asList(new ElementType("Periodical Volume", "en"), new ElementType("Ročník", "cs")),
                ModsConstants.NS,
                ModsCutomEditorType.EDITOR_PERIODICAL_VOLUME,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:periodicalitem", null, null,
                Arrays.asList(new ElementType("Periodical Item", "en"), new ElementType("Výtisk", "cs")),
                ModsConstants.NS,
                ModsCutomEditorType.EDITOR_PERIODICAL_ISSUE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:monograph", true, null,
                Arrays.asList(new ElementType("Monograph", "en"), new ElementType("Monografie", "cs")),
                ModsConstants.NS,
                ModsCutomEditorType.EDITOR_MONOGRAPH,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:monographunit", null, null,
                Arrays.asList(new ElementType("Monograph Unit", "en"), new ElementType("Monografie - volná část", "cs")),
                ModsConstants.NS,
                ModsCutomEditorType.EDITOR_MONOGRAPH_UNIT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                "model:page", null, true,
                Arrays.asList(new ElementType("Page", "en"), new ElementType("Strana", "cs")),
                ModsConstants.NS,
                ModsCutomEditorType.EDITOR_PAGE,
                this,
                EnumSet.complementOf(EnumSet.of(DatastreamEditorType.CHILDREN))
                ));

        return models;
    }

    @Override
    public MetadataHandler<ModsType> createMetadataHandler(DigitalObjectHandler handler) {
        return new NdkMetadataHandler(handler);
    }

    @Override
    public List<ValueMap> getValueMaps(UserProfile user) {
        return Collections.emptyList();
    }

    static class NdkMetadataHandler implements MetadataHandler<ModsType> {

        private static final Logger LOG = Logger.getLogger(NdkMetadataHandler.class.getName());
        private final DigitalObjectHandler handler;
        private final ModsStreamEditor editor;
        private final FedoraObject fobject;

        public NdkMetadataHandler(DigitalObjectHandler handler) {
            this.handler = handler;
            this.fobject = handler.getFedoraObject();
            XmlStreamEditor streamEditor = fobject.getEditor(FoxmlUtils.inlineProfile(
                    DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, DESCRIPTION_DATASTREAM_LABEL));
            this.editor = new ModsStreamEditor(streamEditor, fobject);
        }

        @Override
        public void setMetadata(DescriptionMetadata<ModsType> data, String message) throws DigitalObjectException {
            ModsType mods = data.getData();
            String modelId = handler.relations().getModel();
            if (mods == null) {
                mods = createDefault(modelId);
            }
            write(modelId, mods, data.getTimestamp(), message);
        }

        private ModsType createDefault(String modelId) throws DigitalObjectException {
            return ModsStreamEditor.create(fobject.getPid(), modelId);
        }

        @Override
        public void setMetadataAsJson(DescriptionMetadata<String> jsonData, String message) throws DigitalObjectException {
            String json = jsonData.getData();
            String editorId = jsonData.getEditor();
            String modelId = handler.relations().getModel();
            ModsType mods = json == null ? createDefault(modelId)
                    : editor.read();
            Mapping mapping = new Mapping();
            Class<?> type = mapping.getType(editorId);
            ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
            Object customData;
            try {
                customData = jsMapper.readValue(json, type);
            } catch (Exception ex) {
                throw new DigitalObjectException(fobject.getPid(), null, ModsStreamEditor.DATASTREAM_ID, null, ex);
            }
            mapping.update(mods, customData, editorId);
            if (LOG.isLoggable(Level.FINE)) {
                String toXml = ModsUtils.toXml(mods, true);
                LOG.fine(toXml);
            }
            write(modelId, mods, jsonData.getTimestamp(), message);
        }

        @Override
        public void setMetadataAsXml(DescriptionMetadata<String> xmlData, String message) throws DigitalObjectException {
            ModsType mods;
            String modelId = handler.relations().getModel();
            if (xmlData.getData() != null) {
                mods = ModsStreamEditor.create(fobject.getPid(), modelId, xmlData.getData());
            } else {
                mods = createDefault(modelId);
            }
            write(modelId, mods, xmlData.getTimestamp(), message);
        }

        @Override
        public DescriptionMetadata<ModsType> getMetadata() throws DigitalObjectException {
            ModsType mods = editor.read();
            DescriptionMetadata<ModsType> dm = new DescriptionMetadata<ModsType>();
            dm.setPid(fobject.getPid());
            dm.setTimestamp(editor.getLastModified());
//            dm.setEditor(editorId);
            dm.setData(mods);
            return dm;
        }

        @Override
        public <O> DescriptionMetadata<O> getMetadataAsJsonObject(String mappingId) throws DigitalObjectException {
            if (mappingId == null) {
                throw new NullPointerException("mappingId");
            }
            ModsType mods = editor.read();
            Mapping mapping = new Mapping();
            @SuppressWarnings("unchecked")
            O customMods = (O) mapping.read(mods, mappingId);

            DescriptionMetadata<O> dm = new DescriptionMetadata<O>();
            dm.setPid(fobject.getPid());
            dm.setTimestamp(editor.getLastModified());
            dm.setEditor(mappingId);
            dm.setData(customMods);
            return dm;
        }

        @Override
        public DescriptionMetadata<String> getMetadataAsXml() throws DigitalObjectException {
            String xml = editor.readAsString();
            DescriptionMetadata<String> dm = new DescriptionMetadata<String>();
            dm.setPid(fobject.getPid());
            dm.setTimestamp(editor.getLastModified());
//            dm.setEditor(editorId);
            dm.setData(xml);
            return dm;
        }

        private void write(String modelId, ModsType mods, long timestamp, String message) throws DigitalObjectException {
            editor.write(mods, timestamp, message);

            // DC
            DcStreamEditor dcEditor = handler.objectMetadata();
            dcEditor.write(handler, mods, modelId, dcEditor.getLastModified(), message);

            // Label
            String label = ModsUtils.getLabel(mods, modelId);
            fobject.setLabel(label);
        }
    }

}
