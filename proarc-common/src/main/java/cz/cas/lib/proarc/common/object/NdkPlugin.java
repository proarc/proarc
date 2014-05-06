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
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.stream.StreamSource;
import org.codehaus.jackson.map.ObjectMapper;

/**
 * The plugin to support NDK digital objects.
 *
 * @author Jan Pokorsky
 */
public class NdkPlugin implements DigitalObjectPlugin, HasMetadataHandler<ModsDefinition> {

    /**
     * The plugin ID.
     */
    public static final String ID = "ndk";
    public static final String MODEL_PERIODICAL = "model:ndkperiodical";
    public static final String MODEL_PERIODICALVOLUME = "model:ndkperiodicalvolume";
    public static final String MODEL_PERIODICALISSUE = "model:ndkperiodicalissue";
    public static final String MODEL_PERIODICALSUPPLEMENT = "model:ndkperiodicalsupplement";
    public static final String MODEL_MONOGRAPHTITLE = "model:ndkmonographtitle";
    public static final String MODEL_MONOGRAPHVOLUME = "model:ndkmonographvolume";
    public static final String MODEL_MONOGRAPHSUPPLEMENT = "model:ndkmonographsupplement";
    public static final String MODEL_CARTOGRAPHIC = "model:ndkmap";
    public static final String MODEL_SHEETMUSIC = "model:ndksheetmusic";
    public static final String MODEL_ARTICLE = "model:ndkarticle";
    public static final String MODEL_CHAPTER = "model:ndkchapter";
    public static final String MODEL_PICTURE = "model:ndkpicture";

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
                MODEL_PERIODICAL, true, null,
                Arrays.asList(new ElementType("NDK Periodical", "en"), new ElementType("NDK Periodikum", "cs")),
                ModsConstants.NS,
                MODEL_PERIODICAL,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PERIODICALVOLUME, null, null,
                Arrays.asList(new ElementType("NDK Periodical Volume", "en"), new ElementType("NDK Ročník", "cs")),
                ModsConstants.NS,
                MODEL_PERIODICALVOLUME,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PERIODICALISSUE, null, null,
                Arrays.asList(new ElementType("NDK Periodical Issue", "en"), new ElementType("NDK Výtisk", "cs")),
                ModsConstants.NS,
                MODEL_PERIODICALISSUE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_MONOGRAPHSUPPLEMENT, null, null,
                Arrays.asList(new ElementType("NDK Monograph Supplement", "en"), new ElementType("NDK Příloha Monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHSUPPLEMENT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PERIODICALSUPPLEMENT, null, null,
                Arrays.asList(new ElementType("NDK Periodical Supplement", "en"), new ElementType("NDK Příloha Periodika", "cs")),
                ModsConstants.NS,
                MODEL_PERIODICALSUPPLEMENT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_ARTICLE, null, null,
                Arrays.asList(new ElementType("NDK Article", "en"), new ElementType("NDK Článek", "cs")),
                ModsConstants.NS,
                MODEL_ARTICLE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.OCR, DatastreamEditorType.MEDIA,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PICTURE, null, null,
                Arrays.asList(new ElementType("NDK Picture/Map - internal part", "en"), new ElementType("NDK Obrázek/Mapa - vnitřní část", "cs")),
                ModsConstants.NS,
                MODEL_PICTURE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.OCR, DatastreamEditorType.MEDIA,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_MONOGRAPHTITLE, true, null,
                Arrays.asList(new ElementType("NDK Multipart Monograph", "en"), new ElementType("NDK Vícedílná monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHTITLE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_MONOGRAPHVOLUME, true, null,
                Arrays.asList(new ElementType("NDK Monograph Volume", "en"), new ElementType("NDK Svazek monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHVOLUME,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_CHAPTER, null, null,
                Arrays.asList(new ElementType("NDK Chapter", "en"), new ElementType("NDK Kapitola", "cs")),
                ModsConstants.NS,
                MODEL_CHAPTER,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_CARTOGRAPHIC, true, null,
                Arrays.asList(new ElementType("NDK Cartographic Document", "en"), new ElementType("NDK Kartografický dokument", "cs")),
                ModsConstants.NS,
                MODEL_CARTOGRAPHIC,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_SHEETMUSIC, true, null,
                Arrays.asList(new ElementType("NDK Sheet Music", "en"), new ElementType("NDK Hudebnina", "cs")),
                ModsConstants.NS,
                MODEL_SHEETMUSIC,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));

        return models;
    }

    @Override
    public MetadataHandler<ModsDefinition> createMetadataHandler(DigitalObjectHandler handler) {
        return new NdkMetadataHandler(handler);
    }

    @Override
    public List<ValueMap> getValueMaps(UserProfile user) {
        return Collections.emptyList();
    }

    static class NdkMetadataHandler implements MetadataHandler<ModsDefinition> {

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
        public void setMetadata(DescriptionMetadata<ModsDefinition> data, String message) throws DigitalObjectException {
            ModsDefinition mods = data.getData();
            String modelId = handler.relations().getModel();
            if (mods == null) {
                mods = createDefault(modelId);
            }
            write(modelId, mods, data.getTimestamp(), message);
        }

        private ModsDefinition createDefault(String modelId) throws DigitalObjectException {
            return ModsStreamEditor.defaultMods(fobject.getPid());
        }

        @Override
        public void setMetadataAsJson(DescriptionMetadata<String> jsonData, String message) throws DigitalObjectException {
            String json = jsonData.getData();
            String editorId = jsonData.getEditor();
            String modelId = handler.relations().getModel();
            ModsDefinition mods;
            if (json == null) {
                mods = createDefault(modelId);
            } else {
                ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
                try {
                    mods = jsMapper.readValue(json, ModsWrapper.class).getMods();
                } catch (Exception ex) {
                    throw new DigitalObjectException(fobject.getPid(), null, ModsStreamEditor.DATASTREAM_ID, null, ex);
                }
            }
            write(modelId, mods, jsonData.getTimestamp(), message);
        }

        void fillNdkConstants_(ModsDefinition mods, String modelId) {
            NdkMapper mapper = NdkMapper.get(modelId);
            Context context = new Context(handler);
            mapper.createMods(mods, context);
        }

        @Override
        public void setMetadataAsXml(DescriptionMetadata<String> xmlData, String message) throws DigitalObjectException {
            ModsDefinition mods;
            String modelId = handler.relations().getModel();
            if (xmlData.getData() != null) {
                mods = ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xmlData.getData())));
            } else {
                mods = createDefault(modelId);
            }
            write(modelId, mods, xmlData.getTimestamp(), message);
        }

        @Override
        public DescriptionMetadata<ModsDefinition> getMetadata() throws DigitalObjectException {
            ModsDefinition mods = editor.read();
            DescriptionMetadata<ModsDefinition> dm = new DescriptionMetadata<ModsDefinition>();
            dm.setPid(fobject.getPid());
            dm.setTimestamp(editor.getLastModified());
//            dm.setEditor(editorId);
            dm.setData(mods);
            return dm;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <O> DescriptionMetadata<O> getMetadataAsJsonObject(String mappingId) throws DigitalObjectException {
            DescriptionMetadata<ModsDefinition> dm = getMetadata();
            DescriptionMetadata json = dm;
            json.setData(new ModsWrapper(dm.getData()));
            return json;
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

        private void write(String modelId, ModsDefinition mods, long timestamp, String message) throws DigitalObjectException {
            NdkMapper mapper = NdkMapper.get(modelId);
            Context context = new Context(handler);
            mapper.createMods(mods, context);
            if (LOG.isLoggable(Level.FINE)) {
                String toXml = ModsUtils.toXml(mods, true);
                LOG.fine(toXml);
            }
            editor.write(mods, timestamp, message);

            // DC
            OaiDcType dc = mapper.toDc(mods, context);
            DcStreamEditor dcEditor = handler.objectMetadata();
            DublinCoreRecord dcr = dcEditor.read();
            dcr.setDc(dc);
            dcEditor.write(handler, dcr, message);

            // Label
            String label = mapper.toLabel(mods);
            fobject.setLabel(label);
        }
    }

    public static class ModsWrapper {

        private ModsDefinition mods;

        public ModsWrapper() {
        }

        public ModsWrapper(ModsDefinition mods) {
            this.mods = mods;
        }

        public ModsDefinition getMods() {
            return mods;
        }

        public void setMods(ModsDefinition mods) {
            this.mods = mods;
        }
    }

}
