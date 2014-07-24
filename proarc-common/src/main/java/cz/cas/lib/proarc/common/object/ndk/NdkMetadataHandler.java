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
package cz.cas.lib.proarc.common.object.ndk;

import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.StringReader;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.stream.StreamSource;
import org.codehaus.jackson.map.ObjectMapper;

/**
 * Handles description metadata in the NDK format.
 *
 * @author Jan Pokorsky
 */
public class NdkMetadataHandler implements MetadataHandler<ModsDefinition> {

    private static final Logger LOG = Logger.getLogger(NdkMetadataHandler.class.getName());
    private final DigitalObjectHandler handler;
    private final ModsStreamEditor editor;
    private final FedoraObject fobject;
    private DigitalObjectCrawler crawler;

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
        ModsDefinition defaultMods = ModsStreamEditor.defaultMods(fobject.getPid());
        DigitalObjectHandler parent = handler.getParameterParent();
        if (NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId)) {
            // issue 124
            DigitalObjectHandler title = findEnclosingObject(parent, NdkPlugin.MODEL_PERIODICAL);
            if (title != null) {
                ModsDefinition titleMods = title.<ModsDefinition>metadata().getMetadata().getData();
                inheritTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritLocation(defaultMods, titleMods.getLocation());
                inheritIdentifier(defaultMods, titleMods.getIdentifier());
            }
        } else if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            // issue 137
            DigitalObjectHandler title = findEnclosingObject(parent, NdkPlugin.MODEL_PERIODICAL);
            if (title != null) {
                ModsDefinition titleMods = title.<ModsDefinition>metadata().getMetadata().getData();
                inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                defaultMods.getLanguage().addAll(titleMods.getLanguage());
                inheritIdentifier(defaultMods, titleMods.getIdentifier());
            }
        }
        return defaultMods;
    }

    private void inheritIdentifier(ModsDefinition mods, List<IdentifierDefinition> ids) {
        for (IdentifierDefinition id : ids) {
            String type = id.getType();
            if ("ccnb".equals(type) || "issn".equals(type)) {
                mods.getIdentifier().add(id);
            }
        }
    }

    private void inheritLocation(ModsDefinition mods, List<LocationDefinition> locs) {
        for (LocationDefinition loc : locs) {
            List<PhysicalLocationDefinition> pls = loc.getPhysicalLocation();
            List<StringPlusLanguage> sls = loc.getShelfLocator();
            if (!pls.isEmpty() || !sls.isEmpty()) {
                loc.getUrl().clear();
                mods.getLocation().add(loc);
            }
        }
    }

    private void inheritTitleInfo(ModsDefinition mods, List<TitleInfoDefinition> tis) {
        for (TitleInfoDefinition ti : tis) {
            if (ti.getType() == null) {
                ti.getPartNumber().clear();
                ti.getPartName().clear();
                ti.getNonSort().clear();
                mods.getTitleInfo().add(ti);
            }
        }
    }

    private void inheritSupplementTitleInfo(ModsDefinition mods, List<TitleInfoDefinition> tis) {
        for (TitleInfoDefinition ti : tis) {
            if (ti.getType() == null) {
                ti.getPartNumber().clear();
                ti.getPartName().clear();
                ti.getNonSort().clear();
                ti.getSubTitle().clear();
                mods.getTitleInfo().add(ti);
            }
        }
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

    private DigitalObjectHandler findEnclosingObject(
            DigitalObjectHandler obj, String searcModelId) throws DigitalObjectException {

        if (obj != null) {
            if (searcModelId.equals(obj.relations().getModel())) {
                return obj;
            } else {
                DigitalObjectElement parent = getCrawler().getParent(obj.getFedoraObject().getPid());
                return findEnclosingObject(parent, searcModelId);
            }
        }
        return null;
    }

    private DigitalObjectHandler findEnclosingObject(
            DigitalObjectElement obj, String searcModelId) throws DigitalObjectException {

        if (obj == DigitalObjectElement.NULL) {
            return null;
        } else if (searcModelId.equals(obj.getModelId())) {
            return obj.getHandler();
        } else {
            DigitalObjectElement parent = getCrawler().getParent(obj.getPid());
            return findEnclosingObject(parent, searcModelId);
        }
    }

    public DigitalObjectCrawler getCrawler() {
        if (crawler == null) {
            crawler = new DigitalObjectCrawler(DigitalObjectManager.getDefault(),
                    RemoteStorage.getInstance().getSearch());
        }
        return crawler;
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
