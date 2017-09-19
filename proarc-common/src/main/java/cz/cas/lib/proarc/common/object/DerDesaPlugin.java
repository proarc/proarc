/*
 * Copyright (C) 2013 Jan Pokorsky
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

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.dublincore.DcUtils;
import cz.cas.lib.proarc.common.export.desa.DesaServices;
import cz.cas.lib.proarc.common.export.desa.DesaServices.DesaConfiguration;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.user.Group;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.cas.lib.proarc.desa.nomenclature.Nomenclatures;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.xml.transform.Source;

/**
 * Support of the DER DESA model.
 *
 * @author Jan Pokorsky
 */
public class DerDesaPlugin implements DigitalObjectPlugin,
        HasMetadataHandler<OaiDcType>, HasDisseminationHandler {

    /**
     * The plugin ID.
     */
    public static final String ID = "desa-der";
    public static final String MODEL_FOLDER = "model:derFolder";
    public static final String MODEL_DOCUMENT = "model:derDocument";
    public static final String MODEL_FILE = "model:derFile";

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
        ArrayList<MetaModel> models = new ArrayList<MetaModel>();
        models.add(new MetaModel(
                MODEL_FOLDER, true, null,
                Arrays.asList(new ElementType("Folder", "en"), new ElementType("Složka", "cs")),
                DcConstants.NS_OAIDC, "proarc.metadata.editor.dc.derFolder",
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_DOCUMENT, true, null,
                Arrays.asList(new ElementType("Document", "en"), new ElementType("Dokument", "cs")),
                DcConstants.NS_OAIDC, "proarc.metadata.editor.dc.derDocument",
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_FILE, null, true,
                Arrays.asList(new ElementType("DER Component", "en"), new ElementType("DER Komponenta", "cs")),
                DcConstants.NS_OAIDC, "proarc.metadata.editor.dc.derFile",
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT,
                        DatastreamEditorType.MEDIA, DatastreamEditorType.ATM)
                ));
        return models;
    }

    @Override
    public MetadataHandler<OaiDcType> createMetadataHandler(DigitalObjectHandler handler) {
        return new DerMetadataHandler(handler);
    }

    @Override
    public DisseminationHandler createDisseminationHandler(String dsId, DigitalObjectHandler handler) {
        DefaultDisseminationHandler ddh = new DefaultDisseminationHandler(dsId, handler);
        if (BinaryEditor.RAW_ID.equals(dsId)) {
            return new DerRawDisseminationHandler(handler, ddh);
        } else {
            return ddh;
        }
    }

    @Override
    public List<ValueMap> getValueMaps(ValueMap.Context context) {
        try {
            AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
            DesaServices desaServices = appConfig.getDesaServices();
            DesaConfiguration dc = desaServices.findConfigurationWithModel(MODEL_DOCUMENT, MODEL_FILE, MODEL_FOLDER);
            if (dc != null) {
                Nomenclatures nomenclatures = desaServices.getNomenclatures(dc, context.getUser());
                return desaServices.getValueMap(nomenclatures, ID);
            } else {
                return Collections.emptyList();
            }
        } catch (Exception ex) {
            throw new IllegalStateException("Cannot init value maps for " + ID + " plugin!", ex);
        }
    }

    static class DerRawDisseminationHandler implements DisseminationHandler {

        private final DisseminationHandler defaultHandler;
        private final DigitalObjectHandler handler;

        public DerRawDisseminationHandler(DigitalObjectHandler handler, DefaultDisseminationHandler defaultHandler) {
            this.defaultHandler = defaultHandler;
            this.handler = handler;
        }

        @Override
        public Response getDissemination(Request httpRequest) throws DigitalObjectException {
            return defaultHandler.getDissemination(httpRequest);
        }

        @Override
        public void setDissemination(DisseminationInput input, String message) throws DigitalObjectException {
            defaultHandler.setDissemination(input, message);
            MetadataHandler<OaiDcType> metadata = handler.metadata();
            DescriptionMetadata<OaiDcType> dm = metadata.getMetadata();
            OaiDcType data = dm.getData();
            List<ElementType> titles = data.getTitles();
            titles.clear();
            String filename = input.getFilename();
            titles.add(new ElementType(filename, null));
            List<ElementType> formats = data.getFormats();
            formats.clear();
            formats.add(new ElementType(input.getMime().toString(), null));

            metadata.setMetadata(dm, message);
        }

        @Override
        public void deleteDissemination(String message) throws DigitalObjectException {
            defaultHandler.deleteDissemination(message);
        }

    }

    static class DerMetadataHandler implements MetadataHandler<OaiDcType> {

        private final DigitalObjectHandler handler;
        private final XmlStreamEditor editor;
        private final FedoraObject object;

        DerMetadataHandler(DigitalObjectHandler handler) {
            this.handler = handler;
            this.object = handler.getFedoraObject();
            this.editor = object.getEditor(FoxmlUtils.inlineProfile(
                    DESCRIPTION_DATASTREAM_ID, DcConstants.NS_OAIDC, DESCRIPTION_DATASTREAM_LABEL));
        }

        @Override
        public void setMetadata(DescriptionMetadata<OaiDcType> data, String message) throws DigitalObjectException {
            OaiDcType dc = data.getData();
            if (dc == null) {
                dc = createDefautDc();
            }
            write(dc, data.getTimestamp(), message);
        }

        @Override
        public void setMetadataAsJson(DescriptionMetadata<String> json, String message) throws DigitalObjectException {
            String data = json.getData();
            OaiDcType dc;
            if (data == null) {
                dc = createDefautDc();
            } else {
                ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
                try {
                    dc = jsMapper.readValue(data, OaiDcType.class);
                } catch (Exception ex) {
                    throw new DigitalObjectException(object.getPid(), null, editor.getProfile().getDsID(), null, ex);
                }
            }
            write(dc, json.getTimestamp(), message);
        }

        @Override
        public void setMetadataAsXml(DescriptionMetadata<String> xml, String message) throws DigitalObjectException {
            OaiDcType dc = null;
            if (xml.getData() != null) {
                dc = DcUtils.unmarshal(xml.getData(), OaiDcType.class);
            }
            if (dc == null) {
                dc = createDefautDc();
            }
            write(dc, xml.getTimestamp(), message);
        }

        private OaiDcType createDefautDc() throws DigitalObjectException {
            OaiDcType dc = new OaiDcType();
            DigitalObjectHandler parent = handler.getParameterParent();
            String modelId = handler.relations().getModel();

            if (MODEL_FOLDER.equals(modelId)) {
                String identifier = getProducerCode() + "_";
                dc.getIdentifiers().add(new ElementType(identifier, null));
                dc.getTitles().add(new ElementType(identifier + '-', null));
            } else if (MODEL_DOCUMENT.equals(modelId)) {
                OaiDcType parentDc = getParentDcMetadata(parent);
                if (parentDc != null) {
                    if (!parentDc.getIdentifiers().isEmpty()) {
                        String identifier = parentDc.getIdentifiers().get(0).getValue();
                        dc.getIdentifiers().add(new ElementType(identifier + '-', null));
                        dc.getSubjects().add(new ElementType(identifier, null));
                    }
                    if (!parentDc.getTitles().isEmpty()) {
                        String title = parentDc.getTitles().get(0).getValue()
                                + '-';
                        dc.getTitles().add(new ElementType(title, null));
                    }
                    if (!parentDc.getTypes().isEmpty()) {
                        String type = parentDc.getTypes().get(0).getValue();
                        dc.getTypes().add(new ElementType(type, null));
                    }
                }
            }
            return dc;
        }

        private void write(OaiDcType dc, long timestamp, String message) throws DigitalObjectException {
            EditorResult result = editor.createResult();

            // DO NOT include schemaLocation. Fedora validator does not accept it.
            DcUtils.marshal(result, dc, false);
            editor.write(result, timestamp, message);

            // Label: spec requires identifier + title
            StringBuilder label = new StringBuilder();
            if (!dc.getIdentifiers().isEmpty()) {
                label.append(dc.getIdentifiers().get(0).getValue());
            }
            if (!dc.getTitles().isEmpty()) {
                if (label.length() > 0) {
                    label.append(' ');
                }
                label.append(dc.getTitles().get(0).getValue());
            }
            String objLabel = label.toString().trim();
            object.setLabel(objLabel.isEmpty() ? "?" : objLabel);

            // DC
            DcStreamEditor dcEditor = handler.objectMetadata();
            DublinCoreRecord dcr = dcEditor.read();
            dcr.setDc(dc);
            dcEditor.write(handler, dcr, message);
        }

        @Override
        public DescriptionMetadata<OaiDcType> getMetadata() throws DigitalObjectException {
            Source src = getDataAsSource();
            DescriptionMetadata<OaiDcType> dm = new DescriptionMetadata<OaiDcType>();
            dm.setPid(object.getPid());
            dm.setTimestamp(editor.getLastModified());
//            dm.setEditor(editorId);
            OaiDcType dc = DcUtils.unmarshal(src, OaiDcType.class);
            dm.setData(dc);
            return dm;
        }

        @Override
        public DescriptionMetadata<String> getMetadataAsXml() throws DigitalObjectException {
            DescriptionMetadata<OaiDcType> dm = getMetadata();
            OaiDcType dc = dm.getData();
            DescriptionMetadata<String> result = new DescriptionMetadata<String>();
            result.setPid(dm.getPid());
            result.setBatchId(dm.getBatchId());
            String xml = DcUtils.toXml(dc, true);
            result.setData(xml);
            return result;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <O> DescriptionMetadata<O> getMetadataAsJsonObject(String mapping) throws DigitalObjectException {
            return (DescriptionMetadata<O>) getMetadata();
        }

        private Source getDataAsSource() throws DigitalObjectException {
            Source src = editor.read();
            if (src == null) {
                // it should never arise; it would need to create datastream again with default data
                throw new DigitalObjectException(object.getPid(),
                        null, DESCRIPTION_DATASTREAM_ID,
                        "datastream not initialized!", null);
            }
            return src;
        }

        /**
         * Gets parent DC.
         * @param parent parent to query
         * @return the description metadata or {@code null}
         * @throws DigitalObjectException failure
         */
        private static OaiDcType getParentDcMetadata(DigitalObjectHandler parent) throws DigitalObjectException {
            if (parent != null) {
                MetadataHandler<Object> metadataHandler = parent.metadata();
                if (metadataHandler != null) {
                    Object metadata = metadataHandler.getMetadata().getData();
                    if (metadata instanceof OaiDcType) {
                        return (OaiDcType) metadata;
                    }
                }
            }
            return null;
        }

        private String getProducerCode() {
            String producerCode = "";
            UserProfile user = handler.getParameterUser();
            if (user == null) {
                return producerCode;
            }
            Integer defaultGroupId = user.getDefaultGroup();
            if (defaultGroupId != null) {
                Group group = UserUtil.getDefaultManger().findGroup(defaultGroupId);
                if (group != null && group.getRemoteName() != null) {
                    producerCode = group.getRemoteName();
                }
            }
            return producerCode;
        }

    }

}
