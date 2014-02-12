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

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.export.desa.DesaServices;
import cz.cas.lib.proarc.common.export.desa.DesaServices.DesaConfiguration;
import cz.cas.lib.proarc.common.export.desa.sip2desa.nomen.Nomenclatures;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.object.DerDesaPlugin.DerMetadataHandler;
import cz.cas.lib.proarc.common.object.DerDesaPlugin.DerRawDisseminationHandler;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.desa.nsesss2.Dokument;
import cz.cas.lib.proarc.desa.nsesss2.NsesssConstants;
import cz.cas.lib.proarc.desa.nsesss2.NsesssUtils;
import cz.cas.lib.proarc.desa.nsesss2.Spis;
import cz.cas.lib.proarc.desa.nsesss2.TEvidencniUdajeDokumentu;
import cz.cas.lib.proarc.desa.nsesss2.TEvidencniUdajeSpisu;
import cz.cas.lib.proarc.desa.nsesss2.TIdentifikace;
import cz.cas.lib.proarc.desa.nsesss2.TIdentifikator;
import cz.cas.lib.proarc.desa.nsesss2.TPopis;
import cz.cas.lib.proarc.desa.nsesss2.mapping.NsesssMapper;
import cz.cas.lib.proarc.desa.nsesss2.mapping.NsesssMapper.SubjektExterni;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.transform.Source;
import org.codehaus.jackson.map.ObjectMapper;

/**
 * Support of the DES DESA model. The plugin is used to provide list of all
 * DES models but each model gets custom subclass to handle different data type
 * but to reuse common stuff.
 *
 * @author Jan Pokorsky
 */
public class DesDesaPlugin implements DigitalObjectPlugin {

    public static final String ID = "desa-des";
    /** {@link Spis} */
    public static final String MODEL_FOLDER = "model:desFolder";
    /** {@link Dokument} */
    public static final String MODEL_INTERNAL_RECORD = "model:desInternalRecord";
    /** {@link Dokument} */
    public static final String MODEL_EXTERNAL_RECORD = "model:desExternalRecord";
    public static final String MODEL_FILE = "model:desFile";

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public Collection<MetaModel> getModel() {
        ArrayList<MetaModel> models = new ArrayList<MetaModel>();
        DesNsesssDesaPlugin nsesssPlugin = new DesNsesssDesaPlugin();
        models.add(new MetaModel(
                MODEL_FOLDER, true, null,
                Arrays.asList(new ElementType("File", "en"), new ElementType("Spis", "cs")),
                NsesssConstants.NS,
                "proarc.metadata.editor.nsesss.desFolder",
                nsesssPlugin,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_INTERNAL_RECORD, true, null,
                Arrays.asList(new ElementType("Internal Record", "en"), new ElementType("Vlastní dokument", "cs")),
                NsesssConstants.NS,
                "proarc.metadata.editor.nsesss.desInternalRecord",
                nsesssPlugin,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_EXTERNAL_RECORD, true, null,
                Arrays.asList(new ElementType("External Record", "en"), new ElementType("Doručený dokument", "cs")),
                NsesssConstants.NS,
                "proarc.metadata.editor.nsesss.desExternalRecord",
                nsesssPlugin,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_FILE, null, true,
                Arrays.asList(new ElementType("DES Component", "en"), new ElementType("DES Komponenta", "cs")),
                DcConstants.NS_OAIDC, "proarc.metadata.editor.dc.desFile",
                new DesDcDesaPlugin(),
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT,
                        DatastreamEditorType.MEDIA, DatastreamEditorType.ATM)
                ));
        return models;
    }

    @Override
    public <T extends HasDataHandler> T getHandlerProvider(Class<T> type) {
        return type.isInstance(this) ? type.cast(this): null;
    }

    @Override
    public List<ValueMap> getValueMaps() {
        try {
            AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
            DesaServices desaServices = appConfig.getDesaServices();
            DesaConfiguration dc = desaServices.findConfigurationWithModel(
                    MODEL_EXTERNAL_RECORD, MODEL_FILE, MODEL_FOLDER, MODEL_INTERNAL_RECORD);
            if (dc != null) {
                Nomenclatures nomenclatures = desaServices.getNomenclatures(dc);
                return desaServices.getValueMap(nomenclatures, ID);
            } else {
                return Collections.emptyList();
            }
        } catch (Exception ex) {
            throw new IllegalStateException("Cannot init value maps for " + ID + " plugin!", ex);
        }
    }

    /**
     * Handles NSESSS2 format.
     */
    private static class DesNsesssDesaPlugin extends DesDesaPlugin implements HasMetadataHandler<Object> {

        @Override
        public MetadataHandler<Object> createMetadataHandler(DigitalObjectHandler handler) {
            return new DesMetadataHandler(handler);
        }

    }

    /**
     * Handles DC format.
     */
    private static class DesDcDesaPlugin extends DesDesaPlugin implements HasMetadataHandler<OaiDcType>, HasDisseminationHandler {

        @Override
        public DisseminationHandler createDisseminationHandler(String dsId, DigitalObjectHandler handler) {
            DefaultDisseminationHandler ddh = new DefaultDisseminationHandler(dsId, handler);
            if (BinaryEditor.RAW_ID.equals(dsId)) {
                return new DerRawDisseminationHandler(handler, ddh);
            }
            return ddh;
        }

        @Override
        public MetadataHandler<OaiDcType> createMetadataHandler(DigitalObjectHandler handler) {
            return new DerMetadataHandler(handler);
        }

    }

    public static class DesMetadataHandler implements MetadataHandler<Object> {

        private final DigitalObjectHandler handler;
        private final FedoraObject fobject;
        private final XmlStreamEditor editor;

        public DesMetadataHandler(DigitalObjectHandler handler) {
            this.handler = handler;
            this.fobject = handler.getFedoraObject();
            this.editor = fobject.getEditor(FoxmlUtils.inlineProfile(
                    DESCRIPTION_DATASTREAM_ID, NsesssConstants.NS, DESCRIPTION_DATASTREAM_LABEL));
        }

        @Override
        public void setMetadata(DescriptionMetadata<Object> data, String message) throws DigitalObjectException {
            Object metadata = data.getData();
            if (metadata == null) {
                String modelId = handler.relations().getModel();
                metadata = createDefaultMetadata(modelId);
            }
            write(metadata, data.getTimestamp(), message);
        }

        @Override
        public void setMetadataAsJson(DescriptionMetadata<String> jsonData, String message) throws DigitalObjectException {
            String json = jsonData.getData();
            Object metadata;
            if (json == null) {
                String modelId = handler.relations().getModel();
                metadata = createDefaultMetadata(modelId);
            } else {
                ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
                try {
                    DesObjectWrapper wrapper = jsMapper.readValue(json, DesObjectWrapper.class);
                    metadata = wrapper.getSpis() != null
                            ? mapFromJson(wrapper.getSpis())
                            : mapFromJson(wrapper.getDokument());
                } catch (Exception ex) {
                    throw new DigitalObjectException(fobject.getPid(), null, editor.getProfile().getDsID(), null, ex);
                }
            }
            write(metadata, jsonData.getTimestamp(), message);
        }

        @Override
        public void setMetadataAsXml(DescriptionMetadata<String> xmlData, String message) throws DigitalObjectException {
            String xml = xmlData.getData();
            Object metadata;
            if (xml == null) {
                String modelId = handler.relations().getModel();
                metadata = createDefaultMetadata(modelId);
            } else {
                metadata = NsesssUtils.unmarshal(xml, getType());
            }
            write(metadata, xmlData.getTimestamp(), message);
        }

        @Override
        public DescriptionMetadata<Object> getMetadata() throws DigitalObjectException {
            Source src = editor.read();
            if (src == null) {
                // return null or default?
                throw new DigitalObjectException(fobject.getPid(),
                        null, DESCRIPTION_DATASTREAM_ID,
                        "datastream not initialized!", null);
            }
            Object metadata = NsesssUtils.unmarshal(src, getType());
            DescriptionMetadata<Object> dm = new DescriptionMetadata<Object>();
            dm.setPid(fobject.getPid());
            dm.setTimestamp(editor.getLastModified());
//            dm.setEditor(editorId);
            dm.setData(metadata);
            return dm;
        }

        @Override
        public DescriptionMetadata<String> getMetadataAsXml() throws DigitalObjectException {
            DescriptionMetadata<Object> dm = getMetadata();
            Object metadata = dm.getData();
            DescriptionMetadata<String> result = new DescriptionMetadata<String>();
            result.setPid(dm.getPid());
            result.setBatchId(dm.getBatchId());
            String xml = NsesssUtils.toXml(metadata, true);
            result.setData(xml);
            return result;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <O> DescriptionMetadata<O> getMetadataAsJsonObject(String mappingId) throws DigitalObjectException {
            DescriptionMetadata<Object> dm = getMetadata();
            Object metadata = dm.getData();
            if (metadata instanceof Spis) {
                dm.setData(new DesObjectWrapper((Spis) metadata));
            } else if (metadata instanceof Dokument) {
                dm.setData(new DesObjectWrapper(mapToJson((Dokument) metadata)));
            }
            return (DescriptionMetadata<O>) dm;
        }

        public static Dokument mapToJson(Dokument d) {
            NsesssMapper mapper = new NsesssMapper();
            d = mapper.replaceTSubjektExterni(d);
            return d;
        }

        /**
         * Gets unique ID for NSESSS object.
         * @return ID
         */
        private String nsesssId() {
            // prefix uuid to comply with NCName syntax; cannot start with a number
            return "ID_" + FoxmlUtils.pidAsUuid(fobject.getPid());
        }

        public Spis mapFromJson(Spis s) {
            s.setID(nsesssId());
            NsesssMapper mapper = new NsesssMapper();
            s = mapper.fillZdroj(s);
            return s;
        }

        public Dokument mapFromJson(Dokument d) {
            d.setID(nsesssId());
            NsesssMapper mapper = new NsesssMapper();
            d = mapper.fillZdroj(d);
            return d;
        }

        private Class<?> getType() throws DigitalObjectException {
            String modelId = handler.relations().getModel();
            if (MODEL_FOLDER.equals(modelId)) {
                return Spis.class;
            } else if (MODEL_INTERNAL_RECORD.equals(modelId)) {
                return Dokument.class;
            } else if (MODEL_EXTERNAL_RECORD.equals(modelId)) {
                return Dokument.class;
            }
            throw new DigitalObjectException(fobject.getPid(), null,
                    DESCRIPTION_DATASTREAM_ID, "Unknown model: " + modelId, null);
        }

        private Object createDefaultMetadata(String modelId) throws DigitalObjectException {
            if (MODEL_FOLDER.equals(modelId)) {
                return NsesssUtils.defaultSpis();
            } else if (MODEL_INTERNAL_RECORD.equals(modelId)) {
                return NsesssUtils.defaultInternalDokument();
            } else if (MODEL_EXTERNAL_RECORD.equals(modelId)) {
                return NsesssUtils.defaultExternalDocument();
            }
            throw new DigitalObjectException(fobject.getPid(), null,
                    DESCRIPTION_DATASTREAM_ID, "Unknown model: " + modelId, null);
        }

        private void write(Object metadata, long timestamp, String message) throws DigitalObjectException {
            // DESCRIPTION stream
            EditorResult result = editor.createResult();
            NsesssUtils.marshal(result, metadata, false);
            editor.write(result, timestamp, message);

            if (metadata instanceof Spis) {
                writeSpis((Spis) metadata, message);
            } else if (metadata instanceof Dokument) {
                writeDokument((Dokument) metadata, message);
            } else {
                throw new UnsupportedOperationException();
            }

        }

        private void writeSpis(Spis record, String message) throws DigitalObjectException {
            String objectId = null;
            //LABEL
            String label = null;
            TEvidencniUdajeSpisu evidencniUdaje = record.getEvidencniUdaje();
            if (evidencniUdaje != null) {
                label = findIdentifier(evidencniUdaje.getIdentifikace());
            }
            if (label == null || label.isEmpty()) {
                label = "?";
            } else {
                objectId = label;
            }
            fobject.setLabel(label);

            //DC
            DcStreamEditor dcEditor = handler.objectMetadata();
            DublinCoreRecord dcr = dcEditor.read();
            OaiDcType dc = new OaiDcType();
            dc.getIdentifiers().add(new ElementType(fobject.getPid(), null));
            if (objectId != null) {
                dc.getIdentifiers().add(new ElementType(objectId, null));
            }

            dc.getTypes().add(new ElementType(MODEL_FOLDER, null));

            String title = findSpisNazev(record);
            if (title != null && title.isEmpty()) {
                dc.getTitles().add(new ElementType(title, null));
            }

            dcr.setDc(dc);
            dcEditor.write(dcr, message);
        }

        private void writeDokument(Dokument document, String message) throws DigitalObjectException {
            String objectId = null;
            //LABEL
            String label = null;
            TEvidencniUdajeDokumentu evidencniUdaje = document.getEvidencniUdaje();
            if (evidencniUdaje != null) {
                label = findIdentifier(evidencniUdaje.getIdentifikace());
            }
            if (label == null || label.isEmpty()) {
                label = "?";
            } else {
                objectId = label;
            }
            fobject.setLabel(label);

            //DC
            DcStreamEditor dcEditor = handler.objectMetadata();
            DublinCoreRecord dcr = dcEditor.read();
            OaiDcType dc = new OaiDcType();
            dc.getIdentifiers().add(new ElementType(fobject.getPid(), null));
            if (objectId != null) {
                dc.getIdentifiers().add(new ElementType(objectId, null));
            }

            String modelId = handler.relations().getModel();
            dc.getTypes().add(new ElementType(modelId, null));

            String title = findDokumentNazev(document);
            if (title != null && title.isEmpty()) {
                dc.getTitles().add(new ElementType(title, null));
            }

            dcr.setDc(dc);
            dcEditor.write(dcr, message);
        }

        private static String findSpisNazev(Spis spis) {
            TEvidencniUdajeSpisu evidencniUdaje = spis.getEvidencniUdaje();
            if (evidencniUdaje != null) {
                TPopis popis = evidencniUdaje.getPopis();
                if (popis != null) {
                    String nazev = popis.getNazev();
                    return nazev;
                }
            }
            return null;
        }

        private static String findDokumentNazev(Dokument d) {
            TEvidencniUdajeDokumentu evidencniUdaje = d.getEvidencniUdaje();
            if (evidencniUdaje != null) {
                TPopis popis = evidencniUdaje.getPopis();
                if (popis != null) {
                    String nazev = popis.getNazev();
                    return nazev;
                }
            }
            return null;
        }

        private static String findIdentifier(TIdentifikace tIdentifikace) {
            if (tIdentifikace != null) {
                List<TIdentifikator> identifikator = tIdentifikace.getIdentifikator();
                if (!identifikator.isEmpty()) {
                    return identifikator.get(0).getValue();
                }
            }
            return null;
        }

    }

    /** The helper to serialize/deserialize generic NSESSS2 in JSON format. */
    @XmlSeeAlso(value = SubjektExterni.class)
    public static class DesObjectWrapper {

        @XmlElement(name = "Spis")
        private Spis spis;
        @XmlElement(name = "Dokument")
        private Dokument dokument;

        public DesObjectWrapper() {
        }

        public DesObjectWrapper(Dokument dokument) {
            this.dokument = dokument;
        }

        public DesObjectWrapper(Spis spis) {
            this.spis = spis;
        }

        public Spis getSpis() {
            return spis;
        }

        public void setSpis(Spis spis) {
            this.spis = spis;
        }

        public Dokument getDokument() {
            return dokument;
        }

        public void setDokument(Dokument dokument) {
            this.dokument = dokument;
        }
    }

}
