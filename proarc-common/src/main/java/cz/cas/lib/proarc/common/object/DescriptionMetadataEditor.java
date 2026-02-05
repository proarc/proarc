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

/**
 * The class to handle description metadata of a digital object.
 * <p>
 * The format of metadata is driven by the datastream formatURI.
 *
 * @author Jan Pokorsky
 * @deprecated replaced with {@link DigitalObjectHandler#metadata() }
 */
@Deprecated
public final class DescriptionMetadataEditor {

//    public static final String DATASTREAM_ID = "BIBLIO_MODS";
//    public static final String DATASTREAM_LABEL = "Metadata Object Description";

//    private static final Logger LOG = Logger.getLogger(DescriptionMetadataEditor.class.getName());

//    private final XmlStreamEditor editor;
//    private final FedoraObject object;

//    static XmlStreamEditor createEditor(FedoraObject object, String format) {
//        XmlStreamEditor editor = object.getEditor(
//                FoxmlUtils.inlineProfile(DATASTREAM_ID, format, DATASTREAM_LABEL));
//        return editor;
//    }

//    public DescriptionMetadataEditor(FedoraObject object) {
//        this(object, null);
//    }
//
//    public DescriptionMetadataEditor(FedoraObject object, String format) {
//        this(createEditor(object, format), object);
//    }
//
//    DescriptionMetadataEditor(XmlStreamEditor editor, FedoraObject object) {
//        this.editor = editor;
//        this.object = object;
//    }

//    public void newObject(MetaModel model, String xml, String msg) throws DigitalObjectException {
//        String pid = object.getPid();
//        String format = model.getMetadataFormat();
//        if (ModsConstants.NS.equals(format)) {
//            ModsType metadata = ModsStreamEditor.create(pid, model.getPid(), xml);
//            write(metadata, 0, msg);
////            ModsStreamEditor modsEditor = new ModsStreamEditor(createEditor(object, format), object);
////            modsEditor.write(metadata, 0, msg);
//        } else if (DcConstants.NS_OAIDC.equals(format)) {
//            // use template
//            OaiDcType dc = new OaiDcType();
//            write(dc, 0, msg);
//        }
//    }
//
//    public DescriptionMetadata<?> read(String editorId) throws DigitalObjectException {
//        String format = editor.getProfile().getDsFormatURI();
//        Source src = getDataAsSource();
//        DescriptionMetadata<Object> dm = new DescriptionMetadata<Object>();
//        dm.setPid(object.getPid());
//        dm.setTimestamp(editor.getLastModified());
//        dm.setEditor(editorId);
//        if (ModsConstants.NS.equals(format)) {
//            ModsType mods = ModsUtils.unmarshalModsType(src);
//            Mapping mapping = new Mapping();
//            Object customMods = mapping.read(mods, editorId);
//            dm.setData(customMods);
//        } else if (DcConstants.NS_OAIDC.equals(format)) {
//            OaiDcType dc = DcUtils.unmarshal(src, OaiDcType.class);
//            dm.setData(dc);
//        } else {
//            throw new DigitalObjectException(object.getPid(), "Unsupported data format: " + format);
//        }
//        return dm;
//    }
//
//    public StringRecord readAsString() throws DigitalObjectException {
//        // do not use StringEditor as we need pretty printed content
//        String format = editor.getProfile().getDsFormatURI();
//        Source src = getDataAsSource();
//        String content;
//        if (ModsConstants.NS.equals(format)) {
//            ModsType mods = ModsUtils.unmarshalModsType(src);
//            content = ModsUtils.toXml(mods, true);
//        } else if (DcConstants.NS_OAIDC.equals(format)) {
//            OaiDcType dc = DcUtils.unmarshal(src, OaiDcType.class);
//            content = DcUtils.toXml(dc, true);
//        } else {
//            throw new DigitalObjectException(object.getPid(), "Unsupported data format: " + format);
//        }
//        return new StringRecord(content, getLastModified(), object.getPid());
//    }
//
//    public void write(String editorId, String customJsonData, long timestamp, String message) throws DigitalObjectException, IOException {
//        String format = editor.getProfile().getDsFormatURI();
//        // XXX move impl to editor classes and delegate; new DcStreamEditor(editor) ??
//        // XXX or create DesaMetadataPlugin: writeXml(), readXml(), readJson(json, ...)
//        // XXX create NdkMetadataPlugin: writeXml(), readXml(), readJson(json, ...)
//        // XXX and register for model or editor?
//        if (ModsConstants.NS.equals(format)) {
//            Source src = getDataAsSource();
//            ModsType mods = ModsUtils.unmarshalModsType(src);
////            ModsType mods = (ModsType) read(editorId);
//            Mapping mapping = new Mapping();
//            Class<?> type = mapping.getType(editorId);
//            ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
//            Object customData = jsMapper.readValue(customJsonData, type);
//            mapping.update(mods, customData, editorId);
//            if (LOG.isLoggable(Level.FINE)) {
//                String toXml = ModsUtils.toXml(mods, true);
//                LOG.fine(toXml);
//            }
//            write(mods, timestamp, message);
//        } else if (DcConstants.NS_OAIDC.equals(format)) {
//            ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
//            OaiDcType dc = jsMapper.readValue(customJsonData, OaiDcType.class);
//            write(dc, timestamp, message);
//        } else {
//            throw new DigitalObjectException(object.getPid(), "Unsupported data format: " + format);
//        }
//    }
//
//    public void write(OaiDcType dc, long timestamp, String message) throws DigitalObjectException {
//        EditorResult result = editor.createResult();
//        // DO NOT include schemaLocation. Fedora validator does not accept it.
//        DcUtils.marshal(result, dc, false);
//        editor.write(result, timestamp, message);
//
//        // DC
//        // XXX write helper for DC/DC mapping
//        RelationEditor relationEditor = new RelationEditor(object);
//        String model = relationEditor.getModel();
//        String importFile = relationEditor.getImportFile();
//        DcStreamEditor dcEditor = new DcStreamEditor(object);
//        DublinCoreRecord dcr = dcEditor.read();
//        addPid(dc, object.getPid());
//        addModel(dc, model);
////        if (importFile != null) {
////            addTitle(dc, importFile);
////        }
//        dcr.setDc(dc);
//        dcEditor.write(dcr, message);
//
//        // XXX spec requires identifier + title :-(
//        String label = dc.getTitles().isEmpty() ? "?" : dc.getTitles().get(0).getValue();
//        if ("model:derFile".equals(model)) {
//            label = importFile == null ? "?" : importFile;
//        }
//        object.setLabel(label);
//    }
//
//    /**
//     * Updates metadata on dissemination changes.
//     *
//     * @param filename file name
//     * @param mime MIME type
//     * @param message log message
//     * @throws DigitalObjectException
//     */
//    public void updateDissemination(String filename, MediaType mime, String message) throws DigitalObjectException {
//        DescriptionMetadata<?> dm = read(null);
//        OaiDcType data = (OaiDcType) dm.getData();
//        List<ElementType> titles = data.getTitles();
//        titles.clear();
//        titles.add(new ElementType(filename, null));
//        List<ElementType> formats = data.getFormats();
//        formats.clear();
//        formats.add(new ElementType(mime.toString(), null));
//        write(data, getLastModified(), message);
//    }
//
//    private void write(ModsType mods, long timestamp, String message) throws DigitalObjectException {
//        EditorResult marshaled = editor.createResult();
//        ModsUtils.marshal(marshaled, mods, true);
//        editor.write(marshaled, timestamp, message);
//
//        // DC
//        String model = new RelationEditor(object).getModel();
//        DcStreamEditor dcEditor = new DcStreamEditor(object);
//        dcEditor.write(mods, model, dcEditor.getLastModified(), message);
//
//        String label = ModsUtils.getLabel(mods, model);
//        object.setLabel(label);
//    }
//
//    public long getLastModified() throws DigitalObjectException {
//        return editor.getLastModified();
//    }

//    private Source getDataAsSource() throws DigitalObjectException {
//        Source src = editor.read();
//        if (src == null) {
//            // it should never arise; it would need to create datastream again with default data
//            throw new DigitalObjectException(object.getPid(), DATASTREAM_ID + " not initialized!");
//        }
//        return src;
//    }

}
