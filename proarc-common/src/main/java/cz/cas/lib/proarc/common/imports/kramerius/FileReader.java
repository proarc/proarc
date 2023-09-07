/*
 * Copyright (C) 2019 Lukas Sykora
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
package cz.cas.lib.proarc.common.imports.kramerius;

import com.yourmediashelf.fedora.generated.foxml.ContentLocationType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;
import com.yourmediashelf.fedora.util.DateUtility;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.FileSet;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.imports.ImportProcess;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.imports.TiffAsJpegImporter;
import cz.cas.lib.proarc.common.imports.TiffImporter;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils;
import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

/**
 * It reads the kramerius package and generates digital objects for a batch import.
 *
 * @author Lukas Sykora
 */
public class FileReader {

    private final File targetFolder;
    private final ImportSession iSession;
    private final String type;
    private static final Logger LOG = Logger.getLogger(FileReader.class.getName());

    public static final String NDK_MONOGRAPH_MAP = "ndk_monograph";
    public static final String NDK_PERIODICAL_MAP = "ndk_periodical";
    public static final String STT_MAP = "stt";
    public static final String K4_MAP = "default";

    private final Set<String> KRAMERIUS_PREFIX = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
            "kramerius:hasIntCompPart", "hasIntCompPart", "kramerius:hasItem", "hasItem",
            "kramerius:hasPage", "hasPage", "kramerius:hasUnit", "hasInit", "kramerius:hasVolume", "hasVolume")));

    private final Set<String> KRAMERIUS_DATASTREAMS = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
            "RELS-EXT", "IMG_FULL", "IMG_PREVIEW", "IMG_THUMB", "TEXT_OCR", "ALTO", "BIBLIO_MODS", "DC", "NDK_ARCHIVAL", "NDK_USER", "FULL", "PREVIEW", "THUMBNAIL")));

    private final Map<String, String> datastreamVersionId = new HashMap<String, String>() {
        {
            put("IMG_FULL", "FULL");
            put("IMG_THUMB", "THUMBNAIL");
            put("IMG_PREVIEW", "PREVIEW");
        }
    };

    // K4 model mapping
    private final Map<String, String> modelMonographMap = new HashMap<String, String>() {
        {
            put("model:article", NdkPlugin.MODEL_ARTICLE);
            put("model:map", NdkPlugin.MODEL_CARTOGRAPHIC);
            // put("model:monograph", NdkPlugin.MODEL_MONOGRAPHTITLE);
            put("model:supplement", NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT);
            put("model:monograph", NdkPlugin.MODEL_MONOGRAPHVOLUME);
            put("model:monographunit", NdkPlugin.MODEL_MONOGRAPHUNIT);
            //put("model:periodical", NdkPlugin.MODEL_PERIODICAL);
            //put("model:periodicalitem", NdkPlugin.MODEL_PERIODICALISSUE);
            //put("model:supplement", NdkPlugin.MODEL_PERIODICALSUPPLEMENT);
            //put("model:periodicalvolume", NdkPlugin.MODEL_PERIODICALVOLUME);
            put("model:picture", NdkPlugin.MODEL_PICTURE);
            put("model:sheetmusic", NdkPlugin.MODEL_SHEETMUSIC);
        }
    };

    private final Map<String, String> modelPeriodicalMap = new HashMap<String, String>() {
        {
            put("model:periodical", NdkPlugin.MODEL_PERIODICAL);
            put("model:periodicalitem", NdkPlugin.MODEL_PERIODICALISSUE);
            put("model:supplement", NdkPlugin.MODEL_PERIODICALSUPPLEMENT);
            put("model:periodicalvolume", NdkPlugin.MODEL_PERIODICALVOLUME);
            put("model:picture", NdkPlugin.MODEL_PICTURE);
            put("model:sheetmusic", NdkPlugin.MODEL_SHEETMUSIC);
            put("model:article", NdkPlugin.MODEL_ARTICLE);
            put("model:map", NdkPlugin.MODEL_CARTOGRAPHIC);
        }
    };

    private final Map<String, String> modelSTTMap = new HashMap<String, String>() {
        {
            // Unsupported operation yet
        }
    };

    public Map<String, String> getModelMonographMap() {
        return modelMonographMap;
    }

    public Map<String, String> getModelPeriodicalMap() {
        return modelPeriodicalMap;
    }

    public Map<String, String> getModelSTTMap() {
        return modelSTTMap;
    }

    public FileReader(File targetFolder, ImportSession session, String type) {
        this.targetFolder = targetFolder;
        this.iSession = session;
        this.type = type;
    }

    public void read(File file, ImportOptions ctx, int index) throws IllegalStateException, DigitalObjectException {
        try {
            readImpl(file, ctx, index);
        } catch (DigitalObjectException ex) {
            if (ex != null && ex.getPid() != null && ex.getPid().contains("The repository already contains pid:")) {
                ex.setMessage(file.getAbsolutePath());
                throw ex;
            } else {
                throw new IllegalStateException(file.getAbsolutePath(), ex);
            }
        } catch (Exception ex) {
            throw new IllegalStateException(file.getAbsolutePath(), ex);
        }
    }

    private String getUuidName(String name) {
        return name.substring(0, name.length()-4);
    }

    private void readImpl(File file, ImportOptions ctx, int index) throws DigitalObjectException {
        String pid = FoxmlUtils.PID_PREFIX + getUuidName(file.getName());

        // create foxml
        BatchItemObject importItem = iSession.findItem(pid);
        LocalObject lObj = iSession.findLocalObject(importItem);
        DigitalObject dObj = null;
        boolean isNewObject = lObj == null;
        String model = null;

        if (lObj == null) {
            File objFile = new File(targetFolder, getFoxmlFilename(index, pid));
            FedoraObject object = null;
            if (Storage.FEDORA.equals(iSession.getTypeOfStorage())) {
                object = iSession.getRemotes().find(pid);
            } else if (Storage.AKUBRA.equals(iSession.getTypeOfStorage())) {
                object = iSession.getAkubraStorage().find(pid);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + iSession.getTypeOfStorage());
            }
            try {
                String foxml = object.asText();
                dObj = FoxmlUtils.unmarshal(foxml, DigitalObject.class);
                isNewObject = false;
                if (!isNewObject) {
                    if (!model2Override(object.getModel()) && !ctx.isUseNewMetadata() && !ctx.isUseOriginalMetadata()) {
                        throw new DigitalObjectException("The repository already contains pid: " + pid);
                    }
                }
            } catch (DigitalObjectNotFoundException ex) {
                // no remote
            }
            if (!isNewObject) {
                if (ctx.isUseNewMetadata()) {
                    // vytvoreni noveho foxml z noveho pidu v importni davce
                    dObj = FoxmlUtils.unmarshal(new StreamSource(file), DigitalObject.class);
                    setDateAndUser(dObj);
                    repairDatastreams(dObj);
                    removeDataStreams(dObj);
                    createDataStreams(dObj, ctx);
                    lObj = iSession.getLocals().create(objFile, dObj);
                    updateLocalObject(lObj, ctx);

                    RelationEditor editor = new RelationEditor(object);
                    List<String> members = editor.getMembers();
                    if (members != null && !members.isEmpty()) {
                        RelationEditor editorLObj = new RelationEditor(lObj);
                        members.addAll(editorLObj.getMembers());
                        editorLObj.setMembers(members);
                        editorLObj.write(editorLObj.getLastModified(), "doplneni starych potomku");
                        lObj.flush();
                    }
                    importItem = iSession.addObject(lObj, true);
                } else if (ctx.isUseOriginalMetadata()) {
                    // nejprve vytvorim novy foxml pro jiz existuji pid
                    lObj = iSession.getLocals().create(objFile, dObj);

                    // pote z foxml v importni davce vytrahnu pouze potomky, ktere se maji pripadne doplnit
                    LocalObject localObject = new LocalStorage().load(pid, file);
                    RelationEditor relationEditor = new RelationEditor(localObject);
                    List<String> members = relationEditor.getMembers();
                    if (members != null && !members.isEmpty()) {
                        RelationEditor editorLObj = new RelationEditor(lObj);
                        members.addAll(0, editorLObj.getMembers());
                        editorLObj.setMembers(members);
                        editorLObj.write(editorLObj.getLastModified(), "doplneni novych potomku");
                        lObj.flush();
                    }
                    importItem = iSession.addObject(lObj, true);
                }
            } else {
                if (dObj == null) {
                    dObj = FoxmlUtils.unmarshal(new StreamSource(file), DigitalObject.class);
                }
                setDateAndUser(dObj);
                repairDatastreams(dObj);
                removeDataStreams(dObj);
                createDataStreams(dObj, ctx);
                lObj = iSession.getLocals().create(objFile, dObj);
                updateLocalObject(lObj, ctx);
                importItem = iSession.addObject(lObj, true);
            }
        } else {
            LOG.log(Level.SEVERE, "The object with pid: "+ pid + " was already imported!");
            throw new DigitalObjectException("The object with pid: "+ pid + " was already imported!");
        }
        lObj.flush();
        //modifyMetadataStreams(lObj.getPid(), model);
        importItem.setState(BatchItem.ObjectState.LOADED);
        iSession.getImportManager().update(importItem);
    }

    private boolean model2Override(String model) {
        List<String> acceptableModels = Arrays.asList(
                NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALVOLUME,
                OldPrintPlugin.MODEL_MONOGRAPHTITLE, OldPrintPlugin.MODEL_CONVOLUTTE,
                NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, NdkEbornPlugin.MODEL_EPERIODICAL, NdkEbornPlugin.MODEL_EPERIODICALVOLUME,
                NdkAudioPlugin.MODEL_MUSICDOCUMENT, NdkAudioPlugin.MODEL_PHONOGRAPH, NdkAudioPlugin.MODEL_SONG,
                ChroniclePlugin.MODEL_CHRONICLETITLE);
        return acceptableModels.contains(model);

    }

    private void repairDatastreams(DigitalObject dObj) {
        List<DatastreamType> datastreams = dObj.getDatastream();
        for (DatastreamType datastream : datastreams) {
            datastream.setCONTROLGROUP("M");
            if (datastreamVersionId.containsKey(datastream.getID())) {
                String newId = datastreamVersionId.get(datastream.getID());
                datastream.setID(newId);
                datastream.getDatastreamVersion().get(0).setID(newId + ".0");
            }
        }
    }

    private void removePart(ModsDefinition mods, String model) {
        if (!(NdkPlugin.MODEL_PAGE.equals(model) || NdkPlugin.MODEL_NDK_PAGE.equals(model))) {
            mods.getPart().clear();
        }
    }

    private void setRdaRules(ModsDefinition mods, String model) {
        if (!(NdkPlugin.MODEL_PAGE.equals(model) || NdkPlugin.MODEL_NDK_PAGE.equals(model))) {
            if (mods.getRecordInfo().isEmpty()) {
                mods.getRecordInfo().add(new RecordInfoDefinition());
            }
            if (mods.getRecordInfo().get(0).getDescriptionStandard().isEmpty()) {
                StringPlusLanguagePlusAuthority rdaRules = new StringPlusLanguagePlusAuthority();
                rdaRules.setValue("aacr");
                mods.getRecordInfo().get(0).getDescriptionStandard().add(rdaRules);
            }
        }
    }

    private void setOriginDate(ModsDefinition mods) {
        String date = null;
        if (!checkOriginDate(mods)) {
            for (PartDefinition partDefinition : mods.getPart()) {
                for (DateDefinition dateDefinition : partDefinition.getDate()) {
                    date = dateDefinition.getValue();
                    dateDefinition.setValue(null);
                    break;
                }
                if (date != null) {
                    break;
                }
            }

            if (date != null) {
                if (mods.getOriginInfo().isEmpty()) {
                    mods.getOriginInfo().add(new OriginInfoDefinition());
                }

                DateDefinition dateDefinition = new DateDefinition();
                dateDefinition.setValue(date);
                mods.getOriginInfo().get(0).getDateIssued().add(dateDefinition);
            }
        }
    }

    private boolean checkOriginDate(ModsDefinition mods) {
        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            if (!originInfo.getDateIssued().isEmpty()) {
                return true;
            }
        }
        return false;
    }

    private void setDefualtTitle(ModsDefinition mods) {
        String DEFAULT_NAME = "Nový model (imporován " + getTimestamp() +").";
        boolean emptyTitle = true;
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            if (!titleInfo.getTitle().isEmpty()) {
                emptyTitle = false;
                break;
            }
        }
        if (emptyTitle) {
            StringPlusLanguage title = new StringPlusLanguage();
            title.setValue(DEFAULT_NAME);

            if (mods.getTitleInfo().isEmpty()) {
                TitleInfoDefinition titleInfo = new TitleInfoDefinition();
                mods.getTitleInfo().add(titleInfo);
            }

            //mods.getTitleInfo().get(0).getTitle().add(title);
            mods.getTitleInfo().get(0).getPartNumber().add(title);
        }
    }

    private String getTimestamp() {
        SimpleDateFormat formatter= new SimpleDateFormat("yyyy-MM-dd 'at' HH:mm:ss z");
        Date date = new Date(System.currentTimeMillis());
        return formatter.format(date);
    }

    private void setDateAndUser(DigitalObject dObj) {
        List<PropertyType> properties = dObj.getObjectProperties().getProperty();
        for (PropertyType property: properties) {
            if (FoxmlUtils.PROPERTY_OWNER.equals(property.getNAME())) {
                property.setVALUE(iSession.options.getUsername());
            }
            if (FoxmlUtils.PROPERTY_CREATEDATE.equals(property.getNAME())) {
                String date = DateUtility.getXSDDateTime(new Date(System.currentTimeMillis()));
                property.setVALUE(date);
            }
        }
        return;
    }

    private void removeDataStreams(DigitalObject dObj) {
        List<DatastreamType> datastreams = copyDatastreams(dObj.getDatastream());
        dObj.getDatastream().clear();
        fillDatastreams(datastreams, dObj);
    }

    private void fillDatastreams(List<DatastreamType> datastreams, DigitalObject dObj) {
        for (int i = 0; i < datastreams.size(); i++) {
            if (KRAMERIUS_DATASTREAMS.contains(datastreams.get(i).getID())) {
                dObj.getDatastream().add(datastreams.get(i));
                if ("DC".equals(datastreams.get(i).getID())) {
                    XmlContentType xml = datastreams.get(i).getDatastreamVersion().get(0).getXmlContent();
                    Attr attribute =  xml.getAny().get(0).getAttributeNode("xsi:schemaLocation");

                    if (attribute != null) {
                       xml.getAny().get(0).getAttributes().removeNamedItem(attribute.getName());
                    }
                }
            }
        }
    }

    private List<DatastreamType> copyDatastreams(List<DatastreamType> dObjDatastreams) {
        List<DatastreamType> datastreams = new ArrayList<>();
        for (int i = 0; i < dObjDatastreams.size(); i++) {
            datastreams.add(dObjDatastreams.get(i));
        }
        return datastreams;
    }

    private void createDataStreams(DigitalObject digitalObject, ImportOptions ctx) {
        DatastreamType ndkArchival = null, ndkUser = null, raw = null, full;
        boolean containsArchival = containsDataStream(digitalObject, "NDK_ARCHIVAL");
        boolean containsUser = containsDataStream(digitalObject, "NDK_USER");
        boolean containsRaw = containsDataStream(digitalObject, "RAW");
        for (int i = 0; i < digitalObject.getDatastream().size(); i++) {
            replacePathInContentLocations(digitalObject.getDatastream().get(i), ctx);
            if ("FULL".equals(digitalObject.getDatastream().get(i).getID())) {
                full = digitalObject.getDatastream().get(i);
                if (!containsRaw) {
                    raw = createRaw(full, ctx);
                }
                if (!containsArchival) {
                    ndkArchival = createNdkArchivalDatastream(full, raw, ctx);
                }
                if (!containsUser) {
                    ndkUser = createNdkUserDatastream(full, raw, ctx);
                }
            }
        }
        if (raw != null) {
            digitalObject.getDatastream().add(raw);
        }
        if (ndkArchival != null) {
            digitalObject.getDatastream().add(ndkArchival);
        }
        if (ndkUser != null) {
            digitalObject.getDatastream().add(ndkUser);
        }
    }

    private DatastreamType createRaw(DatastreamType full, ImportOptions ctx) {
        if (full != null && full.getDatastreamVersion() != null && full.getDatastreamVersion().get(0) != null) {
            ContentLocationType fullContentLocation = full.getDatastreamVersion().get(0).getContentLocation();
            if (fullContentLocation != null && fullContentLocation.getREF() != null) {
                String path = fullContentLocation.getREF();
                boolean containsFilePrefix = false;
                if (path.startsWith("file:/")) {
                    containsFilePrefix = true;
                    path = path.substring(6);
                }
                File file = new File(path);
                FileSet fileSet =  new FileSet(ImportFileScanner.getName(file));
                fileSet.getFiles().add(new FileSet.FileEntry(file));
                if (file.exists()) {
                    TiffAsJpegImporter importer = new TiffAsJpegImporter(null);
                    File tiff = importer.getTiff(fileSet, ctx);
                    if (tiff != null) {
                        DatastreamType raw = new DatastreamType();
                        DatastreamVersionType datastreamVersionType = new DatastreamVersionType();

                        raw.setID(BinaryEditor.RAW_ID);
                        raw.setCONTROLGROUP(full.getCONTROLGROUP());
                        raw.setSTATE(full.getSTATE());
                        raw.setVERSIONABLE(full.isVERSIONABLE());
                        raw.getDatastreamVersion().add(datastreamVersionType);

                        datastreamVersionType.setID(BinaryEditor.RAW_ID + ".0");
                        datastreamVersionType.setLABEL(BinaryEditor.RAW_LABEL);
                        datastreamVersionType.setCREATED(full.getDatastreamVersion().get(0).getCREATED());
                        datastreamVersionType.setMIMETYPE(ImportProcess.findMimeType(tiff));
                        ContentLocationType contentLocation = new ContentLocationType();
                        contentLocation.setTYPE("URL");
                        if (containsFilePrefix) {
                            contentLocation.setREF("file:/" + tiff.getAbsolutePath());
                        } else {
                            contentLocation.setREF(tiff.getAbsolutePath());
                        }
                        datastreamVersionType.setContentLocation(contentLocation);

                        return raw;
                    }
                }

            }
        }
        return null;
    }

    private void replacePathInContentLocations(DatastreamType datastream, ImportOptions ctx) {
        if (datastream != null && datastream.getDatastreamVersion() != null && datastream.getDatastreamVersion().get(0) != null &&
        datastream.getDatastreamVersion().get(0).getContentLocation() != null) {
            ContentLocationType contentLocation = datastream.getDatastreamVersion().get(0).getContentLocation();
            if (contentLocation.getREF() != null) {
                String path = contentLocation.getREF();
                if (ctx.getFoxmlFolderPath() != null && ctx.getFoxmlImageServerPath() != null) {
                    if (path.contains(ctx.getFoxmlImageServerPath())) {
                        path = path.replaceAll(ctx.getFoxmlImageServerPath(), ctx.getFoxmlFolderPath());
                    }
                }
                contentLocation.setREF(path);
            }
            datastream.getDatastreamVersion().get(0).setContentLocation(contentLocation);
        }
    }

    private boolean containsDataStream(DigitalObject digitalObject, String name) {
        if (name == null) {
            return false;
        }
        for (int i = 0; i < digitalObject.getDatastream().size(); i++) {
            if (name.equals(digitalObject.getDatastream().get(i).getID())) {
                return true;
            }
        }
        return false;
    }

    private DatastreamType createNdkArchivalDatastream(DatastreamType full, DatastreamType raw, ImportOptions ctx) {
        DatastreamType ndkArchival = new DatastreamType();
        DatastreamVersionType datastreamVersionType = new DatastreamVersionType();

        ndkArchival.setID("NDK_ARCHIVAL");
        ndkArchival.setCONTROLGROUP(full.getCONTROLGROUP());
        ndkArchival.setSTATE(full.getSTATE());
        ndkArchival.setVERSIONABLE(full.isVERSIONABLE());
        ndkArchival.getDatastreamVersion().add(datastreamVersionType);

        datastreamVersionType.setID("NDK_ARCHIVAL.0");
        datastreamVersionType.setLABEL("NDK archive copy of RAW");
        datastreamVersionType.setCREATED(full.getDatastreamVersion().get(0).getCREATED());

        if (raw != null && raw.getDatastreamVersion() != null && raw.getDatastreamVersion().get(0) != null) {
                ContentLocationType rawContentLocation = raw.getDatastreamVersion().get(0).getContentLocation();
                if (rawContentLocation != null && rawContentLocation.getREF() != null) {
                    String path = rawContentLocation.getREF();
                    boolean containsFilePrefix = false;
                    if (path.startsWith("file:/")) {
                        containsFilePrefix = true;
                        path = path.substring(6);
                    }
                    File file = new File(path);
                    FileSet fileSet = new FileSet(ImportFileScanner.getName(file));
                    fileSet.getFiles().add(new FileSet.FileEntry(file));
                    FileSet.FileEntry entry = null;
                    if (file.exists()) {
                        TiffImporter importer = new TiffImporter(ImportBatchManager.getInstance());
                        try {
                            entry = importer.processJp2Copy(fileSet, file, ctx.getTargetFolder(), BinaryEditor.NDK_ARCHIVAL_ID, ctx.getConfig().getNdkArchivalProcessor());
                            if (entry != null && entry.getFile() != null) {
                                datastreamVersionType.setMIMETYPE(ImportProcess.findMimeType(entry.getFile()));
                                ContentLocationType contentLocation = new ContentLocationType();
                                datastreamVersionType.setContentLocation(contentLocation);
                                contentLocation.setTYPE("URL");
                                if (containsFilePrefix) {
                                    contentLocation.setREF("file:/" + entry.getFile().getAbsolutePath());
                                } else {
                                    contentLocation.setREF(entry.getFile().getAbsolutePath());
                                }
                                return ndkArchival;
                            }
                        } catch(IOException e){
                            LOG.log(Level.SEVERE, file.toString(), e);
                            return null;
                        }
                    }
                }
        } else {
            datastreamVersionType.setMIMETYPE(full.getDatastreamVersion().get(0).getMIMETYPE());
            datastreamVersionType.setSIZE(full.getDatastreamVersion().get(0).getSIZE());

            if (full.getDatastreamVersion().get(0).getBinaryContent() != null) {
                datastreamVersionType.setBinaryContent(full.getDatastreamVersion().get(0).getBinaryContent());
            } else if (full.getDatastreamVersion().get(0).getContentLocation() != null) {
                ContentLocationType contentLocation = full.getDatastreamVersion().get(0).getContentLocation();
                datastreamVersionType.setContentLocation(contentLocation);
            }
            return ndkArchival;
        }
        return null;
    }

    private DatastreamType createNdkUserDatastream(DatastreamType full, DatastreamType raw, ImportOptions ctx) {
        DatastreamType ndkUser = new DatastreamType();
        DatastreamVersionType datastreamVersionType = new DatastreamVersionType();

        ndkUser.setID("NDK_USER");
        ndkUser.setCONTROLGROUP(full.getCONTROLGROUP());
        ndkUser.setSTATE(full.getSTATE());
        ndkUser.setVERSIONABLE(full.isVERSIONABLE());
        ndkUser.getDatastreamVersion().add(datastreamVersionType);

        datastreamVersionType.setID("NDK_USER.0");
        datastreamVersionType.setLABEL("NDK user copy of RAW");
        datastreamVersionType.setCREATED(full.getDatastreamVersion().get(0).getCREATED());

        if (raw != null && raw.getDatastreamVersion() != null && raw.getDatastreamVersion().get(0) != null) {
            ContentLocationType rawContentLocation = raw.getDatastreamVersion().get(0).getContentLocation();
            if (rawContentLocation != null && rawContentLocation.getREF() != null) {
                String path = rawContentLocation.getREF();
                boolean containsFilePrefix = false;
                if (path.startsWith("file:/")) {
                    containsFilePrefix = true;
                    path = path.substring(6);
                }
                File file = new File(path);
                FileSet fileSet = new FileSet(ImportFileScanner.getName(file));
                fileSet.getFiles().add(new FileSet.FileEntry(file));
                FileSet.FileEntry entry = null;
                if (file.exists()) {
                    TiffImporter importer = new TiffImporter(ImportBatchManager.getInstance());
                    try {
                        entry = importer.processJp2Copy(fileSet, file, ctx.getTargetFolder(), BinaryEditor.NDK_USER_ID, ctx.getConfig().getNdkUserProcessor());
                        if (entry != null && entry.getFile() != null) {
                            datastreamVersionType.setMIMETYPE(ImportProcess.findMimeType(entry.getFile()));
                            ContentLocationType contentLocation = new ContentLocationType();
                            datastreamVersionType.setContentLocation(contentLocation);
                            contentLocation.setTYPE("URL");
                            if (containsFilePrefix) {
                                contentLocation.setREF("file:/" + entry.getFile().getAbsolutePath());
                            } else {
                                contentLocation.setREF(entry.getFile().getAbsolutePath());
                            }
                            return ndkUser;
                        }
                    } catch(IOException e){
                        LOG.log(Level.SEVERE, file.toString(), e);
                        return null;
                    }
                }
            }
        } else {
            datastreamVersionType.setMIMETYPE(full.getDatastreamVersion().get(0).getMIMETYPE());
            datastreamVersionType.setSIZE(full.getDatastreamVersion().get(0).getSIZE());

            if (full.getDatastreamVersion().get(0).getBinaryContent() != null) {
                datastreamVersionType.setBinaryContent(full.getDatastreamVersion().get(0).getBinaryContent());
            } else if (full.getDatastreamVersion().get(0).getContentLocation() != null) {
                ContentLocationType contentLocation = full.getDatastreamVersion().get(0).getContentLocation();
                datastreamVersionType.setContentLocation(contentLocation);
            }
            return ndkUser;
        }
        return null;
    }

    private void updateLocalObject(LocalObject localObject, ImportOptions ctx) throws DigitalObjectException {
        String modelId = null;
        RelationEditor relationEditor = new RelationEditor(localObject);
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(localObject);
        DigitalObjectHandler handler = new DigitalObjectHandler(localObject, MetaModelRepository.getInstance());

        try {
            ModsDefinition mods = modsStreamEditor.read();

            // set device
            if (isPage(relationEditor)) {
                relationEditor.setDevice(ctx.getDevice());
            }

            //repair mapping
            modelId = repairModelMapping(relationEditor, mods);

            //set members
            List<String> members = getMembers(relationEditor);
            if (members.size() > 0) {
                relationEditor.setMembers(members);
            }

            relationEditor.setRelations(new ArrayList<>());

            relationEditor.setOrganization(ctx.getOrganization());
            relationEditor.setStatus(DigitalObjectStatusUtils.STATUS_NEW);
            relationEditor.setUser(ctx.getConfig().getDefaultProcessor());

            relationEditor.write(relationEditor.getLastModified(), null);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Element RELS-EXT can not be override." + localObject.getPid());
        }

        try {
            NdkMapper mapper = NdkMapper.get(modelId);
            mapper.setModelId(modelId);
            NdkMapper.Context context = new NdkMapper.Context(handler);

            //repair Mods
            ModsStreamEditor modsStream = new ModsStreamEditor(localObject);
            ModsDefinition mods = modsStream.read();
            repairModsIdentifier(mods.getIdentifier());
            setDefualtTitle(mods);
            setOriginDate(mods);
            setRdaRules(mods, modelId);
            removePart(mods, modelId);
            mapper.createMods(mods, context);
            modsStream.write(mods, modsStream.getLastModified(), null);

            //repair Dc
            //repair DcDatastream
            OaiDcType dc = mapper.toDc(mods, context);
            DcStreamEditor dcEditor = handler.objectMetadata();
            DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
            dcr.setDc(dc);
            dcEditor.write(handler, dcr, null);

            //repair Label
            String label = mapper.toLabel(mods);
            localObject.setLabel(label);
            localObject.flush();
        } catch (Exception ex){
            LOG.log(Level.SEVERE, "Stream Mods can not be override. " + localObject.getPid());
        }
    }

    private void repairModsIdentifier(List<IdentifierDefinition> identifiers) {
        for (IdentifierDefinition identifier: identifiers) {
            if ("urn".equalsIgnoreCase(identifier.getType())) {
                identifier.setType("uuid");
            }
        }
    }

    private boolean isPage(RelationEditor relationEditor) throws DigitalObjectException {
        return NdkPlugin.MODEL_PAGE.equals(relationEditor.getModel());
    }

    private List<String> getMembers(RelationEditor relationEditor) throws DigitalObjectException{
        List<String> members = new ArrayList<>();
        for (int i = 0; i < relationEditor.getRelations().size(); i++) {
            Element element = relationEditor.getRelations().get(i);
            if (KRAMERIUS_PREFIX.contains(element.getTagName())) {
                if (element.getAttributeNode("rdf:resource") != null) {
                    String value = element.getAttributeNode("rdf:resource").getValue();
                    String name = value.substring("info:fedora/".length());
                    members.add(name);
                }
            }
        }
        return members;
    }

    private String repairModelMapping(RelationEditor relationEditor, ModsDefinition mods) throws DigitalObjectException {
        String oldModelId = relationEditor.getModel();
        if (oldModelId != null && !NdkPlugin.MODEL_PAGE.equals(oldModelId)) {
            String newModelId = "";
            switch(type) {
                case NDK_MONOGRAPH_MAP:
                    newModelId = getModelMonographMap().get(oldModelId);
                    break;
                case NDK_PERIODICAL_MAP:
                    newModelId = getModelPeriodicalMap().get(oldModelId);
                    break;
                /* not supported yet
                case STT_MAP:
                    newModelId = getModelSTTMap().get(oldModelId);
                    break;*/
                default:
                    newModelId = oldModelId;
                    break;
            }
            if (newModelId == null) {
                newModelId = oldModelId;
            }
            if (NdkPlugin.MODEL_PERIODICALISSUE.equals(newModelId)) {
                newModelId = checkModelInMods(newModelId, mods);
            }
            relationEditor.setModel(newModelId);
            return newModelId;
        }
        return oldModelId;
    }

    private String checkModelInMods(String newModelId, ModsDefinition mods) {
        for (PartDefinition part : mods.getPart()) {
            if ("PeriodicalSupplement".equals(part.getType())) {
                return NdkPlugin.MODEL_PERIODICALSUPPLEMENT;
            }
        }
        return newModelId;
    }

    private static String getFoxmlFilename(int index, String pid) {
        return getFilename(index, getObjectId(pid), "xml");
    }

    public static String getFilename(int index, String name, String ext) {
        return String.format("%04d_%s.%s", index, name, ext);
    }

    public static String getObjectId(String pid) {
        return pid.substring(pid.indexOf(':') + 1);
    }

    static class ImportSession {

        private final ImportBatchManager ibm;
        private final ImportOptions options;
        private final Batch batch;
        private final LocalStorage locals;
        private final SearchView search;
        private final Storage typeOfStorage;
        private RemoteStorage remotes;
        private AkubraStorage akubraStorage;
        /** The user cache. */
        private final Map<String, String> external2internalUserMap = new HashMap<String, String>();

        public ImportSession(ImportBatchManager ibm, ImportOptions options, AppConfiguration config) {
            this.typeOfStorage = config.getTypeOfStorage();
            try {
                if (Storage.FEDORA.equals(typeOfStorage)) {
                    this.remotes = RemoteStorage.getInstance();
                    this.search = this.remotes.getSearch();
                } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                    AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(config.getConfigHome());
                    this.akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                    this.search = this.akubraStorage.getSearch();
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + typeOfStorage);
                }
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
            this.locals = new LocalStorage();
            this.ibm = ibm;
            this.options = options;
            this.batch = options.getBatch();
        }

        public ImportBatchManager getImportManager() {
            return ibm;
        }

        public LocalStorage getLocals() {
            return locals;
        }

        public RemoteStorage getRemotes() {
            return remotes;
        }

        public Storage getTypeOfStorage() {
            return typeOfStorage;
        }

        public AkubraStorage getAkubraStorage() {
            return akubraStorage;
        }

        public LocalObject findLocalObject(BatchItemObject bio) {
            return bio == null || bio.getPid() == null
                    ? null : locals.load(bio.getPid(), bio.getFile());
        }

        public BatchItemObject findItem(String pid) {
            return ibm.findBatchObject(batch.getId(), pid);
        }

        public BatchItemObject addObject(LocalObject lobj, boolean root) throws DigitalObjectException {
            BatchItemObject bio = ibm.addLocalObject(options.getBatch(), lobj);
            if (root) {
                LocalObject rootObj = ibm.getRootObject(batch);
                RelationEditor editor = new RelationEditor(rootObj);
                List<String> members = editor.getMembers();
                members.add(lobj.getPid());
                editor.setMembers(members);
                editor.write(editor.getLastModified(), options.getUsername());
                rootObj.flush();
            }
            return bio;
        }
    }
}
