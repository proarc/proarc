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
package cz.cas.lib.proarc.common.process.imports.kramerius;

import com.yourmediashelf.fedora.generated.foxml.ContentLocationType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;
import com.yourmediashelf.fedora.util.DateUtility;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils;
import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.FileSet;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import cz.cas.lib.proarc.common.process.imports.TiffAsJpegImporter;
import cz.cas.lib.proarc.common.process.imports.TiffImporter;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import static cz.cas.lib.proarc.common.actions.ChangeModels.fixModsFromK4;
import static cz.cas.lib.proarc.common.actions.ChangeModels.fixNdkPageMods;

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
    public static final String NDK_MONOGRAPH_TITLE_MAP = "ndk_monograph_title";
    public static final String NDK_PERIODICAL_MAP = "ndk_periodical";
    public static final String NDK_EMONOGRAPH_MAP = "ndk_emonograph";
    public static final String NDK_EMONOGRAPH_TITLE_MAP = "ndk_emonograph_title";
    public static final String NDK_EPERIODICAL_MAP = "ndk_eperiodical";
    public static final String STT_MAP = "stt";
    public static final String K4_MAP = "default";

    private final Set<String> KRAMERIUS_PREFIX = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
            "kramerius:hasIntCompPart", "hasIntCompPart", "kramerius:hasItem", "hasItem",
            "kramerius:hasPage", "hasPage", "kramerius:hasUnit", "hasUnit", "kramerius:hasVolume", "hasVolume")));

    private final Set<String> KRAMERIUS_DATASTREAMS = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
            "RELS-EXT", "IMG_FULL", "IMG_PREVIEW", "IMG_THUMB", "TEXT_OCR", "ALTO", "BIBLIO_MODS", "DC", "NDK_ARCHIVAL", "NDK_USER", "FULL", "PREVIEW", "THUMBNAIL")));

    private final Set<String> KRAMERIUS_MANAGED_DATASTREAMS = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
            "IMG_FULL", "IMG_PREVIEW", "IMG_THUMB", "TEXT_OCR", "ALTO", "NDK_ARCHIVAL", "NDK_USER", "FULL", "PREVIEW", "THUMBNAIL")));

    private final Map<String, String> datastreamVersionId = new HashMap<String, String>() {
        {
            put("IMG_FULL", "FULL");
            put("IMG_THUMB", "THUMBNAIL");
            put("IMG_PREVIEW", "PREVIEW");
        }
    };

    private final Set<String> EMODELS_KRAMERIUS_DATASTREAMS = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
            "RAW", "RELS-EXT", "IMG_FULL", "IMG_PREVIEW", "IMG_THUMB", "TEXT_OCR", "ALTO", "BIBLIO_MODS", "DC", "NDK_ARCHIVAL", "NDK_USER", "FULL", "PREVIEW", "THUMBNAIL")));

    private final Map<String, String> eModelsDatastreamVersionId = new HashMap<String, String>() {
        {
            put("IMG_FULL", "RAW");
        }
    };

    // K4 model mapping
    private final Map<String, String> modelMonographMap = new HashMap<String, String>() {
        {
            put("model:article", NdkPlugin.MODEL_ARTICLE);
            put("model:map", NdkPlugin.MODEL_CARTOGRAPHIC);
            put("model:graphic", NdkPlugin.MODEL_GRAPHIC);
            put("model:internalpart", NdkPlugin.MODEL_CHAPTER);
            // put("model:monograph", NdkPlugin.MODEL_MONOGRAPHTITLE);
            put("model:supplement", NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT);
            put("model:monograph", NdkPlugin.MODEL_MONOGRAPHVOLUME);
//            put("model:monographunit", NdkPlugin.MODEL_MONOGRAPHUNIT);
            //put("model:periodical", NdkPlugin.MODEL_PERIODICAL);
            //put("model:periodicalitem", NdkPlugin.MODEL_PERIODICALISSUE);
            //put("model:supplement", NdkPlugin.MODEL_PERIODICALSUPPLEMENT);
            //put("model:periodicalvolume", NdkPlugin.MODEL_PERIODICALVOLUME);
            put("model:picture", NdkPlugin.MODEL_PICTURE);
            put("model:sheetmusic", NdkPlugin.MODEL_SHEETMUSIC);
        }
    };

    // Monograph Title model mapping
    private final Map<String, String> modelMonographTitleMap = new HashMap<String, String>() {
        {
            put("model:article", NdkPlugin.MODEL_ARTICLE);
            put("model:map", NdkPlugin.MODEL_CARTOGRAPHIC);
            put("model:graphic", NdkPlugin.MODEL_GRAPHIC);
            put("model:internalpart", NdkPlugin.MODEL_CHAPTER);
            // put("model:monograph", NdkPlugin.MODEL_MONOGRAPHTITLE);
            put("model:supplement", NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT);
            put("model:monograph", NdkPlugin.MODEL_MONOGRAPHTITLE);
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
            put("model:graphic", NdkPlugin.MODEL_GRAPHIC);
            put("model:chapter", NdkPlugin.MODEL_CHAPTER);
        }
    };

    private final Map<String, String> modelEMonographMap = new HashMap<String, String>() {
        {
            put("model:chapter", NdkEbornPlugin.MODEL_ECHAPTER);
            put("model:supplement", NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT);
            put("model:monograph", NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME);
//            put("model:monographunit", NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME);
        }
    };

    private final Map<String, String> modelEMonographTitleMap = new HashMap<String, String>() {
        {
            put("model:chapter", NdkEbornPlugin.MODEL_ECHAPTER);
            put("model:supplement", NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT);
            put("model:monograph", NdkEbornPlugin.MODEL_EMONOGRAPHTITLE);
            put("model:monographunit", NdkEbornPlugin.MODEL_EMONOGRAPHUNIT);
        }
    };

    private final Map<String, String> modelEPeriodicalMap = new HashMap<String, String>() {
        {
            put("model:periodical", NdkEbornPlugin.MODEL_EPERIODICAL);
            put("model:periodicalitem", NdkEbornPlugin.MODEL_EPERIODICALISSUE);
            put("model:supplement", NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT);
            put("model:periodicalvolume", NdkEbornPlugin.MODEL_EPERIODICALVOLUME);
            put("model:article", NdkEbornPlugin.MODEL_EARTICLE);
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

    public Map<String, String> getModelMonographTitleMap() {
        return modelMonographTitleMap;
    }

    public Map<String, String> getModelPeriodicalMap() {
        return modelPeriodicalMap;
    }

    public Map<String, String> getModelEMonographMap() {
        return modelEMonographMap;
    }

    public Map<String, String> getModelEMonographTitleMap() {
        return modelEMonographTitleMap;
    }

    public Map<String, String> getModelEPeriodicalMap() {
        return modelEPeriodicalMap;
    }

    public Map<String, String> getModelSTTMap() {
        return modelSTTMap;
    }

    public FileReader(File targetFolder, ImportSession session, String type) {
        this.targetFolder = targetFolder;
        this.iSession = session;
        this.type = type;
    }

    public void read(File file, ImportProcess.ImportOptions ctx, int index) throws IllegalStateException, DigitalObjectException {
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
        return name.substring(0, name.length() - 4);
    }

    private void readImpl(File file, ImportProcess.ImportOptions ctx, int index) throws DigitalObjectException {
        String pid = FoxmlUtils.PID_PREFIX + getUuidName(file.getName());

        // create foxml
        BatchManager.BatchItemObject importItem = iSession.findItem(pid);
        LocalObject lObj = iSession.findLocalObject(importItem);
        DigitalObject dObj = null;
        boolean isNewObject = lObj == null;
        String model = null;

        if (lObj == null) {
            File objFile = new File(targetFolder, getFoxmlFilename(index, pid));
            ProArcObject object = null;
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
                    repairDatastreams(dObj, ctx);
                    removeDataStreams(dObj, ctx);
                    createDataStreams(dObj, ctx);
                    lObj = iSession.getLocals().create(objFile, dObj);
                    updateLocalObject(lObj, ctx);

                    RelationEditor editor = new RelationEditor(object);
                    List<String> members = editor.getMembers();
                    if (members != null && !members.isEmpty()) {
                        RelationEditor editorLObj = new RelationEditor(lObj);
                        Set<String> membersSet = new LinkedHashSet<>(members);
                        membersSet.addAll(editorLObj.getMembers());
                        editorLObj.setMembers(new ArrayList<>(membersSet));
                        editorLObj.write(editorLObj.getLastModified(), "doplneni starych potomku");
                        lObj.flush();
                    }
                    importItem = iSession.addObject(lObj, true);
                } else if (ctx.isUseOriginalMetadata()) {
                    // nejprve vytvorim novy foxml pro jiz existuji pid
                    lObj = iSession.getLocals().create(objFile, dObj);

                    // pote z foxml v importni davce vytrahnu pouze potomky, ktere se maji pripadne doplnit
                    LocalObject localObject = new LocalStorage().load(pid, file);
                    updateLocalObject(localObject, ctx);
                    RelationEditor relationEditor = new RelationEditor(localObject);
                    List<String> members = relationEditor.getMembers();
                    if (members != null && !members.isEmpty()) {
                        RelationEditor editorLObj = new RelationEditor(lObj);
                        Set<String> membersSet = new LinkedHashSet<>(members);
                        membersSet.addAll(editorLObj.getMembers());
                        editorLObj.setMembers(new ArrayList<>(membersSet));
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
                repairDatastreams(dObj, ctx);
                removeDataStreams(dObj, ctx);
                createDataStreams(dObj, ctx);
                lObj = iSession.getLocals().create(objFile, dObj);
                updateLocalObject(lObj, ctx);

                importItem = iSession.addObject(lObj, true);
            }
        } else {
            LOG.log(Level.SEVERE, "The object with pid: " + pid + " was already imported!");
            throw new DigitalObjectException("The object with pid: " + pid + " was already imported!");
        }
        lObj.flush();
        //modifyMetadataStreams(lObj.getPid(), model);
        importItem.setState(BatchItem.ObjectState.LOADED);
        iSession.getImportManager().update(importItem);
    }

    public static boolean model2Override(String model) {
        List<String> acceptableModels = Arrays.asList(
                NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALVOLUME,
                OldPrintPlugin.MODEL_MONOGRAPHTITLE, OldPrintPlugin.MODEL_CONVOLUTTE,
                NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, NdkEbornPlugin.MODEL_EPERIODICAL, NdkEbornPlugin.MODEL_EPERIODICALVOLUME,
                NdkAudioPlugin.MODEL_MUSICDOCUMENT, NdkAudioPlugin.MODEL_PHONOGRAPH, NdkAudioPlugin.MODEL_SONG,
                ChroniclePlugin.MODEL_CHRONICLETITLE);
        return acceptableModels.contains(model);
    }

    private void repairDatastreams(DigitalObject dObj, ImportProcess.ImportOptions ctx) {
        List<DatastreamType> datastreams = dObj.getDatastream();
        for (DatastreamType datastream : datastreams) {
            if (KRAMERIUS_MANAGED_DATASTREAMS.contains(datastream.getID())) {
                datastream.setCONTROLGROUP("M");
            }

            if (ConfigurationProfile.NDK_EMONOGRAPH_KRAMERIUS_IMPORT.equals(ctx.getProfile().getProfileId())
                    || ConfigurationProfile.NDK_EMONOGRAPH_TITLE_KRAMERIUS_IMPORT.equals(ctx.getProfile().getProfileId())
                    || ConfigurationProfile.NDK_EPERIODICAL_KRAMERIUS_IMPORT.equals(ctx.getProfile().getProfileId())) {
                if (eModelsDatastreamVersionId.containsKey(datastream.getID())) {
                    String newId = eModelsDatastreamVersionId.get(datastream.getID());
                    datastream.setID(newId);
                    datastream.getDatastreamVersion().get(0).setID(newId + ".0");
                }
            } else {
                if (datastreamVersionId.containsKey(datastream.getID())) {
                    String newId = datastreamVersionId.get(datastream.getID());
                    datastream.setID(newId);
                    datastream.getDatastreamVersion().get(0).setID(newId + ".0");
                }
            }
        }
    }

    private void removePart(ModsDefinition mods, String model) {
        if (!(NdkPlugin.MODEL_PAGE.equals(model) || NdkPlugin.MODEL_NDK_PAGE.equals(model) || NdkPlugin.MODEL_CHAPTER.equals(model) || NdkPlugin.MODEL_ARTICLE.equals(model) || BornDigitalModsPlugin.MODEL_ARTICLE.equals(model))) {
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
        String DEFAULT_NAME = "Nový model (imporován " + getTimestamp() + ").";
        boolean emptyTitle = true;
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            if (!titleInfo.getTitleStringPlusLanguage().isEmpty()) {
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
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd 'at' HH:mm:ss z");
        Date date = new Date(System.currentTimeMillis());
        return formatter.format(date);
    }

    private void setDateAndUser(DigitalObject dObj) {
        List<PropertyType> properties = dObj.getObjectProperties().getProperty();
        for (PropertyType property : properties) {
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

    private void removeDataStreams(DigitalObject dObj, ImportProcess.ImportOptions ctx) {
        List<DatastreamType> datastreams = copyDatastreams(dObj.getDatastream());
        dObj.getDatastream().clear();
        fillDatastreams(datastreams, dObj, ctx);
    }

    private void fillDatastreams(List<DatastreamType> datastreams, DigitalObject dObj, ImportProcess.ImportOptions ctx) {
        for (int i = 0; i < datastreams.size(); i++) {
            if (ConfigurationProfile.NDK_EMONOGRAPH_KRAMERIUS_IMPORT.equals(ctx.getProfile().getProfileId())
                    || ConfigurationProfile.NDK_EMONOGRAPH_TITLE_KRAMERIUS_IMPORT.equals(ctx.getProfile().getProfileId())
                    || ConfigurationProfile.NDK_EPERIODICAL_KRAMERIUS_IMPORT.equals(ctx.getProfile().getProfileId())) {
                if (EMODELS_KRAMERIUS_DATASTREAMS.contains(datastreams.get(i).getID())) {
                    dObj.getDatastream().add(datastreams.get(i));
                    if ("DC".equals(datastreams.get(i).getID())) {
                        XmlContentType xml = datastreams.get(i).getDatastreamVersion().get(0).getXmlContent();
                        Attr attribute = xml.getAny().get(0).getAttributeNode("xsi:schemaLocation");

                        if (attribute != null) {
                            xml.getAny().get(0).getAttributes().removeNamedItem(attribute.getName());
                        }
                    }
                }
            } else {
                if (KRAMERIUS_DATASTREAMS.contains(datastreams.get(i).getID())) {
                    dObj.getDatastream().add(datastreams.get(i));
                    if ("DC".equals(datastreams.get(i).getID())) {
                        XmlContentType xml = datastreams.get(i).getDatastreamVersion().get(0).getXmlContent();
                        Attr attribute = xml.getAny().get(0).getAttributeNode("xsi:schemaLocation");

                        if (attribute != null) {
                            xml.getAny().get(0).getAttributes().removeNamedItem(attribute.getName());
                        }
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

    private void createDataStreams(DigitalObject digitalObject, ImportProcess.ImportOptions ctx) {
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

    private DatastreamType createRaw(DatastreamType full, ImportProcess.ImportOptions ctx) {
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
                FileSet fileSet = new FileSet(ImportFileScanner.getName(file));
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

            } else {
                if (full.getDatastreamVersion().get(0).getMIMETYPE() != null && "application/pdf".equals(full.getDatastreamVersion().get(0).getMIMETYPE())) {
                    byte[] binaryContent = full.getDatastreamVersion().get(0).getBinaryContent();
                    if (binaryContent != null && binaryContent.length > 0) {
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
                        datastreamVersionType.setMIMETYPE(full.getDatastreamVersion().get(0).getMIMETYPE());
                        datastreamVersionType.setBinaryContent(binaryContent);
                        return raw;
                    }
                }

            }
        }
        return null;
    }

    private void replacePathInContentLocations(DatastreamType datastream, ImportProcess.ImportOptions ctx) {
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

    private DatastreamType createNdkArchivalDatastream(DatastreamType full, DatastreamType raw, ImportProcess.ImportOptions ctx) {
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
                    TiffImporter importer = new TiffImporter(BatchManager.getInstance());
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
                    } catch (IOException e) {
                        LOG.log(Level.SEVERE, file.toString(), e);
                        return null;
                    }
                }
            }
        } else {
            datastreamVersionType.setMIMETYPE(full.getDatastreamVersion().get(0).getMIMETYPE());
//            datastreamVersionType.setSIZE(full.getDatastreamVersion().get(0).getSIZE());

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

    private DatastreamType createNdkUserDatastream(DatastreamType full, DatastreamType raw, ImportProcess.ImportOptions ctx) {
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
                    TiffImporter importer = new TiffImporter(BatchManager.getInstance());
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
                    } catch (IOException e) {
                        LOG.log(Level.SEVERE, file.toString(), e);
                        return null;
                    }
                }
            }
        } else {
            datastreamVersionType.setMIMETYPE(full.getDatastreamVersion().get(0).getMIMETYPE());
//            datastreamVersionType.setSIZE(full.getDatastreamVersion().get(0).getSIZE());

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

    private void updateLocalObject(LocalObject localObject, ImportProcess.ImportOptions ctx) throws DigitalObjectException {
        String modelId = null;
        RelationEditor relationEditor = new RelationEditor(localObject);
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(localObject);
        boolean containsPdf = containsPdf(localObject);

        try {
            ModsDefinition mods = modsStreamEditor.read();

            if (isPage(relationEditor)) {
                // set device
                relationEditor.setDevice(ctx.getDevice());

                // change page model if config
                if (NdkPlugin.MODEL_NDK_PAGE.equals(ctx.getModel())) {
                    relationEditor.setModel(NdkPlugin.MODEL_NDK_PAGE);
                    mods = fixNdkPageMods(mods);
                    modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), "Migrace na NDK stranu");
                }
            }

            //repair mapping
            modelId = repairModelMapping(relationEditor, mods, containsPdf);

            // nastaveni rootu batche
            if (NdkPlugin.MODEL_PERIODICAL.equals(modelId) || NdkPlugin.MODEL_MONOGRAPHTITLE.equals(modelId) || NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId)
                    || NdkPlugin.MODEL_GRAPHIC.equals(modelId) || NdkPlugin.MODEL_CARTOGRAPHIC.equals(modelId) || NdkPlugin.MODEL_SHEETMUSIC.equals(modelId)
                    || OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(modelId) || OldPrintPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId)
                    || OldPrintPlugin.MODEL_GRAPHICS.equals(modelId) || OldPrintPlugin.MODEL_CARTOGRAPHIC.equals(modelId) || OldPrintPlugin.MODEL_SHEETMUSIC.equals(modelId)
                    || NdkEbornPlugin.MODEL_EPERIODICAL.equals(modelId) || NdkEbornPlugin.MODEL_EMONOGRAPHTITLE.equals(modelId) || NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME.equals(modelId)
                    || NdkAudioPlugin.MODEL_MUSICDOCUMENT.equals(modelId) || NdkAudioPlugin.MODEL_PHONOGRAPH.equals(modelId)) {
                Batch batch = ctx.getBatch();
                batch.setParentPid(localObject.getPid());
                batch = iSession.getImportManager().update(batch);
                ctx.setBatch(batch);
            }

            // zjisteni, zda se jednalo o puvodni K4 modely a zapsani jejich uuid k budoucimu updatu
            if (NdkPlugin.MODEL_PERIODICAL.equals(modelId) || NdkPlugin.MODEL_PERIODICALVOLUME.equals(modelId) || NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId)
                    || NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId) || NdkPlugin.MODEL_MONOGRAPHTITLE.equals(modelId) || NdkPlugin.MODEL_MONOGRAPHUNIT.equals(modelId)) {
                ctx.getPidsToUpdate().add(localObject.getPid());

                if (NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId)) {
                    if (mods != null && mods.getPart() != null && mods.getPart().size() > 0 && mods.getPart().get(0) != null && "PeriodicalIssue".equals(mods.getPart().get(0).getType())) {
                        ctx.setWasK4Model(true);
                    }
                } else if (NdkPlugin.MODEL_MONOGRAPHUNIT.equals(modelId) || NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId)) {
                    if (mods != null && mods.getPart() != null && mods.getPart().size() > 0 && mods.getPart().get(0) != null && "Volume".equals(mods.getPart().get(0).getType())) {
                        ctx.setWasK4Model(true);
                    }
                }
            }

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
        if (BornDigitalModsPlugin.MODEL_ARTICLE.equals(modelId)) {
            localObject.purgeDatastream(BinaryEditor.FULL_ID, "Odebrani nepotrebneho streamu " + BinaryEditor.FULL_ID);
            localObject.purgeDatastream(BinaryEditor.NDK_ARCHIVAL_ID, "Odebrani nepotrebneho streamu " + BinaryEditor.NDK_ARCHIVAL_ID);
            localObject.purgeDatastream(BinaryEditor.NDK_USER_ID, "Odebrani nepotrebneho streamu " + BinaryEditor.NDK_USER_ID);
        }
        localObject.flush();

        DigitalObjectHandler handler = new DigitalObjectHandler(localObject, MetaModelRepository.getInstance());
        try {
            NdkMapper mapper = NdkMapper.get(modelId);
            mapper.setModelId(modelId);
            NdkMapper.Context context = new NdkMapper.Context(handler);

            //repair Mods
            ModsStreamEditor modsStream = new ModsStreamEditor(localObject);
            ModsDefinition mods = modsStream.read();
            try {
                mods = fixModsFromK4(localObject.getPid(), mods, modelId, getK4Model(modelId), null);
            } catch (DigitalObjectException ex) {
                if (!ex.getMessage().contains("ChangeModels:fixMods")) {
                    throw ex;
                }
            }
            repairModsIdentifier(mods.getIdentifier());
//            setDefualtTitle(mods);
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
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Stream Mods can not be override. " + localObject.getPid());
        }
    }


    public static String getK4Model(String modelId) {
        if (NdkPlugin.MODEL_MONOGRAPHTITLE.equals(modelId) || NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId)) {
            return K4Plugin.MODEL_MONOGRAPH;
        } else if (NdkPlugin.MODEL_MONOGRAPHUNIT.equals(modelId)) {
            return K4Plugin.MODEL_MONOGRAPHUNIT;
        } else if (NdkPlugin.MODEL_PERIODICAL.equals(modelId)) {
            return K4Plugin.MODEL_PERIODICAL;
        } else if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(modelId)) {
            return K4Plugin.MODEL_PERIODICALVOLUME;
        } else if (NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId)) {
            return K4Plugin.MODEL_PERIODICALITEM;
        }
        return modelId;
    }

    private boolean containsPdf(LocalObject localObject) {
        if (localObject == null || localObject.getDigitalObject() == null) {
            return false;
        }
        for (DatastreamType datastream : localObject.getDigitalObject().getDatastream()) {
            if (BinaryEditor.FULL_ID.equals(datastream.getID())) {
                for (DatastreamVersionType datastreamVersion : datastream.getDatastreamVersion()) {
                    if (datastreamVersion.getMIMETYPE() != null && "application/pdf".equals(datastreamVersion.getMIMETYPE())) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private void repairModsIdentifier(List<IdentifierDefinition> identifiers) {
        for (IdentifierDefinition identifier : identifiers) {
            if ("urn".equalsIgnoreCase(identifier.getTypeString())) {
                identifier.setTypeString("uuid");
            }
        }
    }

    private boolean isPage(RelationEditor relationEditor) throws DigitalObjectException {
        return NdkPlugin.MODEL_PAGE.equals(relationEditor.getModel());
    }

    private List<String> getMembers(RelationEditor relationEditor) throws DigitalObjectException {
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

    private String repairModelMapping(RelationEditor relationEditor, ModsDefinition mods, boolean containsPdf) throws DigitalObjectException {
        String oldModelId = relationEditor.getModel();
        if (oldModelId != null && !NdkPlugin.MODEL_PAGE.equals(oldModelId)) {
            String newModelId = "";
            switch (type) {
                case NDK_MONOGRAPH_MAP:
                    newModelId = getModelMonographMap().get(oldModelId);
                    break;
                case NDK_MONOGRAPH_TITLE_MAP:
                    newModelId = getModelMonographTitleMap().get(oldModelId);
                    break;
                case NDK_PERIODICAL_MAP:
                    newModelId = getModelPeriodicalMap().get(oldModelId);
                    break;
                case NDK_EMONOGRAPH_MAP:
                    newModelId = getModelEMonographMap().get(oldModelId);
                    break;
                case NDK_EMONOGRAPH_TITLE_MAP:
                    newModelId = getModelEMonographTitleMap().get(oldModelId);
                    break;
                case NDK_EPERIODICAL_MAP:
                    newModelId = getModelEPeriodicalMap().get(oldModelId);
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
            if (NdkPlugin.MODEL_ARTICLE.equals(newModelId) && containsPdf) {
                newModelId = BornDigitalModsPlugin.MODEL_ARTICLE;
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

        private final BatchManager ibm;
        private final ImportProcess.ImportOptions options;
        private final Batch batch;
        private final LocalStorage locals;
        private final SearchView search;
        private final Storage typeOfStorage;
        private FedoraStorage remotes;
        private AkubraStorage akubraStorage;
        /**
         * The user cache.
         */
        private final Map<String, String> external2internalUserMap = new HashMap<String, String>();

        public ImportSession(BatchManager ibm, ImportProcess.ImportOptions options, AppConfiguration config) {
            this.typeOfStorage = config.getTypeOfStorage();
            try {
                if (Storage.FEDORA.equals(typeOfStorage)) {
                    this.remotes = FedoraStorage.getInstance();
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

        public BatchManager getImportManager() {
            return ibm;
        }

        public LocalStorage getLocals() {
            return locals;
        }

        public FedoraStorage getRemotes() {
            return remotes;
        }

        public Storage getTypeOfStorage() {
            return typeOfStorage;
        }

        public AkubraStorage getAkubraStorage() {
            return akubraStorage;
        }

        public LocalObject findLocalObject(BatchManager.BatchItemObject bio) {
            return bio == null || bio.getPid() == null
                    ? null : locals.load(bio.getPid(), bio.getFile());
        }

        public BatchManager.BatchItemObject findItem(String pid) {
            return ibm.findBatchObject(batch.getId(), pid);
        }

        public BatchManager.BatchItemObject addObject(LocalObject lobj, boolean root) throws DigitalObjectException {
            BatchManager.BatchItemObject bio = ibm.addLocalObject(options.getBatch(), lobj);
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
