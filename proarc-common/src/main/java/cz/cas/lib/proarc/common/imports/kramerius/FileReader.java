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

import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;
import com.yourmediashelf.fedora.util.DateUtility;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import java.io.File;
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
            "RELS-EXT", "IMG_FULL", "IMG_PREVIEW", "IMG_THUMB", "TEXT_OCR", "ALTO", "BIBLIO_MODS", "DC", "NDK_ARCHIVAL", "NDK_USER")));

    // K4 model mapping
    private final Map<String, String> modelMonographMap = new HashMap<String, String>() {
        {
            put("model:article", NdkPlugin.MODEL_ARTICLE);
            put("model:map", NdkPlugin.MODEL_CARTOGRAPHIC);
            // put("model:monograph", NdkPlugin.MODEL_MONOGRAPHTITLE);
            put("model:supplement", NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT);
            put("model:monograph", NdkPlugin.MODEL_MONOGRAPHVOLUME);
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

    public void read(File file, ImportOptions ctx, int index) throws IllegalStateException {
        try {
            readImpl(file, ctx, index);
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

        if (lObj == null) {
            File objFile = new File(targetFolder, getFoxmlFilename(index, pid));
            RemoteObject remote = iSession.getRemotes().find(pid);
            try {
                String foxml = remote.asText();
                dObj = FoxmlUtils.unmarshal(foxml, DigitalObject.class);
                isNewObject = false;
                if (!isNewObject) {
                    throw new DigitalObjectException("The repository already contains pid: " + pid);
                }
            } catch (DigitalObjectNotFoundException ex) {
                // no remote
            }
            if (dObj == null) {
                dObj = FoxmlUtils.unmarshal(new StreamSource(file), DigitalObject.class);
            }
            setDateAndUser(dObj);
            removeDataStreams(dObj);
            createDataStreams(dObj);
            lObj = iSession.getLocals().create(objFile, dObj);
            updateLocalObject(lObj, ctx);
            importItem = iSession.addObject(lObj, true);
        } else {
            LOG.log(Level.SEVERE, "The object with pid: "+ pid + " was already imported!");
            throw new DigitalObjectException("The object with pid: "+ pid + " was already imported!");
        }
        lObj.flush();
        importItem.setState(BatchItem.ObjectState.LOADED);
        iSession.getImportManager().update(importItem);
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

    private void createDataStreams(DigitalObject digitalObject) {
        DatastreamType ndkArchival = null, ndkUser = null, full;
        boolean containsArchival = containsDataStream(digitalObject, "NDK_ARCHIVAL");
        boolean containsUser = containsDataStream(digitalObject, "NDK_USER");
        for (int i = 0; i < digitalObject.getDatastream().size(); i++) {
            if ("IMG_FULL".equals(digitalObject.getDatastream().get(i).getID())) {
                full = digitalObject.getDatastream().get(i);
                if (!containsArchival) {
                    ndkArchival = createNdkArchivalDatastream(full);
                }
                if (!containsUser) {
                    ndkUser = createNdkUserDatastream(full);
                }
                break;
            }
        }
        if (ndkArchival != null) {
            digitalObject.getDatastream().add(ndkArchival);
        }
        if (ndkUser != null) {
            digitalObject.getDatastream().add(ndkUser);
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

    private DatastreamType createNdkArchivalDatastream(DatastreamType full) {
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
        datastreamVersionType.setMIMETYPE(full.getDatastreamVersion().get(0).getMIMETYPE());
        datastreamVersionType.setSIZE(full.getDatastreamVersion().get(0).getSIZE());
        datastreamVersionType.setBinaryContent(full.getDatastreamVersion().get(0).getBinaryContent());

        return ndkArchival;
    }

    private DatastreamType createNdkUserDatastream(DatastreamType full) {
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
        datastreamVersionType.setMIMETYPE(full.getDatastreamVersion().get(0).getMIMETYPE());
        datastreamVersionType.setSIZE(full.getDatastreamVersion().get(0).getSIZE());
        datastreamVersionType.setBinaryContent(full.getDatastreamVersion().get(0).getBinaryContent());

        return ndkUser;
    }

    private LocalObject updateLocalObject(LocalObject localObject, ImportOptions ctx) {
        try {
            RelationEditor relationEditor = new RelationEditor(localObject);

            // set device
            if (isPage(relationEditor)) {
                relationEditor.setDevice(ctx.getDevice());
            }

            //repair mapping
            repairModelMapping(relationEditor);

            //set members
            List<String> members = getMembers(relationEditor);
            if (members.size() > 0) {
                relationEditor.setMembers(members);
            }

            relationEditor.setRelations(new ArrayList<>());
            relationEditor.write(relationEditor.getLastModified(), null);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Element RELS-EXT can not be override.");
        }
        return localObject;
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

    private void repairModelMapping(RelationEditor relationEditor) throws DigitalObjectException {
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
            relationEditor.setModel(newModelId);
        }
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
        private final RemoteStorage remotes;
        /** The user cache. */
        private final Map<String, String> external2internalUserMap = new HashMap<String, String>();

        public ImportSession(ImportBatchManager ibm, ImportOptions options) {
            this.remotes = RemoteStorage.getInstance();
            this.search = remotes.getSearch();
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
