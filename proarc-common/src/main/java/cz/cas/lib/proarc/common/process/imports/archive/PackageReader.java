/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.imports.archive;

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.dublincore.DcUtils;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.BatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.process.export.archive.PackageBuilder;
import cz.cas.lib.proarc.common.process.export.archive.PackageBuilder.MdType;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.process.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.Rdf;
import cz.cas.lib.proarc.common.storage.relation.RdfRelation;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.storage.relation.Relations;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.DivType.Fptr;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.FileType.FLocat;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType.FileSec;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXB;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Node;

import static cz.cas.lib.proarc.common.process.export.mets.Const.EXPORT_PATH_ALTO;
import static cz.cas.lib.proarc.common.process.export.mets.Const.EXPORT_PATH_MASTERCOPY;
import static cz.cas.lib.proarc.common.process.export.mets.Const.EXPORT_PATH_MASTERCOPY_AUDIO;
import static cz.cas.lib.proarc.common.process.export.mets.Const.EXPORT_PATH_TXT;
import static cz.cas.lib.proarc.common.process.export.mets.Const.EXPORT_PATH_USERCOPY;
import static cz.cas.lib.proarc.common.process.export.mets.Const.EXPORT_PATH_USERCOPY_AUDIO;
import static cz.cas.lib.proarc.common.process.imports.kramerius.FileReader.model2Override;

/**
 * It reads the proarc archive package and generates digital objects for a batch import.
 *
 * @author Jan Pokorsky
 */
public class PackageReader {

    private File metsFile;
    private final File targetFolder;
    private URI metsUri;
    private Mets mets;
    private final ImportSession iSession;
    private String pkgModelId;
    private boolean isParentObject;
    private final List<String> physicalPath = new ArrayList<String>();
    private final AppConfiguration configuration;
    private int lastPageIndex = 0;
    private int lastAudioIndex = 1;

    public PackageReader(File targetFolder, ImportSession session, AppConfiguration config) {
        this.targetFolder = targetFolder;
        this.iSession = session;
        this.configuration = config;
    }

    public void read(File metsFile, ImportOptions ctx) throws IllegalStateException, DigitalObjectException {
        try {
            readImpl(metsFile, ctx);
        } catch (DigitalObjectException ex) {
            if (ex != null && ex.getPid() != null && ex.getPid().contains("The repository already contains pid:")) {
                ex.setMessage(metsFile.getAbsolutePath());
                throw ex;
            } else {
                throw new IllegalStateException(metsFile.getAbsolutePath(), ex);
            }
        } catch (Exception ex) {
            throw new IllegalStateException(metsFile.getAbsolutePath(), ex);
        }
    }

    private void readImpl(File metsFile, ImportOptions ctx) throws DigitalObjectException {
        this.metsFile = metsFile;
        this.metsUri = metsFile.toURI();
        this.mets = JAXB.unmarshal(metsFile, Mets.class);
        this.isParentObject = true;
        this.physicalPath.clear();
        pkgModelId = mets.getTYPE();
        if (pkgModelId == null) {
            throw new IllegalStateException("Unknown mets@TYPE:" + pkgModelId);
        }
        StructMapType otherMap = getStructMap(mets, PackageBuilder.STRUCTMAP_OTHERS_TYPE);
        HashMap<String, String> devices = processDevices(otherMap, ctx);

        StructMapType physicalMap = getStructMap(mets, PackageBuilder.STRUCTMAP_PHYSICAL_TYPE);
        DivType div = physicalMap.getDiv();
        processObject(1, div, null, devices, ctx);
    }

    private List<String> processChildObjects(List<DivType> divs, HashMap<String, String> devices, ImportOptions ctx) throws DigitalObjectException {
        ArrayList<String> pids = new ArrayList<String>(100);
        for (DivType div : divs) {
            String pid = toPid(div);
            pids.add(pid);
        }
        if (!isParentObject && !pids.isEmpty()) {
            // check whether child objects exist in the repository
            iSession.checkRemote(pids);
        }
        File newArchive = null;
        if (containsNdkFolder(metsFile.getParentFile())) {
            newArchive = getNdkFolder(metsFile.getParentFile());
        }
        int childIndexPage = 1;
        int childIndexAudio = 1;
        int childIndex = 1;
        for (DivType div : divs) {
            int index = 0;
            if (NdkAudioPlugin.MODEL_PAGE.equals(div.getTYPE())) {
                index = childIndexAudio++;
            } else if (NdkPlugin.MODEL_PAGE.equals(div.getTYPE()) || NdkPlugin.MODEL_NDK_PAGE.equals(div.getTYPE()) || OldPrintPlugin.MODEL_PAGE.equals(div.getTYPE())){
                index = childIndexPage++;
            } else {
                index = childIndex++;
            }
            processObject(index, div, newArchive, devices, ctx);
        }
        return pids;
    }

    private boolean containsNdkFolder(File parentFile) {
        return getNdkFolder(parentFile) != null;
    }

    private File getNdkFolder(File parentFile) {
        if (parentFile != null) {
            for (File file : parentFile.listFiles()) {
                if ("NDK".equals(file.getName())) {
                    for (File child : file.listFiles()) {
                        if (child.isDirectory()) {
                            return child;
                        }
                    }
                }
            }
        }
        return null;
    }

    private HashMap<String, String> processDevices(StructMapType structMap, ImportOptions ctx) {
        DivType devicesDiv = null;
        if (structMap != null) {
            DivType div = structMap.getDiv();
            if (div != null && PackageBuilder.DIV_DEVICE_LIST_ID.equals(div.getID())) {
                devicesDiv = div;
            }
        }
        HashMap<String, String> devices = new HashMap<>();
        if (devicesDiv != null) {
            int index = 1;
            for (DivType deviceDiv : devicesDiv.getDiv()) {
                devices = processDevice(index++, deviceDiv, devices, ctx);
            }
        }
        return devices;
    }

    private HashMap<String, String> processDevice(int divIndex, DivType deviceDiv, HashMap devices, ImportOptions ctx) {
        if (!DeviceRepository.METAMODEL_ID.equals(deviceDiv.getTYPE())) {
            throw new IllegalStateException("Unexpected type of device: " + toString(deviceDiv));
        }
        String pid = toPid(deviceDiv);
        try {
            BatchItemObject importItem = iSession.findItem(pid);
            LocalObject lObj = iSession.findLocalObject(importItem);
            boolean isNewObject = lObj == null;
            if (lObj == null) {
                File objFile = new File(targetFolder, getFoxmlFilename("DESCRIPTION", divIndex, pid, DeviceRepository.METAMODEL_ID));
                ProArcObject object = null;
                if (Storage.FEDORA.equals(iSession.getTypeOfStorage())) {
                    object = iSession.getRemotes().find(pid);
                } else if (Storage.AKUBRA.equals(iSession.getTypeOfStorage())) {
                    object = iSession.getAkubraStorage().find(pid);
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + iSession.getTypeOfStorage());
                }
                DigitalObject dObj = null;
                try {
                    String foxml = object.asText();
                    dObj = FoxmlUtils.unmarshal(foxml, DigitalObject.class);
                    isNewObject = false;
                } catch (DigitalObjectNotFoundException ex) {
                    // no remote
                }
                if (dObj == null) { // zkousi najit hlavni pid a s tim pote pracovat
                    String mainPid = getMainPid(pid);
                    if (mainPid != null) {
                        if (Storage.FEDORA.equals(iSession.getTypeOfStorage())) {
                            object = iSession.getRemotes().find(mainPid);
                        } else if (Storage.AKUBRA.equals(iSession.getTypeOfStorage())) {
                            object = iSession.getAkubraStorage().find(mainPid);
                        } else {
                            throw new IllegalStateException("Unsupported type of storage: " + iSession.getTypeOfStorage());
                        }
                        try {
                            String foxml = object.asText();
                            dObj = FoxmlUtils.unmarshal(foxml, DigitalObject.class);
                            devices.put(pid, mainPid);
                            isNewObject = false;
                        } catch (DigitalObjectNotFoundException ex) {
                            // no remote
                        }
                    }
                }


                if (dObj == null) {
                    dObj = FoxmlUtils.createFoxml(pid);
                }
                lObj = iSession.getLocals().create(objFile, dObj);
                if (isNewObject) {
                    lObj.setOwner(iSession.resolveUsername(null));
                    lObj.setLabel(deviceDiv.getLabel3());
                } else {
                    lObj.setRemoteCopy(true);
                }
                importItem = iSession.addObject(lObj, physicalPath.isEmpty());

                if (isNewObject) {
                    createDatastreams(lObj, deviceDiv, Collections.<String>emptyList(), null, devices, ctx);
                }
                lObj.flush();
                importItem.setState(ObjectState.LOADED);
                iSession.getImportManager().update(importItem);
                return devices;
            }
        } catch (Exception ex) {
            throw new IllegalStateException(toString(deviceDiv), ex);
        }
        return devices;
    }

    private String getMainPid(String pid) {
        String mainPid = configuration.getDevices().getMainUUid(pid);
        return mainPid;
    }

    private void processObject(int divIndex, DivType div, File ndkFolder, HashMap<String, String> devices, ImportOptions ctx) throws DigitalObjectException {
        String modelId = div.getTYPE();
        boolean isPkgModel = pkgModelId.equals(modelId);
        String pid = toPid(div);

        // create foxml
        BatchItemObject importItem = iSession.findItem(pid);
        LocalObject lObj = iSession.findLocalObject(importItem);
        boolean isNewObject = lObj == null;
        if (lObj == null) {
            File objFile = new File(targetFolder, getFoxmlFilename("FOXML", divIndex, pid, modelId));
            DigitalObject dObj = null;
            if (isParentObject) {
                iSession.setRootPid(pid);
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
                    if (!model2Override(object.getModel()) && !ctx.isUseNewMetadata() && !ctx.isUseOriginalMetadata()) {
                        throw new DigitalObjectException("The repository already contains pid: " + pid);
                    }
                } catch (DigitalObjectNotFoundException ex) {
                    // no remote
                }
            }
            if (dObj == null) {
                dObj = FoxmlUtils.createFoxml(pid);
            }
            lObj = iSession.getLocals().create(objFile, dObj);
            if (isNewObject) {
                lObj.setOwner(iSession.resolveUsername(null));
                lObj.setLabel(div.getLabel3());
            } else {
                lObj.setRemoteCopy(true);
            }
            importItem = iSession.addObject(lObj, physicalPath.isEmpty());
        } else if (isPkgModel || !isParentObject) {
            throw new IllegalStateException("The object was already imported! " + toString(div));
        }
        try {
            if (isPkgModel) {
                if (!isParentObject) {
                    throw new IllegalStateException("An invalid model inside the package: " + toString(div));
                }
                isParentObject = false;
            }
            if ((isParentObject || isPkgModel) && lObj.isRemoteCopy()) {
                // validate the parents are same
                iSession.checkObjectParent(physicalPath, pid);
            }
            physicalPath.add(pid);
            List<String> childPids = processChildObjects(div.getDiv(), devices, ctx);
            physicalPath.remove(pid);
            if (isNewObject) {
                createDatastreams(lObj, div, childPids, ndkFolder, devices, ctx);
            } else {
                mergeDatastreams(lObj, div, childPids);
            }
            lObj.flush();
            importItem.setState(ObjectState.LOADED);
            iSession.getImportManager().update(importItem);
        } finally {
            if (isPkgModel) {
                isParentObject = true;
            }
        }
    }

    private void mergeDatastreams(LocalObject lObj, DivType objectDiv, List<String> childPids) throws DigitalObjectException {
        List<Fptr> fPtrs = objectDiv.getFptr();
        for (Fptr fPtr : fPtrs) {
            Object fileid = fPtr.getFILEID();
            if (fileid instanceof FileType) {
                FileType fileType = (FileType) fileid;
                mergeDatastream(lObj, fileType, childPids);
            } else {
                throw new IllegalStateException(
                        "METS: Unexpected <fptr> " + fileid + ", div@id: " + objectDiv.getID());
            }
        }
    }

    /**
     * It is supposed to merge an imported data stream to the local object.
     * For now it supports only RELS-EXT.
     * @param lObj the local dig. object
     * @param fileType the imported data stream as an external file
     * @param childPids the list of child PIDs declared in this package
     *      that should be merged to the existing list of members in RELS-EXT
     * @throws DigitalObjectException
     */
    private void mergeDatastream(LocalObject lObj, FileType fileType, List<String> childPids) throws DigitalObjectException {
        String dsId = toValidDsId(fileType);
        if (RelationEditor.DATASTREAM_ID.equals(dsId)) {
            File dsFile = toFile(fileType);
            Rdf rdf = Relations.unmarshal(new StreamSource(dsFile), Rdf.class);
            RelationEditor editor = new RelationEditor(lObj);
            String archivedModelId = RdfRelation.toPid(rdf.getDescription().getModel());
            String existingModelId = editor.getModel();
            if (archivedModelId == null ? existingModelId != null : !archivedModelId.equals(existingModelId)) {
                throw new IllegalStateException(String.format("Unexpected models %s != %s, ", existingModelId, archivedModelId, toString(fileType)));
            }
            List<String> existingMembers = editor.getMembers();
            List<String> archivedMembers = RelationEditor.relationAsPid(rdf.getDescription().getMemberRelations());
            for (String childPid : childPids) {
                mergeMembers(childPid, archivedMembers, existingMembers);
            }
            editor.setMembers(existingMembers);
            editor.write(editor.getLastModified(), null);
        }
    }

    private void createDatastreams(LocalObject lObj, DivType objectDiv, List<String> childPids, File ndkFolder, HashMap<String, String> devices, ImportOptions ctx) throws DigitalObjectException {
        List<Fptr> fPtrs = objectDiv.getFptr();
        for (Fptr fPtr : fPtrs) {
            Object fileid = fPtr.getFILEID();
            if (fileid instanceof FileType) {
                FileType fileType = (FileType) fileid;
                createDatastream(lObj, fileType, childPids, devices, ctx);
            } else {
                throw new IllegalStateException(
                        "METS: Unexpected <fptr> " + fileid + ", div@id: " + objectDiv.getID());
            }
        }

        for (Object dmdObject : objectDiv.getDMDID()) {
            if (dmdObject instanceof MdSecType) {
                createDatastream(lObj, (MdSecType) dmdObject);
            } else {
                throw new IllegalStateException(
                        "METS: Unexpected <dmdSec> " + dmdObject + ", div@id: " + objectDiv.getID());
            }
        }

        if (ndkFolder != null) {
            String[] fileName = lObj.getFoxml().getName().split("_");
            String seq = null;
            String model = null;
            if (fileName.length == 4) {
                seq = fileName[2];
                model = fileName[1];
            }

            if ("page".equals(model) || "ndkpage".equals(model) || "oldprintpage".equals(model)) {
                if (seq != null) {
                    if (lastPageIndex > Integer.valueOf(seq)) {
                        seq = String.format("%04d", lastPageIndex + 1);
                    }
                    lastPageIndex++;
                    if (ndkFolder.isDirectory()) {
                        for (File folder : ndkFolder.listFiles()) {
                            if (folder.isDirectory()) {
                                if (EXPORT_PATH_MASTERCOPY.equals(folder.getName()) || Const.EXPORT_PATH_USERCOPY.equals(folder.getName()) || Const.EXPORT_PATH_TXT.equals(folder.getName()) || EXPORT_PATH_ALTO.equals(folder.getName())) {
                                    for (File file : folder.listFiles()) {
                                        String[] fileNames = file.getName().split("\\.");
                                        if (fileNames[0].endsWith(seq)) {
                                            createDatastream(lObj, file);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else if (NdkAudioPlugin.MODEL_PAGE.contains(model)) {
                if (seq != null) {
                    if (lastAudioIndex > Integer.valueOf(seq)) {
                        seq = String.format("%04d", lastAudioIndex);
                    }
                    lastAudioIndex++;
                    if (ndkFolder.isDirectory()) {
                        for (File folder : ndkFolder.listFiles()) {
                            if (folder.isDirectory()) {
                                if (EXPORT_PATH_MASTERCOPY_AUDIO.equals(folder.getName()) || Const.EXPORT_PATH_USERCOPY_AUDIO.equals(folder.getName()) || Const.EXPORT_PATH_SOURCE_AUDIO.equals(folder.getName())) {
                                    for (File file : folder.listFiles()) {
                                        String[] fileNames = file.getName().split("\\.");
                                        if (fileNames[0].endsWith(seq)) {
                                            createDatastream(lObj, file);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    private void createDatastream(LocalObject lObj, File file) throws DigitalObjectException {
        try {
            String dsId = toValidDsId(file.getParentFile().getName());
            if (AltoDatastream.ALTO_ID.equals(dsId)) {
                AltoDatastream.importAlto(lObj, file.toURI(), null);
            } else if (StringEditor.OCR_ID.equals(dsId)) {
                MediaType mime = MediaType.valueOf(Files.probeContentType(file.toPath()));
                BinaryEditor editor = BinaryEditor.dissemination(lObj, dsId, mime);
                if (editor == null) {
                    editor = new BinaryEditor(lObj, FoxmlUtils.managedProfile(StringEditor.OCR_ID, mime, StringEditor.OCR_LABEL));
                }
                editor.write(file, editor.getLastModified(), null);
            } else if (BinaryEditor.NDK_USER_ID.equals(dsId)) {
                MediaType mime = MediaType.valueOf("image/jp2");
                BinaryEditor editor = BinaryEditor.dissemination(lObj, dsId, mime);
                if (editor == null) {
                    editor = new BinaryEditor(lObj, FoxmlUtils.managedProfile(BinaryEditor.NDK_USER_ID, mime, BinaryEditor.NDK_USER_LABEL));
                }
                editor.write(file, editor.getLastModified(), null);
            } else if (BinaryEditor.NDK_ARCHIVAL_ID.equals(dsId)) {
                MediaType mime = MediaType.valueOf("image/jp2");
                BinaryEditor editor = BinaryEditor.dissemination(lObj, dsId, mime);
                if (editor == null) {
                    editor = new BinaryEditor(lObj, FoxmlUtils.managedProfile(BinaryEditor.NDK_ARCHIVAL_ID, mime, BinaryEditor.NDK_ARCHIVAL_LABEL));
                }
                editor.write(file, editor.getLastModified(), null);
            } else if (BinaryEditor.NDK_AUDIO_ARCHIVAL_FLAC_ID.equals(dsId)) {
                MediaType mime = MediaType.valueOf("audio/flac");
                BinaryEditor editor = BinaryEditor.dissemination(lObj, dsId, mime);
                if (editor == null) {
                    editor = new BinaryEditor(lObj, FoxmlUtils.managedProfile(BinaryEditor.NDK_AUDIO_ARCHIVAL_FLAC_ID, mime, BinaryEditor.NDK_AUDIO_ARCHIVAL_FLAC_LABEL));
                }
                editor.write(file, editor.getLastModified(), null);
            } else if (BinaryEditor.NDK_AUDIO_ARCHIVAL_ID.equals(dsId)) {
                MediaType mime = MediaType.valueOf("audio/wave");
                BinaryEditor editor = BinaryEditor.dissemination(lObj, dsId, mime);
                if (editor == null) {
                    editor = new BinaryEditor(lObj, FoxmlUtils.managedProfile(BinaryEditor.NDK_AUDIO_ARCHIVAL_ID, mime, BinaryEditor.NDK_AUDIO_ARCHIVAL_LABEL));
                }
                editor.write(file, editor.getLastModified(), null);
            } else if (BinaryEditor.NDK_AUDIO_USER_ID.equals(dsId)) {
                MediaType mime = MediaType.valueOf("audio/mp3");
                BinaryEditor editor = BinaryEditor.dissemination(lObj, dsId, mime);
                if (editor == null) {
                    editor = new BinaryEditor(lObj, FoxmlUtils.managedProfile(BinaryEditor.NDK_AUDIO_USER_ID, mime, BinaryEditor.NDK_AUDIO_USER_LABEL));
                }
                editor.write(file, editor.getLastModified(), null);
            }else if (BinaryEditor.NDK_AUDIO_USER_OGG_ID.equals(dsId)) {
                MediaType mime = MediaType.valueOf("audio/ogg");
                BinaryEditor editor = BinaryEditor.dissemination(lObj, dsId, mime);
                if (editor == null) {
                    editor = new BinaryEditor(lObj, FoxmlUtils.managedProfile(BinaryEditor.NDK_AUDIO_USER_OGG_ID, mime, BinaryEditor.NDK_AUDIO_USER_OGG_LABEL));
                }
                editor.write(file, editor.getLastModified(), null);
            }
        } catch (IOException ex) {
            throw new DigitalObjectException(ex.getMessage(), ex);
        }

    }



    private void createDatastream(LocalObject lObj, MdSecType dmdSec) throws DigitalObjectException {
        MdWrap mdWrap = dmdSec.getMdWrap();
        if (mdWrap == null) {
            throw new IllegalStateException("METS: Missing mdWrap for dmdSec@ID: " + dmdSec.getID());
        }
        String dsId = mdWrap.getMDTYPE();
        if (mdWrap.getXmlData() == null || mdWrap.getXmlData().getAny().isEmpty()) {
            throw new IllegalStateException("METS: No XML found for dmdSec@ID: " + dmdSec.getID());
        }
        Node node = (Node) mdWrap.getXmlData().getAny().get(0);
        if (MdType.DC.name().equals(dsId)) {
            OaiDcType dc = DcUtils.unmarshal(new DOMSource(node), OaiDcType.class);
            DcStreamEditor editor = new DcStreamEditor(lObj);
            editor.write(new DublinCoreRecord(dc, 0, lObj.getPid()), null);
        } else if (MdType.MODS.name().equals(dsId)) {
            ModsDefinition mods = ModsUtils.unmarshal(new DOMSource(node), ModsDefinition.class);
            ModsStreamEditor editor = new ModsStreamEditor(lObj);
            editor.write(mods, 0, null);
        } else {
            throw new IllegalStateException("METS: Unexpected @MDTYPE: " + dsId + " of dmdSec@ID: " + dmdSec.getID());
        }
    }

    private void createDatastream(LocalObject lObj, FileType fileType, List<String> childPids, HashMap<String, String> devices, ImportOptions ctx) throws DigitalObjectException {
        File dsFile = toFile(fileType);
        URI dsUri = dsFile.toURI();
        String dsId = toValidDsId(fileType);
        if (RelationEditor.DATASTREAM_ID.equals(dsId)) {
            Rdf rdf = Relations.unmarshal(new StreamSource(dsFile), Rdf.class);
            RelationEditor relationEditor = new RelationEditor(lObj);
            relationEditor.setRdf(rdf);
            relationEditor.setOrganization(ctx.getOrganization());
            if (relationEditor.getStatus() == null) {
                relationEditor.setStatus(DigitalObjectStatusUtils.STATUS_EXPORTED);
            }
            if (relationEditor.getUser() == null) {
                relationEditor.setUser(ctx.getConfig().getDefaultProcessor());
            }

            String modelId = relationEditor.getModel();
            MetaModel model = modelId == null ? null: MetaModelRepository.getInstance().find(modelId);
            if (model == null && !(DeviceRepository.METAMODEL_ID.equals(modelId) || DeviceRepository.METAMODEL_AUDIODEVICE_ID.equals(modelId))) {
                throw new DigitalObjectException(lObj.getPid(), null, dsId, "Unsupported modelId: " + modelId + ", see " + dsFile, null);
            }

            relationEditor.setMembers(childPids);
            if (NdkPlugin.MODEL_PAGE.equals(modelId) || NdkPlugin.MODEL_NDK_PAGE.equals(modelId) || NdkAudioPlugin.MODEL_PAGE.equals(modelId)) {
                if (devices.keySet().contains(relationEditor.getDevice())) {
                    relationEditor.setDevice(devices.get(relationEditor.getDevice()));
                }
            }
            // XXX check group IDs; for now the groups are used just with DESA that is not supported by archive yet
            // see FOXML for owner handling
//            relationEditor.getOwners();
            relationEditor.write(0, null);
        } else if (MixEditor.NDK_ARCHIVAL_ID.equals(dsId)) {
            XmlStreamEditor editor = lObj.getEditor(MixEditor.ndkArchivalProfile());
            editor.write(dsUri, 0, null);
        } else if (MixEditor.RAW_ID.equals(dsId)) {
            XmlStreamEditor editor = lObj.getEditor(MixEditor.rawProfile());
            editor.write(dsUri, 0, null);
        } else if (DcStreamEditor.DATASTREAM_ID.equals(dsId)) {
            // skip
        } else if (ModsStreamEditor.DATASTREAM_ID.equals(dsId)) {
            // skip
        } else if (DeviceRepository.DESCRIPTION_DS_ID.equals(dsId)) {
            XmlStreamEditor editor = DeviceRepository.getMixDescriptionEditor(lObj);
            editor.write(dsUri, 0, null);
        } else if (AltoDatastream.ALTO_ID.equals(dsId)) {
            AltoDatastream.importAlto(lObj, dsUri, null);
        } else if (FoxmlUtils.DS_AUDIT_ID.equals(dsId)) {
            // handled by FOXML if branch
        } else if ("FOXML".equals(dsId)) {
            DigitalObject foxml = FoxmlUtils.unmarshal(new StreamSource(dsFile), DigitalObject.class);
            if (foxml != null) {
                DatastreamType ds = FoxmlUtils.findDatastream(foxml, FoxmlUtils.DS_AUDIT_ID);
                if (ds != null) {
                    lObj.getDigitalObject().getDatastream().add(0, ds);
                }
                // set object owner
                PropertyType ownerProp = FoxmlUtils.findProperty(foxml, FoxmlUtils.PROPERTY_OWNER);
                String foxmlOwner = ownerProp != null ? ownerProp.getVALUE() : null;
                lObj.setOwner(iSession.resolveUsername(foxmlOwner));
            }
        } else {
            MediaType mime = MediaType.valueOf(fileType.getMIMETYPE());
            BinaryEditor editor = BinaryEditor.dissemination(lObj, dsId, mime);
            if (editor == null) {
                editor = new BinaryEditor(lObj, FoxmlUtils.managedProfile(dsId, mime, null));
            }
            editor.write(dsFile, 0, null);
        }
    }

    static void mergeMembers(String memberPid, List<String> oldMembers, List<String> newMembers) {
        int newMemberIdx = findIndex(memberPid, newMembers);
        if (newMemberIdx < 0) {
            int oldMemberIdx = findIndex(memberPid, oldMembers);
            if (oldMemberIdx < 0) {
                // something is wrong
            } else {
                if (oldMemberIdx > 0) {
                    List<String> predecessors = oldMembers.subList(0, oldMemberIdx);
                    for (int i = predecessors.size() - 1; i >= 0; i--) {
                        String predecessor = predecessors.get(i);
                        int predecessorIdx = findIndex(predecessor, newMembers);
                        if (predecessorIdx >= 0) {
                            newMembers.add(predecessorIdx + 1, memberPid);
                            return ;
                        }
                    }
                }
                if (oldMemberIdx + 1 < oldMembers.size()) {
                    List<String> successors = oldMembers.subList(oldMemberIdx + 1, oldMembers.size());
                    for (String successor : successors) {
                        int successorIdx = findIndex(successor, newMembers);
                        if (successorIdx >= 0) {
                            newMembers.add(successorIdx, memberPid);
                            return ;
                        }
                    }
                }
            }
            newMembers.add(memberPid);
        } else {
            // already merged
        }
    }

    private static int findIndex(String memberPid, List<String> newMembers) {
        for (int i = 0; i < newMembers.size(); i++) {
            String pid = newMembers.get(i);
            if (pid.equals(memberPid)) {
                return i;
            }
        }
        return -1;
    }

    private File toFile(FileType fileType) {
        File dsFile = null;
        List<FLocat> fLocats = fileType.getFLocat();
        if (!fLocats.isEmpty()) {
            FLocat fLocat = fLocats.get(0);
            String locType = fLocat.getLOCTYPE(); // URL
            String href = fLocat.getHref();
            if ("URL".equals(locType) && href != null) {
                URI dsUri = metsUri.resolve(href);
                dsFile = new File(dsUri);
                if (!dsFile.exists() && href.contains(".pdf")) {
                    dsFile = fixPdfFile(dsFile, href.split("_")[2], href.split("\\.")[2]);
                }
                if (dsFile.canRead() && dsFile.isFile()) {
                    return dsFile;
                }
            }
        }
        throw new IllegalStateException("Invalid file: " + dsFile + ", " + toString(fileType));
    }

    private File fixPdfFile(File dsFile, String id, String fileExtension) {
        try {
            File importFolder = metsFile.getParentFile();
            if (importFolder.exists() && importFolder.isDirectory()) {
                for (File children : importFolder.listFiles()) {
                    if ("NDK".equals(children.getName())) {
                        children = children.listFiles()[0];
                        if (children.exists() && importFolder.isDirectory()) {
                            for (File originalFolder : children.listFiles()) {
                                if ("original".equals(originalFolder.getName())) {
                                    for (File pdfFile : originalFolder.listFiles()) {
                                        if (pdfFile.getName().endsWith(id + "." + fileExtension)) {
                                            return pdfFile;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return dsFile;
    }

    private static String toPid(DivType div) {
        List<String> contentIds = div.getCONTENTIDS();
        if (contentIds.isEmpty()) {
            throw new IllegalStateException("Missing PID");
        } else {
            return contentIds.get(0);
        }
    }

    private String toValidDsId(FileType file) {
        String dsId = toDsId(file);
        if (dsId == null || dsId.isEmpty()) {
            throw new IllegalStateException("Invalid METS: " + toString(file));
        }
        return dsId;
    }

    private String toValidDsId(String name) {
        if (EXPORT_PATH_ALTO.equals(name)) {
            return AltoDatastream.ALTO_ID;
        } else if (EXPORT_PATH_TXT.equals(name)) {
            return StringEditor.OCR_ID;
        } else if (EXPORT_PATH_USERCOPY.equals(name)) {
            return BinaryEditor.NDK_USER_ID;
        } else if (EXPORT_PATH_MASTERCOPY.equals(name)) {
            return BinaryEditor.NDK_ARCHIVAL_ID;
        } else if (EXPORT_PATH_MASTERCOPY_AUDIO.equals(name)) {
            return BinaryEditor.NDK_AUDIO_ARCHIVAL_ID;
        } else if (EXPORT_PATH_USERCOPY_AUDIO.equals(name)) {
            return BinaryEditor.NDK_AUDIO_USER_ID;
        } else {
            return name;
        }
    }

    private String toDsId(FileType file) {
        FileSec fileSec = mets.getFileSec();
        if (fileSec != null) {
            for (FileGrp fileGrp : fileSec.getFileGrp()) {
                for (FileType f : fileGrp.getFile()) {
                    if (f == file) {
                        return fileGrp.getID();
                    }
                }
            }
        }
        return null;
    }

    private static StructMapType getStructMap(Mets mets, String type) {
        for (StructMapType structMap : mets.getStructMap()) {
            if (type.equals(structMap.getTYPE())) {
                return structMap;
            }
        }
        return new StructMapType();
    }

    private static String getFoxmlFilename(String datastream, int index, String pid, String model) {
        return PackageBuilder.getFilename(datastream, index,
                PackageBuilder.getObjectId(model),
                PackageBuilder.getObjectId(pid),
                "xml");
    }

    private static String toString(DivType div) {
        return div == null ? "null" : String.format("div{%s, %s, %s}", div.getID(), div.getTYPE(), div.getLabel3());
    }

    private static String toString(FLocat fLocat) {
        return fLocat == null ? "null" : String.format("FLocat{href: %s, LOCTYPE: %s}",
                fLocat.getHref(), fLocat.getLOCTYPE());
    }

    private static String toString(List<FLocat> fLocats) {
        StringBuilder sb = new StringBuilder();
        sb.append('[');
        for (FLocat fLocat : fLocats) {
            if (sb.length() > 1) {
                sb.append(", ");
            }
            sb.append(toString(fLocat));
        }
        sb.append(']');
        return sb.toString();
    }

    private static String toString(FileType fileType) {
        return fileType == null ? "null" : String.format("FileType{ID: %s, MIME: %s, FLocat: %s}",
                fileType.getID(), fileType.getMIMETYPE(), toString(fileType.getFLocat()));
    }

    private static String toString(SearchViewItem item) {
        return item == null ? "null" : String.format("Item{%s, %s}", item.getPid(), item.getModel());
    }

    private static String toItemString(List<SearchViewItem> items) {
        StringBuilder sb = new StringBuilder();
        sb.append('[');
        for (SearchViewItem item : items) {
            if (sb.length() > 1) {
                sb.append(", ");
            }
            sb.append(toString(item));
        }
        sb.append(']');
        return sb.toString();
    }

    static class ImportSession {

        private final BatchManager ibm;
        private final ImportOptions options;
        private final Batch batch;
        private final LocalStorage locals;
        private final SearchView search;
        private final Storage typeOfStorage;
        private FedoraStorage remotes;
        private AkubraStorage akubraStorage;
        private String rootPid;
        /** The user cache. */
        private final Map<String, String> external2internalUserMap = new HashMap<String, String>();

        public ImportSession(BatchManager ibm, ImportOptions options, AppConfiguration appConfig) throws IOException {
            try {
                this.typeOfStorage = appConfig.getTypeOfStorage();
                if (Storage.FEDORA.equals(typeOfStorage)) {
                    this.remotes = FedoraStorage.getInstance();
                    this.search = this.remotes.getSearch();
                } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                    AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
                    this.akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                    this.search = this.akubraStorage.getSearch();
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + typeOfStorage);
                }
            } catch (Exception ex) {
                throw new IllegalStateException(ex);
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

        public AkubraStorage getAkubraStorage() {
            return akubraStorage;
        }

        public Storage getTypeOfStorage() {
            return typeOfStorage;
        }

        public LocalObject findLocalObject(BatchItemObject bio) {
            return bio == null || bio.getPid() == null
                    ? null : locals.load(bio.getPid(), bio.getFile());
        }

        public BatchItemObject findItem(String pid) {
            return ibm.findBatchObject(batch.getId(), pid);
        }

        public LocalObject findLocalObject(String pid) {
            BatchItemObject item = findItem(pid);
            return findLocalObject(item);
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

        public void checkRemote(List<String> pids) throws DigitalObjectException {
            List<SearchViewItem> items;
            try {
                items = search.find(false, pids);
            } catch (Exception ex) {
                throw new DigitalObjectException(null, batch.getId(), null, null, ex);
            }
            for (SearchViewItem item : items) {
                // !!! RI states differ from FOXML 'fedora-system:def/model#Active' vs. StateType.A !!!
                String state = item.getState();
//                StateType state = StateType.valueOf(item.getState());
//                if (state == StateType.D) {
//                    // XXX schedule a purge
//                } else {
                    String msg = String.format(
                            "The repository already contains the archived object pid:%s, model:%s, state:%s, %s",
                            item.getPid(), item.getModel(), state, item.getLabel());
                    throw new DigitalObjectException(item.getPid(), batch.getId(), null, msg, null);
//                }
            }
        }

        public void checkObjectParent(List<String> archiveRootLeafPath, String pid) throws DigitalObjectException {
            String parentPid = archiveRootLeafPath.isEmpty() ?
                    null : archiveRootLeafPath.get(archiveRootLeafPath.size() - 1);
            try {
                List<SearchViewItem> referrers = search.findReferrers(pid);
                if (parentPid == null) {
                    if (!referrers.isEmpty()) {
                        String msg = String.format(
                                "Different archive and repository parent of pid %s, null != %s:",
                                pid, toItemString(referrers));
                        throw new DigitalObjectException(pid, batch.getId(), null, msg, null);
                    } else {
                        return ;
                    }
                }
                for (SearchViewItem referrer : referrers) {
                    if (!parentPid.equals(referrer.getPid())) {
                        String msg = String.format(
                                "Different archive and repository parent of pid %s, %s != %s:",
                                pid, parentPid, toItemString(referrers));
                        throw new DigitalObjectException(pid, batch.getId(), null, msg, null);
                    }
                }
            } catch (IOException ex) {
                throw new DigitalObjectException(pid, batch.getId(), null, null, ex);
            } catch (FedoraClientException ex) {
                throw new DigitalObjectException(pid, batch.getId(), null, null, ex);
            }
        }

        public String resolveUsername(String externalName) {
            String cache = external2internalUserMap.get(externalName);
            if (cache == null) {
                UserProfile up = externalName == null ? null : UserUtil.getDefaultManger().find(externalName);
                if (up == null) {
                    cache = options.getUsername();
                } else {
                    cache = up.getUserName();
                }
                external2internalUserMap.put(externalName, cache);
            }
            return cache;
        }

        public String getRootPid() {
            return rootPid;
        }

        public void setRootPid(String rootPid) {
            this.rootPid = rootPid;
        }
    }

}
