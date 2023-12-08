/*
 * Copyright (C) 2023 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.imports.ndk;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcUtils;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.PageView;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.StringEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkNewPageMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import cz.cas.lib.proarc.common.process.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.DivType.Fptr;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.FileType.FLocat;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType.FileSec;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.MetsType.StructLink;
import cz.cas.lib.proarc.mets.StructLinkType.SmLink;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.urnnbn.ResolverUtils;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXB;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Node;

import static cz.cas.lib.proarc.common.process.imports.ndk.StreamFileType.getFileType;

/**
 * It reads the ndk package and generates digital objects for a batch import.
 *
 * @author Lukas Sykora
 */
public class FileReader {

    private File rootFolder;
    private final File targetFolder;
    private final ImportSession iSession;
    private final AppConfiguration configuration;
    private Mets mets;
    private String pkgModelId;
    private boolean singleVolumeMonograph = false;

    private Map<String, ModsDefinition> modsMap = new HashMap<>();
    private Map<String, OaiDcType> dcMap = new HashMap<>();
    private Map<String, FileDescriptor> fileMap = new HashMap<>();

    private Map<String, LocalStorage.LocalObject> objects = new HashMap<>();

    public static final String STRUCTMAP_PHYSICAL_TYPE = "PHYSICAL";
    public static final String STRUCTMAP_LOGICAL_TYPE = "LOGICAL";

    private Logger LOG = Logger.getLogger(FileReader.class.getName());

    public FileReader(File targetFolder, ImportSession iSession, AppConfiguration config) {
        this.targetFolder = targetFolder;
        this.iSession = iSession;
        this.configuration = config;
    }

    public void read(File metsFile, ImportOptions ctx) {
        try {
            readImpl(metsFile, ctx);
        } catch (Exception ex) {
            throw new IllegalStateException(metsFile.getAbsolutePath(), ex);
        }
    }

    private void readImpl(File metsFile, ImportOptions ctx) {
        this.rootFolder = metsFile.getParentFile();
        this.mets = JAXB.unmarshal(metsFile, Mets.class);
        pkgModelId = mets.getTYPE();
        if (pkgModelId == null) {
            throw new IllegalStateException("Unknown mets@TYPE:" + pkgModelId);
        }
        try {
            loadModsAndDc(mets);
            loadFileMap(mets);
            loadStructMaps(mets, ctx);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, ex.getMessage(), ex);
            Batch batch = ctx.getBatch();
            batch.setState(Batch.State.LOADING_FAILED);
            batch.setLog(BatchManager.toString(ex));
            iSession.getImportManager().update(batch);
        }
    }

    private void loadStructMaps(Mets mets, ImportOptions ctx) throws DigitalObjectException, IOException {
        for (StructMapType structMap : mets.getStructMap()) {
            if ("PHYSICAL".equalsIgnoreCase(structMap.getTYPE())) {
                loadPages(structMap, ctx);
            } else if ("LOGICAL".equalsIgnoreCase(structMap.getTYPE())) {
                singleVolumeMonograph = false;
                processDiv(null, null, structMap.getDiv(), ctx, true);
                // eMonograph has only one structMap without TYPE
            } else if (structMap.getTYPE() == null) {
//                processElectronicDiv(null, structMap.getDiv());
                return;
            } else {
                LOG.warning("Unsupported StructMap type: " + structMap.getTYPE()
                        + " for " + structMap.getID());
            }
        }
        processStructLink(mets.getStructLink());
    }

    private void processStructLink(StructLink structLink) throws DigitalObjectException {
        for (Object o : structLink.getSmLinkOrSmLinkGrp()) {
            if (o instanceof SmLink) {
                SmLink smLink = (SmLink) o;
                String parent = smLink.getFrom();
                String child = smLink.getTo();
                if (parent == null || child == null) {
                    continue;
                }
                LocalStorage.LocalObject childObj = objects.get(child);
                if (childObj == null && !(child.startsWith("DIV_STOPA") || child.startsWith("DIV_AUDIO"))) {
                    LOG.warning("Invalid structLink from: " + parent + " to: " + child);
                    continue;
                }
                LocalStorage.LocalObject parentObj = objects.get(parent);
                if (parentObj == null) {
                    LOG.warning("Invalid structLink from: " + parent + " to: " + child);
                    continue;
                }
                // kapitoly nemaji navazany strany
                if (!parent.startsWith("CHAP")) {
                    RelationEditor relationEditor = new RelationEditor(parentObj);
                    List<String> members = relationEditor.getMembers();
                    members.add(childObj.getPid());
                    relationEditor.setMembers(members);
                    relationEditor.write(relationEditor.getLastModified(), "set child");
                    parentObj.flush();
                }
            }
        }
    }

    private LocalStorage.LocalObject processDiv(LocalStorage.LocalObject parentObj, String parentModel, DivType div, ImportOptions ctx, boolean rootObject) throws IllegalStateException, DigitalObjectException {
        String divType = div.getTYPE();
        //divs for PAGES are processed from physical map and structlinks
        if ("PAGE".equalsIgnoreCase(divType)) {
            return null;
        }
        MdSecType modsIdObj = (MdSecType) firstItem(div.getDMDID());
        //special hack to ignore extra div for single volume monograph
        if ("MONOGRAPH".equalsIgnoreCase(divType) && modsIdObj == null) {
            singleVolumeMonograph = true;
            List<DivType> volumeDivs = div.getDiv();
            if (volumeDivs == null) {
                return null;
            }
            //process volume as top level
            if (volumeDivs.size() == 1) {
                processDiv(null, null,volumeDivs.get(0), ctx, true);
                return null;
            }
            //if monograph div contains more subdivs, first is supposed to be the volume, the rest are supplements that will be nested in the volume.
            if (volumeDivs.size() > 1) {
                LocalStorage.LocalObject volume = processDiv(null, null,volumeDivs.get(0), ctx, true);
                for (int i = 1; i < volumeDivs.size(); i++) {
                    processDiv(volume, null, volumeDivs.get(i), ctx, false);
                }
            }
            return null;
        }

        if (modsIdObj == null) {
//            collectAlto(parent, div);
            return null;//we consider only div with associated metadata (DMDID)
        }

        String modsId = modsIdObj.getID();

        ModsDefinition mods = modsMap.get(modsId);
        if (mods == null) {
            throw new IllegalStateException("Cannot find mods: " + modsId);
        }
        Genre specialGenre = getSpecialGenre(mods);

        String model = mapModel(divType, parentModel, specialGenre, false);

        String pid = identifierAsPid(ResolverUtils.getIdentifier("uuid", mods));
        if (pid == null) {
            pid = FoxmlUtils.createPid();
        }

        BatchManager.BatchItemObject importItem = iSession.findItem(pid);
        LocalStorage.LocalObject localObject = iSession.findLocalObject(importItem);
        if (localObject == null) {
            localObject = iSession.getLocals().create(pid, new File(targetFolder, FoxmlUtils.pidAsUuid(pid) + ".foxml"));
            localObject.setOwner(ctx.getUsername());
            localObject.setModel(model);
            importItem = iSession.addObject(localObject, rootObject);
        }
        DigitalObjectHandler dobjHandler = DigitalObjectManager.getDefault().createHandler(localObject);
        createMetadata(dobjHandler, mods, localObject, ctx);
        createRelsExt(dobjHandler, localObject, ctx);
        dobjHandler.commit();
        objects.put(div.getID(), localObject);
        importItem.setState(BatchItem.ObjectState.LOADED);
        iSession.getImportManager().update(importItem);

        if (parentObj != null) {
            RelationEditor relationEditor = new RelationEditor(parentObj);
            List<String> members = relationEditor.getMembers();
            members.add(localObject.getPid());
            relationEditor.setMembers(members);
            relationEditor.write(relationEditor.getLastModified(), "set child");
            parentObj.flush();
        }

        for (DivType partDiv : div.getDiv()) {
            processDiv(localObject, model, partDiv, ctx, false);
        }
        return localObject;
    }

    private Genre getSpecialGenre(ModsDefinition mods) {
        for (GenreDefinition genre : mods.getGenre()) {
            if ("cartographic".equalsIgnoreCase(genre.getValue())) {
                return Genre.CARTOGRAPHIC;
            }
            if ("sheetmusic".equalsIgnoreCase(genre.getValue())) {
                return Genre.SHEETMUSIC;
            }
        }
        return Genre.NONE;
    }

    private String mapModel(String divType, String parentModel, Genre specialGenre, boolean isElectronic) {
        if ("PERIODICAL_TITLE".equalsIgnoreCase(divType)) {
            return !isElectronic ? NdkPlugin.MODEL_PERIODICAL : NdkEbornPlugin.MODEL_EPERIODICAL;
        } else if ("PERIODICAL_VOLUME".equalsIgnoreCase(divType)) {
            return !isElectronic ? NdkPlugin.MODEL_PERIODICALVOLUME : NdkEbornPlugin.MODEL_EPERIODICALVOLUME;
        } else if ("ISSUE".equalsIgnoreCase(divType)) {
            return !isElectronic ? NdkPlugin.MODEL_PERIODICALISSUE : NdkEbornPlugin.MODEL_EPERIODICALISSUE;
        } else if ("ARTICLE".equalsIgnoreCase(divType)) {
            return !isElectronic ? NdkPlugin.MODEL_ARTICLE : NdkEbornPlugin.MODEL_EARTICLE;
        } else if ("SUPPLEMENT".equalsIgnoreCase(divType) || "SUPPL".equalsIgnoreCase(divType)) {
            if (NdkPlugin.MODEL_PERIODICAL.equals(parentModel) || NdkPlugin.MODEL_PERIODICALVOLUME.equals(parentModel) || NdkPlugin.MODEL_PERIODICALISSUE.equals(parentModel)) {
                return NdkPlugin.MODEL_PERIODICALSUPPLEMENT;
            } else if (NdkEbornPlugin.MODEL_EPERIODICAL.equals(parentModel) || NdkEbornPlugin.MODEL_EPERIODICALVOLUME.equals(parentModel) || NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(parentModel)) {
                return NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT;
            } else if (NdkPlugin.MODEL_MONOGRAPHTITLE.equals(parentModel) || NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(parentModel) || NdkPlugin.MODEL_MONOGRAPHUNIT.equals(parentModel)) {
                return NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT;
            } else if (NdkEbornPlugin.MODEL_EMONOGRAPHTITLE.equals(parentModel) || NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME.equals(parentModel)) {
                return NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT;
            } else if (NdkAudioPlugin.MODEL_PHONOGRAPH.equals(parentModel) || NdkAudioPlugin.MODEL_MUSICDOCUMENT.equals(parentModel) || NdkAudioPlugin.MODEL_TRACK.equals(parentModel) || NdkAudioPlugin.MODEL_SONG.equals(parentModel)){
                return NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT;
            } else {
                throw new IllegalArgumentException("Unsupported div type in logical structure: " + divType);
            }
        } else if ("PICTURE".equalsIgnoreCase(divType)) {
            return NdkPlugin.MODEL_PICTURE;
        } else if ("TITLE".equalsIgnoreCase(divType) || "MONOGRAPH".equalsIgnoreCase(divType)) {
            return !isElectronic ? NdkPlugin.MODEL_MONOGRAPHTITLE : NdkEbornPlugin.MODEL_EMONOGRAPHTITLE;
        } else if ("VOLUME".equalsIgnoreCase(divType)) {
            if (singleVolumeMonograph) {
                if (isElectronic) {
                    return NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME;
                } else {
                    if (specialGenre.equals(Genre.NONE)) {
                        return NdkPlugin.MODEL_MONOGRAPHVOLUME;
                    } else if (specialGenre.equals(Genre.CARTOGRAPHIC)) {
                        return NdkPlugin.MODEL_CARTOGRAPHIC;
                    } else if (specialGenre.equals(Genre.SHEETMUSIC)) {
                        return NdkPlugin.MODEL_SHEETMUSIC;
                    }
                }
            } else {
                return !isElectronic ? NdkPlugin.MODEL_MONOGRAPHUNIT : NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME;
            }
        } else if ("CHAPTER".equalsIgnoreCase(divType)) {
            return NdkPlugin.MODEL_CHAPTER;
        } else if ("SOUNDCOLLECTION".equalsIgnoreCase(divType)) {
            return NdkAudioPlugin.MODEL_MUSICDOCUMENT;
        } else if ("SOUNDRECORDING".equalsIgnoreCase(divType)) {
            return NdkAudioPlugin.MODEL_SONG;
        } else if ("SOUNDPART".equalsIgnoreCase(divType)) {
            return NdkAudioPlugin.MODEL_TRACK;
        }
        throw new IllegalArgumentException("Unsupported div type in logical structure: " + divType);
    }

    private void createRelsExt(DigitalObjectHandler dobjHandler, LocalStorage.LocalObject localObject, ImportOptions ctx) throws DigitalObjectException {
        String fedoraModel = localObject.getModel();
        RelationEditor relEditor = dobjHandler.relations();
        relEditor.setModel(fedoraModel);
        relEditor.setOrganization(ctx.getOrganization());
        relEditor.setStatus(DigitalObjectStatusUtils.STATUS_NEW);
        relEditor.write(0, null);
    }

    private void loadPages(StructMapType structMap, ImportOptions ctx) throws DigitalObjectException, IOException {
        DivType object = structMap.getDiv();
        if (object == null) {
            throw new IllegalStateException("Missing children DIV for" + structMap.getID());
        }
        for (DivType pageDiv : object.getDiv()) {
            String type = pageDiv.getTYPE();
            if (type != null && (type.equalsIgnoreCase("sound") || type.equalsIgnoreCase("soundpart"))) {
//                collectAudioFiles(pageDiv); //TODO
                continue;
            }
            BigInteger pageIndex = pageDiv.getORDER();
            String pageNumber = pageDiv.getORDERLABEL();
            String pageType = pageDiv.getTYPE();
            ModsDefinition mods = modsMap.get(pageDiv.getID().replaceFirst("DIV_P", "MODSMD"));
            OaiDcType dc = dcMap.get(pageDiv.getID().replaceFirst("DIV_P", "DCMD"));
            String pid;
            if (mods == null) {
                LOG.info("Creating new Mods for page " + pageNumber);
                pid = FoxmlUtils.createPid();
            } else {
                LOG.info("Using mods from mets for page " + pageNumber);
                pid = identifierAsPid(ResolverUtils.getIdentifier("uuid", mods));
            }

            BatchManager.BatchItemObject importItem = iSession.findItem(pid);
            LocalStorage.LocalObject localObject = iSession.findLocalObject(importItem);
            if (localObject == null) {
                localObject = iSession.getLocals().create(pid, new File(targetFolder, FoxmlUtils.pidAsUuid(pid) + ".foxml"));
                localObject.setOwner(ctx.getUsername());
                importItem = iSession.addObject(localObject, false);
            }
            DigitalObjectHandler dobjHandler = DigitalObjectManager.getDefault().createHandler(localObject);
            createPageRelsExt(dobjHandler, ctx);
            createPageMetadata(dobjHandler, mods, pageIndex, pageNumber, pageType, localObject, ctx);
            createFiles(localObject, pageDiv);
            dobjHandler.commit();
            objects.put(pageDiv.getID(), localObject);
            importItem.setState(BatchItem.ObjectState.LOADED);
            iSession.getImportManager().update(importItem);
        }
    }

    private String identifierAsPid(IdentifierDefinition identifier) {
        return identifier.getType() + ":" + identifier.getValue();
    }

    private void createFiles(LocalStorage.LocalObject localObject, DivType pageDiv) throws IOException, DigitalObjectException {
        for (Fptr fptr : pageDiv.getFptr()) {
            FileType fileId = (FileType) fptr.getFILEID();
            FileDescriptor fileDesc = fileMap.get(fileId.getID());
            if (fileDesc == null) {
                throw new IllegalStateException("Invalid file pointer:" + fileId.getID());
            }
            File file = new File(rootFolder, fileDesc.getFilename());
            if (!file.exists()) {
                throw new IOException("File does not exists: " + file.getAbsolutePath());
            }
            switch (fileDesc.getFileType()) {
                case OCR:
                    XmlStreamEditor ocrEditor = localObject.getEditor(StringEditor.ocrProfile());
                    ocrEditor.write(file.toURI(), 0, null);
                    break;
                case ALTO:
                    AltoDatastream altoEditor = new AltoDatastream(configuration.getImportConfiguration());
                    altoEditor.importAlto(localObject, file.toURI(), null);
                    break;
                case USER_IMAGE:
                    BinaryEditor userEditor = BinaryEditor.dissemination(localObject, BinaryEditor.NDK_USER_ID, BinaryEditor.IMAGE_JP2);
                    userEditor.write(file.toURI(), 0, null);
                    break;
                case MASTER_IMAGE:
                    BinaryEditor archivalEditor = BinaryEditor.dissemination(localObject, BinaryEditor.NDK_ARCHIVAL_ID, BinaryEditor.IMAGE_JP2);
                    archivalEditor.write(file.toURI(), 0, null);
                    break;
                case AMD:
                    // TODO
                    break;
                default:
                    throw new IllegalArgumentException("Unsupported fileType: " + fileDesc.getFileType());
            }
        }
    }

    private void createMetadata(DigitalObjectHandler dobjHandler, ModsDefinition mods, LocalStorage.LocalObject localObject, ImportOptions ctx) throws DigitalObjectException {
        NdkMapper.Context context = new NdkMapper.Context(dobjHandler);
        NdkMapper mapper = NdkMapper.get(localObject.getModel());
        mapper.setModelId(localObject.getModel());

        if (mods == null) {
            throw new IllegalStateException("Missing MODS for: " + localObject.getPid());
        }

        XmlStreamEditor xml = localObject.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, localObject);
        mapper.createMods(mods, context);
        modsStreamEditor.write(mods, 0, null);

        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = dobjHandler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dublinCoreRecord = dcEditor.read();
        dublinCoreRecord.setDc(dc);
        dcEditor.write(dobjHandler, dublinCoreRecord, null);

        localObject.setLabel(mapper.toLabel(mods));
    }

    private void createPageMetadata(DigitalObjectHandler dobjHandler, ModsDefinition mods, BigInteger pageIndex, String pageNumber, String pageType, LocalStorage.LocalObject localObject, ImportOptions ctx) throws DigitalObjectException {
        MetadataHandler<Object> mHandler = dobjHandler.metadata();
        NdkMapper.Context context = new NdkMapper.Context(dobjHandler);
        NdkNewPageMapper mapper = new NdkNewPageMapper();
        mapper.setModelId(localObject.getModel());
        if (mHandler instanceof PageView.PageViewHandler) {
            if (mods == null) {
                mods = mapper.createPage(String.valueOf(pageIndex), pageNumber, pageType, context);
            }
        } else {
            throw new IllegalStateException("Unsupported mHandler");
        }

        XmlStreamEditor xml = localObject.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, localObject);
        mapper.createMods(mods, context);
        modsStreamEditor.write(mods, 0, null);

        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = dobjHandler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dublinCoreRecord = dcEditor.read();
        dublinCoreRecord.setDc(dc);
        dcEditor.write(dobjHandler, dublinCoreRecord, null);

        localObject.setLabel(mapper.toLabel(mods));
    }

    private void createPageRelsExt(DigitalObjectHandler dobjHandler, ImportOptions ctx) throws DigitalObjectException {
        String fedoraModel = ctx.getModel();
        RelationEditor relEditor = dobjHandler.relations();
        relEditor.setModel(fedoraModel);
        relEditor.setDevice(ctx.getDevice());
        relEditor.setOrganization(ctx.getOrganization());
        relEditor.setStatus(DigitalObjectStatusUtils.STATUS_NEW);
        relEditor.write(0, null);
    }

    private void loadFileMap(Mets mets) {
        int fileCounter = 0;
        FileSec fileSec = mets.getFileSec();
        for (FileGrp fileGrp : fileSec.getFileGrp()) {
            String fileGrpId = fileGrp.getID();
            StreamFileType groupType = getFileType(fileGrpId);
            for (FileType fileType : fileGrp.getFile()) {
                String id = fileType.getID();
                FLocat flocat = firstItem(fileType.getFLocat());
                String name = flocat.getHref().replace("\\", "/");
                fileMap.put(id, new FileDescriptor(name, groupType));
                fileCounter++;
            }
        }
        LOG.info("Loaded files: " + fileCounter);
    }

    protected <T> T firstItem(List<T> list) {
        return (list == null || list.size() == 0 || list.get(0) == null) ? null : list.get(0);
    }

    private void loadModsAndDc(Mets mets) {
        int modsCounter = 0;
        int dcCounter = 0;
        for (MdSecType mdSecType : mets.getDmdSec()) {
            String id = mdSecType.getID();
            String type = mdSecType.getMdWrap().getMDTYPE();
            Node data = (Node) mdSecType.getMdWrap().getXmlData().getAny().get(0);
            if ("MODS".equalsIgnoreCase(type)) {
                ModsDefinition mods = ModsUtils.unmarshalModsType(new DOMSource(data));
                if (modsMap.put(id, mods) != null) {
                    LOG.severe("Duplicate MODS records: " + id);
                    throw new IllegalArgumentException("Duplicate Mods records: " + id);
                } else {
                    modsCounter++;
                }
            } else if ("DC".equalsIgnoreCase(type)) {
                OaiDcType dc = DcUtils.unmarshal(new DOMSource(data), OaiDcType.class);
                if (dcMap.put(id, dc) != null) {
                    LOG.severe("Duplicate DC records: " + id);
                    throw new IllegalArgumentException("Duplicate Dc records: " + id);
                } else {
                    dcCounter++;
                }
            } else {
                LOG.severe("Unsupported metadata type: " + type + " for " + id);
                throw new IllegalStateException("Unsupported metadata type: " + type + " for " + id);
            }
        }
        LOG.info("Loaded " + modsCounter + " MODS records and " + dcCounter + " DC records.");
        if (dcCounter != modsCounter) {
            LOG.warning("Different (" + modsCounter + ") MODS and DC (" + dcCounter + ") count!");
            throw new IllegalStateException("Different (" + modsCounter + ") MODS and DC (" + dcCounter + ") count!");
        }
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

    private static String toString(DivType div) {
        return div == null ? "null" : String.format("div{%s, %s, %s}", div.getID(), div.getTYPE(), div.getLabel3());
    }

    private static String toString(FileType.FLocat fLocat) {
        return fLocat == null ? "null" : String.format("FLocat{href: %s, LOCTYPE: %s}",
                fLocat.getHref(), fLocat.getLOCTYPE());
    }

    private static String toString(List<FileType.FLocat> fLocats) {
        StringBuilder sb = new StringBuilder();
        sb.append('[');
        for (FileType.FLocat fLocat : fLocats) {
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

    static class ImportSession {

        private final BatchManager batchManager;
        private final ImportProcess.ImportOptions options;
        private final Batch batch;
        private final LocalStorage locals;
        private final SearchView search;
        private final Storage typeOfStorage;
        private RemoteStorage remotes;
        private AkubraStorage akubraStorage;
        /**
         * The user cache.
         */
        private final Map<String, String> external2internalUserMap = new HashMap<String, String>();

        public ImportSession(BatchManager batchManager, ImportProcess.ImportOptions options, AppConfiguration appConfig) throws IOException {
            try {
                this.typeOfStorage = appConfig.getTypeOfStorage();
                if (Storage.FEDORA.equals(typeOfStorage)) {
                    this.remotes = RemoteStorage.getInstance();
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
            this.batchManager = batchManager;
            this.options = options;
            this.batch = options.getBatch();
        }

        public BatchManager getImportManager() {
            return batchManager;
        }

        public LocalStorage getLocals() {
            return locals;
        }

        public RemoteStorage getRemotes() {
            return remotes;
        }

        public AkubraStorage getAkubraStorage() {
            return akubraStorage;
        }

        public Storage getTypeOfStorage() {
            return typeOfStorage;
        }

        public LocalStorage.LocalObject findLocalObject(BatchManager.BatchItemObject bio) {
            return bio == null || bio.getPid() == null
                    ? null : locals.load(bio.getPid(), bio.getFile());
        }

        public BatchManager.BatchItemObject findItem(String pid) {
            return batchManager.findBatchObject(batch.getId(), pid);
        }

        public LocalStorage.LocalObject findLocalObject(String pid) {
            BatchManager.BatchItemObject item = findItem(pid);
            return findLocalObject(item);
        }

        public BatchManager.BatchItemObject addObject(LocalStorage.LocalObject lobj, boolean root) throws DigitalObjectException {
            BatchManager.BatchItemObject bio = batchManager.addLocalObject(options.getBatch(), lobj);
            if (root) {
                LocalStorage.LocalObject rootObj = batchManager.getRootObject(batch);
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
                        return;
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
    }

    private static enum Genre {
        NONE, CARTOGRAPHIC, SHEETMUSIC
    }
}
