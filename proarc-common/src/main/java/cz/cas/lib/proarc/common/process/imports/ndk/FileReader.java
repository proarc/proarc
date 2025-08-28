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

import cz.cas.lib.proarc.aes57.Aes57Utils;
import cz.cas.lib.proarc.codingHistory.CodingHistoryUtils;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.device.Device;
import cz.cas.lib.proarc.common.device.DeviceException;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcUtils;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkNewPageMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectExistException;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPageMapper;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPageMapper;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.external.ExternalProcess;
import cz.cas.lib.proarc.common.process.external.KakaduExpand;
import cz.cas.lib.proarc.common.process.external.TiffToJpgConvert;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import cz.cas.lib.proarc.common.process.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.process.imports.ImportProfile;
import cz.cas.lib.proarc.common.process.imports.InputUtils;
import cz.cas.lib.proarc.common.storage.AesEditor;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.CodingHistoryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.PageView;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mets.AmdSecType;
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
import cz.cas.lib.proarc.mix.BasicDigitalObjectInformationType;
import cz.cas.lib.proarc.mix.ImageCaptureMetadataType;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.premis.ObjectIdentifierComplexType;
import cz.cas.lib.proarc.premis.PremisUtils;
import cz.cas.lib.proarc.urnnbn.ResolverUtils;
import cz.incad.imgsupport.ImageMimeType;
import cz.incad.imgsupport.ImageSupport;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXB;
import javax.xml.transform.dom.DOMSource;
import org.aes.audioobject.AudioObject;
import org.apache.commons.configuration.Configuration;
import org.w3c.dom.Node;
import edu.harvard.hul.ois.xml.ns.jhove.Property;

import static cz.cas.lib.proarc.common.process.imports.TiffImporter.scale;
import static cz.cas.lib.proarc.common.process.imports.TiffImporter.writeImage;
import static cz.cas.lib.proarc.common.process.imports.ndk.StreamFileType.AMD;
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
    private PackageType packageType;

    private Map<String, ModsDefinition> modsMap = new HashMap<>();
    private Map<String, OaiDcType> dcMap = new HashMap<>();
    private Map<String, FileDescriptor> fileMap = new HashMap<>();

    private Map<String, LocalStorage.LocalObject> objects = new HashMap<>();

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
        setPackageType();
        this.mets = JAXB.unmarshal(metsFile, Mets.class);
        pkgModelId = mets.getTYPE();
        if (pkgModelId == null) {
            throw new IllegalStateException("Unknown mets@TYPE:" + pkgModelId);
        }
        try {
            loadModsAndDc(mets);
            loadFileMap(mets);
            loadStructMaps(mets, ctx);
        } catch (DigitalObjectExistException ex) {
            LOG.log(Level.SEVERE, ex.getMessage(), ex);
            Batch batch = ctx.getBatch();
            batch.setState(Batch.State.LOADING_CONFLICT);
            batch.setLog(BatchManager.toString(ex));
            iSession.getImportManager().update(batch);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, ex.getMessage(), ex);
            Batch batch = ctx.getBatch();
            batch.setState(Batch.State.LOADING_FAILED);
            batch.setLog(BatchManager.toString(ex));
            iSession.getImportManager().update(batch);
        }
    }

    private void setPackageType() {
        File pdf = new File(this.rootFolder, "original");
        if (pdf.exists() && pdf.isDirectory()) {
            this.packageType = PackageType.EBORN;
        } else {
            File masterCopyAudio = new File(this.rootFolder, "mastercopy_audio");
            if (masterCopyAudio.exists() && masterCopyAudio.isDirectory()) {
                this.packageType = PackageType.AUDIO;
            } else {
                File ocr = new File(this.rootFolder, "txt");
                File alto = new File(this.rootFolder, "alto");
                if (ocr.exists() && ocr.isDirectory() && alto.exists() && alto.isDirectory()) {
                    this.packageType = PackageType.NDK;
                } else {
                    this.packageType = PackageType.STT;
                }
            }
        }
    }

    private void loadStructMaps(Mets mets, ImportOptions ctx) throws Exception {
        for (StructMapType structMap : mets.getStructMap()) {
            if ("PHYSICAL".equalsIgnoreCase(structMap.getTYPE())) {
                loadPages(structMap, ctx);
                setDeviceToAudioPage(ctx);
            } else if ("LOGICAL".equalsIgnoreCase(structMap.getTYPE())) {
                singleVolumeMonograph = false;
                processDiv(null, null, structMap.getDiv(), ctx, true);
                // eMonograph has only one structMap without TYPE
            } else if (structMap.getTYPE() == null) {
                singleVolumeMonograph = true;
                this.packageType = PackageType.EBORN;
                processElectronicDiv(null, null, structMap.getDiv(), ctx, true);
                return;
            } else {
                LOG.warning("Unsupported StructMap type: " + structMap.getTYPE()
                        + " for " + structMap.getID());
            }
        }
        processStructLink(mets.getStructLink());
    }

    private void setDeviceToAudioPage(ImportOptions ctx) throws DigitalObjectException {
        if (iSession.getDevicePid() != null) {
            Set<String> keys = objects.keySet();
            for (String key : keys) {
                LocalStorage.LocalObject localObject = objects.get(key);
                if (NdkAudioPlugin.MODEL_PAGE.equals(localObject.getModel())) {
                    RelationEditor relationEditor = new RelationEditor(localObject);
                    relationEditor.setDevice(iSession.getDevicePid());
                    relationEditor.write(relationEditor.getLastModified(), "set child");
                    localObject.flush();
                }
            }
        }
    }

    private LocalStorage.LocalObject processElectronicDiv(LocalStorage.LocalObject parentObj, String parentModel, DivType div, ImportOptions ctx, boolean rootObject) throws Exception {
        String divType = div.getTYPE();

        MdSecType modsIdObj = (MdSecType) firstItem(div.getDMDID());
        if ("TITLE".equalsIgnoreCase(divType) && modsIdObj != null) {
            singleVolumeMonograph = false;
        }

        if ("DOCUMENT".equalsIgnoreCase(divType)) {
            processElectronicDiv(parentObj, parentModel, div.getDiv().get(0), ctx, false);
            return null;
        }

        if ("FILE".equalsIgnoreCase(divType) && modsIdObj == null) {
            createFiles(parentObj, div, ctx);
            parentObj.flush();
            return null;
        }

        String modsId = modsIdObj.getID();

        ModsDefinition mods = modsMap.get(modsId);
        if (mods == null) {
            throw new IllegalStateException("Cannot find mods: " + modsId);
        }

        String model = mapModel(divType, parentModel, Genre.NONE);

        String pid = FoxmlUtils.identifierAsPid(ResolverUtils.getIdentifier("uuid", mods));
        if (pid == null) {
            pid = FoxmlUtils.createPid();
        }

        try {
            iSession.exists(pid);
            if (rootObject) {
                iSession.setRootPid(pid);
            }
        } catch (DigitalObjectExistException ex) {
            if (!override(ctx)) {
                throw ex;
            }
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
            processElectronicDiv(localObject, model, partDiv, ctx, false);
        }
        return localObject;



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
                if (childObj == null) {
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
            return null;//we consider only div with associated metadata (DMDID)
        }

        String modsId = modsIdObj.getID();

        ModsDefinition mods = modsMap.get(modsId);
        if (mods == null) {
            throw new IllegalStateException("Cannot find mods: " + modsId);
        }
        Genre specialGenre = getSpecialGenre(mods);

        String model = mapModel(divType, parentModel, specialGenre);

        String pid = FoxmlUtils.identifierAsPid(ResolverUtils.getIdentifier("uuid", mods));
        if (pid == null) {
            pid = FoxmlUtils.createPid();
        }

        try {
            iSession.exists(pid);
            if (rootObject) {
                iSession.setRootPid(pid);
            }
        } catch (DigitalObjectExistException ex) {
            if (!override(ctx)) {
                throw ex;
            }
        }

        BatchManager.BatchItemObject importItem = iSession.findItem(pid);
        LocalStorage.LocalObject localObject = iSession.findLocalObject(importItem);
        if (localObject == null) {
            localObject = iSession.getLocals().create(pid, new File(targetFolder, FoxmlUtils.pidAsUuid(pid) + ".foxml"));
            localObject.setOwner(ctx.getUsername());
            importItem = iSession.addObject(localObject, rootObject);
        }
        localObject.setModel(model);
        DigitalObjectHandler dobjHandler = DigitalObjectManager.getDefault().createHandler(localObject);
        createRelsExt(dobjHandler, localObject, ctx);
        createMetadata(dobjHandler, mods, localObject, ctx);
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

    private boolean override(ImportOptions ctx) {
        return ctx.isUseOriginalMetadata() || ctx.isUseNewMetadata();
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

    private String mapModel(String divType, String parentModel, Genre specialGenre) {
        if ("PERIODICAL_TITLE".equalsIgnoreCase(divType)) {
            return PackageType.NDK.equals(this.packageType) ? NdkPlugin.MODEL_PERIODICAL : NdkEbornPlugin.MODEL_EPERIODICAL;
        } else if ("PERIODICAL_VOLUME".equalsIgnoreCase(divType)) {
            return PackageType.NDK.equals(this.packageType) ? NdkPlugin.MODEL_PERIODICALVOLUME : NdkEbornPlugin.MODEL_EPERIODICALVOLUME;
        } else if ("ISSUE".equalsIgnoreCase(divType)) {
            return PackageType.NDK.equals(this.packageType) ? NdkPlugin.MODEL_PERIODICALISSUE : NdkEbornPlugin.MODEL_EPERIODICALISSUE;
        } else if ("ARTICLE".equalsIgnoreCase(divType)) {
            return PackageType.NDK.equals(this.packageType) ? NdkPlugin.MODEL_ARTICLE : NdkEbornPlugin.MODEL_EARTICLE;
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
            } else if (OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(parentModel) || OldPrintPlugin.MODEL_MONOGRAPHVOLUME.equals(parentModel) || OldPrintPlugin.MODEL_MONOGRAPHUNIT.equals(parentModel)) {
                return OldPrintPlugin.MODEL_SUPPLEMENT;
            }
        } else if ("PICTURE".equalsIgnoreCase(divType)) {
            switch (packageType) {
                case NDK:
                    return NdkPlugin.MODEL_PICTURE;
                case STT:
                    return OldPrintPlugin.MODEL_GRAPHICS;
            }
        } else if ("TITLE".equalsIgnoreCase(divType) || "MONOGRAPH".equalsIgnoreCase(divType)) {
            switch (packageType) {
                case NDK:
                    return NdkPlugin.MODEL_MONOGRAPHTITLE;
                case STT:
                    return OldPrintPlugin.MODEL_MONOGRAPHTITLE;
                case EBORN:
                    return NdkEbornPlugin.MODEL_EMONOGRAPHTITLE;
            }
        } else if ("VOLUME".equalsIgnoreCase(divType)) {
            if (singleVolumeMonograph) {
                if (PackageType.EBORN.equals(this.packageType)) {
                    return NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME;
                } else {
                    if (specialGenre.equals(Genre.NONE)) {
                        return PackageType.NDK.equals(packageType) ? NdkPlugin.MODEL_MONOGRAPHVOLUME : OldPrintPlugin.MODEL_MONOGRAPHVOLUME;
                    } else if (specialGenre.equals(Genre.CARTOGRAPHIC)) {
                        return PackageType.NDK.equals(packageType) ? NdkPlugin.MODEL_CARTOGRAPHIC : OldPrintPlugin.MODEL_CARTOGRAPHIC;
                    } else if (specialGenre.equals(Genre.GRAPHIC)) {
                        return PackageType.NDK.equals(packageType) ? NdkPlugin.MODEL_GRAPHIC : OldPrintPlugin.MODEL_GRAPHICS;
                    } else if (specialGenre.equals(Genre.SHEETMUSIC)) {
                        return PackageType.NDK.equals(packageType) ? NdkPlugin.MODEL_SHEETMUSIC : OldPrintPlugin.MODEL_SHEETMUSIC;
                    }
                }
            } else {
                switch (packageType) {
                    case NDK:
                        return NdkPlugin.MODEL_MONOGRAPHUNIT;
                    case STT:
                        return OldPrintPlugin.MODEL_MONOGRAPHUNIT;
                    case EBORN:
                        return NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME;
                }
            }
        } else if ("CHAPTER".equalsIgnoreCase(divType)) {
            return PackageType.NDK.equals(packageType) ? NdkPlugin.MODEL_CHAPTER : OldPrintPlugin.MODEL_CHAPTER;
        } else if ("SOUNDCOLLECTION".equalsIgnoreCase(divType)) {
            this.packageType = PackageType.AUDIO;
            return NdkAudioPlugin.MODEL_MUSICDOCUMENT;
        } else if ("SOUNDRECORDING".equalsIgnoreCase(divType)) {
            this.packageType = PackageType.AUDIO;
            return NdkAudioPlugin.MODEL_SONG;
        } else if ("SOUNDPART".equalsIgnoreCase(divType)) {
            this.packageType = PackageType.AUDIO;
            return NdkAudioPlugin.MODEL_TRACK;
        }
        throw new IllegalArgumentException("Unsupported div type in logical structure: " + divType);
    }

    private void createRelsExt(DigitalObjectHandler dobjHandler, LocalStorage.LocalObject localObject, ImportOptions ctx) throws DigitalObjectException {
        String fedoraModel = localObject.getModel();
        RelationEditor relEditor = dobjHandler.relations();
        relEditor.setModel(fedoraModel);
        relEditor.setOrganization(ctx.getOrganization());
        relEditor.setUser(ctx.getConfig().getDefaultProcessor());
        relEditor.setStatus(DigitalObjectStatusUtils.STATUS_NEW);
        relEditor.write(relEditor.getLastModified() < 0 ? 0 : relEditor.getLastModified(), null);
    }

    private void loadPages(StructMapType structMap, ImportOptions ctx) throws Exception {
        DivType object = structMap.getDiv();
        if (object == null) {
            throw new IllegalStateException("Missing children DIV for" + structMap.getID());
        }
        for (DivType pageDiv : object.getDiv()) {
            BigInteger pageIndex = pageDiv.getORDER();
            String pageNumber = pageDiv.getORDERLABEL();
            String pageType = pageDiv.getTYPE();
            ModsDefinition mods = null;
            String model = NdkPlugin.MODEL_NDK_PAGE;
            if (pageDiv.getID().startsWith("DIV_P") || pageDiv.getID().startsWith("PageId")) {
                mods = modsMap.get(pageDiv.getID().replaceFirst("DIV_P", "MODSMD"));
                if (mods == null) {
                    mods = modsMap.get(pageDiv.getID().replaceFirst("PageId", "MODSMD"));
                }
                if (PackageType.NDK.equals(packageType)) {
                    model = NdkPlugin.MODEL_NDK_PAGE;
                } else {
                    model = OldPrintPlugin.MODEL_PAGE;
                }
            } else if (pageDiv.getID().startsWith("DIV_STOPA_") || pageDiv.getID().startsWith("DIV_AUDIO_")) {
                mods = null; // Zvukova nahravka je jen virtualni model, bez metadat
                model = NdkAudioPlugin.MODEL_PAGE;
            }

            String pid;
            if (mods == null) {
                LOG.info("Creating new Mods for page " + pageNumber);
                pid = findPidInAmdSec(pageDiv, ctx);
                if (pid == null) {
                    pid = FoxmlUtils.createPid();
                }
            } else {
                LOG.info("Using mods from mets for page " + pageNumber);
                pid = FoxmlUtils.identifierAsPid(ResolverUtils.getIdentifier("uuid", mods));
            }

            try {
                iSession.exists(pid);
            } catch (DigitalObjectExistException ex) {
                if (!override(ctx)) {
                    throw ex;
                }
            }

            BatchManager.BatchItemObject importItem = iSession.findItem(pid);
            LocalStorage.LocalObject localObject = iSession.findLocalObject(importItem);
            if (localObject == null) {
                localObject = iSession.getLocals().create(pid, new File(targetFolder, FoxmlUtils.pidAsUuid(pid) + ".foxml"));
                localObject.setOwner(ctx.getUsername());
                localObject.setModel(model);
                importItem = iSession.addObject(localObject, false);
            }
            DigitalObjectHandler dobjHandler = DigitalObjectManager.getDefault().createHandler(localObject);
            createPageRelsExt(dobjHandler, model, ctx);
            createPageMetadata(dobjHandler, mods, pageIndex, pageNumber, pageType, localObject, ctx);
            createFiles(localObject, pageDiv, ctx);
            dobjHandler.commit();
            objects.put(pageDiv.getID(), localObject);
            importItem.setState(BatchItem.ObjectState.LOADED);
            iSession.getImportManager().update(importItem);
        }
    }

    private String findPidInAmdSec(DivType pageDiv, ImportOptions ctx) throws IOException {
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
            if (AMD.equals(fileDesc.getFileType())) {
                return findPidInAmdSec(file, ctx);
            }
        }
        return null;
    }

    private String findPidInAmdSec(File amdSec, ImportOptions ctx) {
        Mets amdMets = JAXB.unmarshal(amdSec, Mets.class);
        String pid = null;
        for (AmdSecType amdSecType : amdMets.getAmdSec()) {
            for (MdSecType techMd : amdSecType.getTechMD()) {
                if (techMd.getID().startsWith("OBJ")) {
                    pid = findPidInAmdSec(techMd, ctx);
                }
                if (pid != null && !pid.isEmpty()) {
                    return pid;
                }
            }
        }
        return null;
    }

    private String findPidInAmdSec(MdSecType techMd, ImportOptions ctx) {
        Node data = (Node) techMd.getMdWrap().getXmlData().getAny().get(0);
        cz.cas.lib.proarc.premis.File premisFile = PremisUtils.unmarshal(new DOMSource(data), cz.cas.lib.proarc.premis.File.class);
        for (ObjectIdentifierComplexType objectIdentifier : premisFile.getObjectIdentifier()) {
            if (objectIdentifier.getObjectIdentifierValue() != null) {
                String id = objectIdentifier.getObjectIdentifierValue();
                if (objectIdentifier.getObjectIdentifierType() != null && "ProArc_URI".equals(objectIdentifier.getObjectIdentifierType())) {
                    id = id.substring(id.indexOf("/") + 1, id.lastIndexOf("/"));
                }
                if (FoxmlUtils.isValidPid(id)) {
                    return id;
                }
            }
        }
        return null;
    }

    private void createFiles(LocalStorage.LocalObject localObject, DivType pageDiv, ImportOptions ctx) throws Exception {
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
                    if (checkIfFileHasExtension(file.getName(), ".txt")) {
                        XmlStreamEditor ocrEditor = localObject.getEditor(StringEditor.ocrProfile());
                        ocrEditor.write(file.toURI(), ocrEditor.getLastModified() < 0 ? 0 : ocrEditor.getLastModified(), null);
                    } else {
                        throw new IllegalArgumentException("Unsupported file: " + file.getAbsolutePath());
                    }
                    break;
                case ALTO:
                    if (checkIfFileHasExtension(file.getName(), ".xml")) {
                        AltoDatastream altoEditor = new AltoDatastream(configuration.getImportConfiguration());
                        altoEditor.importAlto(localObject, file.toURI(), null);
                    } else {
                        throw new IllegalArgumentException("Unsupported file: " + file.getAbsolutePath());
                    }
                    break;
                case USER_IMAGE:
                    if (checkIfFileHasExtension(file.getName(), ".jp2") && InputUtils.isJp2000(file)) {
                        BinaryEditor userEditor = BinaryEditor.dissemination(localObject, BinaryEditor.NDK_USER_ID, BinaryEditor.IMAGE_JP2);
                        userEditor.write(file.toURI(), userEditor.getLastModified() < 0 ? 0 : userEditor.getLastModified(), null);
                    } else {
                        throw new IllegalArgumentException("Unsupported file: " + file.getAbsolutePath());
                    }
                    break;
                case MASTER_IMAGE:
                    if (checkIfFileHasExtension(file.getName(), ".jp2") && InputUtils.isJp2000(file)) {
                        BinaryEditor archivalEditor = BinaryEditor.dissemination(localObject, BinaryEditor.NDK_ARCHIVAL_ID, BinaryEditor.IMAGE_JP2);
                        archivalEditor.write(file.toURI(), archivalEditor.getLastModified() < 0 ? 0 : archivalEditor.getLastModified(), null);
                        processImage(localObject, file, ctx);
                    } else {
                        throw new IllegalArgumentException("Unsupported file: " + file.getAbsolutePath());
                    }
                    break;
                case AMD:
                    if (checkIfFileHasExtension(file.getName(), ".xml")) {
                        processTechnicalMetadata(localObject, file, ctx);
                    } else {
                        throw new IllegalArgumentException("Unsupported file: " + file.getAbsolutePath());
                    }
                    break;
                case PDF:
                    if (checkIfFileHasExtension(file.getName(), ".pdf") && InputUtils.isPdf(file)) {
                        BinaryEditor rawEditor = BinaryEditor.dissemination(localObject, BinaryEditor.RAW_ID, BinaryEditor.FILE_PDF);
                        rawEditor.write(file.toURI(), rawEditor.getLastModified() < 0 ? 0 : rawEditor.getLastModified(), null);
                    } else {
                        throw new IllegalArgumentException("Unsupported file: " + file.getAbsolutePath());
                    }
                    break;
                case MASTER_AUDIO:
                    BinaryEditor masterAudioEditor = null;
                    if (checkIfFileHasExtension(file.getName(), ".wav") && InputUtils.isWave(file)) {
                        masterAudioEditor = BinaryEditor.dissemination(localObject, BinaryEditor.NDK_AUDIO_ARCHIVAL_ID, BinaryEditor.AUDIO_WAVE);
                    } else if (checkIfFileHasExtension(file.getName(), ".flac") && InputUtils.isFlac(file)) {
                        masterAudioEditor = BinaryEditor.dissemination(localObject, BinaryEditor.NDK_AUDIO_ARCHIVAL_FLAC_ID, BinaryEditor.AUDIO_FLAC);
                    } else {
                        throw new IllegalArgumentException("Unsupported file: " + file.getAbsolutePath());
                    }
                    masterAudioEditor.write(file.toURI(), masterAudioEditor.getLastModified() < 0 ? 0 : masterAudioEditor.getLastModified(), null);
                    break;
                case SOURCE_AUDIO:
                    BinaryEditor sourceAudioEditor = null;
                    if (checkIfFileHasExtension(file.getName(), ".wav") && InputUtils.isWave(file)) {
                        sourceAudioEditor = BinaryEditor.dissemination(localObject, BinaryEditor.RAW_AUDIO_ID, BinaryEditor.AUDIO_WAVE);
                    } else if (checkIfFileHasExtension(file.getName(), ".flac") && InputUtils.isFlac(file)) {
                        sourceAudioEditor = BinaryEditor.dissemination(localObject, BinaryEditor.RAW_ID, BinaryEditor.AUDIO_FLAC);
                    } else {
                        throw new IllegalArgumentException("Unsupported file: " + file.getAbsolutePath());
                    }
                    sourceAudioEditor.write(file.toURI(), sourceAudioEditor.getLastModified() < 0 ? 0 : sourceAudioEditor.getLastModified(), null);
                    break;
                case USER_AUDIO:
                    BinaryEditor userAudioEditor = null;
                    if (checkIfFileHasExtension(file.getName(), ".mp3") && InputUtils.isMp3(file)) {
                        userAudioEditor = BinaryEditor.dissemination(localObject, BinaryEditor.NDK_AUDIO_USER_ID, BinaryEditor.AUDIO_MP3);
                    } else if (checkIfFileHasExtension(file.getName(), ".ogg") && InputUtils.isOgg(file)) {
                        userAudioEditor = BinaryEditor.dissemination(localObject, BinaryEditor.NDK_AUDIO_USER_OGG_ID, BinaryEditor.AUDIO_OGG);
                    } else {
                        throw new IllegalArgumentException("Unsupported file: " + file.getAbsolutePath());
                    }
                    userAudioEditor.write(file.toURI(), userAudioEditor.getLastModified() < 0 ? 0 : userAudioEditor.getLastModified(), null);
                    break;
                default:
                    throw new IllegalArgumentException("Unsupported fileType: " + fileDesc.getFileType());
            }
        }
    }

    private boolean checkIfFileHasExtension(String filename, String extension) {
        return filename.endsWith(extension);
    }

    private void processTechnicalMetadata(LocalStorage.LocalObject localObject, File amdSec, ImportOptions ctx) throws DigitalObjectException, DeviceException {
        Mets amdMets = JAXB.unmarshal(amdSec, Mets.class);
        for (AmdSecType amdSecType : amdMets.getAmdSec()) {
            for (MdSecType techMd : amdSecType.getTechMD()) {
                if (techMd.getID().startsWith("MIX")) {
                    processMix(techMd, localObject, ctx);
                }
                if (techMd.getID().startsWith("AES")) {
                    processAes(techMd, localObject, ctx);
                }
                if (techMd.getID().startsWith("CODINGHISTORY")) {
                    processCodingHistory(techMd, localObject, ctx);
                }
                if (techMd.getID().startsWith("OBJ")) {
                    processPremis(techMd, localObject, ctx);
                }
            }
        }
    }

    private void processMix(MdSecType techMd, LocalStorage.LocalObject localObject, ImportOptions ctx) throws DigitalObjectException, DeviceException {
        List<Object> objectsData = techMd.getMdWrap().getXmlData().getAny();
        if (objectsData.isEmpty()) {
            return;
        }
        Node data = (Node) objectsData.get(0);
        Mix mix = MixUtils.unmarshalMix(new DOMSource(data));
        String type = getMixType(mix);

        switch (type.toUpperCase()) {
            case "NDK_ARCHIVAL":
                MixEditor mixNdkArchivalEditor = MixEditor.ndkArchival(localObject);
                mixNdkArchivalEditor.write(mix, mixNdkArchivalEditor.getLastModified() < 0 ? 0 : mixNdkArchivalEditor.getLastModified(), null);
                break;
            case "RAW":
                MixEditor mixRawEditor = MixEditor.raw(localObject);
                mixRawEditor.write(getFileMix(mix), mixRawEditor.getLastModified() < 0 ? 0 : mixRawEditor.getLastModified(), null);

                processDevice(getDeviceMix(mix), localObject, ctx);
                break;
            default:
                LOG.info("Unsupported BasicDigitalObjectInformation/ObjectIdentifier/objectIdentifierValue value: " + type);
        }
    }

    private void processAes(MdSecType techMd, LocalStorage.LocalObject localObject, ImportOptions ctx) throws DigitalObjectException {
        List<Object> objectsData = techMd.getMdWrap().getXmlData().getAny();
        if (objectsData.isEmpty()) {
            return;
        }
        Node data = (Node) objectsData.get(0);
        AudioObject aes = Aes57Utils.unmarshalAes(new DOMSource(data));
        String type = getType(techMd);

        switch (type.toUpperCase()) {
            case "NDK_ARCHIVAL":
                AesEditor aesNdkArchivalEditor = AesEditor.ndkArchival(localObject);
                aesNdkArchivalEditor.write(aes, aesNdkArchivalEditor.getLastModified() < 0 ? 0 : aesNdkArchivalEditor.getLastModified(), null);
                break;
            case "RAW":
                AesEditor aesRawEditor = AesEditor.raw(localObject);
                aesRawEditor.write(aes, aesRawEditor.getLastModified() < 0 ? 0 : aesRawEditor.getLastModified(), null);
                break;
            default:
                LOG.info("Unsupported type: " + type);
        }
    }

    private void processCodingHistory(MdSecType techMd, LocalStorage.LocalObject localObject, ImportOptions ctx) throws DigitalObjectException {
        List<Object> objectsData = techMd.getMdWrap().getXmlData().getAny();
        if (objectsData.isEmpty()) {
            return;
        }
        Node data = (Node) objectsData.get(0);
        Property codingHistory = CodingHistoryUtils.unmarshalCodingHistory(new DOMSource(data));
        String type = getType(techMd);

        switch (type.toUpperCase()) {
            case "NDK_ARCHIVAL":
                CodingHistoryEditor codingHistoryNdkArchivalEditor = CodingHistoryEditor.ndkArchival(localObject);
                codingHistoryNdkArchivalEditor.write(codingHistory, codingHistoryNdkArchivalEditor.getLastModified() < 0 ? 0 : codingHistoryNdkArchivalEditor.getLastModified(), null);
                break;
            case "RAW":
                CodingHistoryEditor codingHistoryRawEditor = CodingHistoryEditor.raw(localObject);
                codingHistoryRawEditor.write(codingHistory, codingHistoryRawEditor.getLastModified() < 0 ? 0 : codingHistoryRawEditor.getLastModified(), null);
                break;
            default:
                LOG.info("Unsupported type: " + type);
        }
    }

    private String getType(MdSecType tech) {
        return tech.getID().endsWith("001") ? "RAW" : "NDK_ARCHIVAL";
    }

    private void processPremis(MdSecType techMd, LocalStorage.LocalObject localObject, ImportOptions ctx) throws DigitalObjectException, DeviceException {
        List<Object> objectsData = techMd.getMdWrap().getXmlData().getAny();
        if (objectsData.isEmpty()) {
            return;
        }
        Node data = (Node) objectsData.get(0);
        cz.cas.lib.proarc.premis.File premisFile = PremisUtils.unmarshal(new DOMSource(data), cz.cas.lib.proarc.premis.File.class);
        if (premisFile.getOriginalName() != null) {
            String fileName = premisFile.getOriginalName().getValue();
            if (fileName != null && !fileName.isEmpty()) {
                if (fileName.endsWith("null")) { //hack because audio files dont have
                    fileName = fileName.substring(0, fileName.length() - 4) + ".wav";
                }
                RelationEditor relationEditor = new RelationEditor(localObject);
                relationEditor.setImportFile(fileName);
                relationEditor.write(relationEditor.getLastModified(), "set fileName");
                localObject.flush();
            }
        }
    }

    private void processDevice(Mix deviceMix, LocalStorage.LocalObject localObject, ImportOptions ctx) throws DeviceException, DigitalObjectException {
        DeviceIdentification newDevice = new DeviceIdentification(deviceMix);
        String deviceId = getDevice(newDevice);
        if (deviceId == null || deviceId.isEmpty()) {
            DeviceRepository deviceRepository = iSession.getDeviceRepository();
            Device device = deviceRepository.addDeviceWithMetadata(ctx.getUsername(), PackageType.NDK.equals(this.packageType) ? DeviceRepository.METAMODEL_ID : DeviceRepository.METAMODEL_AUDIODEVICE_ID, (newDevice.getImageProducer() == null ? "Imported device" : newDevice.getScannerManufacturer() + " - imported device"), "Imported device", deviceMix, null);
            deviceId = device.getId();
        }
        if (deviceId != null && !deviceId.isEmpty()) {
            iSession.setDevicePid(deviceId);
            RelationEditor relationEditor = new RelationEditor(localObject);
            relationEditor.setDevice(deviceId);
            relationEditor.write(relationEditor.getLastModified(), "set child");
            localObject.flush();
        } else {
            throw new DigitalObjectNotFoundException(localObject.getPid(), "Missing device Id");
        }
    }

    private String getDevice(DeviceIdentification newDevice) throws DeviceException {
        if (iSession.getDevicePid() != null) {
            return iSession.getDevicePid();
        }
        List<Device> devices  = iSession.findAllDevices();
        for (Device device : devices) {
            if (isEqualDevice(newDevice, device.getDescription())) {
                return device.getId();
            }
        }
        return null;
    }

    private boolean isEqualDevice(DeviceIdentification newDevice, Mix sourceMix) {
        DeviceIdentification sourceDevice = new DeviceIdentification(sourceMix);
        return newDevice.equals(sourceDevice);
    }

    private Mix getDeviceMix(Mix mixOriginal) {
        Mix mixNew = new Mix();
        if (mixOriginal.getImageCaptureMetadata() != null) {
            mixNew.setImageCaptureMetadata(mixOriginal.getImageCaptureMetadata());
        }
        return mixNew;
    }

    private Mix getFileMix(Mix mixOriginal) {
        Mix mixNew = new Mix();
        if (mixOriginal.getBasicDigitalObjectInformation() != null) {
            mixNew.setBasicDigitalObjectInformation(mixOriginal.getBasicDigitalObjectInformation());
        }
        if (mixOriginal.getBasicImageInformation() != null) {
            mixNew.setBasicImageInformation(mixOriginal.getBasicImageInformation());
        }
        if (mixOriginal.getImageAssessmentMetadata() != null) {
            mixNew.setImageAssessmentMetadata(mixOriginal.getImageAssessmentMetadata());
        }
        if (mixOriginal.getChangeHistory() != null) {
            mixNew.setChangeHistory(mixOriginal.getChangeHistory());
        }
        return mixNew;
    }

    private String getMixType(Mix mix) {
        String identifierValue = null;
        if (mix.getBasicDigitalObjectInformation() != null) {
            for (BasicDigitalObjectInformationType.ObjectIdentifier objectIdentifier : mix.getBasicDigitalObjectInformation().getObjectIdentifier()) {
                if (objectIdentifier.getObjectIdentifierValue() != null) {
                    identifierValue = objectIdentifier.getObjectIdentifierValue().getValue();
                }
            }
        }
        if (identifierValue != null) {
            return identifierValue.substring(identifierValue.lastIndexOf("/") + 1);
        }
        if (mix.getBasicDigitalObjectInformation() != null &&
                mix.getBasicDigitalObjectInformation().getFormatDesignation() != null &&
                mix.getBasicDigitalObjectInformation().getFormatDesignation().getFormatName() != null &&
                mix.getBasicDigitalObjectInformation().getFormatDesignation().getFormatName().getValue() != null) {
            String formatName = mix.getBasicDigitalObjectInformation().getFormatDesignation().getFormatName().getValue();
            if (formatName != null && formatName.contains("/tiff")) {
                return "RAW";
            } else if (formatName != null && formatName.contains("/jp2")) {
                return "NDK_ARCHIVAL";
            }
        }

        return null;
    }

    private void processImage(LocalStorage.LocalObject localObject, File jp2File, ImportOptions ctx) throws DigitalObjectException, IOException, AppConfigurationException {
        ImportProfile config = ctx.getConfig();
        if (jp2File != null && jp2File.exists()) {
            File tiffFile = convertToTiff(jp2File, ctx.getTargetFolder(), config.getConvertorJp2Processor());
            if (tiffFile != null && tiffFile.exists()) {
                if (!InputUtils.isTiff(tiffFile)) {
                    throw new IllegalStateException("Not a TIFF content: " + tiffFile);
                }
                BinaryEditor tiffEditor = BinaryEditor.dissemination(localObject, BinaryEditor.RAW_ID, BinaryEditor.IMAGE_TIFF);
                tiffEditor.write(tiffFile.toURI(), tiffEditor.getLastModified() < 0 ? 0 : tiffEditor.getLastModified(), null);

                boolean runCustomConversion = config.isTiffToJpgDefined();

                long start;
                long endRead = 0;
                BufferedImage tiff = null;
                File file = null;

                ImageMimeType imageType = ImageMimeType.JPEG;
                MediaType mediaType = MediaType.valueOf(imageType.getMimeType());


                String targetName = String.format("%s.full.%s", getFileName(tiffFile), imageType.getDefaultFileExtension());
                start = System.nanoTime();
                if (runCustomConversion) {
                    file = new File(ctx.getTargetFolder(), targetName);
                    ExternalProcess p = new TiffToJpgConvert(config.getConvertorTiffToJpgProcessor(), tiffFile, file);
                    p.run();

                    if (!p.isOk()) {
                        throw new IllegalStateException("Converting tiff to FULL jpg failed: " + p.getFullOutput());
                    }
                } else {
                    if (tiff == null) {
                        start = System.nanoTime();
                        tiff = ImageSupport.readImage(tiffFile.toURI().toURL(), ImageMimeType.TIFF);
                        endRead = System.nanoTime() - start;
                    }
                    file = writeImage(tiff, ctx.getTargetFolder(), targetName, imageType);
                }
                if (!InputUtils.isJpeg(file)) {
                    throw new IllegalStateException("Not a JPEG content: " + file);
                }
                long endFull = System.nanoTime() - start;
                BinaryEditor fullEditor = BinaryEditor.dissemination(localObject, BinaryEditor.FULL_ID, mediaType);
                fullEditor.write(file, fullEditor.getLastModified() < 0 ? 0 : fullEditor.getLastModified(), null);


                targetName = String.format("%s.preview.%s", getFileName(tiffFile), imageType.getDefaultFileExtension());
                Integer previewMaxHeight = config.getPreviewMaxHeight();
                Integer previewMaxWidth = config.getPreviewMaxWidth();
                config.checkPreviewScaleParams();
                start = System.nanoTime();
                if (runCustomConversion) {
                    file = new File(ctx.getTargetFolder(), targetName);
                    ExternalProcess p = new TiffToJpgConvert(config.getConvertorTiffToJpgProcessor(), tiffFile, file, previewMaxWidth, previewMaxHeight);
                    p.run();

                    if (!p.isOk()) {
                        throw new IllegalStateException("Converting tiff to PREVIEW jpg failed: " + p.getFullOutput());
                    }
                } else {
                    if (tiff == null) {
                        start = System.nanoTime();
                        tiff = ImageSupport.readImage(tiffFile.toURI().toURL(), ImageMimeType.TIFF);
                        endRead = System.nanoTime() - start;
                    }
                    file = writeImage(
                                scale(tiff, config.getPreviewScaling(), previewMaxWidth, previewMaxHeight),
                                ctx.getTargetFolder(), targetName, imageType);
                }
                if (!InputUtils.isJpeg(file)) {
                    throw new IllegalStateException("Not a JPEG content: " + file);
                }
                long endPreview = System.nanoTime() - start;
                BinaryEditor previewEditor = BinaryEditor.dissemination(localObject, BinaryEditor.PREVIEW_ID, mediaType);
                previewEditor.write(file, previewEditor.getLastModified() < 0 ? 0 : previewEditor.getLastModified(), null);

                targetName = String.format("%s.thumb.%s", getFileName(tiffFile), imageType.getDefaultFileExtension());
                Integer thumbMaxHeight = config.getThumbnailMaxHeight();
                Integer thumbMaxWidth = config.getThumbnailMaxWidth();
                config.checkThumbnailScaleParams();
                start = System.nanoTime();
                if (runCustomConversion) {
                    file = new File(ctx.getTargetFolder(), targetName);
                    ExternalProcess p = new TiffToJpgConvert(config.getConvertorTiffToJpgProcessor(), tiffFile, file, thumbMaxWidth, thumbMaxHeight);
                    p.run();

                    if (!p.isOk()) {
                        throw new IllegalStateException("Converting tiff to THUMBNAIL jpg failed: " + p.getFullOutput());
                    }
                } else {
                    if (tiff == null) {
                        start = System.nanoTime();
                        tiff = ImageSupport.readImage(tiffFile.toURI().toURL(), ImageMimeType.TIFF);
                        endRead = System.nanoTime() - start;
                    }
                    file = writeImage(
                            scale(tiff, config.getThumbnailScaling(), thumbMaxWidth, thumbMaxHeight),
                            ctx.getTargetFolder(), targetName, imageType);
                }
                long endThumb = System.nanoTime() - start;
                BinaryEditor thumbnailEditor = BinaryEditor.dissemination(localObject, BinaryEditor.THUMB_ID, mediaType);
                thumbnailEditor.write(file, thumbnailEditor.getLastModified() < 0 ? 0 : thumbnailEditor.getLastModified(), null);

                LOG.info(String.format("file: %s, read: %s, full: %s, preview: %s, thumb: %s",
                        tiffFile.getName(), endRead / 1000000, endFull / 1000000, endPreview / 1000000, endThumb / 1000000));
            } else {
                throw new IllegalStateException("Convertor JP2 --> TIF not set.");
            }

        }
    }

    private File convertToTiff(File jp2File, File targetFolder, Configuration processorConfig) throws IOException {
        if (processorConfig != null && !processorConfig.isEmpty()) {
            File tiffFile = new File(targetFolder, String.format("%s.%s", getFileName(jp2File), "tif"));
            String processorType = processorConfig.getString("type");
            ExternalProcess process = null;
            if (KakaduExpand.ID.equals(processorType)) {
                process = new KakaduExpand(processorConfig, jp2File, tiffFile);
            } else {
                throw new IllegalArgumentException("No suitable convertor found.");
            }
            process.run();

//            if (!process.isOk()) {
//                throw new IOException(tiffFile.getAbsolutePath() + "\n" + process.getFullOutput());
//            }
            return tiffFile;
        }
        return null;
    }

    private String getFileName(File sourceFile) {
        return sourceFile.getName().substring(0, sourceFile.getName().indexOf("."));
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
        modsStreamEditor.write(mods, modsStreamEditor.getLastModified() < 0 ? 0 : modsStreamEditor.getLastModified(), null);

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
        String label = null;
        if (NdkPlugin.MODEL_NDK_PAGE.equals(localObject.getModel())) {
            NdkNewPageMapper mapper = new NdkNewPageMapper();
            mapper.setModelId(localObject.getModel());
            if (mHandler instanceof PageView.PageViewHandler) {
                if (mods == null) {
                    mods = mapper.createPage(String.valueOf(pageIndex), pageNumber, pageType, context);
                    label = mapper.createObjectLabel(mods);
                }
            } else {
                throw new IllegalStateException("Unsupported mHandler");
            }
        } else if (NdkAudioPlugin.MODEL_PAGE.equals(localObject.getModel())) {
            NdkAudioPageMapper mapper = new NdkAudioPageMapper();
            mapper.setModelId(localObject.getModel());
            if (mHandler instanceof PageView.PageViewHandler) {
                if (mods == null) {
                    mods = mapper.createPage(String.valueOf(pageIndex), pageNumber, "audio", context);
                }
            } else {
                throw new IllegalStateException("Unsupported mHandler");
            }
        } else if (OldPrintPlugin.MODEL_PAGE.equals(localObject.getModel())) {
            OldPrintPageMapper mapper = new OldPrintPageMapper();
            mapper.setModelId(localObject.getModel());
            if (mHandler instanceof PageView.PageViewHandler) {
                if (mods == null) {
                    mods = mapper.createPage(String.valueOf(pageIndex), pageNumber, pageType, context);
                    label = OldPrintPageMapper.getPageTypeLabel(pageType, new Locale("cs"));
                }
            } else {
                throw new IllegalStateException("Unsupported mHandler");
            }
        }

        NdkMapper mapper = NdkMapper.get(localObject.getModel());
        XmlStreamEditor xml = localObject.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, localObject);
        mapper.createMods(mods, context);
        modsStreamEditor.write(mods, modsStreamEditor.getLastModified() < 0 ? 0 : modsStreamEditor.getLastModified(), null);

        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = dobjHandler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dublinCoreRecord = dcEditor.read();
        dublinCoreRecord.setDc(dc);
        dcEditor.write(dobjHandler, dublinCoreRecord, null);

        localObject.setLabel(label == null ? mapper.toLabel(mods) : label);
    }

    private void createPageRelsExt(DigitalObjectHandler dobjHandler, String model, ImportOptions ctx) throws DigitalObjectException {
        RelationEditor relEditor = dobjHandler.relations();
        relEditor.setModel(model);
        relEditor.setDevice(ctx.getDevice());
        relEditor.setOrganization(ctx.getOrganization());
        relEditor.setUser(ctx.getConfig().getDefaultProcessor());
        relEditor.setStatus(DigitalObjectStatusUtils.STATUS_NEW);
        relEditor.write(relEditor.getLastModified() < 0 ? 0 : relEditor.getLastModified(), null);
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
        private final DeviceRepository deviceRepository;
        private final Storage typeOfStorage;
        private final AppConfiguration config;
        private FedoraStorage fedoraStorage;
        private AkubraStorage akubraStorage;
        private String rootPid;
        private String devicePid;

        public ImportSession(BatchManager batchManager, ImportProcess.ImportOptions options, AppConfiguration appConfig) throws IOException {
            try {
                this.typeOfStorage = appConfig.getTypeOfStorage();
                if (Storage.FEDORA.equals(typeOfStorage)) {
                    this.fedoraStorage = FedoraStorage.getInstance(appConfig);
                    this.search = this.fedoraStorage.getSearch();
                    this.deviceRepository = new DeviceRepository(this.fedoraStorage);
                } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                    AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
                    this.akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                    this.search = this.akubraStorage.getSearch();
                    this.deviceRepository = new DeviceRepository(this.akubraStorage);
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + typeOfStorage);
                }
            } catch (Exception ex) {
                throw new IllegalStateException(ex);
            }
            this.config = appConfig;
            this.locals = new LocalStorage();
            this.batchManager = batchManager;
            this.options = options;
            this.batch = options.getBatch();
        }

        public boolean exists(String pid) throws DigitalObjectException {
            if (Storage.FEDORA.equals(this.typeOfStorage)) {
                if (this.fedoraStorage.exist(pid)) {
                    throw new DigitalObjectExistException(pid, null, "Object with PID " + pid + " already exists!", null);
                }
            } else if (Storage.AKUBRA.equals(this.typeOfStorage)) {
                if (this.akubraStorage.exist(pid)) {
                    throw new DigitalObjectExistException(pid, null, "Object with PID " + pid + " already exists!", null);
                }
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + typeOfStorage);
            }
            return false;
        }

        public List<Device> findAllDevices() throws DeviceException {
            return this.deviceRepository.find(config, null, true,0);
        }

        public BatchManager getImportManager() {
            return batchManager;
        }

        public LocalStorage getLocals() {
            return locals;
        }

        public FedoraStorage getFedoraRemotes() {
            return fedoraStorage;
        }

        public AkubraStorage getAkubraStorage() {
            return akubraStorage;
        }

        public DeviceRepository getDeviceRepository() {
            return deviceRepository;
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

        public String getRootPid() {
            return rootPid;
        }

        public void setRootPid(String rootPid) {
            this.rootPid = rootPid;
        }

        public String getDevicePid() {
            return devicePid;
        }

        public void setDevicePid(String devicePid) {
            this.devicePid = devicePid;
        }
    }

    private static enum Genre {
        NONE, CARTOGRAPHIC, SHEETMUSIC, GRAPHIC
    }

    private static enum PackageType {
        NDK, AUDIO, EBORN, STT
    }

    private class DeviceIdentification {

        private String imageProducer = null;
        private String captureDevice = null;
        private String scannerManufacturer = null;
        private String scannerSensor = null;
        private String digitalCameraManufacturer = null;
        private String digitalCameraModelName = null;

        public DeviceIdentification(Mix mix) {
            if (mix != null) {
                ImageCaptureMetadataType imageCapture = mix.getImageCaptureMetadata();
                if (imageCapture != null && imageCapture.getGeneralCaptureInformation() != null) {
                    ImageCaptureMetadataType.GeneralCaptureInformation captureInformation = imageCapture.getGeneralCaptureInformation();
                    if (captureInformation.getImageProducer() != null && !captureInformation.getImageProducer().isEmpty() && captureInformation.getImageProducer().get(0) != null && captureInformation.getImageProducer().get(0).getValue() != null) {
                        this.imageProducer = captureInformation.getImageProducer().get(0).getValue();
                    }
                    if (captureInformation.getCaptureDevice() != null && captureInformation.getCaptureDevice().getValue() != null) {
                        this.captureDevice = captureInformation.getCaptureDevice().getValue().value();
                    }
                }
                if (imageCapture != null && imageCapture.getScannerCapture() != null && imageCapture.getScannerCapture().getScannerManufacturer() != null && imageCapture.getScannerCapture().getScannerManufacturer().getValue() != null) {
                    this.scannerManufacturer = imageCapture.getScannerCapture().getScannerManufacturer().getValue();
                }
                if (imageCapture != null && imageCapture.getScannerCapture() != null && imageCapture.getScannerCapture().getScannerSensor() != null && imageCapture.getScannerCapture().getScannerSensor().getValue() != null) {
                    this.scannerSensor = imageCapture.getScannerCapture().getScannerSensor().getValue().value();
                }
                if (imageCapture != null && imageCapture.getDigitalCameraCapture() != null) {
                    ImageCaptureMetadataType.DigitalCameraCapture digitalCamera = imageCapture.getDigitalCameraCapture();
                    if (digitalCamera.getDigitalCameraManufacturer() != null && digitalCamera.getDigitalCameraManufacturer().getValue() != null) {
                        this.digitalCameraManufacturer = digitalCamera.getDigitalCameraManufacturer().getValue();
                    }
                    if (digitalCamera.getDigitalCameraModel() != null && digitalCamera.getDigitalCameraModel().getDigitalCameraModelName() != null && digitalCamera.getDigitalCameraModel().getDigitalCameraModelName().getValue() != null) {
                        this.digitalCameraModelName = digitalCamera.getDigitalCameraModel().getDigitalCameraModelName().getValue();
                    }
                }
            }
        }

        public String getImageProducer() {
            return imageProducer;
        }

        public String getCaptureDevice() {
            return captureDevice;
        }

        public String getScannerManufacturer() {
            return scannerManufacturer;
        }

        public String getScannerSensor() {
            return scannerSensor;
        }

        public String getDigitalCameraManufacturer() {
            return digitalCameraManufacturer;
        }

        public String getDigitalCameraModelName() {
            return digitalCameraModelName;
        }

        @Override
        public boolean equals(Object sourceDevice) {
            if (sourceDevice instanceof DeviceIdentification) {
                DeviceIdentification device = (DeviceIdentification) sourceDevice;
                int compareVal = compare(imageProducer, device.getImageProducer()) +
                        compare(captureDevice, device.getCaptureDevice()) +
                        compare(scannerManufacturer, device.getScannerManufacturer()) +
                        compare(scannerSensor, device.getScannerSensor()) +
                        compare(digitalCameraManufacturer, device.getDigitalCameraManufacturer()) +
                        compare(digitalCameraModelName, device.getDigitalCameraModelName());
                return compareVal == 0;
            }
            return false;
        }

        private int compare(String value, String sourceValue) {
            if (value == null && sourceValue == null) {
                return 0;
            } else if (value == null && sourceValue != null) {
                return 1;
            } else if (value != null && sourceValue == null) {
                return 1;
            } else {
                return  value.equals(sourceValue) ? 0 : 1;
            }
        }
    }
}
