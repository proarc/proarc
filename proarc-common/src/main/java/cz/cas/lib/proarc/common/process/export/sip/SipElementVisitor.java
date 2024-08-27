/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.common.process.export.sip;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.process.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.process.export.mets.JHoveOutput;
import cz.cas.lib.proarc.common.process.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.ObjectInfo;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElementVisitor;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.akubra.AkubraUtils;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mets.AmdSecType;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.StructMapType;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import org.apache.commons.codec.digest.DigestUtils;
import org.w3c.dom.Node;

import static cz.cas.lib.proarc.common.storage.PremisEditor.addPremisNodeToMets;
import static cz.cas.lib.proarc.common.storage.PremisEditor.getAgent;
import static cz.cas.lib.proarc.common.storage.PremisEditor.getCustomAgent;
import static cz.cas.lib.proarc.common.storage.PremisEditor.getPremisEvent;
import static cz.cas.lib.proarc.common.storage.PremisEditor.getPremisFile;

class SipElementVisitor extends MetsElementVisitor implements IMetsElementVisitor {

    private static final Logger LOG = Logger.getLogger(SipElementVisitor.class.getName());

    private int chapterCounter = 0;
    private int fileCounter = 0;
    private int issueCounter = 0;
    private int articleCounter = 0;
    HashMap<String, FileMD5Info> md5InfosMap = new HashMap<>();
    private boolean ignoreMissingUrnNbn = false;
    private List<IMetsElement> pidsToExport = new ArrayList<>();

    @Override
    protected void initHeader(IMetsElement metsElement) throws MetsExportException {
        super.initHeader(metsElement);
        String label = mets.getLabel1();
        mets.setLabel1(label + getDateIssued(metsElement));
    }

    @Override
    public void insertIntoMets(IMetsElement metsElement) throws MetsExportException {
        Objects.requireNonNull(metsElement, "metsElement can not be null");
        metsElement.getMetsContext().getFileList().clear();
        this.ignoreMissingUrnNbn = metsElement.getIgnoreMissingUrnNbn();
        mets = prepareMets(metsElement);
        initHeader(metsElement);
        LOG.log(Level.FINE, "Inserting into Mets:" + metsElement.getOriginalPid() + "(" + metsElement.getElementType() + ")");
        IMetsElement rootElement = metsElement.getMetsContext().getRootElement();

        Collection<Path> packageFiles = new ArrayList<>();
        metsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(metsElement, ignoreMissingUrnNbn));
        Path packageRoot = createPackageDirection(rootElement);

        boolean saveMets = false;
        switch (rootElement.getElementType()) {
            case Const.MONOGRAPH_UNIT:
                insertMonograph(rootElement);
                if (metsElement.getMetsContext().getPackageDir() == null) {
                    File packageDirFile = createPackageDir(metsElement);
                    metsElement.getMetsContext().setPackageDir(packageDirFile);
                }
                saveMets = true;
                if (metsElement.getChildren().isEmpty()) {
                    packageFiles.addAll(saveStreams(metsElement, packageRoot));
                    pidsToExport.add(metsElement);
                } else {
                    for (IMetsElement childElement : metsElement.getChildren()) {
                        packageFiles.addAll(saveStreams(childElement, packageRoot));
                        pidsToExport.add(childElement);
                    }
                }
                break;
            case Const.MONOGRAPH_MULTIPART:
                packageFiles.addAll(saveStreams(metsElement, packageRoot));
                insertMonograph(rootElement);
                if (metsElement.getMetsContext().getPackageDir() == null) {
                    File packageDirFile = createPackageDir(metsElement);
                    metsElement.getMetsContext().setPackageDir(packageDirFile);
                }
                saveMets = true;
                for (IMetsElement childElement : metsElement.getChildren()) {
                    packageFiles.addAll(saveStreams(childElement, packageRoot));
                    pidsToExport.add(childElement);
                }

                if (metsElement.getParent() != null) {
                    packageFiles.addAll(saveStreams(metsElement.getParent(), packageRoot));
                    pidsToExport.add(metsElement.getParent());
                }

                break;
            case Const.PERIODICAL_TITLE:
                insertPeriodical(metsElement);
                saveMets = true;

                if (metsElement.getMetsContext().getPackageDir() == null) {
                    File packageDirFile = createPackageDir(metsElement);
                    metsElement.getMetsContext().setPackageDir(packageDirFile);
                }


                if (metsElement.getChildren().isEmpty()) {
                    packageFiles.addAll(saveStreams(metsElement, packageRoot));
                    pidsToExport.add(metsElement);
                } else {
                    for (IMetsElement childElement : metsElement.getChildren()) {
                        packageFiles.addAll(saveStreams(childElement, packageRoot));
                        pidsToExport.add(childElement);
                    }
                }

//                IMetsElement parent = metsElement.getParent();
//                while (parent != null) {
//                    packageFiles.addAll(saveStreams(parent, packageRoot));
//                    parent = parent.getParent();
//                }

                break;
            default:
                throw new MetsExportException("Unknown element type " + rootElement.getElementType());
        }

        metsElement.getMetsContext().getFileList().addAll(
                packageFiles.stream().map(filePath -> {
                    String md5 = null;
                    long size = -1;
                    try {
                        md5 = DigestUtils.md5Hex(Files.readAllBytes(filePath));
                        size = Files.size(filePath);
                    } catch (IOException e) {
                        LOG.warning(filePath + ": md5 or size is not calculated");
                    }
                    return new FileMD5Info(filePath.toString(), md5, size);
                }).collect(Collectors.toList()));

        repairPath(metsElement);

        int index = 1;
        for (IMetsElement element : pidsToExport) {
            prepareFileType(element);
            generateTechMetadata(element, index);
            index++;
        }

//        generateTechMetadata(metsElement);
        if (saveMets) {
            saveMets(mets, new File(metsElement.getMetsContext().getPackageDir().getAbsolutePath() + File.separator + "mets_" + MetsUtils.removeNonAlpabetChars(metsElement.getMetsContext().getPackageID()) + ".xml"), metsElement);
        }


        //saveInfoFile(packageRoot, metsElement);

    }

    private void generateTechMetadata(IMetsElement metsElement, Integer index) throws MetsExportException {
        AmdSecType amdSec = new AmdSecType();
        amdSec.setID("AMD_" + metsElement.getModsElementID());
        mets.getAmdSec().add(amdSec);
        ObjectInfo objectInfo = new ObjectInfo();
        for (FileMD5Info file : metsElement.getMetsContext().getFileList()) {
            File originFile = new File(metsElement.getMetsContext().getPackageDir().getAbsolutePath() + File.separator + file.getFileName());
            if (originFile.exists()) {
                JHoveOutput output = JhoveUtility.getObjectInfo(originFile, metsElement.getMetsContext(), null, file.getFileName());
                objectInfo.createObjectInfoFromOutput(output);
            }
        }
        String indexToFirstValue = String.format("%03d", (2 * index) - 1);
        String indexToSecondValue = String.format("%03d", 2 * index);
        addPremisNodeToMets(getPremisFile(metsElement, Const.OC_GRP_ID_CREATION, md5InfosMap.get(Const.OC_GRP_ID_CREATION), null, objectInfo, indexToFirstValue), amdSec, "OBJ_" + indexToFirstValue, true, null);
        addPremisNodeToMets(getPremisEvent(metsElement, Const.OC_GRP_ID_CREATION, md5InfosMap.get(Const.OC_GRP_ID_CREATION), "creation", indexToFirstValue), amdSec, "EVT_" + indexToFirstValue, true, null);
        String validationStatus = getValidationStatus(metsElement);
        String validatorName = validationStatus == null || validationStatus.isEmpty() ? "ProArc" : "veraPDF";
        addPremisNodeToMets(getPremisEvent(metsElement, Const.OC_GRP_ID_VALIDATION, md5InfosMap.get(Const.OC_GRP_ID_VALIDATION), "validation", indexToSecondValue, validatorName, validationStatus), amdSec, "EVT_" + indexToSecondValue, true, null);
        addPremisNodeToMets(getAgent(metsElement), amdSec, "AGENT_" + indexToFirstValue, true, null);
        if (validationStatus != null) {
            addPremisNodeToMets(getCustomAgent(metsElement, validatorName), amdSec, "AGENT_" + indexToSecondValue, true, null);
        }
    }

    private String getValidationStatus(IMetsElement metsElement) throws MetsExportException {
        ProArcObject object = null;
        if (Storage.FEDORA.equals(metsElement.getMetsContext().getTypeOfStorage())) {
            object = metsElement.getMetsContext().getRemoteStorage().find(metsElement.getOriginalPid());
        } else if (Storage.AKUBRA.equals(metsElement.getMetsContext().getTypeOfStorage())) {
            object = metsElement.getMetsContext().getAkubraStorage().find(metsElement.getOriginalPid());
        } else {
            throw new IllegalStateException("Unsupported type of Storage: " + metsElement.getMetsContext().getTypeOfStorage());
        }

        RelationEditor relationEditor = new RelationEditor(object);
        try {
            String value = relationEditor.getPdfValidationStatus();
            if (value == null || value.isEmpty()) {
                return null;
            } else if ("OK".equals(value)) {
                return "successful";
            } else {
                return "failed";
            }
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMessage(), ex);
            throw new MetsExportException(ex.getMessage(), false, ex);
        }
    }

    private void prepareFileType(IMetsElement metsElement) throws MetsExportException {
        try {
            DatastreamType rawDS = FoxmlUtils.findDatastream(metsElement.getSourceObject(), "RAW");
            if (rawDS != null) {
                FileType fileType = new FileType();
                fileType.setCHECKSUMTYPE("MD5");
                GregorianCalendar gregory = new GregorianCalendar();
                gregory.setTime(new Date());

                XMLGregorianCalendar calendar;
                try {
                    calendar = DatatypeFactory.newInstance()
                            .newXMLGregorianCalendar(
                                    gregory);
                } catch (DatatypeConfigurationException e1) {
                    throw new MetsExportException("Unable to create XMLGregorianDate", false, e1);
                }
                fileType.setCREATED(calendar);
                fileType.setSEQ(fileCounter++);

                String fileName = "oc_" + metsElement.getMetsContext().getPackageID() + "_" + metsElement.getElementID().toLowerCase();
                fileType.setID(fileName);

                String extension = null;

                InputStream inputStream = null;
                if (Storage.FEDORA.equals(metsElement.getMetsContext().getTypeOfStorage())) {
                    GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "RAW");
                    inputStream = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                } else if (Storage.AKUBRA.equals(metsElement.getMetsContext().getTypeOfStorage())) {
                    AkubraObject object = metsElement.getMetsContext().getAkubraStorage().find(metsElement.getOriginalPid());
                    inputStream = AkubraUtils.getDatastreamDissemination(object, "RAW");
                } else {
                    throw new IllegalStateException("Unsupported type of Storage: " + metsElement.getMetsContext().getTypeOfStorage());
                }

                FileMD5Info fileMD5Info = MetsUtils.getDigest(inputStream);
                if (rawDS != null && rawDS.getDatastreamVersion() != null && rawDS.getDatastreamVersion().size() > 0 && rawDS.getDatastreamVersion().get(0) != null) {
                    extension = Const.mimeToExtensionMap.get(rawDS.getDatastreamVersion().get(0).getMIMETYPE());
                }
                if (extension == null) {
                    extension = ".pdf";
                }
                fileMD5Info.setFileName(fileName + extension);
                fileMD5Info.setCreated(rawDS.getDatastreamVersion().get(0).getCREATED());
                fileMD5Info.setMimeType(rawDS.getDatastreamVersion().get(0).getMIMETYPE());
                md5InfosMap.put(Const.OC_GRP_ID_CREATION, fileMD5Info);

                FileMD5Info fileMD5InfoValidation = MetsUtils.getDigest(inputStream);
                fileMD5InfoValidation.setFileName(fileName + extension);
                fileMD5InfoValidation.setCreated(calendar);
                md5InfosMap.put(Const.OC_GRP_ID_VALIDATION, fileMD5InfoValidation);

                fileType.setMIMETYPE(rawDS.getDatastreamVersion().get(0).getMIMETYPE());
                fileType.setSIZE(Long.valueOf(fileMD5Info.getSize()));
                fileType.setCHECKSUM(fileMD5Info.getMd5());

                FileType.FLocat fLocat = new FileType.FLocat();
                fLocat.setLOCTYPE("URL");
                URI uri = URI.create("original/" + fileName + extension);
                fLocat.setHref(uri.toASCIIString());
                fileType.getFLocat().add(fLocat);

                fileGrpMap.get(Const.OC_GRP_ID).getFile().add(fileType);
            } else {
                for (IMetsElement childrenElement : metsElement.getChildren()) {
                    prepareFileType(childrenElement);
                }
            }
        } catch (Exception ex) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while getting file datastreams for " + metsElement.getOriginalPid(), false, ex);
        }
    }

    /**
     * Returns the date of titleIssued
     */
    private String getDateIssued(IMetsElement metsElement) throws MetsExportException {
        if (isNdkEmonograph(metsElement)) {
            Node partNode = MetsUtils.xPathEvaluateNode(metsElement.getModsStream(), "//*[local-name()='mods']/*[local-name()='originInfo']/*[local-name()='dateIssued']");
            if (partNode == null) {
                throw new MetsExportException("Error - missing date issued. Please insert it.");
            }
            return ", " + partNode.getTextContent();
        }
        return "";
    }

    /**
     * Returns true if element is issue, else return false
     */
    public boolean isNdkEmonograph(IMetsElement metsElement) throws MetsExportException {
        String type = MetsUtils.xPathEvaluateString(metsElement.getModsStream(), "//*[local-name()='mods']/*[local-name()='genre']");
        return type.equals("electronic title") || type.equals("electronic volume");
    }

    private void repairPath(IMetsElement metsElement) {

        for (FileMD5Info file : metsElement.getMetsContext().getFileList()) {
            String[] fileName = file.getFileName().split("\\\\");
            if (fileName.length == 1) {
                fileName = file.getFileName().split("/");
            }
            String name;
            if (fileName.length == 2) {
                name = File.separator + fileName[fileName.length - 1];
                file.setFileName(name);
            } else if (fileName.length > 2) {
                name = File.separator + fileName[fileName.length - 2] + File.separator + fileName[fileName.length - 1];
                file.setFileName(name);
            }
        }
    }

    private void saveInfoFile(Path packageRoot, IMetsElement metsElement) throws MetsExportException {
        MetsUtils.saveInfoFile(packageRoot.getParent().toString(), metsElement.getMetsContext(), null, null, null);
    }

    /**
     * Scaffold empty SIP package
     *
     * @param metsElement element with specified package id
     * @return path of package
     * @throws MetsExportException translated from IOException
     */
    protected Path createPackageDirection(IMetsElement metsElement) throws MetsExportException {
        if (metsElement.getMetsContext().getPackageID() == null) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Package ID is null", false, null);
        }
        try {
            Path path = Paths.get(metsElement.getMetsContext().getOutputPath()).resolve(metsElement.getMetsContext().getPackageID());
            Path packageDir = Files.createDirectories(path);
            Files.createDirectory(packageDir.resolve("original"));
            //Files.createDirectory(packageDir.resolve("metadata"));
            return packageDir;
        } catch (IOException e) {
            MetsExportException ex = new MetsExportException(e.getMessage());
            ex.addException("can not create package", true, e);
            throw ex;
        }
    }

    private List<Path> saveStreams(IMetsElement metsElement, Path packageDir) throws
            MetsExportException {
        try {
            List<Path> packageFiles = new ArrayList<>();

            Optional<DatastreamType> rawDatastream = metsElement.getSourceObject().getDatastream().stream().filter(stream -> "RAW".equalsIgnoreCase(stream.getID())).findFirst();
            if (rawDatastream.isPresent()) {
                DatastreamType rawDS = FoxmlUtils.findDatastream(metsElement.getSourceObject(), "RAW");
                InputStream inputStream = null;
                if (Storage.FEDORA.equals(metsElement.getMetsContext().getTypeOfStorage())) {
                    GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "RAW");
                    inputStream = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                } else if (Storage.AKUBRA.equals(metsElement.getMetsContext().getTypeOfStorage())) {
                    AkubraObject object = metsElement.getMetsContext().getAkubraStorage().find(metsElement.getOriginalPid());
                    inputStream = AkubraUtils.getDatastreamDissemination(object, "RAW");
                } else {
                    throw new IllegalStateException("Unsupported type of Storage: " + metsElement.getMetsContext().getTypeOfStorage());
                }
                String extension = null;
                if (rawDS != null && rawDS.getDatastreamVersion() != null && rawDS.getDatastreamVersion().size() > 0 && rawDS.getDatastreamVersion().get(0) != null) {
                    extension = Const.mimeToExtensionMap.get(rawDS.getDatastreamVersion().get(0).getMIMETYPE());
                }
                if (extension == null) {
                    extension = ".pdf";
                }
                String name = "original/oc_" + metsElement.getMetsContext().getPackageID() + "_" + metsElement.getElementID().toLowerCase() + extension;
                Path originalPathDoc = packageDir.resolve(name);

                // check null
                if (Files.copy(inputStream, originalPathDoc) == 0) {
                    throw new MetsExportException("empty RAW datastream " + metsElement.getOriginalPid());
                }
                packageFiles.add(originalPathDoc);
            } else {
                if (MetsUtils.getElementType(metsElement.getModel()).equals(Const.MONOGRAPH_UNIT)) {
                    throw new MetsExportException("no RAW datastream " + metsElement.getOriginalPid());
                }
            }

            /*Optional<DatastreamType> modsDatastream = metsElement.getSourceObject().getDatastream().stream().filter(stream -> "BIBLIO_MODS".equalsIgnoreCase(stream.getID())).findFirst();
            if (modsDatastream.isPresent()) {
                GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "BIBLIO_MODS");
                InputStream dsStream = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();

                String modsName;
                switch (MetsUtils.getElementType(metsElement.getModel())) {
                    case MONOGRAPH_UNIT:
                        modsName = "mods_volume.xml";
                        break;
                    case CHAPTER:
                        modsName = "mods_chapter" + String.format("%04d", ++chapterCounter) + ".xml";
                        break;
                    case MONOGRAPH_MULTIPART:
                        modsName = "mods_title.xml";
                        break;
                    case PERIODICAL_TITLE:
                        modsName = "mods_title.xml";
                        break;
                    case PERIODICAL_VOLUME:
                        modsName = "mods_volume.xml";
                        break;
                    case ISSUE:
                        modsName = "mods_issue" + String.format("%04d", ++issueCounter) + ".xml";
                        break;
                    case ARTICLE:
                        modsName = "mods_article" + String.format("%04d", ++articleCounter) + ".xml";
                        break;
                    default:
                        throw new IllegalArgumentException("unknown model " + metsElement.getModel());
                }


                Path metadataPathDoc = packageDir.resolve("metadata").resolve(modsName);
                Files.copy(dsStream, metadataPathDoc);
                packageFiles.add(metadataPathDoc);
            }*/

            return Collections.unmodifiableList(packageFiles);
        } catch (Exception e) {
            MetsExportException ex = new MetsExportException(e.getMessage());
            ex.addException(e.getMessage(), true, e);
            throw ex;
        }
    }

    protected void insertMonograph(IMetsElement metsElement) throws MetsExportException {
        mets.setTYPE("electronic_monograph");
        DivType logicalDiv = new DivType();
        logicalStruct.setDiv(logicalDiv);
        DivType physicalDiv = new DivType();
        physicalStruct.setDiv(physicalDiv);

        boolean containsUnit = false;
        if (Const.MONOGRAPH_MULTIPART.equalsIgnoreCase(metsElement.getElementType())) {
            containsUnit = true;
        }
        for (IMetsElement childMetsElement : metsElement.getChildren()) {
            if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                containsUnit = true;
                break;
            }
        }
        logicalDiv.setLabel3(metsElement.getLabel());
        logicalDiv.setTYPE("TITLE");
        physicalDiv.setLabel3(metsElement.getLabel());
        physicalDiv.setTYPE("TITLE");
        if (!containsUnit) {
            logicalDiv.setID("MONOGRAPH_0001");
            physicalDiv.setID("DIV_P_0000");
            metsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(metsElement, ignoreMissingUrnNbn));
            insertVolume(logicalDiv, physicalDiv, metsElement, false);
            createMonographStructureMap(metsElement, false);
        } else {
            logicalDiv.setID("TITLE_0001");
            physicalDiv.setID("DIV_P_0000");
            metsElement.setModsElementID("TITLE_0001");
            titleCounter++;
            addDmdSec(metsElement);
            logicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            for (IMetsElement childMetsElement : metsElement.getChildren()) {
                if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                    continue;
                } else if (Const.CHAPTER.equals(childMetsElement.getElementType())) {
                    insertChapter(logicalDiv, physicalDiv, childMetsElement, chapterCounter);
                    chapterCounter++;
                } else if (Const.MONOGRAPH_MULTIPART.equals(childMetsElement.getElementType())) {
                    insertMonographTitle(logicalDiv, physicalDiv, childMetsElement, titleCounter);
                    titleCounter++;
                } else
                    throw new MetsExportException(childMetsElement.getOriginalPid(), "Expected Supplement, Monograph unit, Monograph Title, Chapter or Page, got:" + childMetsElement.getElementType(), false, null);
            }
        }
        for (IMetsElement childMetsElement : metsElement.getChildren()) {
            if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                childMetsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(childMetsElement, ignoreMissingUrnNbn));
                insertVolume(logicalDiv, physicalDiv, childMetsElement, true);
                createMonographStructureMap(metsElement, true);
            }
        }
    }

    protected void insertPeriodical(IMetsElement metsElement) throws MetsExportException {
        mets.setTYPE("electronic_coll_journal");
        DivType logicalDiv = new DivType();
        logicalStruct.setDiv(logicalDiv);
        DivType physicalDiv = new DivType();
        physicalStruct.setDiv(physicalDiv);

        logicalDiv.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        logicalDiv.setTYPE("PERIODICAL_TITLE");
        logicalDiv.setID(metsElement.getMetsContext().getRootElement().getElementID());
        physicalDiv.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        physicalDiv.setTYPE("PERIODICAL_TITLE");

        addDmdSec(metsElement.getMetsContext().getRootElement());

        logicalDiv.getDMDID().add(metsElement.getMetsContext().getRootElement().getModsMetsElement());
        physicalDiv.getDMDID().add(metsElement.getMetsContext().getRootElement().getModsMetsElement());

        for (IMetsElement childMetsElement : metsElement.getMetsContext().getRootElement().getChildren()) {
            if (Const.PERIODICAL_VOLUME.equals(childMetsElement.getElementType())) {
                insertVolume(logicalDiv, physicalDiv, childMetsElement, false);
            } else if (Const.ISSUE.equals(childMetsElement.getElementType())) {
                childMetsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(childMetsElement, ignoreMissingUrnNbn));
                insertIssue(logicalDiv, physicalDiv, childMetsElement);
            } else if (Const.SUPPLEMENT.equals(childMetsElement.getElementType())) {
                childMetsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(childMetsElement, ignoreMissingUrnNbn));
                insertSupplement(logicalDiv, physicalDiv, childMetsElement);
            } else
                throw new MetsExportException(childMetsElement.getOriginalPid(), "Expected Supplement, Monograph unit, Monograph Title, Chapter or Page, got:" + childMetsElement.getElementType(), false, null);
        }
        createPeriodicalStructureMap(metsElement);
        //createStructureMap(metsElement, true);
    }

    private void createPeriodicalStructureMap(IMetsElement metsElement) {
        StructMapType logicalMap = null;
        for (StructMapType structMap : mets.getStructMap()) {
            if ("LOGICAL".equals(structMap.getTYPE())) {
                logicalMap = structMap;
                break;
            }
        }
        if (logicalMap != null) {
            mets.getStructMap().clear();
            StructMapType map = copyPeriocialMap(logicalMap, metsElement);
            mets.getStructMap().add(map);
        }
    }

    private void createMonographStructureMap(IMetsElement metsElement, boolean isMultiPartMonograph) {
        StructMapType logicalMap = null;
        for (StructMapType structureMap : mets.getStructMap()) {
            if ("LOGICAL".equals(structureMap.getTYPE())) {
                logicalMap = structureMap;
                break;
            }
        }
        if (logicalMap != null) {
            mets.getStructMap().clear();
            StructMapType map = copyMap(logicalMap, metsElement, isMultiPartMonograph);
            mets.getStructMap().add(map);
        }
    }

    private StructMapType copyMap(StructMapType logicalMap, IMetsElement metsElement, boolean isMultiPartMonograph) {
        int docId = 1;
        StructMapType map = new StructMapType();
        if (isMultiPartMonograph) {
            DivType divTitle = new DivType();
            map.setDiv(divTitle);

            if (logicalMap.getDiv() != null) {
                DivType divOriginalTitle = logicalMap.getDiv();
                copyDiv(divTitle, divOriginalTitle, true, false);
                for (int i = 0; i < divOriginalTitle.getDiv().size(); i++) {
                    DivType divOriginalVolume = divOriginalTitle.getDiv().get(i);
                    IMetsElement childElement = metsElement.getChildren().get(i);
                    DivType divVolume = new DivType();
                    copyDiv(divVolume, divOriginalVolume, true, false);
                    divTitle.getDiv().add(divVolume);
                    if (divOriginalVolume.getDiv().isEmpty()) {
                        createDivDocument(docId++, divVolume, childElement);
                    } else {
                        for (int j = 0; j < divOriginalVolume.getDiv().size(); j++) {
                            DivType divOriginalChapter = divOriginalVolume.getDiv().get(j);
                            IMetsElement childChapterElement = childElement.getChildren().get(j);
                            DivType divChapter = new DivType();
                            copyDiv(divChapter, divOriginalChapter, true, false);
                            divVolume.getDiv().add(divChapter);
                            createDivDocument(docId++, divChapter, childChapterElement);
                        }
                    }
                }
            }
        } else {
            if (logicalMap.getDiv() != null) {
                DivType divOriginalTitle = logicalMap.getDiv();
                for (DivType divOriginalVolume : divOriginalTitle.getDiv()) {
                    DivType divVolume = new DivType();
                    map.setDiv(divVolume);
                    copyDiv(divVolume, divOriginalVolume, true, true);
                    if (divOriginalVolume.getDiv().isEmpty()) {
                        createDivDocument(docId++, divVolume, metsElement);
                    } else {
                        for (int j = 0; j < divOriginalVolume.getDiv().size(); j++) {
                            DivType divOriginalChapter = divOriginalVolume.getDiv().get(j);
                            IMetsElement childChapterElement = metsElement.getChildren().get(j);
                            DivType divChapter = new DivType();
                            copyDiv(divChapter, divOriginalChapter, true, false);
                            divVolume.getDiv().add(divChapter);
                            createDivDocument(docId++, divChapter, childChapterElement);
                        }
                    }
                }
            }

        }
        return map;
    }

    private StructMapType copyPeriocialMap(StructMapType logicalMap, IMetsElement metsElement) {
        int docId = 1;
        StructMapType map = new StructMapType();
        DivType divTitle = new DivType();
        map.setDiv(divTitle);

        if (logicalMap.getDiv() != null) {
            DivType divOriginalTitle = logicalMap.getDiv();
            copyDiv(divTitle, divOriginalTitle, true, false);
            for (DivType divOriginalVolume : divOriginalTitle.getDiv()) {
                DivType divVolume = new DivType();
                copyDiv(divVolume, divOriginalVolume, true, false);
                divTitle.getDiv().add(divVolume);
                for (DivType divOriginalIssue : divOriginalVolume.getDiv()) {
                    DivType divIssue = new DivType();
                    copyDiv(divIssue, divOriginalIssue, true, false);
                    divVolume.getDiv().add(divIssue);
                    if (divOriginalIssue.getDiv().isEmpty()) {
                        createDivDocument(docId++, divIssue, metsElement);
                    } else {
                        for (int i = 0; i < divOriginalIssue.getDiv().size(); i++) {
                            DivType divOriginalArticle = divOriginalIssue.getDiv().get(i);
                            IMetsElement childElement = metsElement.getChildren().get(i);
                            DivType divArticle = new DivType();
                            copyDiv(divArticle, divOriginalArticle, true, false);
                            divIssue.getDiv().add(divArticle);
                            createDivDocument(docId++, divArticle, childElement);
                        }
                    }
                }
            }
        }
        return map;
    }

    private void copyDiv(DivType divDestination, DivType divSource, boolean dmdid, boolean label) {
        divDestination.setID(divSource.getID());
        divDestination.setTYPE(divSource.getTYPE());
        if (dmdid) {
            divDestination.getDMDID().addAll(divSource.getDMDID());
        }
        if (label) {
            divDestination.setLabel3(divSource.getLabel3());
        }
    }

    private void createDivDocument(int docId, DivType divVolume, IMetsElement metsElement) {
        DivType divDocument = new DivType();
        divVolume.getDiv().add(divDocument);

        String fileName = "oc_" + metsElement.getMetsContext().getPackageID() + "_" + metsElement.getElementID().toLowerCase();

        divDocument.setID("DOCUMENT_" + String.format("%04d", docId));
        divDocument.setLabel3(fileName);
        divDocument.setTYPE("DOCUMENT");

        createDivFile(divDocument, fileName, docId);
    }

    private void createDivFile(DivType divDocument, String fileName, int docId) {
        DivType divFile = new DivType();
        divDocument.getDiv().add(divFile);
        divFile.setID("FILE_" + String.format("%04d", docId));
        divFile.setTYPE("FILE");

        DivType.Fptr ftprFile = new DivType.Fptr();
        divFile.getFptr().add(ftprFile);

        FileType fileType = new FileType();
        ftprFile.setFILEID(fileType);
        fileType.setCHECKSUMTYPE("MD5");
        fileType.setMIMETYPE("pdf");
        fileType.setID(fileName);

        FileType.FLocat fLocat = new FileType.FLocat();
        fLocat.setLOCTYPE("URL");
        URI uri = URI.create(fileName);
        fLocat.setHref(uri.toASCIIString());
        fileType.getFLocat().add(fLocat);
    }

    @Override
    protected void addNdkEObjectPidToExport(IMetsElement element) {
        pidsToExport.add(element);
    }

    // u ndk EModelu se nepridava zadny structLink
    @Override
    protected void addStructLink() throws MetsExportException {
        return;
    }
}
