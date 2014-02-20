/*
 * Copyright (C) 2014 Robert Simonovsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cas.lib.proarc.common.export.desa.structure;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import net.lingala.zip4j.core.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.ZipParameters;
import net.lingala.zip4j.util.Zip4jConstants;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;

import cz.cas.lib.proarc.common.export.desa.Const;
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.desa.SIP2DESATransporter;
import cz.cas.lib.proarc.desa.pspsip.PSPSIP;
import cz.cas.lib.proarc.desa.pspsip.PSPSIP.SIP;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.DivType.Fptr;
import cz.cas.lib.proarc.mets.DivType.Mptr;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.FileType.FLocat;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap.XmlData;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType.FileSec;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.nsesss2.Spis;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

/**
 * Visitor class for creating mets document out of Desa objects
 *
 * @author Robert Simonovsky
 *
 */
public class DesaElementVisitor implements IDesaElementVisitor {
    private final Logger LOG = Logger.getLogger(DesaElementVisitor.class.getName());

    /**
     * Archives the list of files to a zip archive
     *
     * @param zipFileName
     * @param fileList
     * @param desaElement
     * @throws MetsExportException
     */
    private void zip(String zipFileName, ArrayList<File> fileList, IDesaElement desaElement) throws MetsExportException {
        try {
            File file = new File(zipFileName);
            if (file.exists()) {
                file.delete();
                file = null;
                LOG.log(Level.FINE, "File:" + zipFileName + " exists, so it was deleted");
            }
            ZipFile zipFile = new ZipFile(zipFileName);
            ZipParameters zip4jZipParameters = new ZipParameters();
            zip4jZipParameters.setCompressionMethod(Zip4jConstants.COMP_DEFLATE);
            zip4jZipParameters.setCompressionLevel(Zip4jConstants.DEFLATE_LEVEL_NORMAL);
            zipFile.addFiles(fileList, zip4jZipParameters);
            LOG.log(Level.FINE, "Zip archive created:" + zipFileName + " for " + desaElement.getElementType());
        } catch (ZipException e) {
            LOG.log(Level.SEVERE, "Unable to create a zip file:" + zipFileName, e);
            throw new MetsExportException(desaElement.getOriginalPid(), "Unable to create a zip file:" + zipFileName, false, e);
        }

    }

    /**
     * Returns a identifier from BIBLIO-MODS stream
     *
     * @param desaElement
     * @return
     * @throws MetsExportException
     */
    private String getIdentifier(IDesaElement desaElement) throws MetsExportException {
        LOG.log(Level.FINE, "Element model:" + desaElement.getModel());
        if (Const.DC_URI.equals(desaElement.getDescriptorType())) {
            LOG.log(Level.FINE, "DC variant descriptor in BIBLIO MODS for " + desaElement.getOriginalPid());
            Document dcDoc = MetsUtils.getDocumentFromList(desaElement.getDescriptor());
            List<String> validationErrors;
            try {
                validationErrors = MetsUtils.validateAgainstXSD(dcDoc, OaiDcType.class.getResourceAsStream("dc_oai.xsd"));
            } catch (Exception ex) {
                LOG.severe("Error while validating BIBLIO_MODS for:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")");
                throw new MetsExportException(desaElement.getOriginalPid(), "Error while validating DC document in BIBLIO_MODS for:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")", false, ex);
            }

            if (validationErrors.size() > 0) {
                MetsExportException metsException = new MetsExportException(desaElement.getOriginalPid(), "Invalid DC in BIBLIO_MODS for:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")", false, null);
                metsException.setValidationErrors(validationErrors);
                throw metsException;
            }
            List<Element> descriptor = desaElement.getDescriptor();
            return MetsUtils.xPathEvaluateString(descriptor, "*[local-name()='dc']/*[local-name()='identifier']");
        }

        if (Const.NSESSS_URI.equals(desaElement.getDescriptorType())) {
            LOG.log(Level.FINE, "NSESS variant descriptor in BIBLIO MODS for " + desaElement.getOriginalPid());
            Document nsessDoc = MetsUtils.getDocumentFromList(desaElement.getDescriptor());
            List<String> validationErrors;
            try {
                validationErrors = MetsUtils.validateAgainstXSD(nsessDoc, Spis.class.getResourceAsStream("nsesss2.xsd"));
            } catch (Exception ex) {
                LOG.severe("Invalid NSESSS document in BIBLIO_MODS for:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")");
                throw new MetsExportException(desaElement.getOriginalPid(), "Error while validating NSESSS document in BIBLIO_MODS for:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")", false, ex);
            }
            if (validationErrors.size() > 0) {
                MetsExportException metsException = new MetsExportException(desaElement.getOriginalPid(), "Invalid NSESSS in BIBLIO_MODS for:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")", false, null);
                metsException.setValidationErrors(validationErrors);
                throw metsException;
            }

            List<Element> descriptor = desaElement.getDescriptor();
            if (Const.FOLDER.equalsIgnoreCase(desaElement.getElementType())) {
                return MetsUtils.xPathEvaluateString(descriptor, "*[local-name()='Spis']/*[local-name()='EvidencniUdaje']/*[local-name()='Identifikace']/*[local-name()='Identifikator']");
            }
            if (Const.DOCUMENT.equalsIgnoreCase(desaElement.getElementType())) {
                return MetsUtils.xPathEvaluateString(descriptor, "*[local-name()='Dokument']/*[local-name()='EvidencniUdaje']/*[local-name()='Identifikace']/*[local-name()='Identifikator']");
            }
            LOG.log(Level.SEVERE, "Element nit DOCUMENT or FOLDER" + desaElement.getElementType());
            throw new MetsExportException(desaElement.getOriginalPid(), "Element not DOCUMENT or FOLDER:" + desaElement.getElementType(), false, null);
        }

        LOG.log(Level.SEVERE, "Unable to get Identifier - DER/DES descriptor missing - " + desaElement.getModel());
        throw new MetsExportException(desaElement.getOriginalPid(), "Unable to get Identifier - DER/DES descriptor missing - " + desaElement.getModel(), false, null);
    }

    /**
     * Returns a label for LOGICAL div from DC stream
     *
     * @param desaElement
     * @return
     * @throws MetsExportException
     */
    private String getLabel(IDesaElement desaElement) throws MetsExportException {
        LOG.log(Level.FINE, "Element model:" + desaElement.getModel());
        if (MetsUtils.xPathEvaluateNode(desaElement.getDescriptor(), "*[namespace-uri()='http://www.openarchives.org/OAI/2.0/oai_dc/']") != null) {
            LOG.log(Level.FINE, "DC variant descriptor in BIBLIO MODS for " + desaElement.getOriginalPid());
            List<Element> descriptor = desaElement.getDescriptor();
            return MetsUtils.xPathEvaluateString(descriptor, "*[local-name()='dc']/*[local-name()='title']");
        }
        if (MetsUtils.xPathEvaluateNode(desaElement.getDescriptor(), "*[namespace-uri()='http://www.mvcr.cz/nsesss/v2']") != null) {
            LOG.log(Level.FINE, "NSESS variant descriptor in BIBLIO MODS for " + desaElement.getOriginalPid());
            List<Element> descriptor = desaElement.getDescriptor();
            if (Const.FOLDER.equalsIgnoreCase(desaElement.getElementType())) {
                return MetsUtils.xPathEvaluateString(descriptor, "*[local-name()='Spis']/*[local-name()='EvidencniUdaje']/*[local-name()='Identifikace']/*[local-name()='Identifikator']");
            }
            if (Const.DOCUMENT.equalsIgnoreCase(desaElement.getElementType())) {
                return MetsUtils.xPathEvaluateString(descriptor, "*[local-name()='Dokument']/*[local-name()='EvidencniUdaje']/*[local-name()='Identifikace']/*[local-name()='Identifikator']");
            }
            LOG.log(Level.SEVERE, "Element nit DOCUMENT or FOLDER" + desaElement.getElementType());
            throw new MetsExportException(desaElement.getOriginalPid(), "Element not DOCUMENT or FOLDER:" + desaElement.getElementType(), false, null);
        }

        LOG.log(Level.SEVERE, "Unable to get Label - DER/DES descriptor missing - " + desaElement.getModel());
        throw new MetsExportException(desaElement.getOriginalPid(), "Unable to get Label - DER/DES descriptor missing - " + desaElement.getModel(), false, null);
    }

    /**
     * Creates a temporary folder
     *
     * @param desaElement
     * @return
     * @throws MetsExportException
     */
    private File createTempFolder(IDesaElement desaElement) throws MetsExportException {
        File tmpFileFolder = null;
        try {
            tmpFileFolder = File.createTempFile("tmp" + MetsUtils.removeNonAlpabetChars(desaElement.getElementID()), ".tmp");
            tmpFileFolder.delete();
            tmpFileFolder = new File(tmpFileFolder.getAbsolutePath());
        } catch (IOException e) {
            LOG.log(Level.SEVERE, "Unable to create a temp file for:" + desaElement.getElementID());
            throw new MetsExportException(desaElement.getOriginalPid(), "Unable to create a temp folder", false, e);
        }
        tmpFileFolder.mkdir();
        LOG.log(Level.FINE, "TMP folder:" + tmpFileFolder.getAbsolutePath() + " created for:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")");
        return tmpFileFolder;
    }

    /**
     * Deletes a folder
     *
     * @param folder
     */
    private static void deleteFolder(File folder) {
        File[] files = folder.listFiles();
        if (files != null) {
            for (File f : files) {
                if (f.isDirectory()) {
                    deleteFolder(f);
                } else {
                    f.delete();
                }
            }
        }
        folder.delete();
    }

    /**
     * Prepares the generic mets information
     *
     * @param desaElement
     * @return
     * @throws MetsExportException
     */
    private Mets prepareMets(IDesaElement desaElement) throws MetsExportException {
        Mets mets = new Mets();
        MdSecType mdSecType = new MdSecType();
        mdSecType.setID("DM_0001");
        MdWrap mdWrap = new MdWrap();
        mdWrap.setMDTYPE("OTHER");
        mdWrap.setMIMETYPE("text/xml");
        XmlData xmlData = new XmlData();
        xmlData.getAny().addAll(desaElement.getDescriptor());
        mdWrap.setXmlData(xmlData);
        mdSecType.setMdWrap(mdWrap);
        mets.getDmdSec().add(mdSecType);
        StructMapType structMapType = new StructMapType();
        structMapType.setTYPE("LOGICAL");
        mets.getStructMap().add(structMapType);
        return mets;
    }

    /**
     * Returns a file name of original file in RAW datastream
     *
     * @param desaElement
     * @return
     * @throws MetsExportException
     */
    private String getFileName(IDesaElement desaElement) throws MetsExportException {
        String node = MetsUtils.xPathEvaluateString(desaElement.getRelsExt(), "*[local-name()='RDF']/*[local-name()='Description']/*[local-name()='importFile']");
        return node;
    }

    /**
     * Saves the mets document into a file
     *
     * @param mets
     * @param outputFile
     * @throws MetsExportException
     */
    private void saveMets(Mets mets, File outputFile, IDesaElement desaElement) throws MetsExportException {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class, OaiDcType.class, ModsDefinition.class);
            Marshaller marshaller = jaxbContext.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
            marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, "http://www.w3.org/2001/XMLSchema-instance http://www.w3.org/2001/XMLSchema.xsd http://www.loc.gov/METS/ http://www.loc.gov/standards/mets/mets.xsd http://www.loc.gov/mods/v3 http://www.loc.gov/standards/mods/mods.xsd http://www.openarchives.org/OAI/2.0/oai_dc/ http://www.openarchives.org/OAI/2.0/oai_dc.xsd");
            marshaller.marshal(mets, outputFile);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Unable to save mets file:" + outputFile.getAbsolutePath(), ex);
            throw new MetsExportException(desaElement.getOriginalPid(), "Unable to save mets file:" + outputFile.getAbsolutePath(), false, ex);
        }
        List<String> validationErrors;
        try {
            validationErrors = MetsUtils.validateAgainstXSD(outputFile, Mets.class.getResourceAsStream("mets.xsd"));
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Error while validating Mets file: " + outputFile);
            throw new MetsExportException("Error while validating Mets file: " + outputFile, false, ex);
        }
        if (validationErrors.size() > 0) {
            MetsExportException metsException = new MetsExportException("Error while validating Mets file:" + outputFile, false, null);
            metsException.setValidationErrors(validationErrors);
            throw metsException;
        }
        LOG.log(Level.FINE, "Element validated:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")");
    }

    /**
     * Adds a file to the apropriate fileGroup
     *
     * @param fileGrpMap
     * @param desaElement
     * @param fileType
     * @throws MetsExportException
     */
    private void addFiletoFileGrp(Map<String, FileGrp> fileGrpMap, IDesaElement desaElement, FileType fileType) throws MetsExportException {
        String type = MetsUtils.xPathEvaluateString(desaElement.getDescriptor(), "*[local-name()='dc']/*[local-name()='type']");
        if (type == null) {
            LOG.log(Level.SEVERE, "Element type is missing for pid:" + desaElement.getOriginalPid());
            throw new MetsExportException(desaElement.getOriginalPid(), "Element type is missing", false, null);
        }

        if (type.length() == 0) {
            throw new MetsExportException(desaElement.getOriginalPid(), "Type of component is empty", false, null);
        }

        String fileGrpType = Const.fileGrpMap.get(type);
        if (fileGrpType == null) {
            LOG.log(Level.SEVERE, "Unable to find mapping for file type:" + type);
            throw new MetsExportException(desaElement.getOriginalPid(), "Unable to find mapping for file type:" + type, false, null);
        }
        fileGrpMap.get(fileGrpType).getFile().add(fileType);
    }

    /**
     * Adds all non-empty filegroups to the mets
     *
     * @param fileGrpMap
     * @param fileSec
     */
    private void addFileGrpToMets(Map<String, FileGrp> fileGrpMap, FileSec fileSec) {
        int order = 0;
        for (String key : fileGrpMap.keySet()) {
            FileGrp fileGrp = fileGrpMap.get(key);
            if (fileGrp.getFile().size() > 0) {
                order++;
                fileGrp.setID("GRP_000" + order);
                fileSec.getFileGrp().add(fileGrp);
            }
        }
    }

    /**
     *
     * Prepares a map of all FileGroups
     *
     * @return
     */
    private HashMap<String, FileGrp> prepareFileGrpMap() {
        HashMap<String, FileGrp> grpMap = new HashMap<String, FileGrp>();
        String[] uses = { Const.ORIGINAL, Const.PREVIEW, Const.DIGITIZED, Const.MIGRATED, Const.INPUT };
        for (String use : uses) {
            FileGrp fileGrp = new FileGrp();
            fileGrp.setUSE(use);
            grpMap.put(use, fileGrp);
        }
        return grpMap;
    }

    /**
     * Inserts a document element into a mets
     *
     * @param desaElement
     * @param suffix
     * @throws MetsExportException
     */
    private void insertDocument(IDesaElement desaElement, String suffix) throws MetsExportException {
        Map<String, FileGrp> fileGrpMap = prepareFileGrpMap();
        if (suffix == null) {
            suffix = "0001";
        }
        File tmpFolder = createTempFolder(desaElement);
        try {
            File outputMets = new File(tmpFolder.getAbsolutePath() + File.separator + "mets.xml");
            Mets mets = prepareMets(desaElement);
            DivType divType = new DivType();
            divType.setLabel(getLabel(desaElement));
            divType.setTYPE("record");
            divType.getDMDID().add(mets.getDmdSec().get(0));
            ArrayList<File> fileList = new ArrayList<File>();
            StructMapType structMapType = mets.getStructMap().get(0);
            structMapType.setDiv(divType);

            // if no files are present, then skip
            if (desaElement.getChildren().size() > 0) {
                FileSec fileSec = new FileSec();
                mets.setFileSec(fileSec);
                int fileOrder = 0;
                for (DesaElement fileElement : desaElement.getChildren()) {
                    fileOrder++;
                    DatastreamType rawDS = FoxmlUtils.findDatastream(fileElement.getSourceObject(), "RAW");
                    byte[] fileContent = new byte[0];
                    String mimeType = "empty";
                    if (rawDS != null) {
                        if (rawDS.getDatastreamVersion().get(0).getContentLocation() != null) {
                            if ("INTERNAL_ID".equals(rawDS.getDatastreamVersion().get(0).getContentLocation().getTYPE())) {
                                if (desaElement.getDesaContext().getFedoraClient() == null) {
                                    LOG.log(Level.SEVERE, "Datastream dissemination allowed only for Fedora storage - for filesystem only binary content is allowed");
                                    throw new MetsExportException(fileElement.getOriginalPid(), "Datastream dissemination allowed only for Fedora storage", false, null);
                                }
                                desaElement.getDesaContext().getFedoraClient();
                                GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(fileElement.getOriginalPid(), "RAW");
                                try {
                                    InputStream is = dsRaw.execute(desaElement.getDesaContext().getFedoraClient()).getEntityInputStream();
                                    ByteArrayOutputStream bos = new ByteArrayOutputStream();
                                    try {
                                        MetsUtils.copyStream(is, bos);
                                        bos.close();
                                        fileContent = bos.toByteArray();
                                    } catch (IOException e) {
                                        LOG.log(Level.SEVERE, "Unable to copy raw datastream content", e);
                                        throw new MetsExportException(desaElement.getOriginalPid(), "Unable to copy raw datastream content", false, e);
                                    }
                                } catch (FedoraClientException e) {
                                    LOG.log(Level.SEVERE, "Unable to read raw datastream content", e);
                                    throw new MetsExportException(desaElement.getOriginalPid(), "Unable to read raw datastream content", false, e);
                                }
                            } else {
                                LOG.log(Level.SEVERE, "Expecting INTERNAL_ID type in ContentLocation - found:" + rawDS.getDatastreamVersion().get(0).getContentLocation().getTYPE());
                                throw new MetsExportException(fileElement.getOriginalPid(), "Expecting INTERNAL_ID type in ContentLocation - found:" + rawDS.getDatastreamVersion().get(0).getContentLocation().getTYPE(), false, null);
                            }
                        } else {
                            fileContent = rawDS.getDatastreamVersion().get(0).getBinaryContent();
                        }
                        mimeType = rawDS.getDatastreamVersion().get(0).getMIMETYPE();
                    } else {
                        LOG.log(Level.WARNING, "RAW datastream is null for:" + desaElement.getOriginalPid() + " - skipping");
                        desaElement.getDesaContext().getMetsExportException().addException(fileElement.getOriginalPid(), "RAW datastream is missing", false, null);
                        continue;
                    }
                    FileOutputStream fos;
                    String outputFileName = MetsUtils.removeNonAlpabetChars(getFileName(fileElement));
                    /*
                     * Generates a filename if it's not provided from the
                     * original document
                     */
                    if ((outputFileName == null) || (outputFileName.trim().length() == 0)) {
                        outputFileName = "file_" + String.format("%04d", fileOrder) + "." + MetsUtils.getMimeToExtension().getProperty(mimeType);
                        LOG.log(Level.INFO, "importFile name was not specified for:" + fileElement.getOriginalPid() + " new name was generated:" + outputFileName);
                    }
                    String fullOutputFileName = tmpFolder.getAbsolutePath() + File.separator + outputFileName;
                    try {
                        File outputFile = new File(fullOutputFileName);
                        fos = new FileOutputStream(outputFile);
                        fileList.add(outputFile);
                    } catch (FileNotFoundException e) {
                        LOG.log(Level.SEVERE, "Unable to create a temp file:" + fullOutputFileName);
                        throw new MetsExportException(fileElement.getOriginalPid(), "Unable to create a temp file:" + fullOutputFileName, false, e);
                    }
                    FileMD5Info fileMd5Info;
                    try {
                        fileMd5Info = MetsUtils.getDigestAndCopy(new ByteArrayInputStream(fileContent), fos);
                    } catch (NoSuchAlgorithmException e) {
                        LOG.log(Level.SEVERE, "Unable to generate MD5 digest", e);
                        throw new MetsExportException(fileElement.getOriginalPid(), "Unable to generate MD5 digest", false, e);
                    } catch (IOException e) {
                        LOG.log(Level.SEVERE, "Unable to save file", e);
                        throw new MetsExportException(fileElement.getOriginalPid(), "Unable to save file", false, e);
                    }
                    FileType fileType = new FileType();
                    String validIdentifier = MetsUtils.validateIdentifier(getIdentifier(desaElement) + "_" + String.format("%04d", fileOrder));
                    fileType.setID(validIdentifier);
                    fileType.setCHECKSUMTYPE("MD5");
                    fileType.setCHECKSUM(fileMd5Info.getMd5());
                    FLocat flocat = new FLocat();
                    flocat.setLOCTYPE("URL");
                    flocat.setHref(outputFileName);
                    fileType.getFLocat().add(flocat);
                    addFiletoFileGrp(fileGrpMap, fileElement, fileType);
                    Fptr fptr = new Fptr();
                    fptr.setFILEID(fileType);
                    divType.getFptr().add(fptr);
                }
                addFileGrpToMets(fileGrpMap, fileSec);
            }
            saveMets(mets, outputMets, desaElement);
            desaElement.setZipName(MetsUtils.removeNonAlpabetChars(getIdentifier(desaElement)));
            fileList.add(outputMets);
            String zipFileName = desaElement.getDesaContext().getOutputPath() + File.separator + desaElement.getZipName() + ".zip";
            zip(zipFileName, fileList, desaElement);
        } finally {
            deleteFolder(tmpFolder);
        }
        LOG.fine("Document successfuly exported");
    }

    /**
     * Insert a folder element into a mets
     *
     * @param desaElement
     * @throws MetsExportException
     */
    private void insertFolder(IDesaElement desaElement) throws MetsExportException {
        Mets mets = prepareMets(desaElement);
        DivType divType = new DivType();
        divType.setTYPE("file");
        divType.setLabel(getLabel(desaElement));
        divType.getDMDID().add(mets.getDmdSec().get(0));
        StructMapType structMapType = mets.getStructMap().get(0);
        structMapType.setDiv(divType);
        int documentOrder = 0;
        for (DesaElement documentElement : desaElement.getChildren()) {
            documentOrder++;
            insertDocument(documentElement, String.format("%04d", documentOrder));
            DivType documentDiv = new DivType();
            documentDiv.setTYPE("record");
            documentDiv.setLabel(getLabel(documentElement));
            documentDiv.setORDER(BigInteger.valueOf(documentOrder));
            Mptr mptr = new Mptr();
            mptr.setLOCTYPE("OTHER");
            mptr.setOTHERLOCTYPE("internal_reference");
            mptr.setHref(getIdentifier(documentElement));
            documentDiv.getMptr().add(mptr);
            divType.getDiv().add(documentDiv);
        }
        File tmpFolder = createTempFolder(desaElement);
        try {
            File outputMets = new File(tmpFolder.getAbsolutePath() + File.separator + "mets.xml");
            saveMets(mets, outputMets, desaElement);
            desaElement.setZipName(MetsUtils.removeNonAlpabetChars(getIdentifier(desaElement)) + "_FILE");
            String zipFileName = desaElement.getDesaContext().getOutputPath() + File.separator + desaElement.getZipName() + ".zip";
            ArrayList<File> fileList = new ArrayList<File>();
            fileList.add(outputMets);
            zip(zipFileName, fileList, desaElement);
        } finally {
            deleteFolder(tmpFolder);
        }

        LOG.fine("Folder successfuly exported");
    }

    /**
     * Updates desaElements with the IdSIPVersion from DESA transport
     *
     * @param desaElement
     * @param transportResult
     */
    private void updateSIPversion(IDesaElement desaElement, PSPSIP transportResult) {
        for (DesaElement desaElementChild : desaElement.getChildren()) {
            updateSIPversion(desaElementChild, transportResult);
        }

        if (desaElement.getZipName() != null) {
            for (SIP sip : transportResult.getSIP()) {
                if (sip.getIdentifier().equals(desaElement.getZipName())) {
                    desaElement.setIdSIPVersion(sip.getIdSIPVersion());
                    break;
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see cz.cas.lib.proarc.common.export.desa.structure.IDesaElementVisitor#
     * insertIntoMets
     * (cz.cas.lib.proarc.common.export.desa.structure.IDesaElement)
     */
    @Override
    public void insertIntoMets(IDesaElement desaElement, HashMap<String, String> desaProps) throws MetsExportException {
        LOG.log(Level.FINE, "Inserting into Mets:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")");
        // get root element first
        IDesaElement rootElement = desaElement.getDesaContext().getRootElement();
        if (rootElement == null) {
            throw new MetsExportException("Element does not have a root set:" + desaElement.getModel() + " - " + desaElement.getOriginalPid(), false);
        }
        if (Const.DOCUMENT.equalsIgnoreCase(rootElement.getElementType())) {
            insertDocument(rootElement, null);
        } else if (Const.FOLDER.equalsIgnoreCase(rootElement.getElementType())) {
            insertFolder(rootElement);
        } else
            throw new MetsExportException(rootElement.getOriginalPid(), "Unknown type:" + rootElement.getElementType() + " model:" + rootElement.getModel(), false, null);

        SIP2DESATransporter sipTransporter = desaElement.getDesaContext().getTransporter();
        if (sipTransporter != null) {
            LOG.log(Level.INFO, "Exporting to desa");
            try {
                String desaResultPath = rootElement.getDesaContext().getDesaResultPath();
                if (desaResultPath != null) {
                    sipTransporter.transport(rootElement.getDesaContext().getOutputPath(), desaResultPath, desaResultPath);
                    updateSIPversion(rootElement, sipTransporter.getResults());
                } else {
                    throw new MetsExportException(desaElement.getOriginalPid(), "Result dir (desa.resultDir) is not set", false, null);
                }
            } catch (Exception ex) {
                throw new MetsExportException(desaElement.getOriginalPid(), "Unable to transport mets to desa", false, ex);
            }
        }
    }
}
