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

package cz.cas.lib.proarc.common.export.mets.structure;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.apache.commons.codec.binary.Hex;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.client.response.GetDatastreamsResponse;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;

import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.MimeType;
import cz.cas.lib.proarc.mets.AmdSecType;
import cz.cas.lib.proarc.mets.AreaType;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.MetsType;
import cz.cas.lib.proarc.mets.DivType.Fptr;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.FileType.FLocat;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap.XmlData;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType.FileSec;
import cz.cas.lib.proarc.mets.MetsType.MetsHdr;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.MetsType.StructLink;
import cz.cas.lib.proarc.mets.StructLinkType.SmLink;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

/**
 * Visitor class for creating mets document out of Mets objects
 *
 * @author Robert Simonovsky
 *
 */
public class MetsElementVisitor implements IMetsElementVisitor {
    private final Logger LOG = Logger.getLogger(MetsElementVisitor.class.getName());
    private Mets mets;
    private StructMapType logicalStruct;
    private StructMapType physicalStruct;
    private HashMap<String, FileGrp> fileGrpMap;
    private final HashMap<String, String> outputFileNames = new HashMap<String, String>();
    int pageCounter = 0;

    /**
     * creates directory structure for mets elements
     */

    private void createDirectoryStructure(MetsContext metsContext) {
        for (String directory : Const.streamMappingFile.values()) {
            File file = new File(metsContext.getOutputPath() + File.separator + directory);
            if (file.exists()) {
                deleteFolder(file);
            }
            file.mkdir();
        }
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
     * Inits the Mets header info
     */
    protected void initHeader(IMetsElement metsElement) {
        mets.setLabel1(metsElement.getLabel());
        MetsHdr metsHdr = new MetsHdr();
        metsHdr.setCREATEDATE(metsElement.getCreateDate());
        metsHdr.setLASTMODDATE(metsElement.getLastUpdateDate());
        mets.setMetsHdr(metsHdr);
        fileGrpMap = MetsUtils.initFileGroups(mets);
    }

    /**
     * Prepares the generic mets information
     *
     * @param metsElement
     * @return
     * @throws MetsExportException
     */
    private Mets prepareMets(IMetsElement metsElement) throws MetsExportException {
        Mets mets = new Mets();
        logicalStruct = new StructMapType();
        logicalStruct.setTYPE("LOGICAL");
        logicalStruct.setLabel2("Logical_Structure");
        mets.getStructMap().add(logicalStruct);
        physicalStruct = new StructMapType();
        physicalStruct.setTYPE("PHYSICAL");
        physicalStruct.setLabel2("Physical_Structure");
        mets.getStructMap().add(physicalStruct);
        return mets;
    }

    /**
     * Saves the mets document into a file
     *
     * @param mets
     * @param outputFile
     * @throws MetsExportException
     */
    private void saveMets(Mets mets, File outputFile, IMetsElement metsElement) throws MetsExportException {
        String fileMd5Name;
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class, OaiDcType.class, ModsDefinition.class);
            Marshaller marshaller = jaxbContext.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
            marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, "http://www.w3.org/2001/XMLSchema-instance http://www.w3.org/2001/XMLSchema.xsd http://www.loc.gov/METS/ http://www.loc.gov/standards/mets/mets.xsd http://www.loc.gov/mods/v3 http://www.loc.gov/standards/mods/mods.xsd http://www.openarchives.org/OAI/2.0/oai_dc/ http://www.openarchives.org/OAI/2.0/oai_dc.xsd");
            marshaller.marshal(mets, outputFile);
            MessageDigest md;
            try {
                md = MessageDigest.getInstance("MD5");
            } catch (NoSuchAlgorithmException e) {
                throw new MetsExportException("Unable to create MD5 hash", false, e);
            }
            md.reset();
            InputStream is;
            try {
                is = new FileInputStream(outputFile);
            } catch (FileNotFoundException e) {
                throw new MetsExportException("Unable to open file:" + outputFile.getAbsolutePath(), false, e);
            }
            byte[] bytes = new byte[2048];
            int numBytes;
            int totalBytes = 0;
            try {
                while ((numBytes = is.read(bytes)) != -1) {
                    totalBytes = totalBytes + numBytes;

                    md.update(bytes, 0, numBytes);
                }
            } catch (IOException e) {
                throw new MetsExportException("Unable to generate MD5 hash", false, e);
            }
            byte[] digest = md.digest();
            String result = new String(Hex.encodeHex(digest));
            metsElement.getMetsContext().getFileList().add(new FileMD5Info("." + File.separator + "mets.xml", result, totalBytes));
            fileMd5Name = "MD5_" + MetsUtils.removeNonAlpabetChars(metsElement.getMetsContext().getPackageID()) + ".md5";
            File fileMd5 = new File(metsElement.getMetsContext().getOutputPath() + File.separator + fileMd5Name);
            OutputStreamWriter osw = new OutputStreamWriter(new FileOutputStream(fileMd5));
            for (FileMD5Info info : metsElement.getMetsContext().getFileList()) {
                osw.write(info.getMd5() + " " + info.getFileName() + "\n");
            }
            osw.close();
            is.close();
            metsElement.getMetsContext().getFileList().add(new FileMD5Info("." + File.separator + fileMd5Name, null, (int) fileMd5.length()));
            MetsUtils.saveInfoFile(metsElement.getMetsContext().getOutputPath(), metsElement.getMetsContext(), result, fileMd5Name, outputFile.length());
        } catch (Exception ex) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Unable to save mets file:" + outputFile.getAbsolutePath(), false, ex);
        }
        List<String> validationErrors;
        try {
            validationErrors = MetsUtils.validateAgainstXSD(outputFile, Mets.class.getResourceAsStream("mets.xsd"));
        } catch (Exception ex) {
            throw new MetsExportException("Error while validation document:" + outputFile, false, ex);
        }
        if (validationErrors.size() > 0) {
            MetsExportException metsException = new MetsExportException("Invalid mets file:" + outputFile, false, null);
            metsException.getExceptions().get(0).setValidationErrors(validationErrors);
            for (String error : validationErrors) {
                LOG.info(error);
            }
            throw metsException;
        }
        LOG.log(Level.FINE, "Element validated:" + metsElement.getOriginalPid() + "(" + metsElement.getElementType() + ")");
    }

    /**
     * Adds all non-empty filegroups to the mets
     *
     * @param fileGrpMap
     * @param fileSec
     */
    private void addFileGrpToMets(Map<String, FileGrp> fileGrpMap, FileSec fileSec) {
        for (String key : fileGrpMap.keySet()) {
            FileGrp fileGrp = fileGrpMap.get(key);
            if (fileGrp.getFile().size() > 0) {
                fileSec.getFileGrp().add(fileGrp);
            }
        }
    }

    /**
     *
     * Adds an element descriptors (DC, BIBLIO_MODS) to the mets document
     *
     * @param metsElement
     */
    private void addDmdSec(IMetsElement metsElement) {
        // MODS
        if (metsElement.getModsStream() != null) {
            MdSecType modsMdSecType = new MdSecType();
            metsElement.setModsMetsElement(modsMdSecType);
            mets.getDmdSec().add(modsMdSecType);
            modsMdSecType.setID("MODSMD_" + metsElement.getModsElementID());
            MdWrap modsMdWrap = new MdWrap();
            modsMdWrap.setMDTYPE("MODS");
            modsMdWrap.setMIMETYPE("text/xml");
            XmlData modsxmlData = new XmlData();
            modsxmlData.getAny().addAll(metsElement.getModsStream());
            modsMdWrap.setXmlData(modsxmlData);
            modsMdSecType.setMdWrap(modsMdWrap);
        }

        // DC
        if (metsElement.getDescriptor() != null) {
            MdSecType dcMdSecType = new MdSecType();
            mets.getDmdSec().add(dcMdSecType);
            dcMdSecType.setID("DCMD_" + metsElement.getModsElementID());
            MdWrap dcMdWrap = new MdWrap();
            dcMdWrap.setMDTYPE("DC");
            dcMdWrap.setMIMETYPE("text/xml");
            XmlData dcxmlData = new XmlData();
            dcxmlData.getAny().addAll(metsElement.getDescriptor());
            dcMdWrap.setXmlData(dcxmlData);
            dcMdSecType.setMdWrap(dcMdWrap);
        }
    }

    /**
     * adds an order and index attributes to pageDiv
     *
     * @param metsElement
     * @param pageDiv
     * @throws MetsExportException
     */
    private void fillPageIndexOrder(IMetsElement metsElement, DivType pageDiv) throws MetsExportException {
        Node partNode = MetsUtils.xPathEvaluateNode(metsElement.getModsStream(), "*[local-name()='modsCollection']/*[local-name()='mods']/*[local-name()='part']");
        if (partNode == null) {
            partNode = MetsUtils.xPathEvaluateNode(metsElement.getModsStream(), "*[local-name()='mods']/*[local-name()='part']");
        }
        pageDiv.setTYPE(partNode.getAttributes().getNamedItem("type").getNodeValue());
        NodeList nodeList = partNode.getChildNodes();
        for (int a = 0; a < nodeList.getLength(); a++) {
            if (nodeList.item(a).getNodeName().equalsIgnoreCase("mods:detail")) {
                Node numberNode = nodeList.item(a).getChildNodes().item(0).getFirstChild();
                if (nodeList.item(a).getAttributes().getNamedItem("type").getNodeValue().equalsIgnoreCase("pageNumber")) {
                    pageDiv.setORDERLABEL(numberNode.getNodeValue());
                }
                if (nodeList.item(a).getAttributes().getNamedItem("type").getNodeValue().equalsIgnoreCase("pageIndex")) {
                    pageDiv.setORDER(new BigInteger(numberNode.getNodeValue()));
                }
            }
        }
    }

    /**
     * Prepares a mets FileType element for a file
     *
     * @param seq
     * @param metsStreamName
     * @return
     */
    private FileType prepareFileType(int seq, String metsStreamName, HashMap<String, Object> fileNames, HashMap<String, String> mimeTypes, MetsContext metsContext) throws MetsExportException {
        String streamName = Const.streamMapping.get(metsStreamName);
        FileType fileType = new FileType();
        fileType.setCHECKSUMTYPE("MD5");
        fileType.setSEQ(seq);
        fileType.setMIMETYPE(mimeTypes.get(streamName));
        InputStream is = null;
        fileType.setID(Const.streamMappingPrefix.get(metsStreamName) + "_" + MetsUtils.removeNonAlpabetChars(metsContext.getPackageID()) + "_" + String.format("%04d", seq));
        if (fileNames.get(streamName) instanceof String) {
            String fileNameOriginal = (String) fileNames.get(streamName);
            int lastIndex = fileNameOriginal.lastIndexOf(File.separator);
            int preLastIndex = fileNameOriginal.substring(1, lastIndex).lastIndexOf(File.separator);
            String fileName = metsContext.getPath() + fileNameOriginal.substring(preLastIndex + 2);
            File file = new File(fileName);
            try {
                is = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                throw new MetsExportException("File not found:" + fileName, false, e);
            }
        }
        if (fileNames.get(streamName) instanceof byte[]) {
            byte[] bytes = (byte[]) fileNames.get(streamName);
            is = new ByteArrayInputStream(bytes);
        }
        if (fileNames.get(streamName) instanceof InputStream) {
            is = (InputStream) fileNames.get(streamName);
        }
        String outputFileName = fileType.getID() + "." + MimeType.getExtension(mimeTypes.get(streamName));
        String fullOutputFileName = metsContext.getOutputPath() + File.separator + Const.streamMappingFile.get(metsStreamName) + File.separator + outputFileName;
        outputFileNames.put(metsStreamName, fullOutputFileName);
        try {
            FileMD5Info fileMD5Info = MetsUtils.getDigestAndCopy(is, new FileOutputStream(fullOutputFileName));
            fileType.setSIZE(Long.valueOf(fileMD5Info.getSize()));
            fileMD5Info.setFileName("." + File.separator + Const.streamMappingFile.get(metsStreamName) + File.separator + outputFileName);
            fileType.setCHECKSUM(fileMD5Info.getMd5());
            metsContext.getFileList().add(fileMD5Info);
        } catch (Exception e) {
            throw new MetsExportException("Unable to process file " + fullOutputFileName, false, e);
        }
        FLocat flocat = new FLocat();
        flocat.setLOCTYPE("URL");
        flocat.setHref("." + File.separator + Const.streamMappingFile.get(metsStreamName) + File.separator + outputFileName);
        fileType.getFLocat().add(flocat);
        return fileType;
    }

    /**
     * Reads files/streams for each stream and puts it into the map (fileNames)
     *
     * @param metsElement
     * @param seq
     * @param fileNames
     * @param mimeTypes
     * @throws MetsExportException
     */
    private void processPageFiles(IMetsElement metsElement, int seq, HashMap<String, Object> fileNames, HashMap<String, String> mimeTypes) throws MetsExportException {
        for (String streamName : Const.streamMapping.values()) {
            if (metsElement.getMetsContext().getFedoraClient() != null) {
                try {
                    GetDatastreamsResponse streams = FedoraClient.getDatastreams(metsElement.getOriginalPid()).execute(metsElement.getMetsContext().getFedoraClient());
                    List<DatastreamProfile> profiles = streams.getDatastreamProfiles();
                    for (DatastreamProfile profile : profiles) {
                        if (profile.getDsID().contains(streamName)) {
                            GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), streamName);
                            try {
                                InputStream is = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                                fileNames.put(streamName, is);
                            } catch (FedoraClientException e) {
                                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to read raw datastream content", false, e);
                            }
                            mimeTypes.put(streamName, profile.getDsMIME());
                        }
                    }
                } catch (Exception ex) {
                    throw new MetsExportException(metsElement.getOriginalPid(), "Error while getting file datastreams for " + metsElement.getOriginalPid(), false, ex);
                }
            } else {
                List<DatastreamType> datastreams = metsElement.getSourceObject().getDatastream();
                for (DatastreamType ds : datastreams) {
                    if (MetsUtils.equalDataStreams(ds.getID(), streamName)) {
                        Iterator<DatastreamVersionType> dvIter = ds.getDatastreamVersion().iterator();
                        while (dvIter.hasNext()) {
                            DatastreamVersionType dv = dvIter.next();
                            mimeTypes.put(streamName, dv.getMIMETYPE());
                            if (dv.getContentLocation() != null) {
                                fileNames.put(streamName, dv.getContentLocation().getREF());
                            }
                            if (dv.getBinaryContent() != null) {
                                fileNames.put(streamName, dv.getBinaryContent());
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Generates technical metadata using JHOVE
     *
     * @param metsElement
     * @param fileNames
     * @param seq
     * @param fileTypes
     * @param mimeTypes
     * @param pageDiv
     * @throws MetsExportException
     */
    private void generateTechMetadata(IMetsElement metsElement, HashMap<String, Object> fileNames, int seq, List<FileType> fileTypes, HashMap<String, String> mimeTypes, DivType pageDiv) throws MetsExportException {
        if (fileNames.get(Const.streamMapping.get("TECHMDGRP")) == null) {
            LOG.log(Level.FINE, "Generating tech");
            Mets amdSecMets = new Mets();
            StructMapType mapType = new StructMapType();
            mapType.setID(Const.DIV_PHYSICAL_ID);
            amdSecMets.getStructMap().add(mapType);
            AmdSecType amdSec = new AmdSecType();
            amdSecMets.getAmdSec().add(amdSec);
            DivType divType = new DivType();
            if (Const.PERIODICAL_TITLE.equalsIgnoreCase(metsElement.getMetsContext().getRootElement().getElementType())) {
                divType.setTYPE("PERIODICAL_PAGE");
            } else {
                divType.setTYPE("MONOGRAPH_PAGE");
            }

            for (String name : Const.streamMapping.keySet()) {
                if (("ALTOGRP".equalsIgnoreCase(name)) || ("TXTGRP".equalsIgnoreCase(name))) {
                    continue;
                }
                if (fileNames.get(Const.streamMapping.get(name)) != null) {
                    String outputFileName = outputFileNames.get(name);
                    MdSecType mdSec = new MdSecType();
                    mdSec.setID("MIX_" + String.format("%03d", seq));
                    MdWrap mdWrap = new MdWrap();
                    mdWrap.setMIMETYPE("text/xml");
                    mdWrap.setMDTYPE("NISOIMG");
                    XmlData xmlData = new XmlData();
                    Node mixNode = JhoveUtility.getMixNode(new File(outputFileName), metsElement.getMetsContext());
                    mdWrap.setXmlData(xmlData);
                    xmlData.getAny().add(mixNode);
                    mdSec.setMdWrap(mdWrap);
                    amdSec.getTechMD().add(mdSec);
                }
                FileSec fileSec = new FileSec();
                amdSecMets.setFileSec(fileSec);

                for (String fileMap : fileGrpMap.keySet()) {
                    fileSec.getFileGrp().add(fileGrpMap.get(fileMap));
                }
            }

            for (FileType fileType : fileTypes) {
                Fptr fptr = new Fptr();
                fptr.setFILEID(fileType);
                divType.getFptr().add(fptr);
            }

            mapType.setDiv(divType);
            saveAmdSec(metsElement, amdSecMets, fileNames, mimeTypes);
            FileType fileType = prepareFileType(seq, "TECHMDGRP", fileNames, mimeTypes, metsElement.getMetsContext());
            fileTypes.add(fileType);
            this.fileGrpMap.get("TECHMDGRP").getFile().add(fileType);
            Fptr fptr = new Fptr();
            fptr.setFILEID(fileType);
            pageDiv.getFptr().add(fptr);
        }
    }

    /**
     *
     * Parses an ALTO stream and returns a list of internal elements
     *
     * @param document
     * @return
     */
    private List<IntPartInfo> parseAltoInfo(Document document) {
        List<IntPartInfo> intPartInfoList = new ArrayList<IntPartInfo>();
        Node partElement = document.getFirstChild();
        NodeList partsList = partElement.getChildNodes();
        for (int a = 0; a < partsList.getLength(); a++) {
            Node node = partsList.item(a);
            if ((node instanceof Element) && (node.hasAttributes())) {
                String type = node.getAttributes().getNamedItem("type").getNodeValue();
                String alto = node.getAttributes().getNamedItem("alto").getNodeValue();
                String begin = node.getAttributes().getNamedItem("begin").getNodeValue();
                String order = node.getAttributes().getNamedItem("order").getNodeValue();
                IntPartInfo info = new IntPartInfo(type, alto.substring(0, alto.indexOf("/")), begin, order);
                intPartInfoList.add(info);
            }
        }
        return intPartInfoList;
    }

    /**
     *
     * Fills the "isOnPage" structure - currently not used, prepared for future
     * release
     *
     * @param metsElement
     * @throws MetsExportException
     */
    @SuppressWarnings("unused")
    private void fillIsOnPage(IMetsElement metsElement) throws MetsExportException {
        Node node = MetsUtils.xPathEvaluateNode(metsElement.getRelsExt(), "*[local-name()='RDF']/*[local-name()='Description']");
        NodeList hasPageNodes = node.getChildNodes();
        for (int a = 0; a < hasPageNodes.getLength(); a++) {
            if (hasPageNodes.item(a).getNodeName().equalsIgnoreCase(Const.ISONPAGE)) {
                String fileName = hasPageNodes.item(a).getAttributes().getNamedItem("rdf:resource").getNodeValue();
                IMetsElement page = metsElement.getMetsContext().getPidElements().get(fileName.substring(fileName.indexOf(File.separator) + 1));
                SmLink smLink = new SmLink();
                smLink.setFrom(metsElement.getElementID());
                smLink.setTo(page.getElementID());
                if (mets.getStructLink() == null) {
                    mets.setStructLink(new MetsType.StructLink());
                }
                mets.getStructLink().getSmLinkOrSmLinkGrp().add(smLink);
            }
        }
    }

    /**
     *
     * saves technical metadata
     *
     * @param amdSecMets
     */
    private void saveAmdSec(IMetsElement metsElement, Mets amdSecMets, HashMap<String, Object> fileNames, HashMap<String, String> mimeTypes) throws MetsExportException {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class);
            Marshaller marshaller = jaxbContext.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
            marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, "http://www.w3.org/2001/XMLSchema-instance http://www.w3.org/2001/XMLSchema.xsd http://www.loc.gov/METS/ http://www.loc.gov/standards/mets/mets.xsd http://www.loc.gov/MIX/ http://www.loc.gov/mix/v20");
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            marshaller.marshal(amdSecMets, bos);
            byte[] byteArray = bos.toByteArray();
            fileNames.put("FULL_AMD", byteArray);
            mimeTypes.put("FULL_AMD", "text/xml");
            Document document = MetsUtils.getDocumentFromBytes(byteArray);
            MetsUtils.validateAgainstXSD(document, Mets.class.getResourceAsStream("mets.xsd"));
        } catch (Exception ex) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while saving AMDSec file", false, ex);
        }
    }

    /**
     * Inserts Issue structure to the mets
     *
     * @param physicalDiv
     * @param logicalDiv
     * @param metsElement
     * @throws MetsExportException
     */
    private void insertIssue(DivType physicalDiv, DivType logicalDiv, IMetsElement metsElement) throws MetsExportException {
        addDmdSec(metsElement);
        DivType divType = new DivType();
        divType.setID(metsElement.getElementID());
        divType.setLabel(metsElement.getMetsContext().getRootElement().getLabel());
        divType.setTYPE(metsElement.getElementType());
        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);

        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement);
                pageCounter++;
            }
        }
    }

    /**
     * Inserts Page structure to the mets
     *
     * @param physicalDiv
     * @param metsElement
     * @param pageCounter
     * @param sourceElement
     * @throws MetsExportException
     */
    private void insertPage(DivType physicalDiv, IMetsElement metsElement, int pageCounter, IMetsElement sourceElement) throws MetsExportException {
        outputFileNames.clear();
        if (!Const.PAGE.equals(metsElement.getElementType()) && !Const.MONOGRAPH_UNIT.equals(metsElement.getElementType())) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Expected page, got " + metsElement.getElementType(), false, null);
        }
        List<FileType> fileTypes = new ArrayList<FileType>();
        DivType pageDiv = new DivType();
        physicalDiv.getDiv().add(pageDiv);
        fillPageIndexOrder(metsElement, pageDiv);
        String ID = "DIV_P_PAGE_" + metsElement.getElementID().replace("PAGE_", "");
        pageDiv.setID(ID);
        HashMap<String, Object> fileNames = new HashMap<String, Object>();
        HashMap<String, String> mimeTypes = new HashMap<String, String>();
        processPageFiles(metsElement, pageCounter, fileNames, mimeTypes);
        for (String streamName : Const.streamMapping.keySet()) {
            if (fileNames.containsKey(Const.streamMapping.get(streamName))) {
            FileType fileType = prepareFileType(pageCounter, streamName, fileNames, mimeTypes, metsElement.getMetsContext());
            fileTypes.add(fileType);
            fileGrpMap.get(streamName).getFile().add(fileType);
            Fptr fptr = new Fptr();
            fptr.setFILEID(fileType);
            pageDiv.getFptr().add(fptr);
            if ("ALTOGRP".equals(streamName)) {
                metsElement.setAltoFile(fileType);
            }
            }
        }
        generateTechMetadata(metsElement, fileNames, pageCounter, fileTypes, mimeTypes, pageDiv);
        StructLink structLink = mets.getStructLink();
        if (structLink == null) {
            structLink = new StructLink();
            mets.setStructLink(structLink);
        }
        SmLink smLink = new SmLink();
        smLink.setFrom(sourceElement.getModsElementID());
        smLink.setTo(ID);
        structLink.getSmLinkOrSmLinkGrp().add(smLink);
    }

    /**
     * Inserts Volume structure to the mets
     *
     * @param logicalDiv
     * @param physicalDiv
     * @param metsElement
     * @param volumeCounter
     * @param isMultiPartMonograph
     * @throws MetsExportException
     */
    private void insertVolume(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement, int volumeCounter, boolean isMultiPartMonograph) throws MetsExportException {
        addDmdSec(metsElement);
        DivType divType = new DivType();
        divType.setID(metsElement.getElementID());
        // Label for volume is inherited from the parent monograph
        divType.setLabel(metsElement.getMetsContext().getRootElement().getLabel());
        divType.setTYPE(metsElement.getElementType());

        if (Const.MONOGRAPH.equals(metsElement.getElementType())) {
            if (isMultiPartMonograph) {
                divType.setTYPE(Const.MONOGRAPH);
            } else {
                divType.setTYPE(Const.VOLUME);
            }
        }

        if (Const.MONOGRAPH_UNIT.equals(metsElement.getElementType())) {
            divType.setTYPE("VOLUME");
            divType.setID(metsElement.getElementID().replaceAll(Const.MONOGRAPH_UNIT, Const.VOLUME));
        }

        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);
        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.ISSUE.equals(element.getElementType())) {
                insertIssue(physicalDiv, divType, element);
                continue;
            }
            if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement);
                pageCounter++;
                continue;
            }
        }
    }

    /**
     * Inserts Monograph structure to the mets
     *
     * @param metsElement
     * @throws MetsExportException
     */
    private void insertMonograph(IMetsElement metsElement) throws MetsExportException {
        mets.setTYPE("monograph");
        DivType logicalDiv = new DivType();
        logicalStruct.setDiv(logicalDiv);
        DivType physicalDiv = new DivType();
        physicalStruct.setDiv(physicalDiv);

        boolean containsUnit = false;
        for (IMetsElement childMetsElement : metsElement.getChildren()) {
            if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                containsUnit = true;
            }
        }
        logicalDiv.setLabel(metsElement.getLabel());
        logicalDiv.setTYPE("MONOGRAPH");
        logicalDiv.setID("MONOGRAPH_0001");
        if (!containsUnit) {
            insertVolume(logicalDiv, physicalDiv, metsElement, 1, false);
        } else {
            metsElement.setModsElementID("TITLE_0001");
            addDmdSec(metsElement);
            int volumeCounter = 0;
            logicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            for (IMetsElement childMetsElement : metsElement.getChildren()) {
                if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                    volumeCounter++;
                    insertVolume(logicalDiv, physicalDiv, childMetsElement, volumeCounter, true);
                }
                if (Const.PAGE.equals(childMetsElement.getElementType())) {
                    pageCounter++;
                    insertPage(physicalDiv, childMetsElement, pageCounter, metsElement);
                }
            }
        }
    }

    /**
     *
     * Adds the internal elements into the mets div
     *
     * @param parentType
     */
    private void addInternalElements(DivType parentType, IMetsElement metsElement) throws MetsExportException {
        byte[] structStream;
        if (metsElement.getMetsContext().getFedoraClient() != null) {
            structStream = MetsUtils.getBinaryDataStreams(metsElement.getMetsContext().getFedoraClient(), metsElement.getOriginalPid(), "STRUCT_MAP");
        } else {
            structStream = MetsUtils.getBinaryDataStreams(metsElement.getSourceObject().getDatastream(), "STRUCT_MAP");
        }
        List<IntPartInfo> partInfoList = parseAltoInfo(MetsUtils.getDocumentFromBytes(structStream));
        for (IntPartInfo partInfo : partInfoList) {
            DivType divType = new DivType();
            divType.setTYPE(partInfo.getType());
            try {
                divType.setORDER(new BigInteger(partInfo.getOrder()));
            } catch (NumberFormatException ex) {
                LOG.log(Level.WARNING, partInfo.getOrder() + " is not a number in  object " + metsElement.getOriginalPid(), ex);
            }
            String number = String.format("%04d", metsElement.getMetsContext().addElementId(metsElement.getElementType()));

            /**
             * if an internal element is part of article, then the ID is
             * inherited
             */
            if ("ARTICLE".equalsIgnoreCase(metsElement.getParent().getElementType())) {
                divType.setID(metsElement.getParent().getElementID() + "_" + number);
            } else {
                divType.setID(metsElement + "_" + number);
            }
            Fptr fptr = new Fptr();
            AreaType area = new AreaType();
            IMetsElement refPage = metsElement.getMetsContext().getPidElements().get(partInfo.getAltoPID());
            area.setFILEID(refPage.getAltoFile());
            area.setBEGIN(partInfo.getBegin());
            area.setBETYPE("IDREF");
            fptr.setArea(area);
            divType.getFptr().add(fptr);
            parentType.getDiv().add(divType);
        }
    }


    /**
     * Inserts Periodical structure to the mets
     *
     * @param metsElement
     * @throws MetsExportException
     */
    private void insertPeriodical(IMetsElement metsElement) throws MetsExportException {
        mets.setTYPE("periodical");
        addDmdSec(metsElement);
        DivType divType = new DivType();
        logicalStruct.setDiv(divType);
        DivType physicalDiv = new DivType();
        physicalStruct.setDiv(physicalDiv);

        divType.setID(metsElement.getElementID());
        // Label for volume is inherited from the parent monograph
        divType.setLabel(metsElement.getMetsContext().getRootElement().getLabel());
        divType.setTYPE(metsElement.getElementType());
        divType.getDMDID().add(metsElement.getModsMetsElement());

        DivType phyDivType = new DivType();
        physicalDiv.getDiv().add(phyDivType);
        phyDivType.setID("DIV_P_0000");
        phyDivType.setTYPE("periodical");

        int volumeCounter = 0;
         for (IMetsElement childMetsElement : metsElement.getChildren()) {
            insertVolume(divType, physicalDiv, childMetsElement, volumeCounter, false);
            volumeCounter++;
         }
    }

    /**
     * Inserts Internal element structure into mets - currently unused, prepared
     * for future release
     *
     * @param logicalDiv
     * @param physicalDiv
     * @param metsElement
     * @param counterIntPart
     * @throws MetsExportException
     */
    @SuppressWarnings("unused")
    private void insertInternalElement(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement, int counterIntPart) throws MetsExportException {
        addDmdSec(metsElement);
        DivType elementDivType = new DivType();
        if (Const.ARTICLE.equalsIgnoreCase(metsElement.getParent().getElementType())) {
            int seq = metsElement.getMetsContext().addElementId(metsElement.getParent().getElementType());
            // TODO elementDivType.setORDER
            // BigInteger(partInfo.getOrder()));
            String number = String.format("%04d", seq);
            elementDivType.setID(metsElement.getParent().getElementID() + "_" + number);
        } else {
            elementDivType.setID(metsElement.getElementID());
            elementDivType.setORDER(BigInteger.valueOf(counterIntPart));
        }

        elementDivType.setLabel(metsElement.getLabel());
        elementDivType.setTYPE(metsElement.getElementType());
        elementDivType.getDMDID().add(metsElement.getModsMetsElement());

        logicalDiv.getDiv().add(elementDivType);
        addInternalElements(elementDivType, metsElement);
    }

    /*
     * (non-Javadoc)
     *
     * @see cz.cas.lib.proarc.common.export.mets.structure.IMetsElementVisitor#
     * insertIntoMets
     * (cz.cas.lib.proarc.common.export.mets.structure.IMetsElement)
     */
    @Override
    public void insertIntoMets(IMetsElement metsElement) throws MetsExportException {
        try {
            mets = prepareMets(metsElement);
            initHeader(metsElement);
            createDirectoryStructure(metsElement.getMetsContext());
            LOG.log(Level.FINE, "Inserting into Mets:" + metsElement.getOriginalPid() + "(" + metsElement.getElementType() + ")");
            // get root element first
            IMetsElement rootElement = metsElement.getMetsContext().getRootElement();
            if (rootElement == null) {
                throw new MetsExportException("Element does not have a root set:" + metsElement.getModel() + " - " + metsElement.getOriginalPid(), false);
            }
            if (Const.PERIODICAL_TITLE.equalsIgnoreCase(rootElement.getElementType())) {
                insertPeriodical(rootElement);
            } else if (Const.MONOGRAPH.equalsIgnoreCase(rootElement.getElementType())) {
                insertMonograph(rootElement);
            } else
                throw new MetsExportException(rootElement.getOriginalPid(), "Unknown type:" + rootElement.getElementType() + " model:" + rootElement.getModel(), false, null);
            addFileGrpToMets(fileGrpMap, mets.getFileSec());
            saveMets(mets, new File(metsElement.getMetsContext().getOutputPath() + File.separator + "METS_" + MetsUtils.removeNonAlpabetChars(metsElement.getMetsContext().getPackageID()) + ".xml"), metsElement);
        } finally {
            if (metsElement.getMetsContext().jhoveConfig != null) {
                JhoveUtility.destroyConfigFiles(metsElement.getMetsContext().jhoveConfig);
            }
        }
    }
}
