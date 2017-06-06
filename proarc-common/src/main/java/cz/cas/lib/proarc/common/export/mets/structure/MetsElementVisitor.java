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
import java.io.StringWriter;
import java.math.BigInteger;
import java.net.URI;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.apache.commons.codec.binary.Hex;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;

import cz.cas.lib.proarc.common.device.Device;
import cz.cas.lib.proarc.common.device.DeviceException;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.JHoveOutput;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.MimeType;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.MixEditor;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
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
import cz.cas.lib.proarc.mets.MetsType.MetsHdr.Agent;
import cz.cas.lib.proarc.mets.MetsType.StructLink;
import cz.cas.lib.proarc.mets.StructLinkType.SmLink;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mix.BasicImageInformationType.BasicImageCharacteristics.PhotometricInterpretation;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.premis.AgentComplexType;
import cz.cas.lib.proarc.premis.AgentIdentifierComplexType;
import cz.cas.lib.proarc.premis.CreatingApplicationComplexType;
import cz.cas.lib.proarc.premis.EventComplexType;
import cz.cas.lib.proarc.premis.EventIdentifierComplexType;
import cz.cas.lib.proarc.premis.EventOutcomeInformationComplexType;
import cz.cas.lib.proarc.premis.FixityComplexType;
import cz.cas.lib.proarc.premis.FormatComplexType;
import cz.cas.lib.proarc.premis.FormatDesignationComplexType;
import cz.cas.lib.proarc.premis.FormatRegistryComplexType;
import cz.cas.lib.proarc.premis.LinkingAgentIdentifierComplexType;
import cz.cas.lib.proarc.premis.LinkingEventIdentifierComplexType;
import cz.cas.lib.proarc.premis.LinkingObjectIdentifierComplexType;
import cz.cas.lib.proarc.premis.ObjectCharacteristicsComplexType;
import cz.cas.lib.proarc.premis.ObjectFactory;
import cz.cas.lib.proarc.premis.ObjectIdentifierComplexType;
import cz.cas.lib.proarc.premis.OriginalNameComplexType;
import cz.cas.lib.proarc.premis.PremisComplexType;
import cz.cas.lib.proarc.premis.PreservationLevelComplexType;
import cz.cas.lib.proarc.premis.RelatedEventIdentificationComplexType;
import cz.cas.lib.proarc.premis.RelatedObjectIdentificationComplexType;
import cz.cas.lib.proarc.premis.RelationshipComplexType;

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
    private final Map<StructLinkMapping, String> pageOrderToDivMap = new HashMap<StructLinkMapping, String>();
    private final Map<String, List<StructLinkMapping>> structToPageMap = new HashMap<String, List<StructLinkMapping>>();
    int pageCounter = 0;
    int articleCounter = 0;
    int chapterCounter = 0;

    /**
     * creates directory structure for mets elements
     */

    private void createDirectoryStructure(MetsContext metsContext) {
        for (String directory : Const.streamMappingFile.values()) {
            File file = new File(metsContext.getOutputPath() + File.separator + metsContext.getPackageID() + File.separator + directory);
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
    protected void initHeader(IMetsElement metsElement) throws MetsExportException {
        mets.setLabel1(metsElement.getLabel());
        MetsHdr metsHdr = new MetsHdr();
        metsHdr.setCREATEDATE(metsElement.getCreateDate());
        metsHdr.setLASTMODDATE(metsElement.getLastUpdateDate());

        setAgent(metsHdr, "CREATOR", "ORGANIZATION", metsElement.getMetsContext().getOptions().getCreator());
        setAgent(metsHdr, "ARCHIVIST", "ORGANIZATION",metsElement.getMetsContext().getOptions().getArchivist());

        mets.setMetsHdr(metsHdr);
        fileGrpMap = MetsUtils.initFileGroups();
    }

    /**
     * Agent setting - used in metsHeader
     *
     * @throws MetsExportException if Archivist/Creator in proarc.cfg is empty
     */
    private void setAgent(MetsHdr metsHdr, String role, String type, String name) throws MetsExportException {

        if (name == null) {
            throw new MetsExportException("Error - missing role. Please insert value in proarc.cfg into export.ndk.agent.creator and export.ndk.agent.archivist", false);
        } else {

            Agent agent = new Agent();
            agent.setName(name);
            agent.setROLE(role);
            agent.setTYPE(type);
            metsHdr.getAgent().add(agent);
        }
    }

    /** Prepares the generic mets information */
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
            addFileGrpToMets(fileGrpMap);
            addStructLink();
            try {
                JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class, OaiDcType.class, ModsDefinition.class);
                Marshaller marshaller = jaxbContext.createMarshaller();
                marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
                marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
                // marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
                // "http://www.w3.org/2001/XMLSchema-instance http://www.w3.org/2001/XMLSchema.xsd http://www.loc.gov/METS/ http://www.loc.gov/standards/mets/mets.xsd http://www.loc.gov/mods/v3 http://www.loc.gov/standards/mods/mods.xsd http://www.openarchives.org/OAI/2.0/oai_dc/ http://www.openarchives.org/OAI/2.0/oai_dc.xsd");
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
                long totalBytes = 0;
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
                metsElement.getMetsContext().getFileList().add(new FileMD5Info("." + File.separator + outputFile.getName(), result, totalBytes));
                fileMd5Name = "MD5_" + MetsUtils.removeNonAlpabetChars(metsElement.getMetsContext().getPackageID()) + ".md5";
                File fileMd5 = new File(metsElement.getMetsContext().getOutputPath() + File.separator + metsElement.getMetsContext().getPackageID() + File.separator + fileMd5Name);
                OutputStreamWriter osw = new OutputStreamWriter(new FileOutputStream(fileMd5));
                for (FileMD5Info info : metsElement.getMetsContext().getFileList()) {
                    osw.write(info.getMd5() + " " + info.getFileName() + "\n");
                }
                osw.close();
                is.close();

                // calculate md5 for md5file - it's inserted into info.xml
                is = new FileInputStream(fileMd5);
                FileMD5Info md5InfoMd5File = MetsUtils.getDigest(is);
                is.close();
                metsElement.getMetsContext().getFileList().add(new FileMD5Info("." + File.separator + fileMd5Name, null, fileMd5.length()));
                MetsUtils.saveInfoFile(metsElement.getMetsContext().getOutputPath(), metsElement.getMetsContext(), md5InfoMd5File.getMd5(), fileMd5Name, outputFile);
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
                    LOG.fine(error);
                }
                throw metsException;
            }
            LOG.log(Level.FINE, "Element validated:" + metsElement.getOriginalPid() + "(" + metsElement.getElementType() + ")");
        } finally {
            JhoveUtility.destroyConfigFiles(metsElement.getMetsContext().getJhoveContext());
        }
        metsElement.getMetsContext().getGeneratedPSP().add(metsElement.getMetsContext().getPackageID());
    }

    /**
     * Adds all non-empty filegroups to the mets
     *
     * @param fileGrpMap
     * @param fileSec
     */
    private void addFileGrpToMets(Map<String, FileGrp> fileGrpMap) {
        for (String key : fileGrpMap.keySet()) {
            FileGrp fileGrp = fileGrpMap.get(key);
            if (fileGrp.getFile().size() > 0) {
                if (mets.getFileSec() == null) {
                    mets.setFileSec(new FileSec());
                }
                mets.getFileSec().getFileGrp().add(fileGrp);
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
            metsElement.getModsStream().get(0).setAttribute("ID", "MODS_" + metsElement.getModsElementID());
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
        if ((partNode.getAttributes() != null) && (partNode.getAttributes().getNamedItem("type") != null)) {
            pageDiv.setTYPE(partNode.getAttributes().getNamedItem("type").getNodeValue());
        } else {
            pageDiv.setTYPE("NormalPage");
        }
        NodeList nodeList = partNode.getChildNodes();
        for (int a = 0; a < nodeList.getLength(); a++) {
            if ((nodeList.item(a).getLocalName() != null) && (nodeList.item(a).getLocalName().equalsIgnoreCase("detail"))) {
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

    private InputStream addLabelToAmdSec(InputStream is, MetsContext metsContext) throws MetsExportException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try {
            MetsUtils.copyStream(is, bos);
            bos.close();
        } catch (IOException ex) {
            throw new MetsExportException("Unable to copy stream", false, ex);
        }
        Document TECHDoc = MetsUtils.getDocumentFromBytes(bos.toByteArray());
        Element element = (Element) TECHDoc.getFirstChild();
        element.setAttribute("LABEL", mets.getLabel1());
        element.setAttribute("TYPE", mets.getTYPE());
        DOMSource domSource = new DOMSource(TECHDoc);
        StringWriter xmlAsWriter = new StringWriter();
        StreamResult result = new StreamResult(xmlAsWriter);
        try {
            TransformerFactory.newInstance().newTransformer().transform(domSource, result);
            InputStream resultIS = new ByteArrayInputStream(xmlAsWriter.toString().getBytes("UTF-8"));
            is.close();
            return resultIS;
        } catch (Exception ex) {
            throw new MetsExportException("Unable to transform Tech metadata to XML", false, ex);
        }
    }

    /**
     * Prepares a mets FileType element for a file
     *
     * @param seq
     * @param metsStreamName
     * @return
     */
    private FileType prepareFileType(int seq, String metsStreamName, HashMap<String, Object> fileNames, HashMap<String, String> mimeTypes, MetsContext metsContext, HashMap<String, String> outputFileNames, HashMap<String, FileMD5Info> md5InfosMap) throws MetsExportException {
        // String streamName = Const.streamMapping.get(metsStreamName);
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
        fileType.setSEQ(seq);
        fileType.setMIMETYPE(mimeTypes.get(metsStreamName));
        InputStream is = null;
        fileType.setID(Const.streamMappingPrefix.get(metsStreamName) + "_" + MetsUtils.removeNonAlpabetChars(metsContext.getPackageID()) + "_" + String.format("%04d", seq));
        if (fileNames.get(metsStreamName) instanceof String) {
            String fileNameOriginal = (String) fileNames.get(metsStreamName);
            int lastIndex = fileNameOriginal.lastIndexOf('/');
            int preLastIndex = fileNameOriginal.substring(1, lastIndex).lastIndexOf('/');
            String fileName = metsContext.getPath() + fileNameOriginal.substring(preLastIndex + 2);
            File file = new File(fileName);
            try {
                is = new FileInputStream(file);

            } catch (FileNotFoundException e) {
                throw new MetsExportException("File not found:" + fileName, false, e);
            }
        }
        if (fileNames.get(metsStreamName) instanceof byte[]) {
            byte[] bytes = (byte[]) fileNames.get(metsStreamName);
            is = new ByteArrayInputStream(bytes);
        }
        if (fileNames.get(metsStreamName) instanceof InputStream) {
            is = (InputStream) fileNames.get(metsStreamName);
        }

        if (metsStreamName.equalsIgnoreCase("TECHMDGRP")) {
            is = addLabelToAmdSec(is, metsContext);
        }

        String outputFileName = fileType.getID() + "." + MimeType.getExtension(mimeTypes.get(metsStreamName));
        String fullOutputFileName = metsContext.getPackageDir().getAbsolutePath() + File.separator + Const.streamMappingFile.get(metsStreamName) + File.separator + outputFileName;
        outputFileNames.put(metsStreamName, fullOutputFileName);
        try {
            FileMD5Info fileMD5Info;
            if (md5InfosMap.get(metsStreamName) == null) {
                fileMD5Info = MetsUtils.getDigestAndCopy(is, new FileOutputStream(fullOutputFileName));
                md5InfosMap.put(metsStreamName, fileMD5Info);
            } else {
                FileMD5Info tempMd5 = MetsUtils.getDigestAndCopy(is, new FileOutputStream(fullOutputFileName));
                fileMD5Info = md5InfosMap.get(metsStreamName);
                fileMD5Info.setSize(tempMd5.getSize());
                fileMD5Info.setMd5(tempMd5.getMd5());
            }
            fileType.setSIZE(Long.valueOf(fileMD5Info.getSize()));
            fileMD5Info.setFileName("." + File.separator + Const.streamMappingFile.get(metsStreamName) + File.separator + outputFileName);
            fileMD5Info.setMimeType(fileType.getMIMETYPE());
            fileType.setCHECKSUM(fileMD5Info.getMd5());
            metsContext.getFileList().add(fileMD5Info);
        } catch (Exception e) {
            throw new MetsExportException("Unable to process file " + fullOutputFileName, false, e);
        }
        FLocat flocat = new FLocat();
        flocat.setLOCTYPE("URL");
        String href = "." + "/" + Const.streamMappingFile.get(metsStreamName) + "/" + outputFileName;
        URI uri;
        uri = URI.create(href);
        flocat.setHref(uri.toASCIIString());
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
    private void processPageFiles(IMetsElement metsElement, int seq, HashMap<String, Object> fileNames, HashMap<String, String> mimeTypes, HashMap<String, XMLGregorianCalendar> createDates, HashMap<String, FileMD5Info> md5InfosMap) throws MetsExportException {
        for (String streamName : Const.streamMapping.keySet()) {
            if (metsElement.getMetsContext().getFedoraClient() != null) {
                try {
                    // GetDatastreamsResponse streams =
                    // FedoraClient.getDatastreams(metsElement.getOriginalPid()).execute(metsElement.getMetsContext().getFedoraClient());
                    // List<DatastreamProfile> profiles =
                    // streams.getDatastreamProfiles();
                    // for (DatastreamProfile profile : profiles) {
                    // if (profile.getDsID().contains(streamName)) {
                    for (String dataStream : Const.streamMapping.get(streamName)) {
                        DatastreamType rawDS = FoxmlUtils.findDatastream(metsElement.getSourceObject(), dataStream);
                        if (rawDS != null) {
                            FileMD5Info fileMd5Info;
                            if (md5InfosMap.get(streamName) == null) {
                                fileMd5Info = new FileMD5Info();
                                md5InfosMap.put(streamName, fileMd5Info);
                            } else {
                                fileMd5Info = md5InfosMap.get(streamName);
                            }
                            fileMd5Info.setCreated(rawDS.getDatastreamVersion().get(0).getCREATED());

                            GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), dataStream);
                            createDates.put(streamName, rawDS.getDatastreamVersion().get(0).getCREATED());
                            try {
                                InputStream is = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                                fileNames.put(streamName, is);
                            } catch (FedoraClientException e) {
                                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to read raw datastream content", false, e);
                            }
                            // mimeTypes.put(streamName, profile.getDsMIME());
                            mimeTypes.put(streamName, rawDS.getDatastreamVersion().get(0).getMIMETYPE());
                            break;
                            //
                        }
                    }
                } catch (Exception ex) {
                    throw new MetsExportException(metsElement.getOriginalPid(), "Error while getting file datastreams for " + metsElement.getOriginalPid(), false, ex);
                }
            } else {
                List<DatastreamType> datastreams = metsElement.getSourceObject().getDatastream();
                for (String dataStream : Const.streamMapping.get(streamName)) {
                    if (fileNames.get(streamName) != null) {
                        break;
                    }
                    for (DatastreamType ds : datastreams) {
                        if (MetsUtils.equalDataStreams(ds.getID(), dataStream)) {
                            Iterator<DatastreamVersionType> dvIter = ds.getDatastreamVersion().iterator();
                            while (dvIter.hasNext()) {
                                DatastreamVersionType dv = dvIter.next();
                                mimeTypes.put(streamName, dv.getMIMETYPE());
                                if (dv.getContentLocation() != null) {
                                    fileNames.put(streamName, dv.getContentLocation().getREF());
                                    FileMD5Info fileMd5Info;
                                    if (md5InfosMap.get(streamName) == null) {
                                        fileMd5Info = new FileMD5Info();
                                        md5InfosMap.put(streamName, fileMd5Info);
                                    } else {
                                        fileMd5Info = md5InfosMap.get(streamName);
                                    }
                                    fileMd5Info.setCreated(dv.getCREATED());
                                }
                                if (dv.getBinaryContent() != null) {
                                    fileNames.put(streamName, dv.getBinaryContent());
                                    FileMD5Info fileMd5Info;
                                    if (md5InfosMap.get(streamName) == null) {
                                        fileMd5Info = new FileMD5Info();
                                        md5InfosMap.put(streamName, fileMd5Info);
                                    } else {
                                        fileMd5Info = md5InfosMap.get(streamName);
                                    }
                                    fileMd5Info.setCreated(dv.getCREATED());
                                }
                            }
                            break;
                        }
                    }
                }
            }
        }
    }


    /**
     * Returns the description of scanner
     *
     * @param metsElement
     * @return
     * @throws MetsExportException
     */
    private Mix getScannerMix(IMetsElement metsElement) throws MetsExportException {
       if (metsElement.getMetsContext().getRemoteStorage()!=null) {
           Node deviceNode = MetsUtils.xPathEvaluateNode(metsElement.getRelsExt(), "*[local-name()='RDF']/*[local-name()='Description']/*[local-name()='hasDevice']");
           if (deviceNode == null) {
               return null;
           }
           Node attrNode = deviceNode.getAttributes().getNamedItem("rdf:resource");
           if (attrNode==null) {
               return null;
           }
           DeviceRepository deviceRepository = new DeviceRepository(metsElement.getMetsContext().getRemoteStorage());
            String deviceId = attrNode.getNodeValue().replaceAll("info:fedora/", "");
            List<Device> deviceList;
            try {
                deviceList = deviceRepository.find(deviceId, true);
            } catch (DeviceException e) {
                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to get scanner info", false, e);
            }
            if (deviceList.size() != 1) {
                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to get scanner info - expected 1 device, got:" + deviceList.size(), false, null);
           }
            Device device = deviceList.get(0);
            if ((device.getDescription() == null) || (device.getDescription().getImageCaptureMetadata() == null)) {
                throw new MetsExportException(metsElement.getOriginalPid(), "Scanner device does not have the description/imageCaptureMetadata set", false, null);
            }
            Mix mix = device.getDescription();
            return mix;
       }
       return null;
   }


    private Node getAgent(IMetsElement metsElement) throws MetsExportException {
        AgentComplexType agent = new AgentComplexType();
        ObjectFactory factory = new ObjectFactory();
        JAXBElement<AgentComplexType> jaxbPremix = factory.createAgent(agent);
        AgentIdentifierComplexType agentIdentifier = new AgentIdentifierComplexType();
        agent.getAgentIdentifier().add(agentIdentifier);
        agentIdentifier.setAgentIdentifierType("ProArc_AgentID");
        agentIdentifier.setAgentIdentifierValue("ProArc");
        agent.setAgentType("software");
        agent.getAgentName().add("ProArc");

        JAXBContext jc;
        try {
            jc = JAXBContext.newInstance(AgentComplexType.class);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document document = db.newDocument();

            // Marshal the Object to a Document
            Marshaller marshaller = jc.createMarshaller();
            marshaller.marshal(jaxbPremix, document);
            XPath xpath = XPathFactory.newInstance().newXPath();
            Node agentNode = (Node) xpath.compile("*[local-name()='agent']").evaluate(document, XPathConstants.NODE);
            return agentNode;
        } catch (Exception e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
        }

    }

    private Node getPremisEvent(IMetsElement metsElement, String datastream, FileMD5Info md5Info, String eventDetail) throws MetsExportException {
        PremisComplexType premis = new PremisComplexType();
        ObjectFactory factory = new ObjectFactory();
        JAXBElement<PremisComplexType> jaxbPremix = factory.createPremis(premis);
        EventComplexType event = factory.createEventComplexType();
        premis.getEvent().add(event);
        event.setEventDateTime(md5Info.getCreated().toXMLFormat());
        event.setEventDetail(eventDetail);
        EventIdentifierComplexType eventIdentifier = new EventIdentifierComplexType();
        event.setEventIdentifier(eventIdentifier);
        event.setEventType("derivation");
        eventIdentifier.setEventIdentifierType("ProArc_EventID");
        eventIdentifier.setEventIdentifierValue(Const.dataStreamToEvent.get(datastream));
        EventOutcomeInformationComplexType eventInformation = new EventOutcomeInformationComplexType();
        event.getEventOutcomeInformation().add(eventInformation);
        eventInformation.getContent().add(factory.createEventOutcome("successful"));
        LinkingAgentIdentifierComplexType linkingAgentIdentifier = new LinkingAgentIdentifierComplexType();
        linkingAgentIdentifier.setLinkingAgentIdentifierType("ProArc_AgentID");
        linkingAgentIdentifier.setLinkingAgentIdentifierValue("ProArc");
        linkingAgentIdentifier.getLinkingAgentRole().add("software");
        LinkingObjectIdentifierComplexType linkingObject = new LinkingObjectIdentifierComplexType();
        linkingObject.setLinkingObjectIdentifierType("ProArc_URI");
        linkingObject.setLinkingObjectIdentifierValue(Const.FEDORAPREFIX + metsElement.getOriginalPid() + "/" + Const.dataStreamToModel.get(datastream));
        event.getLinkingObjectIdentifier().add(linkingObject);
        event.getLinkingAgentIdentifier().add(linkingAgentIdentifier);
        JAXBContext jc;
        try {
            jc = JAXBContext.newInstance(PremisComplexType.class);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document document = db.newDocument();

            // Marshal the Object to a Document
            Marshaller marshaller = jc.createMarshaller();
            marshaller.marshal(jaxbPremix, document);
            XPath xpath = XPathFactory.newInstance().newXPath();
            Node premisNode = (Node) xpath.compile("*[local-name()='premis']/*[local-name()='event']").evaluate(document, XPathConstants.NODE);
            return premisNode;
        } catch (Exception e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
        }
    }

    /**
     * Generates the premis for amdSec
     *
     * @param metsElement
     * @param datastream
     * @param md5Info
     * @param created
     * @param formatVersion
     * @return
     * @throws MetsExportException
     */
    private Node getPremisFile(IMetsElement metsElement, String datastream, FileMD5Info md5Info) throws MetsExportException {
        PremisComplexType premis = new PremisComplexType();
        ObjectFactory factory = new ObjectFactory();
        JAXBElement<PremisComplexType> jaxbPremix = factory.createPremis(premis);
        cz.cas.lib.proarc.premis.File file = factory.createFile();
        premis.getObject().add(file);
        ObjectIdentifierComplexType objectIdentifier = new ObjectIdentifierComplexType();
        objectIdentifier.setObjectIdentifierType("ProArc_URI");
        objectIdentifier.setObjectIdentifierValue(Const.FEDORAPREFIX + metsElement.getOriginalPid() + "/" + Const.dataStreamToModel.get(datastream));
        file.getObjectIdentifier().add(objectIdentifier);
        PreservationLevelComplexType preservation = new PreservationLevelComplexType();
        if ("RAW".equals(datastream)) {
            preservation.setPreservationLevelValue("deleted");
        } else {
            preservation.setPreservationLevelValue("preservation");
        }
        file.getPreservationLevel().add(preservation);
        ObjectCharacteristicsComplexType characteristics = new ObjectCharacteristicsComplexType();
        characteristics.setCompositionLevel(BigInteger.ZERO);
        file.getObjectCharacteristics().add(characteristics);
        FixityComplexType fixity = new FixityComplexType();
        fixity.setMessageDigest(md5Info.getMd5());
        fixity.setMessageDigestAlgorithm("MD5");
        fixity.setMessageDigestOriginator("ProArc");
        characteristics.getFixity().add(fixity);
        characteristics.setSize(md5Info.getSize());
        FormatComplexType format = new FormatComplexType();
        characteristics.getFormat().add(format);
        FormatDesignationComplexType formatDesignation = new FormatDesignationComplexType();
        formatDesignation.setFormatName(md5Info.getMimeType());
        formatDesignation.setFormatVersion(md5Info.getFormatVersion());
        JAXBElement<FormatDesignationComplexType> jaxbDesignation = factory.createFormatDesignation(formatDesignation);
        format.getContent().add(jaxbDesignation);
        FormatRegistryComplexType formatRegistry = new FormatRegistryComplexType();
        formatRegistry.setFormatRegistryName("PRONOM");
        formatRegistry.setFormatRegistryKey(Const.mimeToFmtMap.get(md5Info.getMimeType()));
        JAXBElement<FormatRegistryComplexType> jaxbRegistry = factory.createFormatRegistry(formatRegistry);
        format.getContent().add(jaxbRegistry);

        CreatingApplicationComplexType creatingApplication = new CreatingApplicationComplexType();
        characteristics.getCreatingApplication().add(creatingApplication);
        creatingApplication.getContent().add(factory.createCreatingApplicationName("ProArc"));

        creatingApplication.getContent().add(factory.createCreatingApplicationVersion(metsElement.getMetsContext().getProarcVersion()));
        creatingApplication.getContent().add(factory.createDateCreatedByApplication(MetsUtils.getCurrentDate().toXMLFormat()));

        RelationshipComplexType relationShip = new RelationshipComplexType();

        if (!("RAW").equals(datastream)) {
            relationShip.setRelationshipType("derivation");
            relationShip.setRelationshipSubType("created from");
            RelatedObjectIdentificationComplexType relatedObject = new RelatedObjectIdentificationComplexType();
            relationShip.getRelatedObjectIdentification().add(relatedObject);
            relatedObject.setRelatedObjectIdentifierType("ProArc_URI");
            relatedObject.setRelatedObjectIdentifierValue(Const.FEDORAPREFIX + metsElement.getOriginalPid() + "/" + Const.dataStreamToModel.get("RAW"));
            RelatedEventIdentificationComplexType eventObject = new RelatedEventIdentificationComplexType();
            relationShip.getRelatedEventIdentification().add(eventObject);
            eventObject.setRelatedEventIdentifierType("ProArc_EventID");
            eventObject.setRelatedEventIdentifierValue(Const.dataStreamToEvent.get(datastream));
            eventObject.setRelatedEventSequence(BigInteger.ONE);
            file.getRelationship().add(relationShip);
        } else {
            relationShip.setRelationshipType("creation");
            relationShip.setRelationshipSubType("created from");
            LinkingEventIdentifierComplexType eventIdentifier = new LinkingEventIdentifierComplexType();
            file.getLinkingEventIdentifier().add(eventIdentifier);
            eventIdentifier.setLinkingEventIdentifierType("ProArc_EventID");
            eventIdentifier.setLinkingEventIdentifierValue(Const.dataStreamToEvent.get(datastream));
        }

        String originalFile = MetsUtils.xPathEvaluateString(metsElement.getRelsExt(), "*[local-name()='RDF']/*[local-name()='Description']/*[local-name()='importFile']");
        OriginalNameComplexType originalName = factory.createOriginalNameComplexType();
        originalName.setValue(originalFile);
        file.setOriginalName(originalName);

        JAXBContext jc;
        try {
            jc = JAXBContext.newInstance(PremisComplexType.class);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document document = db.newDocument();

            // Marshal the Object to a Document
            Marshaller marshaller = jc.createMarshaller();
            marshaller.marshal(jaxbPremix, document);
            XPath xpath = XPathFactory.newInstance().newXPath();
            Node premisNode = (Node) xpath.compile("*[local-name()='premis']/*[local-name()='object']").evaluate(document, XPathConstants.NODE);
            return premisNode;
        } catch (Exception e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
        }
    }

    private void addPremisNodeToMets(Node premisNode, AmdSecType amdSec, String Id, boolean isDigiprov, HashMap<String, FileGrp> amdSecFileGrpMap) {
        MdSecType mdSec = new MdSecType();
        mdSec.setID(Id);
        MdWrap mdWrap = new MdWrap();
        mdWrap.setMIMETYPE("text/xml");
        mdWrap.setMDTYPE("PREMIS");
        XmlData xmlData = new XmlData();
        xmlData.getAny().add(premisNode);
        mdWrap.setXmlData(xmlData);
        mdSec.setMdWrap(mdWrap);
        if (isDigiprov) {
            amdSec.getDigiprovMD().add(mdSec);
        } else {
            amdSec.getTechMD().add(mdSec);
        }
        if ("OBJ_002".equals(Id) || ("EVT_002".equals(Id))) {
            if ((amdSecFileGrpMap.get("MC_IMGGRP") != null) && (amdSecFileGrpMap.get("MC_IMGGRP").getFile().get(0) != null)) {
                amdSecFileGrpMap.get("MC_IMGGRP").getFile().get(0).getADMID().add(mdSec);
            }
        }
        if ("OBJ_003".equals(Id) || ("EVT_003".equals(Id))) {
            if ((amdSecFileGrpMap.get("ALTOGRP") != null) && (amdSecFileGrpMap.get("ALTOGRP").getFile().get(0) != null)) {
                amdSecFileGrpMap.get("ALTOGRP").getFile().get(0).getADMID().add(mdSec);
            }
        }
    }

    private void addPremisToAmdSec(AmdSecType amdSec, HashMap<String, FileMD5Info> md5InfosMap, IMetsElement metsElement, HashMap<String, FileGrp> amdSecFileGrpMap) throws MetsExportException {
        HashMap<String, String> toGenerate = new HashMap<String, String>();
        toGenerate.put("OBJ_001", "RAW");
        toGenerate.put("OBJ_002", "MC_IMGGRP");
        toGenerate.put("OBJ_003", "ALTOGRP");

        for (String obj : toGenerate.keySet()) {
            String stream = toGenerate.get(obj);
            if (md5InfosMap.get(stream) == null) {
                continue;
            }
            addPremisNodeToMets(getPremisFile(metsElement, stream, md5InfosMap.get(stream)), amdSec, obj, false, amdSecFileGrpMap);
        }

        if (md5InfosMap.get("RAW") != null) {
            addPremisNodeToMets(getPremisEvent(metsElement, "RAW", md5InfosMap.get("RAW"), "capture/digitization"), amdSec, "EVT_001", true, null);
        }

        if (md5InfosMap.get("MC_IMGGRP") != null) {
            addPremisNodeToMets(getPremisEvent(metsElement, "MC_IMGGRP", md5InfosMap.get("MC_IMGGRP"), "migration/MC_creation"), amdSec, "EVT_002", true, amdSecFileGrpMap);
        }
        if (md5InfosMap.get("ALTOGRP") != null) {
            addPremisNodeToMets(getPremisEvent(metsElement, "ALTOGRP", md5InfosMap.get("ALTOGRP"), "capture/XML_creation"), amdSec, "EVT_003", true, amdSecFileGrpMap);
        }

        addPremisNodeToMets(getAgent(metsElement), amdSec, "AGENT_001", true, null);
    }

    /**
     * Fixes PS Mix
     *
     * @param jHoveOutputRaw
     * @param metsElement
     * @param rawCreated
     */
    public static void fixPSMix(JHoveOutput jHoveOutputRaw, String originalPid, XMLGregorianCalendar rawCreated) {
        JhoveUtility.insertObjectIdentifier(jHoveOutputRaw.getMix(), originalPid, "RAW");
        JhoveUtility.addDenominator(jHoveOutputRaw);
        JhoveUtility.addOrientation(jHoveOutputRaw);
        JhoveUtility.insertDateCreated(jHoveOutputRaw.getMix(), rawCreated);
    }

    /**
     * Fixes MC Mix
     *
     * @param jHoveOutputMC
     * @param metsElement
     * @param mcCreated
     * @param originalFile
     * @param photometricInterpretation
     */
    public static void fixMCMix(JHoveOutput jHoveOutputMC, String originalPid, XMLGregorianCalendar mcCreated, String originalFile, PhotometricInterpretation photometricInterpretation) {
        JhoveUtility.insertChangeHistory(jHoveOutputMC.getMix(), mcCreated, originalFile);
        JhoveUtility.insertObjectIdentifier(jHoveOutputMC.getMix(), originalPid, "MC_IMGGRP");
        JhoveUtility.addPhotometricInformation(jHoveOutputMC, photometricInterpretation);
        JhoveUtility.addDenominator(jHoveOutputMC);
        JhoveUtility.addOrientation(jHoveOutputMC);
        JhoveUtility.insertDateCreated(jHoveOutputMC.getMix(), mcCreated);
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
    private void generateTechMetadata(IMetsElement metsElement, HashMap<String, Object> fileNames, int seq, HashMap<String, FileGrp> fileGrpPage, HashMap<String, String> mimeTypes, DivType pageDiv, HashMap<String, String> outputFileNames, HashMap<String, FileMD5Info> md5InfosMap) throws MetsExportException {
        if (fileNames.get("TECHMDGRP") == null) {
            LOG.log(Level.FINE, "Generating tech");
            Mets amdSecMets = new Mets();
            amdSecMets.setLabel1(mets.getLabel1());
            amdSecMets.setTYPE(mets.getTYPE());
            StructMapType mapType = new StructMapType();
            mapType.setTYPE(Const.DIV_PHYSICAL_ID);
            amdSecMets.getStructMap().add(mapType);
            AmdSecType amdSec = new AmdSecType();
            amdSec.setID(metsElement.getElementID());
            amdSecMets.getAmdSec().add(amdSec);
            DivType divType = new DivType();
            if (Const.PERIODICAL_TITLE.equalsIgnoreCase(metsElement.getMetsContext().getRootElement().getElementType())) {
                divType.setTYPE("PERIODICAL_PAGE");
            } else {
                divType.setTYPE("MONOGRAPH_PAGE");
            }

            FileSec fileSec = new FileSec();
            amdSecMets.setFileSec(fileSec);
            HashMap<String, FileGrp> amdSecFileGrpMap = new HashMap<String, MetsType.FileSec.FileGrp>();
            for (String fileMap : fileGrpPage.keySet()) {
                FileGrp fileGrp = fileGrpPage.get(fileMap);
                if (fileGrp.getFile().size() > 0) {
                    FileGrp fileGrpAmd = new FileGrp();
                    amdSecFileGrpMap.put(fileMap, fileGrpAmd);
                    fileGrpAmd.setID(fileGrp.getID());
                    fileGrpAmd.setUSE(fileGrp.getUSE());
                    fileSec.getFileGrp().add(fileGrpAmd);
                    for (FileType fileTypePage : fileGrp.getFile()) {
                        FileType fileTypeAmdSec = new FileType();
                        fileTypeAmdSec.setCHECKSUM(fileTypePage.getCHECKSUM());
                        fileTypeAmdSec.setCHECKSUMTYPE(fileTypePage.getCHECKSUMTYPE());
                        fileTypeAmdSec.setCREATED(fileTypePage.getCREATED());
                        fileTypeAmdSec.setID(fileTypePage.getID());
                        fileTypeAmdSec.setMIMETYPE(fileTypePage.getMIMETYPE());
                        fileTypeAmdSec.setSEQ(fileTypePage.getSEQ());
                        fileTypeAmdSec.setSIZE(fileTypePage.getSIZE());
                        fileGrpAmd.getFile().add(fileTypeAmdSec);
                        if (fileTypePage.getFLocat().get(0) != null) {
                            FLocat flocatAmd = new FLocat();
                            FLocat pageFlocat = fileTypePage.getFLocat().get(0);
                            if (pageFlocat.getHref() != null) {
                                flocatAmd.setHref(".." + pageFlocat.getHref().substring(1));
                            }
                            flocatAmd.setLOCTYPE(pageFlocat.getLOCTYPE());
                            fileTypeAmdSec.getFLocat().add(flocatAmd);
                        }
                        Fptr fptr = new Fptr();
                        fptr.setFILEID(fileTypeAmdSec);
                        divType.getFptr().add(fptr);
                    }
                }
            }

            HashMap<String, String> toGenerate = new HashMap<String, String>();
            File rawFile = null;
            XMLGregorianCalendar rawCreated = null;
            Mix mixDevice = getScannerMix(metsElement);
            // RAW datastream for MIX_001 - only for Fedora
            PhotometricInterpretation photometricInterpretation = null;
            JHoveOutput jHoveOutputRaw = null;
            JHoveOutput jHoveOutputMC = null;
            if (metsElement.getMetsContext().getFedoraClient() != null) {
                try {
                    DatastreamType rawDS = FoxmlUtils.findDatastream(metsElement.getSourceObject(), "RAW");
                    if (rawDS != null) {
                        GetDatastreamDissemination dsRaw = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), "RAW");
                        try {
                            rawCreated = rawDS.getDatastreamVersion().get(0).getCREATED();
                            InputStream is = dsRaw.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                            String rawExtendsion = MimeType.getExtension(rawDS.getDatastreamVersion().get(0).getMIMETYPE());
                            rawFile = new File(metsElement.getMetsContext().getOutputPath() + File.separator + metsElement.getMetsContext().getPackageID() + File.separator + "raw" + "." + rawExtendsion);
                            FileMD5Info rawinfo;
                            try {
                                rawinfo = MetsUtils.getDigestAndCopy(is, new FileOutputStream(rawFile));
                            } catch (NoSuchAlgorithmException e) {
                                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to copy RAW image and get digest", false, e);
                            }
                            rawinfo.setMimeType(rawDS.getDatastreamVersion().get(0).getMIMETYPE());
                            rawinfo.setCreated(rawDS.getDatastreamVersion().get(0).getCREATED());
                            md5InfosMap.put("RAW", rawinfo);
                            outputFileNames.put("RAW", rawFile.getAbsolutePath());
                            toGenerate.put("MIX_001", "RAW");

                            // If mix is present in fedora, then use this one
                            if (metsElement.getMetsContext().getFedoraClient() != null) {
                                jHoveOutputRaw = JhoveUtility.getMixFromFedora(metsElement, MixEditor.RAW_ID);
                            }
                            // If not present, then generate new
                            if (jHoveOutputRaw == null) {
                                jHoveOutputRaw = JhoveUtility.getMix(new File(rawFile.getAbsolutePath()), metsElement.getMetsContext(), mixDevice, rawCreated, null);
                                if (jHoveOutputRaw.getMix() == null) {
                                    throw new MetsExportException(metsElement.getOriginalPid(), "Unable to generate Mix information for RAW image", false, null);
                                }
                            } else {
                                // Merges the information from the device mix
                                JhoveUtility.mergeMix(jHoveOutputRaw.getMix(), mixDevice);
                            }
                            if ((jHoveOutputRaw.getMix() != null) && (jHoveOutputRaw.getMix().getBasicImageInformation() != null) && (jHoveOutputRaw.getMix().getBasicImageInformation().getBasicImageCharacteristics() != null) && (jHoveOutputRaw.getMix().getBasicImageInformation().getBasicImageCharacteristics().getPhotometricInterpretation() != null)) {
                                photometricInterpretation = jHoveOutputRaw.getMix().getBasicImageInformation().getBasicImageCharacteristics().getPhotometricInterpretation();
                            }
                            fixPSMix(jHoveOutputRaw, metsElement.getOriginalPid(), rawCreated);
                        } catch (FedoraClientException e) {
                            throw new MetsExportException(metsElement.getOriginalPid(), "Unable to read raw datastream content", false, e);
                        }
                    }
                } catch (IOException ex) {
                    throw new MetsExportException(metsElement.getOriginalPid(), "Error while getting RAW datastream " + metsElement.getOriginalPid(), false, ex);
                }
            }

            if (fileNames.get("MC_IMGGRP") != null) {
                toGenerate.put("MIX_002", "MC_IMGGRP");
                String outputFileName = outputFileNames.get("MC_IMGGRP");
                if (outputFileName != null) {
                    String originalFile = MetsUtils.xPathEvaluateString(metsElement.getRelsExt(), "*[local-name()='RDF']/*[local-name()='Description']/*[local-name()='importFile']");
                    if (metsElement.getMetsContext().getFedoraClient() != null) {
                        jHoveOutputMC = JhoveUtility.getMixFromFedora(metsElement, MixEditor.NDK_ARCHIVAL_ID);
                    }
                    if (jHoveOutputMC == null) {
                        jHoveOutputMC = JhoveUtility.getMix(new File(outputFileName), metsElement.getMetsContext(), null, md5InfosMap.get("MC_IMGGRP").getCreated(), originalFile);
                        if (jHoveOutputMC.getMix() == null) {
                            throw new MetsExportException(metsElement.getOriginalPid(), "Unable to generate Mix information for MC image", false, null);
                        }
                    }
                    fixMCMix(jHoveOutputMC, metsElement.getOriginalPid(), md5InfosMap.get("MC_IMGGRP").getCreated(), originalFile, photometricInterpretation);
                }
            }

            for (String name : toGenerate.keySet()) {
                String streamName = toGenerate.get(name);

                if (streamName != null) {
                    MdSecType mdSec = new MdSecType();
                    mdSec.setID(name);
                    MdWrap mdWrap = new MdWrap();
                    mdWrap.setMIMETYPE("text/xml");
                    mdWrap.setMDTYPE("NISOIMG");
                    XmlData xmlData = new XmlData();
                    Node mixNode = null;

                    if ("RAW".equals(streamName)) {
                        if (jHoveOutputRaw != null) {
                            mixNode = jHoveOutputRaw.getMixNode();
                            if (md5InfosMap.get(streamName) != null) {
                                md5InfosMap.get(streamName).setFormatVersion(jHoveOutputRaw.getFormatVersion());
                            }
                        }
                    } else if (("MC_IMGGRP".equals(streamName)) && (md5InfosMap.get("MC_IMGGRP") != null)) {
                        if (jHoveOutputMC != null) {
                            mixNode = jHoveOutputMC.getMixNode();
                            if (md5InfosMap.get(streamName) != null) {
                                md5InfosMap.get(streamName).setFormatVersion(jHoveOutputMC.getFormatVersion());
                            }
                            if (mixNode != null) {
                                if ((amdSecFileGrpMap.get("MC_IMGGRP") != null) && (amdSecFileGrpMap.get("MC_IMGGRP").getFile().get(0) != null)) {
                                    amdSecFileGrpMap.get("MC_IMGGRP").getFile().get(0).getADMID().add(mdSec);
                                }
                            }
                        }
                    }

                    if (mixNode != null) {
                        xmlData.getAny().add(mixNode);
                    } else {
                        throw new MetsExportException(metsElement.getOriginalPid(), "Unable to generate image metadata (MIX) for " + streamName, false, null);
                    }

                    mdWrap.setXmlData(xmlData);
                    mdSec.setMdWrap(mdWrap);
                    amdSec.getTechMD().add(mdSec);
                }
            }

            if (rawFile != null) {
                outputFileNames.remove("RAW");
                rawFile.delete();
            }

            if (outputFileNames.get("ALTOGRP") != null) {
                File altoFile = new File(outputFileNames.get("ALTOGRP"));
                if (altoFile.exists()) {
                    Schema altoSchema;
                    try {
                        altoSchema = AltoDatastream.getSchema();
                    } catch (SAXException e) {
                        throw new MetsExportException("Unable to get ALTO schema", false);
                    }
                    try {
                        altoSchema.newValidator().validate(new StreamSource(altoFile));
                    } catch (Exception exSax) {
                        throw new MetsExportException(metsElement.getOriginalPid(), "Invalid ALTO", false, exSax);
                    }
                    md5InfosMap.get("ALTOGRP").setFormatVersion("2.0");
                }
            }

            addPremisToAmdSec(amdSec, md5InfosMap, metsElement, amdSecFileGrpMap);
            mapType.setDiv(divType);
            saveAmdSec(metsElement, amdSecMets, fileNames, mimeTypes);
            FileType fileType = prepareFileType(seq, "TECHMDGRP", fileNames, mimeTypes, metsElement.getMetsContext(), outputFileNames, md5InfosMap);
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
            // marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
            // "http://www.w3.org/2001/XMLSchema-instance http://www.w3.org/2001/XMLSchema.xsd http://www.loc.gov/METS/ http://www.loc.gov/standards/mets/mets.xsd http://www.loc.gov/MIX/ http://www.loc.gov/mix/v20");
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            marshaller.marshal(amdSecMets, bos);
            byte[] byteArray = bos.toByteArray();
            fileNames.put("TECHMDGRP", byteArray);
            mimeTypes.put("TECHMDGRP", "text/xml");
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
        physicalDiv.setID("DIV_P_0000");
        physicalDiv.setLabel3(metsElement.getLabel());
        physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
        physicalDiv.setTYPE(metsElement.getElementType());

        DivType divType = new DivType();
        divType.setID(metsElement.getElementID());
        divType.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        divType.setTYPE(metsElement.getElementType());
        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);

        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement);
                pageCounter++;
                continue;
            }

            if (Const.SUPPLEMENT.equals(element.getElementType())) {
                insertSupplement(divType, physicalDiv, element);
                continue;
            }

            if (Const.PICTURE.equals(element.getElementType())) {
                insertPicture(divType, physicalDiv, element);
                continue;
            }

            if (Const.ARTICLE.equals(element.getElementType())) {
                continue;
            }

            throw new MetsExportException(element.getOriginalPid(),
                    "This type is not accepted in Issue:" +
                            metsElement.getElementType(), false, null);
        }

        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.ARTICLE.equals(element.getElementType())) {
                insertArticle(divType, physicalDiv, element, articleCounter);
                articleCounter++;
                continue;
            }
        }
    }

    /**
     * Return the first parent, which can contain pages
     *
     * @param metsElement
     * @return
     * @throws MetsExportException
     */
    private IMetsElement findFirstParentWithPage(IMetsElement metsElement) throws MetsExportException {
        if (metsElement == null) {
            throw new MetsExportException("Unable to find parent with pages", false, null);
        }
        if (!Const.canContainPage.contains(metsElement.getElementType())) {
            if (metsElement.getParent() == null) {
                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to find parent with pages", false, null);
            }
            return findFirstParentWithPage(metsElement.getParent());
        }
        return metsElement;
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
        List<IMetsElement> sourceElements = new ArrayList<IMetsElement>();
        sourceElements.add(sourceElement);
        insertPage(physicalDiv, metsElement, pageCounter, sourceElements);
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
    private void insertPage(DivType physicalDiv, IMetsElement metsElement, int pageCounter, List<IMetsElement> sourceElements) throws MetsExportException {
        if (metsElement.getMetsContext().getPackageDir() == null) {
            File packageDir = createPackageDir(metsElement);
            metsElement.getMetsContext().setPackageDir(packageDir);
        }
        HashMap<String, String> outputFileNames = new HashMap<String, String>();

        if (!Const.PAGE.equals(metsElement.getElementType()) && !Const.MONOGRAPH_UNIT.equals(metsElement.getElementType())) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Expected page, got " + metsElement.getElementType(), false, null);
        }
        HashMap<String, FileGrp> fileGrpPage = MetsUtils.initFileGroups();
        DivType pageDiv = new DivType();
        physicalDiv.getDiv().add(pageDiv);
        fillPageIndexOrder(metsElement, pageDiv);
        String ID = "DIV_P_PAGE_" + metsElement.getElementID().replace("PAGE_", "");
        pageDiv.setID(ID);
        HashMap<String, Object> fileNames = new HashMap<String, Object>();
        HashMap<String, String> mimeTypes = new HashMap<String, String>();
        HashMap<String, XMLGregorianCalendar> createDates = new HashMap<String, XMLGregorianCalendar>();
        HashMap<String, FileMD5Info> md5InfosMap = new HashMap<String, FileMD5Info>();
        processPageFiles(metsElement, pageCounter, fileNames, mimeTypes, createDates, md5InfosMap);
        for (String streamName : Const.streamMapping.keySet()) {
            if (fileNames.containsKey(streamName)) {
                FileType fileType = prepareFileType(pageCounter, streamName, fileNames, mimeTypes, metsElement.getMetsContext(), outputFileNames, md5InfosMap);
                fileGrpPage.get(streamName).getFile().add(fileType);
                fileGrpMap.get(streamName).getFile().add(fileType);
                Fptr fptr = new Fptr();
                fptr.setFILEID(fileType);
                pageDiv.getFptr().add(fptr);
                if ("ALTOGRP".equals(streamName)) {
                    metsElement.setAltoFile(fileType);
                }
            } else {
                if ((Const.mandatoryStreams.contains(streamName)) && (!metsElement.getMetsContext().isAllowNonCompleteStreams())) {
                    throw new MetsExportException(metsElement.getOriginalPid(), "Stream:" + streamName + " is missing", false, null);
                }
            }
        }
        generateTechMetadata(metsElement, fileNames, pageCounter, fileGrpPage, mimeTypes, pageDiv, outputFileNames, md5InfosMap);

        StructLinkMapping structLinkMapping = new StructLinkMapping();
        structLinkMapping.pageDiv = metsElement.getParent().getModsElementID();
        structLinkMapping.pageOrder = pageDiv.getORDER();
        pageOrderToDivMap.put(structLinkMapping, ID);
        for (IMetsElement sourceElement : sourceElements) {
            addMappingPageStruct(structLinkMapping, sourceElement.getModsElementID());
        }
    }

    class StructLinkMapping {
        String pageDiv;
        BigInteger pageOrder;

        @Override
        public int hashCode() {
            return pageDiv.hashCode() * 1000 + pageOrder.hashCode();
        };

        @Override
        public boolean equals(Object obj) {
            StructLinkMapping structLinkMapping = (StructLinkMapping) obj;
            if (structLinkMapping.pageDiv.equals(this.pageDiv) && (structLinkMapping.pageOrder.equals(this.pageOrder))) {
                return true;
            }
            return false;
        }
    }

    private void addMappingPageStruct(StructLinkMapping structLinkMapping, String fromDiv) {
        if (structToPageMap.get(fromDiv) == null) {
            structToPageMap.put(fromDiv, new ArrayList<StructLinkMapping>());
        }
        structToPageMap.get(fromDiv).add(structLinkMapping);
    }

    /**
     * Adds the struct-link to the mets
     *
     * @throws MetsExportException
     */
    private void addStructLink() throws MetsExportException {
        if (structToPageMap.keySet().size() > 0) {
            StructLink structLink = mets.getStructLink();
            if (structLink == null) {
                structLink = new StructLink();
                mets.setStructLink(structLink);
            }

            for (String structFrom : structToPageMap.keySet()) {
                if (structToPageMap.get(structFrom) != null) {
                    for (StructLinkMapping structLinkMapping : structToPageMap.get(structFrom)) {
                        if (pageOrderToDivMap.get(structLinkMapping) != null) {
                            SmLink smLink = new SmLink();
                            smLink.setFrom(structFrom);
                            smLink.setTo(pageOrderToDivMap.get(structLinkMapping));
                            structLink.getSmLinkOrSmLinkGrp().add(smLink);
                        } else {
                            throw new MetsExportException("Unable to find DIV for page order:" + structLinkMapping.pageDiv + " " + structLinkMapping.pageOrder, false, null);
                        }
                    }
                }
            }
        }
    }

    /**
     * Generates PackageID from the metsElement info
     *
     * @param element
     * @return
     * @throws MetsExportException
     */
    private String getPackageID(IMetsElement element) throws MetsExportException {
        Map<String, String> identifiersMap = element.getModsIdentifiers();
        if (identifiersMap.containsKey(Const.URNNBN)) {
            String urnnbn = identifiersMap.get(Const.URNNBN);
            return urnnbn.substring(urnnbn.lastIndexOf(":") + 1);
        } else if (element.getMetsContext().isAllowMissingURNNBN()) {
            // if missing URNNBN is allowed, then try to use UUID - otherwise
            // throw an exception
            element.getMetsContext().getMetsExportException().addException(element.getOriginalPid(), "URNNBN identifier is missing", true, null);
            if (identifiersMap.containsKey(Const.UUID)) {
                return identifiersMap.get(Const.UUID);
            } else {
                throw new MetsExportException(element.getOriginalPid(), "Unable to find identifier URNNBN and UUID is missing", false, null);
            }
        } else {
            // URNNBN is mandatory
            throw new MetsExportException(element.getOriginalPid(), "URNNBN identifier is missing", true, null);
        }
    }

    /**
     * Creates a directory for package
     *
     * @param metsElement
     * @return
     * @throws MetsExportException
     */
    private File createPackageDir(IMetsElement metsElement) throws MetsExportException {
        if (metsElement.getMetsContext().getPackageID() == null) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Package ID is null", false, null);
        }
        File file = new File(metsElement.getMetsContext().getOutputPath() + File.separator + metsElement.getMetsContext().getPackageID());
        if (file.exists()) {
            if (file.isDirectory()) {
                createDirectoryStructure(metsElement.getMetsContext());
                return file;
            } else {
                throw new MetsExportException(metsElement.getOriginalPid(), "File:" + file.getAbsolutePath() + " exists, but is not directory", false, null);
            }
        } else {
            file.mkdir();
            createDirectoryStructure(metsElement.getMetsContext());
            return file;
        }
    }

    /**
     * Inserts Supplement structure to the mets
     *
     * @param logicalDiv
     * @param physicalDiv
     * @param metsElement
     * @throws MetsExportException
     */
    private void insertSupplement(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement) throws MetsExportException {
        addDmdSec(metsElement);
        if (physicalDiv.getID() == null) {
            physicalDiv.setID("DIV_P_0000");
            physicalDiv.setLabel3(metsElement.getLabel());
            physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            physicalDiv.setTYPE(metsElement.getElementType());
        }

        DivType divType = new DivType();
        divType.setID(metsElement.getElementID());
        divType.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        divType.setTYPE(Const.typeNameMap.get(metsElement.getElementType()));
        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);
        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement);
                pageCounter++;
            } else if (Const.PICTURE.equals(element.getElementType())) {
                insertPicture(divType, physicalDiv, element);
            } else
                throw new MetsExportException(element.getOriginalPid(), "Expected Page or Picture, got:" + element.getElementType(), false, null);
        }
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
    private void insertVolume(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement, boolean isMultiPartMonograph) throws MetsExportException {
        addDmdSec(metsElement);
        DivType divType = new DivType();
        divType.setID(metsElement.getElementID());
        // Label for volume is inherited from the parent monograph
        divType.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        if (Const.PERIODICAL_VOLUME.equals(metsElement.getElementType())) {
            divType.setTYPE("PERIODICAL_VOLUME");
        } else
        if (Const.MONOGRAPH_MULTIPART.equals(metsElement.getElementType())) {
            divType.setTYPE(Const.MONOGRAPH);
        } else if (Const.MONOGRAPH_UNIT.equals(metsElement.getElementType())) {
            divType.setTYPE("VOLUME");
            divType.setID(metsElement.getElementID().replaceAll(Const.MONOGRAPH_UNIT, Const.VOLUME));
            physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
        } else {
            divType.setTYPE(Const.VOLUME);
        }

        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);
        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.ISSUE.equals(element.getElementType())) {
                element.getMetsContext().setPackageID(getPackageID(element));
                insertIssue(physicalDiv, divType, element);
                continue;
            } else
            if (Const.SUPPLEMENT.equals(element.getElementType())) {
                if (!Const.MONOGRAPH_UNIT.equals(metsElement.getElementType())) {
                    element.getMetsContext().setPackageID(getPackageID(element));
                }
                insertSupplement(divType, physicalDiv, element);
            } else
            if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement);
                pageCounter++;
                continue;
            } else if (Const.CHAPTER.equals(element.getElementType())) {
                insertChapter(divType, physicalDiv, element, chapterCounter);
                chapterCounter++;
            } else if (Const.PICTURE.equals(element.getElementType())) {
                insertPicture(divType, physicalDiv, element);
            }
            else
                throw new MetsExportException(element.getOriginalPid(), "Expected Issue, Supplement, Picture or Page, got:" + element.getElementType(), false, null);
        }
    }

    /**
     * Inserts Monograph structure to the mets
     *
     * @param metsElement
     * @throws MetsExportException
     */
    private void insertMonograph(IMetsElement metsElement) throws MetsExportException {
        mets.setTYPE("Monograph");
        DivType logicalDiv = new DivType();
        logicalStruct.setDiv(logicalDiv);
        DivType physicalDiv = new DivType();
        physicalDiv.setLabel3(metsElement.getLabel());
        physicalDiv.setID("DIV_P_0000");
        physicalDiv.setTYPE("MONOGRAPH");
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
        logicalDiv.setTYPE("MONOGRAPH");
        logicalDiv.setID("MONOGRAPH_0001");
        if (!containsUnit) {
            metsElement.getMetsContext().setPackageID(getPackageID(metsElement));
            insertVolume(logicalDiv, physicalDiv, metsElement, false);
        } else {
            metsElement.setModsElementID("TITLE_0001");
            addDmdSec(metsElement);
            logicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            for (IMetsElement childMetsElement : metsElement.getChildren()) {
                if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                    continue;
                } else
                if (Const.SUPPLEMENT.equals(childMetsElement.getElementType())) {
                    childMetsElement.getMetsContext().setPackageID(getPackageID(childMetsElement));
                    insertSupplement(logicalDiv, physicalDiv, childMetsElement);
                } else
                if (Const.PAGE.equals(childMetsElement.getElementType())) {
                    pageCounter++;
                    insertPage(physicalDiv, childMetsElement, pageCounter, metsElement);
                } else if (Const.CHAPTER.equals(childMetsElement.getElementType())) {
                    insertChapter(logicalDiv, physicalDiv, childMetsElement, chapterCounter);
                    chapterCounter++;
                } else
                    throw new MetsExportException(childMetsElement.getOriginalPid(), "Expected Supplement, Monograph unit, Chapter or Page, got:" + childMetsElement.getElementType(), false, null);
            }

            for (IMetsElement childMetsElement : metsElement.getChildren()) {
                if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                    childMetsElement.getMetsContext().setPackageID(getPackageID(childMetsElement));
                    insertVolume(logicalDiv, physicalDiv, childMetsElement, true);
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
            structStream = MetsUtils.getBinaryDataStreams(metsElement.getMetsContext().getFedoraClient(), metsElement, "STRUCT_MAP");
        } else {
            structStream = MetsUtils.getBinaryDataStreams(metsElement.getSourceObject().getDatastream(), "STRUCT_MAP");
        }
        if (structStream == null) {
            return;
        }
        List<IntPartInfo> partInfoList = parseAltoInfo(MetsUtils.getDocumentFromBytes(structStream));
        for (IntPartInfo partInfo : partInfoList) {
            DivType divType = new DivType();
            divType.setTYPE(partInfo.getType());
            if ((partInfo.getOrder() != null) && (!("null".equalsIgnoreCase(partInfo.getOrder())))) {
            try {
                divType.setORDER(new BigInteger(partInfo.getOrder()));
            } catch (NumberFormatException ex) {
                LOG.log(Level.WARNING, partInfo.getOrder() + " is not a number in  object " + metsElement.getOriginalPid(), ex);
            }
            }
            String number = "";
            if (Const.ARTICLE.equals(metsElement.getParent().getElementType())) {
                number = String.format("%04d", metsElement.getMetsContext().addElementId(metsElement.getParent().getElementID()));
            } else {
                number = String.format("%04d", metsElement.getMetsContext().addElementId(metsElement.getElementID()));
            }

            /**
             * if an internal element is part of article, then the ID is
             * inherited
             */
            if ("ARTICLE".equalsIgnoreCase(metsElement.getParent().getElementType())) {
                divType.setID(metsElement.getParent().getElementID() + "_" + number);
            } else {
                divType.setID(metsElement.getElementID() + "_" + number);
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
        mets.setTYPE("Periodical");
        addDmdSec(metsElement);
        DivType divType = new DivType();
        logicalStruct.setDiv(divType);
        DivType physicalDiv = new DivType();
        physicalStruct.setDiv(physicalDiv);

        divType.setID(metsElement.getElementID());
        // Label for volume is inherited from the parent monograph
        divType.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        divType.setTYPE(metsElement.getElementType());
        divType.getDMDID().add(metsElement.getModsMetsElement());

        for (IMetsElement childMetsElement : metsElement.getChildren()) {
            if (Const.PERIODICAL_VOLUME.equals(childMetsElement.getElementType())) {
                insertVolume(divType, physicalDiv, childMetsElement, false);
            } else if (Const.ISSUE.equals(childMetsElement.getElementType())) {
                childMetsElement.getMetsContext().setPackageID(getPackageID(childMetsElement));
                insertIssue(physicalDiv, divType, childMetsElement);
            } else if (Const.SUPPLEMENT.equals(childMetsElement.getElementType())) {
                childMetsElement.getMetsContext().setPackageID(getPackageID(childMetsElement));
                insertSupplement(divType, physicalDiv, childMetsElement);
            } else
                throw new MetsExportException(childMetsElement.getOriginalPid(), "Expected Supplement, Volume or Issue, got:" + childMetsElement.getElementType(), false, null);
        }
    }

    /**
     * Inserts Picture structure into mets
     *
     * @param logicalDiv
     * @param physicalDiv
     * @param metsElement
     * @param counterIntPart
     * @throws MetsExportException
     */
    private void insertPicture(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement) throws MetsExportException {
        if (!Const.PICTURE.equals(metsElement.getElementType())) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Expected picture got " + metsElement.getElementType(), false, null);
        }
        addDmdSec(metsElement);
        DivType elementDivType = new DivType();
        if (Const.ARTICLE.equalsIgnoreCase(metsElement.getParent().getElementType())) {
            int seq = metsElement.getMetsContext().addElementId(metsElement.getParent().getElementID());
            String number = String.format("%04d", seq);
            elementDivType.setID(metsElement.getParent().getElementID() + "_" + number);
        } else {
            elementDivType.setID(metsElement.getElementID());
        }

        elementDivType.setLabel3(metsElement.getLabel());
        elementDivType.setTYPE(metsElement.getElementType());
        elementDivType.getDMDID().add(metsElement.getModsMetsElement());

        logicalDiv.getDiv().add(elementDivType);
        addInternalElements(elementDivType, metsElement);
        addStructLinkFromMods(metsElement);
    }

    /**
     * Adds the info about linkage between an element and page into the
     * struct-link
     *
     * @param metsElement
     */
    private void addStructLinkFromMods(IMetsElement metsElement) throws MetsExportException {
        if ((metsElement.getModsStart() != null) && (metsElement.getModsEnd() != null)) {
            if (metsElement.getModsEnd().longValue() < metsElement.getModsStart().longValue()) {
                throw new MetsExportException(metsElement.getOriginalPid(), "Mods start is bigger than mods end", false,null);
            }
            for (long i=metsElement.getModsStart().longValue(); i<=metsElement.getModsEnd().longValue();i++) {
                StructLinkMapping structLinkMapping = new StructLinkMapping();
                structLinkMapping.pageDiv = findFirstParentWithPage(metsElement).getModsElementID();
                structLinkMapping.pageOrder = BigInteger.valueOf(i);
                addMappingPageStruct(structLinkMapping, metsElement.getModsElementID());
            }
        }


    }

    /**
     * Inserts Article element structure into mets for future release
     *
     * @param logicalDiv
     * @param physicalDiv
     * @param metsElement
     * @param counterIntPart
     * @throws MetsExportException
     */
    private void insertArticle(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement, int counterIntPart) throws MetsExportException {
        if (!Const.ARTICLE.equals(metsElement.getElementType())) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Expected article got " + metsElement.getElementType(), false, null);
        }
        addDmdSec(metsElement);
        DivType elementDivType = new DivType();
        elementDivType.setID(metsElement.getElementID());
        elementDivType.setORDER(BigInteger.valueOf(counterIntPart));

        elementDivType.setLabel3(metsElement.getLabel());
        elementDivType.setTYPE(metsElement.getElementType());
        elementDivType.getDMDID().add(metsElement.getModsMetsElement());

        logicalDiv.getDiv().add(elementDivType);
        addInternalElements(elementDivType, metsElement);
        for (MetsElement element : metsElement.getChildren()) {
            if (Const.PICTURE.equals(element.getElementType())) {
                insertPicture(elementDivType, physicalDiv, element);
            } else
                throw new MetsExportException(element.getOriginalPid(), "Expected Picture got:" + element.getElementType(), false, null);
        }
        addStructLinkFromMods(metsElement);
    }

    /**
     * Inserts Chapter element structure into mets for future release
     *
     * @param logicalDiv
     * @param physicalDiv
     * @param metsElement
     * @param counterIntPart
     * @throws MetsExportException
     */
    private void insertChapter(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement, int counterIntPart) throws MetsExportException {
        if (!Const.CHAPTER.equals(metsElement.getElementType())) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Expected chapter got " + metsElement.getElementType(), false, null);
        }
        addDmdSec(metsElement);
        DivType elementDivType = new DivType();
        elementDivType.setID(metsElement.getElementID());
        elementDivType.setORDER(BigInteger.valueOf(counterIntPart));

        elementDivType.setLabel3(metsElement.getLabel());
        elementDivType.setTYPE(metsElement.getElementType());
        elementDivType.getDMDID().add(metsElement.getModsMetsElement());

        logicalDiv.getDiv().add(elementDivType);
        addInternalElements(elementDivType, metsElement);
        addStructLinkFromMods(metsElement);
        for (MetsElement element : metsElement.getChildren()) {
            if (Const.PICTURE.equals(element.getElementType())) {
                insertPicture(elementDivType, physicalDiv, element);
                // } else if (Const.PAGE.equals(element.getElementType())) {
                // List<IMetsElement> sourceElements = new
                // ArrayList<IMetsElement>();
                // sourceElements.add(metsElement);
                // sourceElements.add(metsElement.getParent());
                // insertPage(physicalDiv, element, pageCounter,
                // sourceElements);
                // pageCounter++;
            } else
            {
                throw new MetsExportException(metsElement.getOriginalPid(), "Unexpected element under Chapter:" + element.getElementType(), false, null);
            }
        }
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
            // clear the output fileList before the generation starts
            metsElement.getMetsContext().getFileList().clear();
            mets = prepareMets(metsElement);
            initHeader(metsElement);
            LOG.log(Level.FINE, "Inserting into Mets:" + metsElement.getOriginalPid() + "(" + metsElement.getElementType() + ")");
            // get root element first
            IMetsElement rootElement = metsElement.getMetsContext().getRootElement();
            if (rootElement == null) {
                throw new MetsExportException("Element does not have a root set:" + metsElement.getModel() + " - " + metsElement.getOriginalPid(), false);
            }
            if (Const.PERIODICAL_TITLE.equalsIgnoreCase(rootElement.getElementType())) {
                insertPeriodical(rootElement);
            } else if (Const.MONOGRAPH_UNIT.equalsIgnoreCase(rootElement.getElementType())) {
                insertMonograph(rootElement);
            } else if (Const.MONOGRAPH_MULTIPART.equalsIgnoreCase(rootElement.getElementType())) {
                insertMonograph(rootElement);
            } else
                throw new MetsExportException(rootElement.getOriginalPid(), "Unknown type:" + rootElement.getElementType() + " model:" + rootElement.getModel(), false, null);


            if (metsElement.getMetsContext().getPackageID()==null) {
                throw new MetsExportException(metsElement.getOriginalPid(),"Package ID is null",false,null);
            }

            if (metsElement.getMetsContext().getPackageDir() == null) {
                File packageDirFile = createPackageDir(metsElement);
                metsElement.getMetsContext().setPackageDir(packageDirFile);
            }

            saveMets(mets, new File(metsElement.getMetsContext().getPackageDir().getAbsolutePath() + File.separator + "METS_" + MetsUtils.removeNonAlpabetChars(metsElement.getMetsContext().getPackageID()) + ".xml"), metsElement);
        } finally {
            JhoveUtility.destroyConfigFiles(metsElement.getMetsContext().getJhoveContext());
        }
    }
}
