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

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import cz.cas.lib.proarc.audiopremis.AudioObjectFactory;
import cz.cas.lib.proarc.audiopremis.NkComplexType;
import cz.cas.lib.proarc.common.device.Device;
import cz.cas.lib.proarc.common.device.DeviceException;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.JHoveOutput;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.MimeType;
import cz.cas.lib.proarc.common.fedora.AesEditor;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.CodingHistoryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.MixEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.mets.AmdSecType;
import cz.cas.lib.proarc.mets.AreaType;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.DivType.Fptr;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.FileType.FLocat;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap.XmlData;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType;
import cz.cas.lib.proarc.mets.MetsType.FileSec;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.MetsType.MetsHdr;
import cz.cas.lib.proarc.mets.MetsType.MetsHdr.Agent;
import cz.cas.lib.proarc.mets.MetsType.StructLink;
import cz.cas.lib.proarc.mets.StructLinkType.SmLink;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mix.BasicImageInformationType.BasicImageCharacteristics.PhotometricInterpretation;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.premis.AgentComplexType;
import cz.cas.lib.proarc.premis.AgentIdentifierComplexType;
import cz.cas.lib.proarc.premis.CreatingApplicationComplexType;
import cz.cas.lib.proarc.premis.EventComplexType;
import cz.cas.lib.proarc.premis.EventIdentifierComplexType;
import cz.cas.lib.proarc.premis.EventOutcomeInformationComplexType;
import cz.cas.lib.proarc.premis.ExtensionComplexType;
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
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
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

/**
 * Visitor class for creating mets document out of Mets objects
 *
 * @author Robert Simonovsky
 *
 */

public class MetsElementVisitor implements IMetsElementVisitor {
    private final Logger LOG = Logger.getLogger(MetsElementVisitor.class.getName());
    protected Mets mets;
    protected StructMapType logicalStruct;
    protected StructMapType physicalStruct;
    protected HashMap<String, FileGrp> fileGrpMap;
    protected final Map<StructLinkMapping, String> pageOrderToDivMap = new HashMap<StructLinkMapping, String>();
    private final Map<StructLinkMapping, String> audioPageOrderToDivMap = new HashMap<StructLinkMapping, String>();
    private final Map<String, List<StructLinkMapping>> structToPageMap = new HashMap<String, List<StructLinkMapping>>();
    private final Map<String, List<StructLinkMapping>> structToAudioPageMap = new HashMap<String, List<StructLinkMapping>>();
    int pageCounter = 0;
    int articleCounter = 0;
    protected int chapterCounter = 0;
    protected int titleCounter = 1;
    int audioPageCounter = 0;

    /**
     * creates directory structure for mets elements
     */

    private void createDirectoryStructure(IMetsElement metsElement) {
        for (String directory : Const.streamMappingFile.values()) {
            if (Const.SOUND_PAGE.equals(metsElement.getElementType())
                    || (Const.PAGE.equals(metsElement.getElementType()) && !("mastercopy_audio".equals(directory) || "sourceaudio".equals(directory) || "usercopy_audio".equals(directory)))) {
                File file = new File(metsElement.getMetsContext().getOutputPath() + File.separator + metsElement.getMetsContext().getPackageID() + File.separator + directory);
                if (file.exists()) {
                    deleteFolder(file);
                }
                file.mkdir();
            }
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
        mets.setLabel1(getTitle(metsElement) + metsElement.getLabel());
        mets.setMetsHdr(createMetsHdr(metsElement));

        if (Const.SOUND_COLLECTION.equals(metsElement.getElementType())
                || Const.SOUND_RECORDING.equals(metsElement.getElementType())
                || Const.SOUND_PART.equals(metsElement.getElementType())) {
            fileGrpMap = MetsUtils.initAudioFileGroups(fileGrpMap);
        } else if (metsElement.getModel().contains(Const.NDK_EBORN_MODELS_IDENTIFIER)) {
            fileGrpMap = MetsUtils.initEbornFileGroups();
        } else {
            fileGrpMap = MetsUtils.initFileGroups();
        }
    }

    /**
     * Creates the Mets header info
     */
    protected MetsHdr createMetsHdr(IMetsElement metsElement) throws MetsExportException {
        MetsHdr metsHdr = new MetsHdr();
        metsHdr.setCREATEDATE(metsElement.getCreateDate());
        metsHdr.setLASTMODDATE(metsElement.getLastUpdateDate());
        setAgent(metsHdr, "CREATOR", "ORGANIZATION", metsElement.getMetsContext().getOptions().getCreator());
        setAgent(metsHdr, "ARCHIVIST", "ORGANIZATION",metsElement.getMetsContext().getOptions().getArchivist());
        return metsHdr;
    }

    /**
     * Returns the name of title if element is issue
     */
    private String getTitle(IMetsElement metsElement) throws MetsExportException {
        if (isIssue(metsElement)) {
            Node partNode = MetsUtils.xPathEvaluateNode(metsElement.getModsStream(), "//*[local-name()='mods']/*[local-name()='titleInfo']/*[local-name()='title']");
            if (partNode == null){
                throw new MetsExportException("Error - missing title. Please insert title.");
            }
            return partNode.getTextContent() + ", ";
        }
        return "";
    }

    /**
     * Returns true if element is issue, else return false
     */
    public boolean isIssue(IMetsElement metsElement) throws MetsExportException {
        String type = MetsUtils.xPathEvaluateString(metsElement.getModsStream(), "//*[local-name()='mods']/*[local-name()='part']/@type");
        if (!type.isEmpty()) {
            return type.equals("issue");
        }
        return metsElement.getModel().contains("issue");
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
    protected Mets prepareMets(IMetsElement metsElement) throws MetsExportException {
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
    protected void saveMets(Mets mets, File outputFile, IMetsElement metsElement) throws MetsExportException {
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
                metsElement.getMetsContext().getFileList().add(new FileMD5Info(File.separator + outputFile.getName(), result, totalBytes));
                fileMd5Name = "md5_" + MetsUtils.removeNonAlpabetChars(metsElement.getMetsContext().getPackageID()) + ".md5";
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
                metsElement.getMetsContext().getFileList().add(new FileMD5Info(File.separator + fileMd5Name, null, fileMd5.length()));
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
    protected void addDmdSec(IMetsElement metsElement) {
        // MODS
        if (metsElement.getModsStream() != null) {
            MdSecType modsMdSecType = new MdSecType();
            metsElement.setModsMetsElement(modsMdSecType);
            mets.getDmdSec().add(modsMdSecType);
            modsMdSecType.setID("MODSMD_" + metsElement.getModsElementID());
            MdWrap modsMdWrap = new MdWrap();
            modsMdWrap.setMDTYPE("MODS");
            //fillMdTypeVersion(modsMdWrap, metsElement);
            modsMdWrap.setMDTYPEVERSION(getModsVersion(metsElement));
            modsMdWrap.setMIMETYPE("text/xml");
            XmlData modsxmlData = new XmlData();
            metsElement.getModsStream().get(0).setAttribute("ID", "MODS_" + metsElement.getModsElementID());
            metsElement.getModsStream().get(0).removeAttribute("version");
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

    private void fillMdTypeVersion(MdWrap modsMdWrap, IMetsElement metsElement) {
        if (Const.PERIODICAL_TITLE.equals(metsElement.getMetsContext().getRootElement().getElementType())) {
            modsMdWrap.setMDTYPEVERSION(getModsVersion(metsElement));
        }
    }

    private String getModsVersion(IMetsElement metsElement) {
        if (metsElement.getModsStream() != null && metsElement.getModsStream().get(0).getAttributeNode("version") != null) {
            return metsElement.getModsStream().get(0).getAttributeNode("version").getValue();
        }
        return ModsUtils.VERSION;
    }

    /**
     * adds an order and index attributes to pageDiv
     *
     * @param metsElement
     * @param pageDiv
     * @throws MetsExportException
     */
    protected void fillPageIndexOrder(IMetsElement metsElement, DivType pageDiv) throws MetsExportException {
        Node partNode = MetsUtils.xPathEvaluateNode(metsElement.getModsStream(), "*[local-name()='modsCollection']/*[local-name()='mods']/*[local-name()='part']");
        if (partNode == null) {
            partNode = MetsUtils.xPathEvaluateNode(metsElement.getModsStream(), "*[local-name()='mods']/*[local-name()='part']");
        }
        if ((partNode.getAttributes() != null) && (partNode.getAttributes().getNamedItem("type") != null)) {
            pageDiv.setTYPE(partNode.getAttributes().getNamedItem("type").getNodeValue());
        } else {
            pageDiv.setTYPE("normalPage");
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
        if (pageDiv.getORDER() == null || pageDiv.getORDERLABEL() == null || pageDiv.getTYPE() == null) {
            ModsDefinition mods = getMods(metsElement);
            for (PartDefinition part : mods.getPart()) {
                if (pageDiv.getTYPE() == null) {
                    pageDiv.setTYPE(part.getType());
                }
                for (DetailDefinition detail : part.getDetail()) {
                    if (pageDiv.getORDERLABEL() == null) {
                        if ("page number".equals(detail.getType()) || "pageNumber".equals(detail.getType())) {
                            if (!detail.getNumber().isEmpty()) {
                                pageDiv.setORDERLABEL(detail.getNumber().get(0).getValue());
                            }
                        }
                    }
                    if (pageDiv.getORDER() == null) {
                        if ("pageIndex".equals(detail.getType())) {
                            if (!detail.getNumber().isEmpty()) {
                                pageDiv.setORDER(new BigInteger(detail.getNumber().get(0).getValue()));
                            }
                        }
                    }
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
    protected FileType prepareFileType(int seq, String metsStreamName, HashMap<String, Object> fileNames, HashMap<String, String> mimeTypes, IMetsElement metsElement, HashMap<String, String> outputFileNames, HashMap<String, FileMD5Info> md5InfosMap) throws MetsExportException {
        // String streamName = Const.streamMapping.get(metsStreamName);
        MetsContext metsContext = metsElement.getMetsContext();
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
        seq = seq + 1;
        if (Const.SOUND_PAGE.equals(metsElement.getElementType())) {
            fileType.setID(Const.streamMappingPrefix.get(metsStreamName) + "_" + MimeType.getExtension(mimeTypes.get(metsStreamName)) + "_" + MetsUtils.removeNonAlpabetChars(metsContext.getPackageID()) + "_" + String.format("%04d", seq));
        } else {
            fileType.setID(Const.streamMappingPrefix.get(metsStreamName) + "_" + MetsUtils.removeNonAlpabetChars(metsContext.getPackageID()) + "_" + String.format("%04d", seq));
        }
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
            fileMD5Info.setFileName(File.separator + Const.streamMappingFile.get(metsStreamName) + File.separator + outputFileName);
            fileMD5Info.setMimeType(fileType.getMIMETYPE());
            fileType.setCHECKSUM(fileMD5Info.getMd5());
            metsContext.getFileList().add(fileMD5Info);
        } catch (Exception e) {
            throw new MetsExportException("Unable to process file " + fullOutputFileName, false, e);
        }
        FLocat flocat = new FLocat();
        flocat.setLOCTYPE("URL");
        URI uri;
        uri = URI.create(Const.streamMappingFile.get(metsStreamName) + "/" + outputFileName);
        flocat.setHref(uri.toASCIIString());
        fileType.getFLocat().add(flocat);
        return fileType;
    }


    /**
     * Prepares a mets FileType element for a file
     *
     * @param seq
     * @param metsStreamName
     * @return
     */
    protected FileType prepareAudioFileType(int seq, String metsStreamName, String metsStreamNameExtension, HashMap<String, Object> fileNames, HashMap<String, String> mimeTypes, IMetsElement metsElement, HashMap<String, String> outputFileNames, HashMap<String, FileMD5Info> md5InfosMap) throws MetsExportException {
        // String streamName = Const.streamMapping.get(metsStreamName);
        MetsContext metsContext = metsElement.getMetsContext();
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
        seq = seq + 1;
        if (Const.SOUND_PAGE.equals(metsElement.getElementType())) {
            fileType.setID(Const.streamMappingPrefix.get(metsStreamName) + "_" + MimeType.getExtension(mimeTypes.get(metsStreamName)) + "_" + MetsUtils.removeNonAlpabetChars(metsContext.getPackageID()) + "_" + String.format("%04d", seq));
        } else {
            fileType.setID(Const.streamMappingPrefix.get(metsStreamName) + "_" + MimeType.getExtension(mimeTypes.get(metsStreamName)) + "_" + MetsUtils.removeNonAlpabetChars(metsContext.getPackageID()) + "_" + String.format("%04d", seq));
        }
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
            fileMD5Info.setFileName(File.separator + Const.streamMappingFile.get(metsStreamName) + File.separator + outputFileName);
            fileMD5Info.setMimeType(fileType.getMIMETYPE());
            fileType.setCHECKSUM(fileMD5Info.getMd5());
            metsContext.getFileList().add(fileMD5Info);
        } catch (Exception e) {
            throw new MetsExportException("Unable to process file " + fullOutputFileName, false, e);
        }
        FLocat flocat = new FLocat();
        flocat.setLOCTYPE("URL");
        URI uri;
        uri = URI.create(Const.streamMappingFile.get(metsStreamName) + "/" + outputFileName);
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
        if (metsElement.getMetsContext().getRemoteStorage() != null) {
            Device device = getDevice(metsElement);
            if ((device.getDescription() == null) || (device.getDescription().getImageCaptureMetadata() == null)) {
                throw new MetsExportException(metsElement.getOriginalPid(), "Scanner device does not have the description/imageCaptureMetadata set", false, null);
            }
            Mix mix = device.getDescription();
            return mix;
        }
        return null;
    }

    /**
     * Returns the audiodescription of scanner
     *
     * @param metsElement
     * @return
     * @throws MetsExportException
     */
    private Mets getScannerMets(IMetsElement metsElement) throws MetsExportException {
        if (metsElement.getMetsContext().getRemoteStorage() != null) {
            Device device = getDevice(metsElement);
            if ((device.getAudioDescription() == null) || device.getAudioDescription().getAmdSec() == null) {
                throw new MetsExportException(metsElement.getOriginalPid(), "Scanner device does not have the audiodescription/Premis set", false, null);
            }
            return device.getAudioDescription();
        }
        return null;
    }

    /**
     * Returns the device
     *
     * @param metsElement
     * @return
     * @throws MetsExportException
     */
    private Device getDevice(IMetsElement metsElement) throws MetsExportException {
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
            deviceList = deviceRepository.find(deviceId, true, 0);
        } catch (DeviceException e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Unable to get scanner info", false, e);
        }
        if (deviceList.size() != 1) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Unable to get scanner info - expected 1 device, got:" + deviceList.size(), false, null);
        }
        return deviceList.get(0);
    }

    private Node getAgent(AmdSecType amd, IMetsElement metsElement) throws Exception {
        ObjectFactory factory = new ObjectFactory();
        AgentComplexType agent = fillAgent(amd, factory);
        JAXBElement<AgentComplexType> jaxbPremix = factory.createAgent(agent);
        AgentIdentifierComplexType agentIdentifier = new AgentIdentifierComplexType();
        agent.getAgentIdentifier().add(agentIdentifier);
        agentIdentifier.setAgentIdentifierType("ProArc_AgentID");
        agentIdentifier.setAgentIdentifierValue("ProArc");
        JAXBContext jc = JAXBContext.newInstance(AgentComplexType.class);
        return createNode(jaxbPremix, jc, "*[local-name()='agent']");
    }

    private AgentComplexType fillAgent(AmdSecType amd, ObjectFactory factory) {
        String agentType;
        String agentName;
        AgentComplexType agentComplexType = new AgentComplexType();
        try{
            AgentComplexType agent = ((PremisComplexType)((JAXBElement)amd.getDigiprovMD().get(0).getMdWrap().getXmlData().getAny().get(0)).getValue()).getAgent().get(0);
            agentName = agent.getAgentName().get(0);
            agentType = agent.getAgentType();
            ExtensionComplexType extension = factory.createExtensionComplexType();
            agentComplexType.getAgentExtension().add(extension);
            extension.getAny().add(addNkNode(agent));
        } catch (Exception ex) {
            LOG.log(Level.INFO, "Can not get value from Premis, set defualt values");
            agentName = "ProArc";
            agentType = "software";
        }
        agentComplexType.getAgentName().add(agentName);
        agentComplexType.setAgentType(agentType);
        return agentComplexType;
    }

    private Node addNkNode(AgentComplexType agent) throws Exception {
        NkComplexType nk = new NkComplexType();
        String manufacturer = "";
        String serialNumber = "";
        String settings = "";

        Element extension = (Element)agent.getAgentExtension().get(0).getAny().get(0);
        if (extension != null) {
            try {
                if ("manufacturer".equals(extension.getFirstChild().getLocalName())) {
                    manufacturer =  extension.getFirstChild().getFirstChild().getNodeValue();
                } else if ("serialNumber".equals(extension.getFirstChild().getLocalName())) {
                    serialNumber = extension.getFirstChild().getFirstChild().getNodeValue();
                } else if ("settings".equals(extension.getFirstChild().getLocalName()))
                    settings = extension.getFirstChild().getFirstChild().getNodeValue();
            } catch (Exception ex) {
                LOG.log(Level.FINE, "Error in premis:agentExtension");
            }
            try {
                if ("serialNumber".equals(extension.getFirstChild().getNextSibling().getLocalName())) {
                    serialNumber = extension.getFirstChild().getNextSibling().getFirstChild().getNodeValue();
                } else if ("settings".equals(extension.getFirstChild().getNextSibling().getLocalName()))
                    settings = extension.getFirstChild().getNextSibling().getFirstChild().getNodeValue();
            } catch (Exception ex) {
                LOG.log(Level.FINE, "Error in premis:agentExtension");
            }
            try {
                if ("settings".equals(extension.getFirstChild().getNextSibling().getNextSibling().getLocalName()))
                    settings = extension.getFirstChild().getNextSibling().getNextSibling().getFirstChild().getNodeValue();
            } catch (Exception ex) {
                LOG.log(Level.FINE, "Error in premis:agentExtension");
            }
        }
        nk.setManufacturer(manufacturer);
        nk.setSerialNumber(serialNumber);
        nk.setSettings(settings);
        AudioObjectFactory factory = new AudioObjectFactory();
        JAXBElement<NkComplexType> jaxb = factory.createNk(nk);
        JAXBContext jc = JAXBContext.newInstance(NkComplexType.class);

        return createNode(jaxb, jc, "*[local-name()='nk']");
    }

    private Node createNode(JAXBElement jaxb, JAXBContext jc, String expression) throws  Exception{
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document document = db.newDocument();
        Marshaller marshaller = jc.createMarshaller();
        marshaller.marshal(jaxb, document);
        XPath xpath = XPathFactory.newInstance().newXPath();
        Node node = (Node) xpath.compile(expression).evaluate(document, XPathConstants.NODE);
        return node;
    }

    protected Node getAgent(IMetsElement metsElement) throws MetsExportException {
        try {
            return getAgent(null, metsElement);
        } catch (Exception e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
        }
    }

    private Node getPremisEvent(AmdSecType amd, IMetsElement metsElement, String datastream, FileMD5Info md5Info, String eventDetail) throws Exception {
        PremisComplexType premis = new PremisComplexType();
        ObjectFactory factory = new ObjectFactory();
        JAXBElement<PremisComplexType> jaxbPremix = factory.createPremis(premis);
        EventComplexType event = factory.createEventComplexType();
        premis.getEvent().add(event);
        event.setEventDateTime(md5Info.getCreated().toXMLFormat());
        event.setEventDetail(eventDetail);
        EventIdentifierComplexType eventIdentifier = new EventIdentifierComplexType();
        event.setEventIdentifier(eventIdentifier);
        event.setEventType(StringUtils.substringBefore(eventDetail, "/"));
        eventIdentifier.setEventIdentifierType("ProArc_EventID");
        eventIdentifier.setEventIdentifierValue(Const.dataStreamToEvent.get(datastream));
        EventOutcomeInformationComplexType eventInformation = new EventOutcomeInformationComplexType();
        event.getEventOutcomeInformation().add(eventInformation);
        eventInformation.getContent().add(factory.createEventOutcome("successful"));
        LinkingAgentIdentifierComplexType linkingAgentIdentifier = fillLinkingAgentIdentifier(amd);
        LinkingObjectIdentifierComplexType linkingObject = new LinkingObjectIdentifierComplexType();
        linkingObject.setLinkingObjectIdentifierType("ProArc_URI");
        linkingObject.setLinkingObjectIdentifierValue(Const.FEDORAPREFIX + metsElement.getOriginalPid() + "/" + Const.dataStreamToModel.get(datastream));
        event.getLinkingObjectIdentifier().add(linkingObject);
        event.getLinkingAgentIdentifier().add(linkingAgentIdentifier);
        JAXBContext jc = JAXBContext.newInstance(PremisComplexType.class);
        return createNode(jaxbPremix, jc, "*[local-name()='premis']/*[local-name()='event']");
    }

    private LinkingAgentIdentifierComplexType fillLinkingAgentIdentifier(AmdSecType amd) {
        String identifierType;
        String identifierValue;
        String role;
        try{
            EventComplexType event = ((PremisComplexType)((JAXBElement)amd.getDigiprovMD().get(0).getMdWrap().getXmlData().getAny().get(0)).getValue()).getEvent().get(0);
            identifierType = event.getLinkingAgentIdentifier().get(0).getLinkingAgentIdentifierType();
            identifierValue = event.getLinkingAgentIdentifier().get(0).getLinkingAgentIdentifierValue();
            role = event.getLinkingAgentIdentifier().get(0).getLinkingAgentRole().get(0);
        } catch (Exception ex) {
            identifierType = "ProArc_AgentID";
            identifierValue = "ProArc";
            role = "software";
        }
        LinkingAgentIdentifierComplexType linkingAgent = new LinkingAgentIdentifierComplexType();
        linkingAgent.setLinkingAgentIdentifierType(identifierType);
        linkingAgent.setLinkingAgentIdentifierValue(identifierValue);
        linkingAgent.getLinkingAgentRole().add(role);
        return linkingAgent;
    }

    protected Node getPremisEvent(IMetsElement metsElement, String datastream, FileMD5Info md5Info, String eventDetail) throws MetsExportException {
        try {
            return getPremisEvent(null, metsElement, datastream, md5Info, eventDetail);
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
        if (!md5Info.getMimeType().equals(md5Info.getFormatVersion())) {
            formatDesignation.setFormatVersion(md5Info.getFormatVersion());
        } else {
            formatDesignation.setFormatVersion("1.0");
        }
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
        creatingApplication.getContent().add(factory.createCreatingApplicationVersion
                (metsElement.getMetsContext().getOptions().getVersion()));

        //creatingApplication.getContent().add(factory.createCreatingApplicationVersion(metsElement.getMetsContext().getProarcVersion()));
        creatingApplication.getContent().add(factory.createDateCreatedByApplication(MetsUtils.getCurrentDate().toXMLFormat()));

        RelationshipComplexType relationShip = new RelationshipComplexType();

        if (!(Const.RAW_GRP_ID).equals(datastream)) {
            relationShip.setRelationshipType("derivation");
            relationShip.setRelationshipSubType("created from");
            RelatedObjectIdentificationComplexType relatedObject = new RelatedObjectIdentificationComplexType();
            relationShip.getRelatedObjectIdentification().add(relatedObject);
            relatedObject.setRelatedObjectIdentifierType("ProArc_URI");
            if (Const.MC_GRP_ID.equals(datastream)) {
                relatedObject.setRelatedObjectIdentifierValue(Const.FEDORAPREFIX + metsElement.getOriginalPid() + "/" + Const.dataStreamToModel.get(Const.RAW_GRP_ID));
            } else {
                relatedObject.setRelatedObjectIdentifierValue(Const.FEDORAPREFIX + metsElement.getOriginalPid() + "/" + Const.dataStreamToModel.get(Const.MC_GRP_ID));
            }

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
        String extension = Const.mimeToExtensionMap.get(md5Info.getMimeType());
        int position = originalFile.indexOf(".");
        originalFile = originalFile.substring(0, position) + extension;
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

    protected void addPremisNodeToMets(Node premisNode, AmdSecType amdSec, String Id, boolean isDigiprov, HashMap<String, FileGrp> amdSecFileGrpMap) {
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
            if ((amdSecFileGrpMap!= null) && (amdSecFileGrpMap.get(Const.MC_GRP_ID) != null) && (amdSecFileGrpMap.get(Const.MC_GRP_ID).getFile().get(0) != null)) {
                amdSecFileGrpMap.get(Const.MC_GRP_ID).getFile().get(0).getADMID().add(mdSec);
            }
        }
        if ("OBJ_003".equals(Id) || ("EVT_003".equals(Id))) {
            if ((amdSecFileGrpMap!= null) && (amdSecFileGrpMap.get(Const.ALTO_GRP_ID) != null) && (amdSecFileGrpMap.get(Const.ALTO_GRP_ID).getFile().get(0) != null)) {
                amdSecFileGrpMap.get(Const.ALTO_GRP_ID).getFile().get(0).getADMID().add(mdSec);
            }
        }
    }

    private void addPremisToAmdSec(AmdSecType amdSec, HashMap<String, FileMD5Info> md5InfosMap, IMetsElement metsElement, HashMap<String, FileGrp> amdSecFileGrpMap, Mets mets) throws MetsExportException {
        HashMap<String, String> toGenerate = new HashMap<String, String>();
        toGenerate.put("OBJ_001", Const.RAW_GRP_ID);
        toGenerate.put("OBJ_002", Const.MC_GRP_ID);
        toGenerate.put("OBJ_003", Const.ALTO_GRP_ID);
       // toGenerate.put("OBJ_004", Const.UC_GRP_ID);
       // toGenerate.put("OBJ_005", Const.TXT_GRP_ID);
        toGenerate.put("OBJ_006", Const.AUDIO_RAW_GRP_ID);
        toGenerate.put("OBJ_007", Const.AUDIO_MC_GRP_ID);
        int seqEvent = 1;
        int seqAgent = 1;

        for (String obj : toGenerate.keySet()) {
            String stream = toGenerate.get(obj);
            if (md5InfosMap.get(stream) == null) {
                continue;
            }
            addPremisNodeToMets(getPremisFile(metsElement, stream, md5InfosMap.get(stream)), amdSec, obj, false, amdSecFileGrpMap);
        }

        if (mets != null) {
            if (md5InfosMap.get(Const.AUDIO_RAW_GRP_ID) != null) {
                for (AmdSecType amd : mets.getAmdSec()) {
                    try {
                        addPremisNodeToMets(getPremisEvent(amd, metsElement, Const.AUDIO_RAW_GRP_ID, md5InfosMap.get(Const.AUDIO_RAW_GRP_ID), "capture/digitization"), amdSec, "EVT_" + String.format("%03d", seqEvent), true, null);
                        seqEvent++;
                    } catch (Exception e) {
                        throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
                    }
                }
            }
            if (md5InfosMap.get(Const.AUDIO_MC_GRP_ID) != null) {
                addPremisNodeToMets(getPremisEvent(metsElement, Const.AUDIO_MC_GRP_ID, md5InfosMap.get(Const.AUDIO_MC_GRP_ID), "migration/MC_creation"), amdSec, "EVT_002", true, amdSecFileGrpMap);
            }
            if (md5InfosMap.get(Const.AUDIO_UC_GRP_ID) != null) {
                addPremisNodeToMets(getPremisEvent(metsElement, Const.AUDIO_UC_GRP_ID, md5InfosMap.get(Const.AUDIO_UC_GRP_ID), "derivation/UC_creation"), amdSec, "EVT_004", true, amdSecFileGrpMap);
            }
        }

        if (md5InfosMap.get(Const.RAW_GRP_ID) != null) {
            addPremisNodeToMets(getPremisEvent(metsElement, Const.RAW_GRP_ID, md5InfosMap.get(Const.RAW_GRP_ID), "capture/digitization"), amdSec, "EVT_001", true, null);
        }

        if (md5InfosMap.get(Const.MC_GRP_ID) != null) {
            addPremisNodeToMets(getPremisEvent(metsElement, Const.MC_GRP_ID, md5InfosMap.get(Const.MC_GRP_ID), "migration/MC_creation"), amdSec, "EVT_002", true, amdSecFileGrpMap);
        }
        if (md5InfosMap.get(Const.ALTO_GRP_ID) != null) {
            addPremisNodeToMets(getPremisEvent(metsElement, Const.ALTO_GRP_ID, md5InfosMap.get(Const.ALTO_GRP_ID), "capture/XML_creation"), amdSec, "EVT_003", true, amdSecFileGrpMap);
        }
        /*if(md5InfosMap.get(Const.UC_GRP_ID) != null){
            addPremisNodeToMets(getPremisEvent(metsElement, Const.UC_GRP_ID, md5InfosMap.get(Const.UC_GRP_ID), "derivation/UC_creation"), amdSec, "EVT_004", true, amdSecFileGrpMap);
        }
        if (md5InfosMap.get(Const.TXT_GRP_ID) != null){
            addPremisNodeToMets(getPremisEvent(metsElement, Const.TXT_GRP_ID, md5InfosMap.get(Const.TXT_GRP_ID), "capture/TXT_creation"), amdSec, "EVT_005", true, amdSecFileGrpMap);
        }*/

        if (mets != null && mets.getAmdSec().size() != 0) {
            for (AmdSecType amd : mets.getAmdSec()) {
                try {
                    addPremisNodeToMets(getAgent(amd, metsElement), amdSec, "AGENT_" + String.format("%03d", seqAgent), true, null);
                    seqAgent++;
                } catch (Exception e) {
                    throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
                }
            }
        } else {
            addPremisNodeToMets(getAgent(metsElement), amdSec, "AGENT_001", true, null);
        }
    }

    /**
     * Fixes PS Mix
     *
     * @param jHoveOutputRaw
     * @param originalPid
     * @param rawCreated
     */
    public static void fixPSMix(JHoveOutput jHoveOutputRaw, String originalPid, XMLGregorianCalendar rawCreated) {
        JhoveUtility.insertObjectIdentifier(jHoveOutputRaw.getMix(), originalPid, "RAW");
        JhoveUtility.addDenominator(jHoveOutputRaw);
        JhoveUtility.addOrientation(jHoveOutputRaw);
        JhoveUtility.insertImageCaptureMetadata(jHoveOutputRaw.getMix(), rawCreated);
    }

    /**
     * Fixes MC Mix
     *
     * @param jHoveOutputMC
     * @param originalPid
     * @param mcCreated
     * @param originalFile
     * @param photometricInterpretation
     */
    public static void fixMCMix(JHoveOutput jHoveOutputMC, String originalPid, XMLGregorianCalendar mcCreated, String originalFile, PhotometricInterpretation photometricInterpretation) {
        JhoveUtility.insertChangeHistory(jHoveOutputMC.getMix(), mcCreated, originalFile);
        JhoveUtility.insertObjectIdentifier(jHoveOutputMC.getMix(), originalPid, Const.MC_GRP_ID);
        JhoveUtility.addPhotometricInformation(jHoveOutputMC, photometricInterpretation);
        JhoveUtility.addDenominator(jHoveOutputMC);
        JhoveUtility.addOrientation(jHoveOutputMC);
    }

    /**
     * Generates technical metadata using JHOVE
     *
     * @param metsElement
     * @param fileNames
     * @param seq
     * @param fileGrpPage
     * @param mimeTypes
     * @param pageDiv
     * @throws MetsExportException
     */
    private void generateTechMetadata(IMetsElement metsElement, HashMap<String, Object> fileNames, int seq, HashMap<String, FileGrp> fileGrpPage, HashMap<String, String> mimeTypes, DivType pageDiv, HashMap<String, String> outputFileNames, HashMap<String, FileMD5Info> md5InfosMap) throws MetsExportException {
        if (fileNames.get("TECHMDGRP") == null) {
            LOG.log(Level.FINE, "Generating tech");
            Mets amdSecMets = new Mets();
            amdSecMets.setMetsHdr(createMetsHdr(metsElement));
            amdSecMets.setLabel1(mets.getLabel1());
            amdSecMets.setTYPE(mets.getTYPE());
            StructMapType mapType = new StructMapType();
            mapType.setTYPE(Const.DIV_PHYSICAL_ID);
            amdSecMets.getStructMap().add(mapType);
            AmdSecType amdSec = new AmdSecType();
            amdSec.setID(metsElement.getElementID());
            amdSecMets.getAmdSec().add(amdSec);
            DivType divType = new DivType();
            if (Const.SOUND_COLLECTION.equalsIgnoreCase(metsElement.getMetsContext().getRootElement().getElementType())) {
                divType.setTYPE("TRACK");
            } else if (Const.PERIODICAL_TITLE.equalsIgnoreCase(metsElement.getMetsContext().getRootElement().getElementType())) {
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
                                flocatAmd.setHref(pageFlocat.getHref());
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
            Mets metsDevice = getScannerMets(metsElement);
            // RAW datastream for MIX_001 - only for Fedora
            PhotometricInterpretation photometricInterpretation = null;
            JHoveOutput jHoveOutputRaw = null;
            JHoveOutput jHoveOutputMC = null;
            JHoveOutput jHoveOutputRawAes = null;
            JHoveOutput jHoveOutputMcAes = null;
            JHoveOutput jhoveOutputRawCodingHistory = null;
            JHoveOutput jhoveOutputMcCodingHistory = null;

            if (!Const.SOUND_PAGE.equals(metsElement.getElementType())) {
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
                                toGenerate.put(Const.MIX001, "RAW");

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

                if (fileNames.get(Const.MC_GRP_ID) != null) {
                    toGenerate.put(Const.MIX002, Const.MC_GRP_ID);
                    String outputFileName = outputFileNames.get(Const.MC_GRP_ID);
                    if (outputFileName != null) {
                        String originalFile = MetsUtils.xPathEvaluateString(metsElement.getRelsExt(), "*[local-name()='RDF']/*[local-name()='Description']/*[local-name()='importFile']");
                        if (metsElement.getMetsContext().getFedoraClient() != null) {
                            jHoveOutputMC = JhoveUtility.getMixFromFedora(metsElement, MixEditor.NDK_ARCHIVAL_ID);
                        }
                        if (jHoveOutputMC == null) {
                            jHoveOutputMC = JhoveUtility.getMix(new File(outputFileName), metsElement.getMetsContext(), null, md5InfosMap.get(Const.MC_GRP_ID).getCreated(), originalFile);
                            if (jHoveOutputMC.getMix() == null) {
                                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to generate Mix information for MC image", false, null);
                            }
                        }
                        fixMCMix(jHoveOutputMC, metsElement.getOriginalPid(), md5InfosMap.get(Const.MC_GRP_ID).getCreated(), originalFile, photometricInterpretation);
                    }
                }
            } else {
                if (metsElement.getMetsContext().getFedoraClient() != null) {
                    try {
                        DatastreamType rawDsAes = FoxmlUtils.findDatastream(metsElement.getSourceObject(), BinaryEditor.RAW_AUDIO_ID);
                        if (rawDsAes != null) {
                            GetDatastreamDissemination dsRawAes = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), BinaryEditor.RAW_AUDIO_ID);
                            try {
                                rawCreated = rawDsAes.getDatastreamVersion().get(0).getCREATED();
                                InputStream is = dsRawAes.execute(metsElement.getMetsContext().getFedoraClient()).getEntityInputStream();
                                String rawExtendsion = MimeType.getExtension(rawDsAes.getDatastreamVersion().get(0).getMIMETYPE());
                                rawFile = new File(metsElement.getMetsContext().getOutputPath() + File.separator + metsElement.getMetsContext().getPackageID() + File.separator + "rawAes" + "." + rawExtendsion);
                                FileMD5Info rawinfo;
                                try {
                                    rawinfo = MetsUtils.getDigestAndCopy(is, new FileOutputStream(rawFile));
                                } catch (NoSuchAlgorithmException e) {
                                    throw new MetsExportException(metsElement.getOriginalPid(), "Unable to copy RAW image and get digest", false, e);
                                }
                                rawinfo.setMimeType(rawDsAes.getDatastreamVersion().get(0).getMIMETYPE());
                                rawinfo.setCreated(rawDsAes.getDatastreamVersion().get(0).getCREATED());
                                md5InfosMap.put("RAW", rawinfo);
                                outputFileNames.put("RAW", rawFile.getAbsolutePath());
                                toGenerate.put(Const.AES001, "RAW");
                                toGenerate.put(Const.CODINGHISTORY001, "RAW");

                                // If aes is present in fedora, then use this one
                                if (metsElement.getMetsContext().getFedoraClient() != null) {
                                    jHoveOutputRawAes = JhoveUtility.getAesFromFedora(metsElement, AesEditor.RAW_ID);
                                }
                                // If not present, then generate new
                                if (jHoveOutputRawAes == null) {
                                    jHoveOutputRawAes = JhoveUtility.getAes(new File(rawFile.getAbsolutePath()), metsElement.getMetsContext(), null, rawCreated, null);
                                    if (jHoveOutputRawAes.getAes() == null) {
                                        throw new MetsExportException(metsElement.getOriginalPid(), "Unable to generate Aes information for RAW audio", false, null);
                                    }
                                }
                                // If coding history is present in fedora, then use this one
                                if (metsElement.getMetsContext().getFedoraClient() != null) {
                                    jhoveOutputRawCodingHistory = JhoveUtility.getCodingHistoryFromFedora(metsElement, CodingHistoryEditor.RAW_ID);
                                }
                                // If not present, then generate new
                                if (jhoveOutputRawCodingHistory == null) {
                                    jhoveOutputRawCodingHistory = JhoveUtility.getCodingHistory(new File(rawFile.getAbsolutePath()), metsElement.getMetsContext(), rawCreated, null);
                                    if (jhoveOutputRawCodingHistory.getCodingHistory() == null) {
                                        throw new MetsExportException(metsElement.getOriginalPid(), "Unable to generate Aes information for RAW audio", false, null);
                                    }
                                }

                            } catch (FedoraClientException e) {
                                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to read raw datastream content", false, e);
                            }
                        }
                    } catch (IOException ex) {
                        throw new MetsExportException(metsElement.getOriginalPid(), "Error while getting RAW datastream " + metsElement.getOriginalPid(), false, ex);
                    }
                }

                if (fileNames.get(Const.AUDIO_MC_GRP_ID) != null) {
                    toGenerate.put(Const.AES002, Const.AUDIO_MC_GRP_ID);
                    toGenerate.put(Const.CODINGHISTORY002, Const.AUDIO_MC_GRP_ID);
                    String outputFileName = outputFileNames.get(Const.AUDIO_MC_GRP_ID);
                    if (outputFileName != null) {
                        String originalFile = MetsUtils.xPathEvaluateString(metsElement.getRelsExt(), "*[local-name()='RDF']/*[local-name()='Description']/*[local-name()='importFile']");
                        if (metsElement.getMetsContext().getFedoraClient() != null) {
                            jHoveOutputMcAes = JhoveUtility.getAesFromFedora(metsElement, AesEditor.NDK_ARCHIVAL_ID);
                        }
                        if (jHoveOutputMcAes == null) {
                            jHoveOutputMcAes = JhoveUtility.getAes(new File(outputFileName), metsElement.getMetsContext(), null, md5InfosMap.get(Const.AUDIO_MC_GRP_ID).getCreated(), originalFile);
                            if (jHoveOutputMcAes.getAes() == null) {
                                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to generate Aes information for MC audio", false, null);
                            }
                        }
                        if (metsElement.getMetsContext().getFedoraClient() != null) {
                            jhoveOutputMcCodingHistory = JhoveUtility.getCodingHistoryFromFedora(metsElement, CodingHistoryEditor.NDK_ARCHIVAL_ID);
                        }
                        if (jhoveOutputMcCodingHistory == null) {
                            jhoveOutputMcCodingHistory = JhoveUtility.getCodingHistory(new File(outputFileName), metsElement.getMetsContext(), md5InfosMap.get(Const.AUDIO_MC_GRP_ID).getCreated(), originalFile);
                            if (jhoveOutputMcCodingHistory.getCodingHistory() == null) {
                                throw new MetsExportException(metsElement.getOriginalPid(), "Unable to generate Aes information for MC audio", false, null);
                            }
                        }
                    }
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
                    Node aesNode = null;
                    Node codingHistoryNode = null;

                    if ("RAW".equals(streamName)) {
                        if (jHoveOutputRaw != null) {
                            mixNode = jHoveOutputRaw.getMixNode();
                            if (md5InfosMap.get(streamName) != null) {
                                md5InfosMap.get(streamName).setFormatVersion(jHoveOutputRaw.getFormatVersion());
                            }
                        } else if (jHoveOutputRawAes != null && Const.AES001.equals(name)) {
                            aesNode = jHoveOutputRawAes.getAesNode();
                            if (md5InfosMap.get(streamName) != null) {
                                md5InfosMap.get(streamName).setFormatVersion(jHoveOutputRawAes.getFormatVersion());
                            }
                        } else if (jhoveOutputRawCodingHistory != null && Const.CODINGHISTORY001.equals(name)) {
                            codingHistoryNode = jhoveOutputRawCodingHistory.getCodingHistoryNode();
                            if (md5InfosMap.get(streamName) != null) {
                                md5InfosMap.get(streamName).setFormatVersion(jhoveOutputRawCodingHistory.getFormatVersion());
                            }
                        }
                    } else if ((Const.MC_GRP_ID.equals(streamName)) && (md5InfosMap.get(Const.MC_GRP_ID) != null)) {
                        if (jHoveOutputMC != null) {
                            mixNode = jHoveOutputMC.getMixNode();
                            if (md5InfosMap.get(streamName) != null) {
                                md5InfosMap.get(streamName).setFormatVersion(jHoveOutputMC.getFormatVersion());
                            }
                            if (mixNode != null) {
                                if ((amdSecFileGrpMap.get(Const.MC_GRP_ID) != null) && (amdSecFileGrpMap.get(Const.MC_GRP_ID).getFile().get(0) != null)) {
                                    amdSecFileGrpMap.get(Const.MC_GRP_ID).getFile().get(0).getADMID().add(mdSec);
                                }
                            }
                        }
                    } else if ((Const.AUDIO_MC_GRP_ID.equals(streamName)) && (md5InfosMap.get(Const.AUDIO_MC_GRP_ID) != null)) {
                        if (jHoveOutputMcAes != null && Const.AES002.equals(name)) {
                            aesNode = jHoveOutputMcAes.getAesNode();
                            if (md5InfosMap.get(streamName) != null) {
                                md5InfosMap.get(streamName).setFormatVersion(jHoveOutputMcAes.getFormatVersion());
                            }
                            if (aesNode != null) {
                                if ((amdSecFileGrpMap.get(Const.AUDIO_MC_GRP_ID) != null) && (amdSecFileGrpMap.get(Const.AUDIO_MC_GRP_ID).getFile().get(0) != null)) {
                                    amdSecFileGrpMap.get(Const.AUDIO_MC_GRP_ID).getFile().get(0).getADMID().add(mdSec);
                                }
                            }
                        } else if (jhoveOutputMcCodingHistory != null && Const.CODINGHISTORY002.equals(name)) {
                            codingHistoryNode = jhoveOutputMcCodingHistory.getCodingHistoryNode();
                            if (md5InfosMap.get(streamName) != null) {
                                md5InfosMap.get(streamName).setFormatVersion(jhoveOutputMcCodingHistory.getFormatVersion());
                            }
                            if (codingHistoryNode != null) {
                                if ((amdSecFileGrpMap.get(Const.AUDIO_MC_GRP_ID) != null) && (amdSecFileGrpMap.get(Const.AUDIO_MC_GRP_ID).getFile().get(0) != null)) {
                                    amdSecFileGrpMap.get(Const.AUDIO_MC_GRP_ID).getFile().get(0).getADMID().add(mdSec);
                                }
                            }
                        }
                    }

                    if (mixNode != null) {
                        xmlData.getAny().add(mixNode);
                    } else if (aesNode != null) {
                        xmlData.getAny().add(aesNode);
                    } else if (codingHistoryNode != null) {
                        xmlData.getAny().add(codingHistoryNode);
                    } else {throw new MetsExportException(metsElement.getOriginalPid(), "Unable to generate image/audo metadata (MIX/AES) for " + streamName, false, null);
                    }

                    mdWrap.setXmlData(xmlData);
                    mdSec.setMdWrap(mdWrap);
                    amdSec.getTechMD().add(mdSec);
                }
            }

            if (rawFile != null) {
                outputFileNames.remove(Const.RAW_GRP_ID);
                rawFile.delete();
            }

            if (outputFileNames.get(Const.ALTO_GRP_ID) != null) {
                File altoFile = new File(outputFileNames.get(Const.ALTO_GRP_ID));
                if (altoFile.exists()) {
                    List<Schema> altoSchemas;
                    try {
                        altoSchemas = AltoDatastream.getSchemasList();
                    } catch (SAXException e) {
                        throw new MetsExportException("Unable to get ALTO schema", false);
                    }
                    try {
                        validateAlto(altoSchemas, altoFile);
                    } catch (Exception exSax) {
                        throw new MetsExportException(metsElement.getOriginalPid(), "Invalid ALTO", false, exSax);
                    }
                    md5InfosMap.get(Const.ALTO_GRP_ID).setFormatVersion("2.0");
                }
            }

            addPremisToAmdSec(amdSec, md5InfosMap, metsElement, amdSecFileGrpMap, metsDevice);
            mapType.setDiv(divType);
            saveAmdSec(metsElement, amdSecMets, fileNames, mimeTypes);
            FileType fileType = prepareFileType(seq, "TECHMDGRP", fileNames, mimeTypes, metsElement, outputFileNames, md5InfosMap);
            this.fileGrpMap.get("TECHMDGRP").getFile().add(fileType);
            Fptr fptr = new Fptr();
            fptr.setFILEID(fileType);
            pageDiv.getFptr().add(fptr);
        }
    }

    private void validateAlto(List<Schema> altoSchemas, File altoFile) throws IOException, SAXException {
        Boolean valid = false;
        SAXException exeption = new SAXException();
        for (Schema altoSchema : altoSchemas) {
            try {
                altoSchema.newValidator().validate(new StreamSource(altoFile));
                valid = true;
                break;
            } catch (SAXException ex) {
                valid = false;
                exeption = ex;
            }
        }
        if (valid = false) {
            throw exeption;
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

        containChildren(metsElement);
        int pageIndex = 1;
        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement, pageIndex++);
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
    private void insertPage(DivType physicalDiv, IMetsElement metsElement, int pageCounter, IMetsElement sourceElement, int pageIndex) throws MetsExportException {
        List<IMetsElement> sourceElements = new ArrayList<IMetsElement>();
        if (metsElement.getModel().contains(NdkPlugin.MODEL_NDK_PAGE)) {
            addDmdSec(metsElement);
        }
        sourceElements.add(sourceElement);
        insertPage(physicalDiv, metsElement, pageCounter, sourceElements, pageIndex);
    }

    /**
     * Inserts Page structure to the mets
     *
     * @param physicalDiv
     * @param metsElement
     * @param pageCounter
     * @param sourceElements
     * @param pageIndex
     * @throws MetsExportException
     */
    private void insertPage(DivType physicalDiv, IMetsElement metsElement, int pageCounter, List<IMetsElement> sourceElements, int pageIndex) throws MetsExportException {
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
        validatePageIndexAndNumber(metsElement, pageIndex);
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
                FileType fileType = prepareFileType(pageCounter, streamName, fileNames, mimeTypes, metsElement, outputFileNames, md5InfosMap);
                fileGrpPage.get(streamName).getFile().add(fileType);
                fileGrpMap.get(streamName).getFile().add(fileType);
                Fptr fptr = new Fptr();
                fptr.setFILEID(fileType);
                pageDiv.getFptr().add(fptr);
                if (Const.ALTO_GRP_ID.equals(streamName)) {
                    metsElement.setAltoFile(fileType);
                }
            } else {
                if (isMandatoryStream(streamName)&& (!metsElement.getMetsContext().isAllowNonCompleteStreams())) {
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
            addMappingPageStruct(structLinkMapping, transformSupplementId(sourceElement.getModsElementID()));
        }
    }

    private void validatePageIndexAndNumber(IMetsElement metsElement, int pageIndexExpected) throws MetsExportException {
        ModsDefinition mods = getMods(metsElement);
        int pageIndex = getPageIndex(mods);
        if (!containPageNumber(mods)) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Strana nemá vyplněný číslo stránky.", false, null);
        }
        if (pageIndexExpected != pageIndex) {
            if (pageIndex == -1) {
                throw new MetsExportException(metsElement.getOriginalPid(), "Strana nemá vyplněný index strany. Očekávaná hodnota " + pageIndexExpected + ".", false, null);
            } else {
                throw new MetsExportException(metsElement.getOriginalPid(), "Strana má neočekávaný index strany. Očekávaná hodnota " + pageIndexExpected + ", ale byl nalezen index "+ pageIndex + ".", false, null);
            }
        }
    }

    private int getPageIndex(ModsDefinition mods) {
        if (mods.getPart().size() > 0) {
            for (PartDefinition part : mods.getPart()) {
                for (DetailDefinition detail : part.getDetail()) {
                    if ("pageIndex".equals(detail.getType()) && detail.getNumber().size() > 0) {
                        return Integer.valueOf(detail.getNumber().get(0).getValue());
                    }
                }
            }
        }
        return -1;
    }

    private boolean containPageNumber(ModsDefinition mods) {
        if (mods.getPart().size() > 0) {
            for (DetailDefinition detail : mods.getPart().get(0).getDetail()) {
                if ("pageNumber".equals(detail.getType()) && detail.getNumber().size() > 0) {
                    return !detail.getNumber().get(0).getValue().isEmpty();
                }
                if ("page number".equals(detail.getType()) && detail.getNumber().size() > 0) {
                    return !detail.getNumber().get(0).getValue().isEmpty();
                }
            }
        }
        return false;
    }

    private ModsDefinition getMods(IMetsElement metsElement) throws MetsExportException {
        try {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            FedoraObject foNew = dom.find(metsElement.getOriginalPid(), null);
            XmlStreamEditor streamEditorNew = foNew.getEditor(FoxmlUtils.inlineProfile(
                    MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
            ModsStreamEditor modsStreamEditorNew = new ModsStreamEditor(streamEditorNew, foNew);
            return modsStreamEditorNew.read();
        } catch (DigitalObjectException ex) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Nepodařilo se zkontrolovat index a pořadí strany", false, null);
        }
    }

    protected boolean isMandatoryStream(String streamName) {
        return Const.mandatoryStreams.contains(streamName);
    }

    public class StructLinkMapping {
        public String pageDiv;
        public BigInteger pageOrder;

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

    private void addMappingAudioPageStruct(StructLinkMapping structLinkMapping, String fromDiv) {
        if (structToAudioPageMap.get(fromDiv) == null) {
            structToAudioPageMap.put(fromDiv, new ArrayList<StructLinkMapping>());
        }
        structToAudioPageMap.get(fromDiv).add(structLinkMapping);
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
            for (String structFrom : structToAudioPageMap.keySet()) {
                if (structToAudioPageMap.get(structFrom) != null) {
                    for (StructLinkMapping structLinkMapping : structToAudioPageMap.get(structFrom)) {
                        if (audioPageOrderToDivMap.get(structLinkMapping) != null) {
                            SmLink smLink = new SmLink();
                            smLink.setFrom(structFrom);
                            smLink.setTo(audioPageOrderToDivMap.get(structLinkMapping));
                            structLink.getSmLinkOrSmLinkGrp().add(smLink);
                        } else {
                            throw new MetsExportException("Unable to find DIV for audio page order:" + structLinkMapping.pageDiv + " " + structLinkMapping.pageOrder, false, null);
                        }
                    }
                }
            }
        }
    }

    /**
     * Creates a directory for package
     *
     * @param metsElement
     * @return
     * @throws MetsExportException
     */
    protected File createPackageDir(IMetsElement metsElement) throws MetsExportException {
        if (metsElement.getMetsContext().getPackageID() == null) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Package ID is null", false, null);
        }
        File file = new File(metsElement.getMetsContext().getOutputPath() + File.separator + metsElement.getMetsContext().getPackageID());
        if (file.exists()) {
            if (file.isDirectory()) {
                createDirectoryStructure(metsElement);
                return file;
            } else {
                throw new MetsExportException(metsElement.getOriginalPid(), "File:" + file.getAbsolutePath() + " exists, but is not directory", false, null);
            }
        } else {
            file.mkdir();
            createDirectoryStructure(metsElement);
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
        divType.setID(transformSupplementId(metsElement.getModsElementID()));
        divType.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        divType.setTYPE(transformSupplement(Const.typeNameMap.get(metsElement.getElementType())));
        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);
        int pageIndex = 1;
        containChildren(metsElement);
        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement, pageIndex++);
                pageCounter++;
            } else if (Const.PICTURE.equals(element.getElementType())) {
                insertPicture(divType, physicalDiv, element);
            } else
                throw new MetsExportException(element.getOriginalPid(), "Expected Page or Picture, got:" + element.getElementType(), false, null);
        }
    }

    private String transformSupplementId(String modsElementID) {
        if (modsElementID != null && !modsElementID.isEmpty() && modsElementID.startsWith(Const.MODS_SUPPLEMENT)) {
            modsElementID = Const.SUPPLEMENT + "_" + modsElementID.substring(6);
        }
        return modsElementID;
    }


    /**
     *  Transform value SUPPL to SUPPLMENENT
     */
    private String transformSupplement(String modelType) {
        if (Const.MODS_SUPPLEMENT.equals(modelType)) {
            return Const.SUPPLEMENT;
        }
        return modelType;
    }

    /**
     * Inserts Volume structure to the mets
     *
     * @param logicalDiv
     * @param physicalDiv
     * @param metsElement
     * @param isMultiPartMonograph
     * @throws MetsExportException
     */
    protected void insertVolume(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement, boolean isMultiPartMonograph) throws MetsExportException {
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
            physicalDiv.getDMDID().clear();
            physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
        } else {
            divType.setTYPE(Const.VOLUME);
        }

        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);
        int pageIndex = 1;
        if (!metsElement.getModel().contains(Const.NDK_EBORN_MODELS_IDENTIFIER)) {
            containChildren(metsElement);
        }
        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.ISSUE.equals(element.getElementType())) {
                element.getMetsContext().setPackageID(MetsUtils.getPackageID(element));
                insertIssue(physicalDiv, divType, element);
                continue;
            } else
            if (Const.SUPPLEMENT.equals(element.getElementType())) {
                if (!Const.MONOGRAPH_UNIT.equals(metsElement.getElementType())) {
                    element.getMetsContext().setPackageID(MetsUtils.getPackageID(element));
                }
                insertSupplement(divType, physicalDiv, element);
            } else
            if (Const.PAGE.equals(element.getElementType())) {

                if (Const.PERIODICAL_VOLUME.equals(metsElement.getElementType())) {
                    throw new MetsExportException(metsElement.getOriginalPid(), "Moodel " + metsElement.getElementType() + " nesmí mít přímo pod sebou model strana.", false, null);
                }
                insertPage(physicalDiv, element, pageCounter, metsElement, pageIndex++);
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

    private void containChildren(IMetsElement metsElement) throws MetsExportException {
        if (metsElement.getChildren().size() == 0) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Moodel " + metsElement.getElementType() + " s identifikátorem " + metsElement.getOriginalPid() + " neobsahuje žádné navázané objekty, proto nebyl export úspěšný.", false, null);
        }
    }

    /**
     * Inserts Monograph structure to the mets
     *
     * @param metsElement
     * @throws MetsExportException
     */
    protected void insertMonograph(IMetsElement metsElement) throws MetsExportException {
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
            metsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(metsElement));
            insertVolume(logicalDiv, physicalDiv, metsElement, false);
        } else {
            metsElement.setModsElementID("TITLE_0001");
            titleCounter++;
            addDmdSec(metsElement);
            logicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            containChildren(metsElement);
            int pageIndex = 1;
            for (IMetsElement childMetsElement : metsElement.getChildren()) {
                if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                    continue;
                } else
                if (Const.SUPPLEMENT.equals(childMetsElement.getElementType())) {
                    childMetsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(childMetsElement));
                    insertSupplement(logicalDiv, physicalDiv, childMetsElement);
                } else
                if (Const.PAGE.equals(childMetsElement.getElementType())) {
                    pageCounter++;
                    insertPage(physicalDiv, childMetsElement, pageCounter, metsElement, pageIndex++);
                } else if (Const.CHAPTER.equals(childMetsElement.getElementType())) {
                    insertChapter(logicalDiv, physicalDiv, childMetsElement, chapterCounter);
                    chapterCounter++;
                } else if (Const.MONOGRAPH_MULTIPART.equals(childMetsElement.getElementType())) {
                    insertMonographTitle(logicalDiv, physicalDiv, childMetsElement, titleCounter);
                    titleCounter++;
                } else
                    throw new MetsExportException(childMetsElement.getOriginalPid(), "Expected Supplement, Monograph unit, Monograph Title, Chapter or Page, got:" + childMetsElement.getElementType(), false, null);
            }

            for (IMetsElement childMetsElement : metsElement.getChildren()) {
                if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                    childMetsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(childMetsElement));
                    insertVolume(logicalDiv, physicalDiv, childMetsElement, true);
                }
            }
        }
    }

    protected void insertMonographTitle(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement, int counter) throws MetsExportException {
        metsElement.setModsElementID("TITLE_000" + counter);
        addDmdSec(metsElement);
        DivType divType = new DivType();
        divType.setID("MONOGRAPH_000" + counter);
        divType.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        if (Const.MONOGRAPH_MULTIPART.equals(metsElement.getElementType())) {
            divType.setTYPE(Const.MONOGRAPH);
            physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
        }
        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);
        containChildren(metsElement);
        int pageIndex = 1;
        for (IMetsElement childMetsElement : metsElement.getChildren()) {
            if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                continue;
            } else
            if (Const.SUPPLEMENT.equals(childMetsElement.getElementType())) {
                childMetsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(childMetsElement));
                insertSupplement(divType, physicalDiv, childMetsElement);
            } else
            if (Const.PAGE.equals(childMetsElement.getElementType())) {
                pageCounter++;
                insertPage(physicalDiv, childMetsElement, pageCounter, metsElement, pageIndex++);
            } else if (Const.CHAPTER.equals(childMetsElement.getElementType())) {
                insertChapter(divType, physicalDiv, childMetsElement, chapterCounter);
                chapterCounter++;
            } else if (Const.PICTURE.equals(childMetsElement.getElementType())) {
                insertPicture(divType, physicalDiv, childMetsElement);
            } else if (Const.MONOGRAPH_MULTIPART.equals(childMetsElement.getElementType())) {
                insertMonographTitle(divType, physicalDiv, childMetsElement, titleCounter);
                titleCounter++;
            } else
                throw new MetsExportException(childMetsElement.getOriginalPid(), "Expected Supplement, Monograph unit, Monograph Title, Chapter or Page, got:" + childMetsElement.getElementType(), false, null);
        }

        for (IMetsElement childMetsElement : metsElement.getChildren()) {
            if (Const.MONOGRAPH_UNIT.equals(childMetsElement.getElementType())) {
                childMetsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(childMetsElement));
                insertVolume(divType, physicalDiv, childMetsElement, true);
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

        containChildren(metsElement);
        for (IMetsElement childMetsElement : metsElement.getChildren()) {
            if (Const.PERIODICAL_VOLUME.equals(childMetsElement.getElementType())) {
                insertVolume(divType, physicalDiv, childMetsElement, false);
            } else if (Const.ISSUE.equals(childMetsElement.getElementType())) {
                childMetsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(childMetsElement));
                insertIssue(physicalDiv, divType, childMetsElement);
            } else if (Const.SUPPLEMENT.equals(childMetsElement.getElementType())) {
                childMetsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(childMetsElement));
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
     * @implNote This only creates number sequence from Mods (Part->Extent->Start:Part->Extent->End).
     * @implNote When the pages not exist the exception is thrown later by @link{ {@link #addStructLink()} }
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
        elementDivType.setID(metsElement.getModsElementID());
        elementDivType.setORDER(BigInteger.valueOf(counterIntPart));

        elementDivType.setLabel3(metsElement.getLabel());
        elementDivType.setTYPE(metsElement.getElementType());
        elementDivType.getDMDID().add(metsElement.getModsMetsElement());

        logicalDiv.getDiv().add(elementDivType);
        addInternalElements(elementDivType, metsElement);
        for (IMetsElement element : metsElement.getChildren()) {
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
    protected void insertChapter(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement, int counterIntPart) throws MetsExportException {
        if (!Const.CHAPTER.equals(metsElement.getElementType())) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Expected chapter got " + metsElement.getElementType(), false, null);
        }
        addDmdSec(metsElement);
        DivType elementDivType = new DivType();
        elementDivType.setID(metsElement.getModsElementID());
        elementDivType.setORDER(BigInteger.valueOf(counterIntPart));

        elementDivType.setLabel3(metsElement.getLabel());
        elementDivType.setTYPE(metsElement.getElementType());
        elementDivType.getDMDID().add(metsElement.getModsMetsElement());

        logicalDiv.getDiv().add(elementDivType);
        addInternalElements(elementDivType, metsElement);
        addStructLinkFromMods(metsElement);
        for (IMetsElement element : metsElement.getChildren()) {
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
            } else if (Const.SOUND_COLLECTION.equalsIgnoreCase(rootElement.getElementType())) {
                insertSoundCollection(rootElement);
            } else
                throw new MetsExportException(rootElement.getOriginalPid(), "Unknown type:" + rootElement.getElementType() + " model:" + rootElement.getModel(), false, null);


            if (metsElement.getMetsContext().getPackageID()==null) {
                throw new MetsExportException(metsElement.getOriginalPid(),"Package ID is null",false,null);
            }

            if (metsElement.getMetsContext().getPackageDir() == null) {
                File packageDirFile = createPackageDir(metsElement);
                metsElement.getMetsContext().setPackageDir(packageDirFile);
            }

            saveMets(mets, new File(metsElement.getMetsContext().getPackageDir().getAbsolutePath() + File.separator +"mets_"+ MetsUtils.removeNonAlpabetChars(metsElement.getMetsContext().getPackageID()) + ".xml"), metsElement);
        } finally {
            JhoveUtility.destroyConfigFiles(metsElement.getMetsContext().getJhoveContext());
        }
    }

    private void insertSoundCollection(IMetsElement metsElement) throws MetsExportException {
        mets.setTYPE("Sound recording");
        addDmdSec(metsElement);
        DivType logicalDiv = new DivType();
        logicalStruct.setDiv(logicalDiv);
        DivType physicalDiv = new DivType();
        physicalStruct.setDiv(physicalDiv);

        physicalDiv.setLabel3(metsElement.getLabel());
        physicalDiv.setID("DIV_P_0000");
        physicalDiv.setTYPE("sound collection");

        logicalDiv.setLabel3(metsElement.getLabel());
        logicalDiv.setTYPE(metsElement.getElementType());
        logicalDiv.setID(metsElement.getElementID());
        logicalDiv.getDMDID().add(metsElement.getModsMetsElement());
        physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
        metsElement.getMetsContext().setPackageID(MetsUtils.getPackageID(metsElement));
        int pageIndex = 1;
        for (IMetsElement childMetsElement : metsElement.getChildren()) {
            if (Const.SOUND_RECORDING.equals(childMetsElement.getElementType())) {
                insertSoundRecording(logicalDiv, physicalDiv, childMetsElement);
            } else if (Const.PAGE.equals(childMetsElement.getElementType())) {
                insertPage(physicalDiv, childMetsElement, pageCounter, metsElement, pageIndex++);
                pageCounter++;
            } else if (Const.SUPPLEMENT.equals(childMetsElement.getElementType())) {
                insertAudioSupplement(logicalDiv, physicalDiv, childMetsElement);
            } else {
                throw new MetsExportException(childMetsElement.getOriginalPid(), "Expected SoundRecording or Suplement, got:" + childMetsElement.getElementType(), false, null);
            }
        }

    }

    private void insertSoundRecording(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement) throws MetsExportException{
        addDmdSec(metsElement);
        if (physicalDiv.getID() == null) {
            String id = "DIV_P_"+ metsElement.getElementID();
            physicalDiv.setID(id);
            physicalDiv.setLabel3(metsElement.getLabel());
            physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            physicalDiv.setTYPE(metsElement.getElementType());
        }

        DivType divType = new DivType();
        divType.setID(metsElement.getElementID());
        divType.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        divType.setTYPE(metsElement.getElementType());
        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);
        int pageIndex = 1;
        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.SOUND_PAGE.equals(element.getElementType())){
                insertAudioPage(physicalDiv, element, audioPageCounter, metsElement);
                audioPageCounter++;
            } else if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement, pageIndex++);
                pageCounter++;
            } else if (Const.SOUND_PART.equals(element.getElementType())) {
                insertSoundPart(logicalDiv, physicalDiv, element);
            } else if (Const.SUPPLEMENT.equals(element.getElementType())) {
                insertAudioSupplement(logicalDiv, physicalDiv, element);
            } else
                throw new MetsExportException(element.getOriginalPid(), "Expected Supplement, SoundPart, AudioPage got:" + element.getElementType(), false, null);
        }
    }

    private void insertAudioPage(DivType physicalDiv, IMetsElement metsElement, int audioPageCounter, IMetsElement sourceElement) throws MetsExportException {
        List<IMetsElement> sourceElements = new ArrayList<IMetsElement>();
        sourceElements.add(sourceElement);
        insertAudioPage(physicalDiv, metsElement, audioPageCounter, sourceElements, sourceElement);
    }

    private void insertAudioPage(DivType physicalDiv, IMetsElement metsElement, int audioPageCounter, List<IMetsElement> sourceElements, IMetsElement sourceElement) throws MetsExportException{
        if (metsElement.getMetsContext().getPackageDir() == null) {
            File packageDir = createPackageDir(metsElement);
            metsElement.getMetsContext().setPackageDir(packageDir);
        }
        HashMap<String, String> outputFileName = new HashMap<String, String>();
        if (!Const.SOUND_PAGE.equals(metsElement.getElementType())) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Expected audiopage, got " + metsElement.getElementType(), false, null);
        }
        HashMap<String, FileGrp> fileGrpAudioPage = MetsUtils.initAudioFileGroups();
        DivType audioPageDiv = new DivType();
        physicalDiv.getDiv().add(audioPageDiv);
        fillAudioPageIndexOrder(metsElement, audioPageDiv, sourceElement);
        String ID = "DIV_AUDIO_" + metsElement.getElementID().replace("SOUNDPAGE_", "");
        audioPageDiv.setID(ID);
        HashMap<String, Object> fileNames = new HashMap<String, Object>();
        HashMap<String, String> mimeTypes = new HashMap<String, String>();
        HashMap<String, XMLGregorianCalendar> createDates = new HashMap<String, XMLGregorianCalendar>();
        HashMap<String, FileMD5Info> md5InfosMap = new HashMap<String, FileMD5Info>();
        processAudioPageFiles(metsElement, fileNames, mimeTypes, createDates, md5InfosMap);
        String streamNameExtension = "";
        for (String streamName : Const.audioStremMapping.keySet()) {
            if (Const.AUDIO_MC_GRP_ID_FLAC.equals(streamName)) {
                streamNameExtension = Const.AUDIO_MC_GRP_ID;
            } else if (Const.AUDIO_UC_GRP_ID_OGG.equals(streamName)) {
                streamNameExtension = Const.AUDIO_UC_GRP_ID;
            } else {
                streamNameExtension = streamName;
            }
            if (fileNames.containsKey(streamName)) {
                FileType fileType = prepareAudioFileType(audioPageCounter, streamName, streamNameExtension, fileNames, mimeTypes, metsElement, outputFileName, md5InfosMap);
                fileGrpAudioPage.get(streamNameExtension).getFile().add(fileType);
                fileGrpMap.get(streamNameExtension).getFile().add(fileType);
                Fptr fptr = new Fptr();
                fptr.setFILEID(fileType);
                audioPageDiv.getFptr().add(fptr);
            } else {
                if (Const.audioMandatoryStreams.contains(streamName) && !metsElement.getMetsContext().isAllowNonCompleteStreams()) {
                    throw new MetsExportException(metsElement.getOriginalPid(), "Stream: " + streamName + " is missing", false, null);
                }

            }
        }

        generateTechMetadata(metsElement, fileNames, audioPageCounter, fileGrpAudioPage, mimeTypes, audioPageDiv, outputFileName, md5InfosMap);

        StructLinkMapping structLinkMapping = new StructLinkMapping();
        structLinkMapping.pageDiv = metsElement.getParent().getModsElementID();
        structLinkMapping.pageOrder = audioPageDiv.getORDER();
        audioPageOrderToDivMap.put(structLinkMapping, ID);
        for (IMetsElement sourceElm : sourceElements) {
            addMappingAudioPageStruct(structLinkMapping, sourceElm.getModsElementID());
        }
    }

    private void processAudioPageFiles(IMetsElement metsElement, HashMap<String, Object> fileNames, HashMap<String, String> mimeTypes, HashMap<String, XMLGregorianCalendar> createDates, HashMap<String, FileMD5Info> md5InfosMap) throws MetsExportException {
        for (String streamName : Const.audioStremMapping.keySet()) {
            if (metsElement.getMetsContext().getFedoraClient() != null) {
                try {
                    for (String dataStream : Const.audioStremMapping.get(streamName)) {
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
                for (String dataStream : Const.audioStremMapping.get(streamName)) {
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

    private void fillAudioPageIndexOrder(IMetsElement metsElement, DivType audioPageDiv, IMetsElement sourceElement) throws MetsExportException {
        Node partNode = MetsUtils.xPathEvaluateNode(metsElement.getModsStream(), "*[local-name()='modsCollection']/*[local-name()='mods']/*[local-name()='part']");
        if (partNode == null) {
            partNode = MetsUtils.xPathEvaluateNode(metsElement.getModsStream(), "*[local-name()='mods']/*[local-name()='part']");
        }
        audioPageDiv.setTYPE(transformType(sourceElement.getElementType()));

        NodeList nodeList = partNode.getChildNodes();
        for (int i = 0; i < nodeList.getLength(); i++) {
            if (nodeList.item(i).getLocalName() != null && nodeList.item(i).getLocalName().equalsIgnoreCase("detail")) {
                Node numberNode = nodeList.item(i).getChildNodes().item(0).getFirstChild();
                if (nodeList.item(i).getAttributes().getNamedItem("type").getNodeValue().equalsIgnoreCase("pageNumber")) {
                    audioPageDiv.setORDERLABEL(numberNode.getNodeValue());
                }
                if (nodeList.item(i).getAttributes().getNamedItem("type").getNodeValue().equalsIgnoreCase("pageIndex")) {
                    audioPageDiv.setORDER(new BigInteger(numberNode.getNodeValue()));
                }
            }
        }
    }

    private String transformType(String elementName) {
        if (Const.SOUND_PART.equalsIgnoreCase(elementName)) {
            return Const.NDK_EXPORT_STRUCTMAP_SOUNDPART;
        } else if (Const.SOUND_RECORDING.equalsIgnoreCase(elementName)) {
            return Const.NDK_EXPORT_STRUCTMAP_SOUNDRECORDING;
        } else if (Const.SOUND_COLLECTION.equalsIgnoreCase(elementName)) {
            return Const.NDK_EXPORT_STRUCTMAP_SOUNDCOLLECTION;
        } else return null;
    }

    private void insertSoundPart(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement) throws MetsExportException {
        addDmdSec(metsElement);
        if (physicalDiv.getID() == null) {
            String id = "DIV_P_"+ metsElement.getElementID();
            physicalDiv.setID(id);
            physicalDiv.setLabel3(metsElement.getLabel());
            physicalDiv.getDMDID().add(metsElement.getModsMetsElement());
            physicalDiv.setTYPE(metsElement.getElementType());
        }

        DivType divType = new DivType();
        divType.setID(metsElement.getElementID());
        divType.setLabel3(metsElement.getMetsContext().getRootElement().getLabel());
        divType.setTYPE(metsElement.getElementType());
        divType.getDMDID().add(metsElement.getModsMetsElement());
        logicalDiv.getDiv().add(divType);
        int pageIndex = 1;
        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.SOUND_PAGE.equals(element.getElementType())) {
                insertAudioPage(physicalDiv, element, audioPageCounter, metsElement);
                audioPageCounter++;
            } else if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement, pageIndex++);
                pageCounter++;
            } else if (Const.SUPPLEMENT.equals(element.getElementType())) {
                insertAudioSupplement(logicalDiv, physicalDiv, element);
            }
            else
                throw new MetsExportException(element.getOriginalPid(), "Expected AudioPage or Supplement, got:" + element.getElementType(), false, null);
        }
    }

    private void insertAudioSupplement(DivType logicalDiv, DivType physicalDiv, IMetsElement metsElement) throws MetsExportException {
        addDmdSec(metsElement);
        if (physicalDiv.getID() == null) {
            String id = "DIV_P_"+ metsElement.getElementID();
            physicalDiv.setID(id);
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
        int pageIndex = 1;
        for (IMetsElement element : metsElement.getChildren()) {
            if (Const.PAGE.equals(element.getElementType())) {
                insertPage(physicalDiv, element, pageCounter, metsElement, pageIndex++);
                pageCounter++;
            } else
                throw new MetsExportException(element.getOriginalPid(), "Expected Page, got:" + element.getElementType(), false, null);
        }
    }
}
