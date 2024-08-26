/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.common.storage;


import edu.harvard.hul.ois.xml.ns.jhove.Property;

import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.audiopremis.AudioObjectFactory;
import cz.cas.lib.proarc.audiopremis.NkComplexType;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.process.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.process.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.NdkExportOptions;
import cz.cas.lib.proarc.common.process.export.mets.ObjectInfo;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.xml.ProArcPrefixNamespaceMapper;
import cz.cas.lib.proarc.common.xml.docmd.DocumentMd;
import cz.cas.lib.proarc.common.xml.ndktech.NdkTechnical;
import cz.cas.lib.proarc.mets.AmdSecType;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsConstants;
import cz.cas.lib.proarc.mets.MetsType;
import cz.cas.lib.proarc.mix.Mix;
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
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import org.apache.commons.lang.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildAkubraContext;
import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildFedoraContext;

/**
 * Edits technical metadata in Coding history format.
 *
 * @author Lukas Sykora
 */
public class PremisEditor {

    public static final String NDK_ARCHIVAL_ID = "NDK_ARCHIVAL_PREMIS";
    public static final String RAW_ID = "RAW_PREMIS";
    private static final String METS_FORMAT_URI = MetsConstants.NS_METS;

    private final XmlStreamEditor editor;
    private final ProArcObject object;
    private final DatastreamProfile profileTemplate;

    private static final Logger LOG = Logger.getLogger(PremisEditor.class.getName());

    public static DatastreamProfile rawProfile() {
        return FoxmlUtils.managedProfile(RAW_ID, METS_FORMAT_URI, "Technical metadata (premis) for RAW stream.");
    }

    public static DatastreamProfile ndkArchivalProfile() {
        return FoxmlUtils.managedProfile(NDK_ARCHIVAL_ID, METS_FORMAT_URI, "Technical metadata (premis) for NDK_ARCHIVAL stream.");
    }

    /**
     * Gets editor to manage NDK_ARCHIVAL datastream metadata.
     */
    public static PremisEditor ndkArchival(ProArcObject object) {
        return new PremisEditor(object, ndkArchivalProfile());
    }

    /**
     * Gets editor to manage RAW datastream metadata. AES does not contain info
     * about the scanner.
     */
    public static PremisEditor raw(ProArcObject object) {
        return new PremisEditor(object, rawProfile());
    }

    public PremisEditor(ProArcObject object, DatastreamProfile profile) {
        this.editor = object.getEditor(profile);
        this.object = object;
        this.profileTemplate = profile;
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    /**
     * Gets persisted Premis.
     * @return PropertyType or {@code null}
     * @throws DigitalObjectException failure
     */
    public Mets read() throws DigitalObjectException {
        Source src = editor.read();
        Mets result = null;
        if (src != null) {
            result = MetsUtils.unmarshal(src, Mets.class);
        }
        return result;
    }

    /**
     * Gets persisted Premis as {@link Property} class.
     * @return Property or {@code null}
     * @throws DigitalObjectException failure
     */
    public Mets readMets() throws DigitalObjectException {
        Source src = editor.read();
        Mets result = null;
        if (src != null) {
            result = MetsUtils.unmarshal(src, Mets.class);
        }
        return result;
    }

    public String readAsString() throws DigitalObjectException {
        Mets mets = readMets();
        if (mets != null) {
            return MetsUtils.toXml(mets, true);
        }
        return null;
    }

    public void write(Mets mets, long timestamp, String msg) throws DigitalObjectException {
        EditorResult result = editor.createResult();
        MetsUtils.marshal(result, mets, true);
        editor.write(result, timestamp, msg);
    }

    /**
     * Generates and writes Coding History for the passed content.
     *
     * @param content file containing e.g. an image
     * @param jhoveCtx jHove context
     * @param timestamp timestamp
     * @param msg log message
     * @throws DigitalObjectException failure
     */
    public void write(File content, JhoveContext jhoveCtx, long timestamp, String msg) throws DigitalObjectException {
//        try {
//            Property premis = JhoveUtility.getCodingHistory(content, jhoveCtx, null, null).getCodingHistory();
//            if (codingHistory == null) {
//                LOG.warning("jHove cannot generate Coding history for " + content.toString() + ".");
//                //throw new DigitalObjectException(object.getPid(), null, profileTemplate.getDsID(), "jHove cannot generate Coding history for " + content.toString(), null);
//            } else {
//                CodingHistoryMapper mapper = new CodingHistoryMapper();
//                mapper.update(codingHistory);
//                write(codingHistory, timestamp, msg);
//            }
//        } catch (DigitalObjectException ex) {
//            throw ex;
//        } catch (Exception ex) {
//            throw new DigitalObjectException(
//                    object.getPid(), null, profileTemplate.getDsID(), null, ex);
//        }
        return;
    }

    public Mets generate(ProArcObject fobject, AppConfiguration config, AkubraConfiguration akubraConfiguration, String importFile) throws DigitalObjectException {
        try {
            MetsElement metsElement = getElement(fobject.getPid(), config, akubraConfiguration);

            Mets amdSecMets = new Mets();
            AmdSecType amdSec = new AmdSecType();
            amdSec.setID(metsElement.getElementID());
            amdSecMets.getAmdSec().add(amdSec);

            Mets deviceMets = MetsElementVisitor.getScannerMets(metsElement);
            HashMap<String, FileMD5Info> md5InfosMap = createMd5InfoMap(metsElement);
            addPremisToAmdSec(config.getNdkExportOptions(), amdSec, md5InfosMap, metsElement, null, deviceMets, null);
            return amdSecMets;
        } catch (Exception e) {
            throw new DigitalObjectException(fobject.getPid(), "Nepodarilo se vytvorit Technicka metadata");
        }
    }

    private HashMap<String, FileMD5Info> createMd5InfoMap(MetsElement metsElement) throws DigitalObjectException {
        HashMap<String, FileMD5Info> md5InfosMap = new HashMap<>();

        DatastreamType rawDS = FoxmlUtils.findDatastream(metsElement.getSourceObject(), "RAW");
        if (rawDS != null) {
            FileMD5Info rawinfo = new FileMD5Info();
            rawinfo.setMimeType(rawDS.getDatastreamVersion().get(0).getMIMETYPE());
            rawinfo.setCreated(rawDS.getDatastreamVersion().get(0).getCREATED());
            rawinfo.setSize(rawDS.getDatastreamVersion().get(0).getSIZE());
            md5InfosMap.put("RAW", rawinfo);
        }

        for (String streamName : Const.streamMapping.keySet()) {
            if (Storage.FEDORA.equals(metsElement.getMetsContext().getTypeOfStorage())) {
                try {
                    for (String dataStream : Const.streamMapping.get(streamName)) {
                        rawDS = FoxmlUtils.findDatastream(metsElement.getSourceObject(), dataStream);
                        if (rawDS != null) {
                            FileMD5Info fileMd5Info;
                            if (md5InfosMap.get(streamName) == null) {
                                fileMd5Info = new FileMD5Info();
                                md5InfosMap.put(streamName, fileMd5Info);
                            } else {
                                fileMd5Info = md5InfosMap.get(streamName);
                            }
                            fileMd5Info.setCreated(rawDS.getDatastreamVersion().get(0).getCREATED());
                            fileMd5Info.setMimeType(rawDS.getDatastreamVersion().get(0).getMIMETYPE());
                            fileMd5Info.setSize(rawDS.getDatastreamVersion().get(0).getSIZE());
                        }
                    }
                } catch (Exception ex) {
                    throw new DigitalObjectException(metsElement.getOriginalPid(), "Error while getting file datastreams for " + metsElement.getOriginalPid(), ex);
                }
            } else if (Storage.AKUBRA.equals(metsElement.getMetsContext().getTypeOfStorage()) || Storage.LOCAL.equals(metsElement.getMetsContext().getTypeOfStorage())) {
                List<DatastreamType> datastreams = metsElement.getSourceObject().getDatastream();
                for (String dataStream : Const.streamMapping.get(streamName)) {
                    for (DatastreamType ds : datastreams) {
                        if (MetsUtils.equalDataStreams(ds.getID(), dataStream)) {
                            Iterator<DatastreamVersionType> dvIter = ds.getDatastreamVersion().iterator();
                            while (dvIter.hasNext()) {
                                DatastreamVersionType dv = dvIter.next();
                                if (dv.getContentLocation() != null) {
                                    FileMD5Info fileMd5Info;
                                    if (md5InfosMap.get(streamName) == null) {
                                        fileMd5Info = new FileMD5Info();
                                        md5InfosMap.put(streamName, fileMd5Info);
                                    } else {
                                        fileMd5Info = md5InfosMap.get(streamName);
                                    }
                                    fileMd5Info.setCreated(dv.getCREATED());
                                    fileMd5Info.setMimeType(dv.getMIMETYPE());
                                    fileMd5Info.setSize(dv.getSIZE());
                                }
                                if (dv.getBinaryContent() != null) {
                                    FileMD5Info fileMd5Info;
                                    if (md5InfosMap.get(streamName) == null) {
                                        fileMd5Info = new FileMD5Info();
                                        md5InfosMap.put(streamName, fileMd5Info);
                                    } else {
                                        fileMd5Info = md5InfosMap.get(streamName);
                                    }
                                    fileMd5Info.setCreated(dv.getCREATED());
                                    fileMd5Info.setMimeType(dv.getMIMETYPE());
                                    fileMd5Info.setSize(dv.getSIZE());
                                }
                            }
                            break;
                        }
                    }
                }
            }
        }
        return md5InfosMap;
    }

    public static void addPremisToAmdSec(NdkExportOptions options, AmdSecType amdSec, HashMap<String, FileMD5Info> md5InfosMap, IMetsElement metsElement, HashMap<String, MetsType.FileSec.FileGrp> amdSecFileGrpMap, Mets mets, Mix mixDevice) throws MetsExportException {
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
            if ("OBJ_001".equals(obj)) {
                addPremisNodeToMets(getPremisFile(metsElement, stream, md5InfosMap.get(stream), mixDevice, null, null), amdSec, obj, false, amdSecFileGrpMap);
            } else {
                addPremisNodeToMets(getPremisFile(metsElement, stream, md5InfosMap.get(stream), null, null, null), amdSec, obj, false, amdSecFileGrpMap);
            }
        }

        if (mets != null) {
            if (md5InfosMap.get(Const.AUDIO_RAW_GRP_ID) != null) {
                for (AmdSecType amd : mets.getAmdSec()) {
                    try {
                        addPremisNodeToMets(getPremisEvent(amd, metsElement, Const.AUDIO_RAW_GRP_ID, md5InfosMap.get(Const.AUDIO_RAW_GRP_ID), "capture/digitization", null), amdSec, "EVT_" + String.format("%03d", seqEvent), true, null);
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
        if (md5InfosMap.get(Const.RAW_GRP_ID) != null && options.getPremisEventTypeDeletion()) {
            addPremisNodeToMets(getPremisEvent(metsElement, Const.RAW_GRP_ID, md5InfosMap.get(Const.RAW_GRP_ID), "deletion/PS_deletion"), amdSec, "EVT_004", true, null);
        }

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

    public static void addPremisNodeToMets(Node premisNode, AmdSecType amdSec, String Id, boolean isDigiprov, HashMap<String, MetsType.FileSec.FileGrp> amdSecFileGrpMap) {
        MdSecType mdSec = new MdSecType();
        mdSec.setID(Id);
        MdSecType.MdWrap mdWrap = new MdSecType.MdWrap();
        mdWrap.setMIMETYPE("text/xml");
        mdWrap.setMDTYPE("PREMIS");
        MdSecType.MdWrap.XmlData xmlData = new MdSecType.MdWrap.XmlData();
        xmlData.getAny().add(premisNode);
        mdWrap.setXmlData(xmlData);
        mdSec.setMdWrap(mdWrap);
        if (isDigiprov) {
            amdSec.getDigiprovMD().add(mdSec);
        } else {
            amdSec.getTechMD().add(mdSec);
        }
        if ("OBJ_002".equals(Id) || ("EVT_002".equals(Id))) {
            if ((amdSecFileGrpMap != null) && (amdSecFileGrpMap.get(Const.MC_GRP_ID) != null) && (amdSecFileGrpMap.get(Const.MC_GRP_ID).getFile().get(0) != null)) {
                amdSecFileGrpMap.get(Const.MC_GRP_ID).getFile().get(0).getADMID().add(mdSec);
            }
        }
        if ("OBJ_003".equals(Id) || ("EVT_003".equals(Id))) {
            if ((amdSecFileGrpMap != null) && (amdSecFileGrpMap.get(Const.ALTO_GRP_ID) != null) && (amdSecFileGrpMap.get(Const.ALTO_GRP_ID).getFile().get(0) != null)) {
                amdSecFileGrpMap.get(Const.ALTO_GRP_ID).getFile().get(0).getADMID().add(mdSec);
            }
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
    public static Node getPremisFile(IMetsElement metsElement, String datastream, FileMD5Info md5Info, Mix mix, ObjectInfo objectInfo, String newId) throws MetsExportException {
        JAXBElement<PremisComplexType> jaxbPremix = PremisEditor.createPremisComplexType(metsElement, datastream, md5Info, mix, objectInfo, newId);

        JAXBContext jc;
        try {
            jc = JAXBContext.newInstance(PremisComplexType.class, DocumentMd.class, NdkTechnical.class);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document document = db.newDocument();

            // Marshal the Object to a Document
            Marshaller marshaller = jc.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
            marshaller.setProperty("com.sun.xml.bind.namespacePrefixMapper", new ProArcPrefixNamespaceMapper());
            marshaller.marshal(jaxbPremix, document);
            XPath xpath = XPathFactory.newInstance().newXPath();
            Node premisNode = (Node) xpath.compile("*[local-name()='premis']/*[local-name()='object']").evaluate(document, XPathConstants.NODE);
            return premisNode;
        } catch (Exception e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
        }
    }

    public static JAXBElement<PremisComplexType> createPremisComplexType(IMetsElement metsElement, String datastream, FileMD5Info md5Info, Mix mix, ObjectInfo objectInfo, String newId) throws MetsExportException {
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
        } else if (Const.OC_GRP_ID_CREATION.equals(datastream)) {
            preservation.setPreservationLevelValue("logical preservation");
            preservation.setPreservationLevelDateAssigned(MetsUtils.getCurrentDate().toXMLFormat());
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
        if (md5Info.getMimeType() != null && !md5Info.getMimeType().equals(md5Info.getFormatVersion())) {
            formatDesignation.setFormatVersion(md5Info.getFormatVersion() == null ? setFormatVersion(mix) : md5Info.getFormatVersion());
        } else {
            formatDesignation.setFormatVersion(setFormatVersion(mix));
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
        creatingApplication.getContent().add(factory.createCreatingApplicationName((objectInfo != null && objectInfo.getApplicationName() != null) ? objectInfo.getApplicationName() : "ProArc"));
        creatingApplication.getContent().add(factory.createCreatingApplicationVersion((objectInfo != null && objectInfo.getApplicationVersion() != null) ? objectInfo.getApplicationVersion() : metsElement.getMetsContext().getOptions().getVersion()));

        //creatingApplication.getContent().add(factory.createCreatingApplicationVersion(metsElement.getMetsContext().getProarcVersion()));
        creatingApplication.getContent().add(factory.createDateCreatedByApplication((objectInfo != null && objectInfo.getApplicationCreationDate() != null) ? objectInfo.getApplicationCreationDate() : MetsUtils.getCurrentDate().toXMLFormat()));
        if (objectInfo != null) {
            ExtensionComplexType extension = new ExtensionComplexType();
            characteristics.getObjectCharacteristicsExtension().add(extension);
            DocumentMd documentMd = getDocumentMd(objectInfo);
            if (documentMd != null) {
                extension.getAny().add(documentMd);
            }
        }

        RelationshipComplexType relationShip = new RelationshipComplexType();

        String relatedEventIdentifierValue = Const.dataStreamToEvent.get(datastream);
        if (newId != null) {
            relatedEventIdentifierValue.replace("001", newId);
        }

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
            eventObject.setRelatedEventIdentifierValue(relatedEventIdentifierValue);
            eventObject.setRelatedEventSequence(BigInteger.ONE);
            file.getRelationship().add(relationShip);
        } else {
            relationShip.setRelationshipType("creation");
            relationShip.setRelationshipSubType("created from");
            LinkingEventIdentifierComplexType eventIdentifier = new LinkingEventIdentifierComplexType();
            file.getLinkingEventIdentifier().add(eventIdentifier);
            eventIdentifier.setLinkingEventIdentifierType("ProArc_EventID");
            eventIdentifier.setLinkingEventIdentifierValue(relatedEventIdentifierValue);
        }

        String originalFile = MetsUtils.xPathEvaluateString(metsElement.getRelsExt(), "*[local-name()='RDF']/*[local-name()='Description']/*[local-name()='importFile']");
        if (originalFile == null || originalFile.isEmpty()) {
            originalFile = md5Info.getFileName();
        }
        String extension = Const.mimeToExtensionMap.get(md5Info.getMimeType());
        int position = originalFile.indexOf(".");
        originalFile = originalFile.substring(0, position) + extension;
        OriginalNameComplexType originalName = factory.createOriginalNameComplexType();
        originalName.setValue(originalFile);
        file.setOriginalName(originalName);
        return jaxbPremix;
    }

    private static DocumentMd getDocumentMd(ObjectInfo objectInfo) {
        if (!(objectInfo != null && objectInfo.getPageCount() != null && objectInfo.getLanguage() != null &&
                objectInfo.getImageCount() != null && objectInfo.getFonts() != null && objectInfo.getFilters() != null)) {
            DocumentMd documentMd = new DocumentMd();
            if (objectInfo.getPageCount() != null) {
                documentMd.setPageCount(objectInfo.getPageCount());
            }
            if (objectInfo.getLanguage() != null) {
                documentMd.setLanguage(Collections.singletonList(objectInfo.getLanguage()));
            }
            if (objectInfo.getFonts() != null && !objectInfo.getFonts().isEmpty()) {
                for (String font : objectInfo.getFonts()) {
                    DocumentMd.FontType fontType = new DocumentMd.FontType();
                    fontType.setFontName(font);
                    fontType.setValue(font);
                    fontType.setEmbedded(true);
                    documentMd.getFont().add(fontType);
                }
            }
            if (!(objectInfo.getFilters() != null && objectInfo.getImageCount() != null && objectInfo.getObjectCount() != null)) {
                NdkTechnical ndkTechnical = new NdkTechnical();
                DocumentMd.ExtensionType extensionType = new DocumentMd.ExtensionType();
                documentMd.setDocumentMetadataExtension(extensionType);
                extensionType.getAny().add(ndkTechnical);

                if (objectInfo.getFilters() != null) {
                    NdkTechnical.FiltersType filters = new NdkTechnical.FiltersType();
                    ndkTechnical.setFilters(filters);

                    for (String filterValue : objectInfo.getFilters()) {
                        filters.getFilter().add(filterValue);
                    }
                }

                if (objectInfo.getObjectCount() != null) {
                    ndkTechnical.setIndirectObjectsNumber(objectInfo.getObjectCount());
                }

                if (objectInfo.getImageCount() != null) {
                    ndkTechnical.setImagesCount(objectInfo.getImageCount());
                }
            }
            return documentMd;
        }
        return null;
    }

    private static String setFormatVersion(Mix mix) {
        if (mix != null && mix.getBasicDigitalObjectInformation() != null && mix.getBasicDigitalObjectInformation().getFormatDesignation() != null &&
                mix.getBasicDigitalObjectInformation().getFormatDesignation().getFormatVersion() != null && mix.getBasicDigitalObjectInformation().getFormatDesignation().getFormatVersion().getValue() != null) {
            return mix.getBasicDigitalObjectInformation().getFormatDesignation().getFormatVersion().getValue();
        } else {
            return "1.0";
        }
    }

    public static Node getPremisEvent(IMetsElement metsElement, String datastream, FileMD5Info md5Info, String eventDetail) throws MetsExportException {
        try {
            return getPremisEvent(null, metsElement, datastream, md5Info, eventDetail, null);
        } catch (Exception e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
        }
    }

    public static Node getPremisEvent(IMetsElement metsElement, String datastream, FileMD5Info md5Info, String eventDetail, String newId) throws MetsExportException {
        try {
            return getPremisEvent(null, metsElement, datastream, md5Info, eventDetail, newId);
        } catch (Exception e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
        }
    }

    public static Node getPremisEvent(IMetsElement metsElement, String datastream, FileMD5Info md5Info, String eventDetail, String newId, String eventDetailOutcome) throws MetsExportException {
        try {
            return getPremisEvent(null, metsElement, datastream, md5Info, eventDetail, newId, eventDetailOutcome);
        } catch (Exception e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
        }
    }

    private static Node getPremisEvent(AmdSecType amd, IMetsElement metsElement, String datastream, FileMD5Info md5Info, String eventDetail, String newId) throws Exception {
        return getPremisEvent(amd, metsElement, datastream, md5Info, eventDetail, newId, "successful");
    }

    private static Node getPremisEvent(AmdSecType amd, IMetsElement metsElement, String datastream, FileMD5Info md5Info, String eventDetail, String newId, String eventDetailOutcome) throws Exception {

        String relatedEventIdentifierValue = Const.dataStreamToEvent.get(datastream);
        if (newId != null) {
            relatedEventIdentifierValue.replace("001", newId);
        }
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
        eventIdentifier.setEventIdentifierValue(relatedEventIdentifierValue);
        EventOutcomeInformationComplexType eventInformation = new EventOutcomeInformationComplexType();
        event.getEventOutcomeInformation().add(eventInformation);
        if (eventDetailOutcome != null && !eventDetailOutcome.isEmpty()) {
            eventInformation.getContent().add(factory.createEventOutcome(eventDetailOutcome));
        } else {
            eventInformation.getContent().add(factory.createEventOutcome("successful"));
        }

        LinkingAgentIdentifierComplexType linkingAgentIdentifier = fillLinkingAgentIdentifier(amd);
        LinkingObjectIdentifierComplexType linkingObject = new LinkingObjectIdentifierComplexType();
        linkingObject.setLinkingObjectIdentifierType("ProArc_URI");
        linkingObject.setLinkingObjectIdentifierValue(Const.FEDORAPREFIX + metsElement.getOriginalPid() + "/" + Const.dataStreamToModel.get(datastream));
        event.getLinkingObjectIdentifier().add(linkingObject);
        event.getLinkingAgentIdentifier().add(linkingAgentIdentifier);
        JAXBContext jc = JAXBContext.newInstance(PremisComplexType.class);
        return createNode(jaxbPremix, jc, "*[local-name()='premis']/*[local-name()='event']");
    }

    private static LinkingAgentIdentifierComplexType fillLinkingAgentIdentifier(AmdSecType amd) {
        String identifierType;
        String identifierValue;
        String role;
        try {
            EventComplexType event = ((PremisComplexType) ((JAXBElement) amd.getDigiprovMD().get(0).getMdWrap().getXmlData().getAny().get(0)).getValue()).getEvent().get(0);
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

    public static Node getAgent(IMetsElement metsElement) throws MetsExportException {
        try {
            return getAgent(null, metsElement);
        } catch (Exception e) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while generating premis data", false, e);
        }
    }

    private static Node getAgent(AmdSecType amd, IMetsElement metsElement) throws Exception {
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

    private static AgentComplexType fillAgent(AmdSecType amd, ObjectFactory factory) {
        String agentType;
        String agentName;
        AgentComplexType agentComplexType = new AgentComplexType();
        try {
            AgentComplexType agent = ((PremisComplexType) ((JAXBElement) amd.getDigiprovMD().get(0).getMdWrap().getXmlData().getAny().get(0)).getValue()).getAgent().get(0);
            agentName = agent.getAgentName().get(0);
            agentType = agent.getAgentType();
            ExtensionComplexType extension = factory.createExtensionComplexType();
            agentComplexType.getAgentExtension().add(extension);
            extension.getAny().add(addNkNode(agent));
        } catch (Exception ex) {
            LOG.log(Level.FINE, "Can not get value from Premis, set defualt values");
            agentName = "ProArc";
            agentType = "software";
        }
        agentComplexType.getAgentName().add(agentName);
        agentComplexType.setAgentType(agentType);
        return agentComplexType;
    }

    private static Node addNkNode(AgentComplexType agent) throws Exception {
        NkComplexType nk = new NkComplexType();
        String manufacturer = "";
        String serialNumber = "";
        String settings = "";

        Element extension = (Element) agent.getAgentExtension().get(0).getAny().get(0);
        if (extension != null) {
            try {
                if ("manufacturer".equals(extension.getFirstChild().getLocalName())) {
                    manufacturer = extension.getFirstChild().getFirstChild().getNodeValue();
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

    private static Node createNode(JAXBElement jaxb, JAXBContext jc, String expression) throws Exception {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document document = db.newDocument();
        Marshaller marshaller = jc.createMarshaller();
        marshaller.marshal(jaxb, document);
        XPath xpath = XPathFactory.newInstance().newXPath();
        Node node = (Node) xpath.compile(expression).evaluate(document, XPathConstants.NODE);
        return node;
    }

    private MetsElement getElement(String pid, AppConfiguration config, AkubraConfiguration akubraConfiguration) throws IOException, MetsExportException {
        MetsContext metsContext = null;
        ProArcObject object = null;

        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            FedoraStorage fedoraStorage = FedoraStorage.getInstance(config);
            object = fedoraStorage.find(pid);
            metsContext = buildFedoraContext(object, null, null, fedoraStorage, config.getNdkExportOptions());
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
            object = akubraStorage.find(pid);
            metsContext = buildAkubraContext(object, null, null, akubraStorage, config.getNdkExportOptions());
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }

        DigitalObject dobj = MetsUtils.readFoXML(object.getPid(), metsContext);
        if (dobj == null) {
            return null;
        }
        return MetsElement.getElement(dobj, null, metsContext, true);
    }

}
