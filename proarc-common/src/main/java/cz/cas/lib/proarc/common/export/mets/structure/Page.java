/*
 * Copyright (C) 2013 Robert Simonovsky
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
import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.response.GetDatastreamsResponse;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;

import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.MimeType;
import cz.cas.lib.proarc.mets.AmdSecType;
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
import cz.cas.lib.proarc.mets.StructLinkType.SmLink;
import cz.cas.lib.proarc.mets.StructMapType;

/**
 * Java class representing Mets page
 * 
 * @author eskymo
 * 
 */
public class Page extends MetsElement {
    private final HashMap<String, Object> fileNames = new HashMap<String, Object>();
    private final List<FileType> fileTypes = new ArrayList<FileType>();
    private final HashMap<String, String> mimeTypes = new HashMap<String, String>();
    private final HashMap<String, String> outputFileNames = new HashMap<String, String>();
    private static HashMap<String, String> streamMapping = new HashMap<String, String>();
    public static HashMap<String, String> streamMappingFile = new HashMap<String, String>();
    private static HashMap<String, String> streamMappingPrefix = new HashMap<String, String>();
    private static Logger LOG = Logger.getLogger(Page.class.getName());

    private final String partType;
    private String outputDirectory;
    private final int seq;
    private FileType ALTOfile;

    private String pageNumber;

    private String pageIndex;

    /* Stream mapping */
    static {
        streamMapping.put("MC_IMGGRP", "FULL");
        streamMapping.put("UC_IMGGRP", "PREVIEW");
        streamMapping.put("ALTOGRP", "ALTO");
        streamMapping.put("TXTGRP", "TEXT_OCR");
        streamMapping.put("TECHMDGRP", "FULL_AMD");

        streamMappingPrefix.put("MC_IMGGRP", "MC");
        streamMappingPrefix.put("UC_IMGGRP", "UC");
        streamMappingPrefix.put("ALTOGRP", "ALTO");
        streamMappingPrefix.put("TXTGRP", "TXT");
        streamMappingPrefix.put("TECHMDGRP", "AMD_METS");

        streamMappingFile.put("MC_IMGGRP", "masterCopy");
        streamMappingFile.put("UC_IMGGRP", "userCopy");
        streamMappingFile.put("ALTOGRP", "ALTO");
        streamMappingFile.put("TXTGRP", "txt");
        streamMappingFile.put("TECHMDGRP", "amdSec");
    }

    /**
     * Returns the ALTOfile section of mets document
     * 
     * @return
     */
    public FileType getALTOfile() {
        return ALTOfile;
    }

    /**
     * Constructor
     * 
     * @param object
     * @param path
     * @param parent
     * @param withChildren
     * @param metsInfo
     */
    public Page(DigitalObject object, Object parent, boolean withChildren, MetsInfo metsInfo) throws MetsExportException {
        super(object, parent, withChildren, metsInfo);
        Node partNode = MetsUtils.xPathEvaluateNode(MODSstream, "*[local-name()='modsCollection']/*[local-name()='mods']/*[local-name()='part']");
        if (partNode == null) {
            partNode = MetsUtils.xPathEvaluateNode(MODSstream, "*[local-name()='mods']/*[local-name()='part']");
        }
        this.seq = metsInfo.getSeq();
        this.partType = partNode.getAttributes().getNamedItem("type").getNodeValue();
        NodeList nodeList = partNode.getChildNodes();
        for (int a = 0; a < nodeList.getLength(); a++) {
            if (nodeList.item(a).getNodeName().equalsIgnoreCase("mods:detail")) {
                Node numberNode = nodeList.item(a).getChildNodes().item(0).getFirstChild();
                if (nodeList.item(a).getAttributes().getNamedItem("type").getNodeValue().equalsIgnoreCase("pageNumber")) {
                    this.setPageNumber(numberNode.getNodeValue());
                }
                if (nodeList.item(a).getAttributes().getNamedItem("type").getNodeValue().equalsIgnoreCase("pageIndex")) {
                    this.setPageIndex(numberNode.getNodeValue());
                }
            }
        }
        fillFileNameInternal(object);
    }

    /**
     * Inserts a file reference into a physical DIV
     * 
     * @param physDivType
     * @param mets
     */
    private void addFileToMetsDiv(DivType physDivType) {
        DivType divType = new DivType();
        physDivType.getDiv().add(divType);
        divType.setID(getPageId());
        divType.setORDER(new BigInteger(getPageIndex()));
        divType.setORDERLABEL(getPageNumber());
        divType.setTYPE(partType);
        for (FileType fileType : fileTypes) {
            Fptr fptr = new Fptr();
            fptr.setFILEID(fileType);
            divType.getFptr().add(fptr);
        }
    }

    /**
     * Fills the internal structure (fileNames) with the definition of a file in
     * digital object
     * 
     * @param object
     */
    private void fillFileNameInternal(DigitalObject object) throws MetsExportException {
        for (String streamName : streamMapping.values()) {
            if (metsInfo.fedoraClient != null) {
                try {
                    GetDatastreamsResponse streams = FedoraClient.getDatastreams(object.getPID()).execute(metsInfo.fedoraClient);
                    List<DatastreamProfile> profiles = streams.getDatastreamProfiles();
                    for (DatastreamProfile profile : profiles) {
                        if (profile.getDsID().contains(streamName)) {
                            fileNames.put(streamName, MetsUtils.getBinaryDataStreams(metsInfo.fedoraClient, object.getPID(), profile.getDsID()));
                            mimeTypes.put(streamName, profile.getDsMIME());
                        }
                    }
                } catch (Exception ex) {
                    LOG.log(Level.SEVERE, "Error while getting file datastreams for " + this.originalPID, ex);
                    throw new MetsExportException("Error while getting file datastreams for " + this.originalPID, false, ex);
                }
            } else {
                List<DatastreamType> datastreams = object.getDatastream();
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
     * Returns an ID of a file
     * 
     * @return
     */
    private String getFileId() {
        return metsInfo.getPackageId() + "_" + String.format("%04d", seq);
    }

    /**
     * Returns the pageID
     * 
     * @return
     */
    public String getPageId() {
        return "DIV_P_PAGE_" + String.format("%04d", new BigInteger(getPageIndex()));
    }

    /**
     * Returns a pageIndex attribute
     * 
     * @return
     */
    public String getPageIndex() {
        return pageIndex;
    }

    /**
     * Returns a pageNumber attribute
     * 
     * @return
     */
    public String getPageNumber() {
        return pageNumber;
    }

    /**
     * @return
     */
    public int getSeq() {
        return seq;
    }

    /**
     * 
     * Generates amdSec metadata (MIX) using Jhove
     * 
     */
    private Mets generateTechMetadata(MetsInfo metsInfo) throws MetsExportException {
        int seq = 0;
        Mets infoMets = new Mets();
        AmdSecType amdSec = new AmdSecType();
        infoMets.getAmdSec().add(amdSec);

        for (String name : streamMapping.keySet()) {
            if (("ALTOGRP".equalsIgnoreCase(name)) || ("TXTGRP".equalsIgnoreCase(name))) {
                continue;
            }
            if (fileNames.get(streamMapping.get(name)) != null) {
                seq++;
                String outputFileName = outputFileNames.get(name);
                MdSecType mdSec = new MdSecType();
                mdSec.setID("MIX_" + String.format("%03d", seq));
                MdWrap mdWrap = new MdWrap();
                mdWrap.setMIMETYPE("text/xml");
                mdWrap.setMDTYPE("NISOIMG");
                XmlData xmlData = new XmlData();
                Node mixNode = JhoveUtility.getMixNode(new File(outputFileName), metsInfo);
                mdWrap.setXmlData(xmlData);
                xmlData.getAny().add(mixNode);
                mdSec.setMdWrap(mdWrap);
                amdSec.getTechMD().add(mdSec);
            }
            FileSec fileSec = new FileSec();
            infoMets.setFileSec(fileSec);
            for (String fileMap : metsInfo.fileGrpMap.keySet()) {
                fileSec.getFileGrp().add(metsInfo.fileGrpMap.get(fileMap));
            }
        }
        return infoMets;
    }

    /**
     * 
     * saves tech metadata
     * 
     * @param amdSecMets
     */
    private void saveAmdSec(Mets amdSecMets) {
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
            LOG.log(Level.SEVERE, "Error while saving AMDSec file for " + this.originalPID, ex);
            throw new IllegalStateException(ex);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.kramerius.importFoXML.structure.MetsElement#insertIntoMets(org.kramerius
     * .mets.Mets, boolean, java.lang.String)
     */
    @Override
    public void insertIntoMets(Mets mets, boolean withChildren, String outputDirectory) throws MetsExportException {
        if (parent.modsMetsElement == null) {
            parent.insertIntoMets(mets, false, outputDirectory);
        }
        this.outputDirectory = outputDirectory;
        for (String name : streamMapping.keySet()) {
            if (fileNames.get(streamMapping.get(name)) != null) {
                FileType fileType = prepareFileType(this.getSeq(), name);
                if ("ALTOGRP".equals(name)) {
                    ALTOfile = fileType;
                }
                metsInfo.fileGrpMap.get(name).getFile().add(fileType);
            }
        }

        if (fileNames.get(streamMapping.get("TECHMDGRP")) == null) {
            LOG.log(Level.INFO, "Generating tech");
            Mets amdSecMets = generateTechMetadata(metsInfo);
            StructMapType mapType = new StructMapType();
            mapType.setID(Const.DIV_PHYSICAL_ID);
            amdSecMets.getStructMap().add(mapType);
            DivType divType = new DivType();
            if (Const.PERIODICAL.equalsIgnoreCase(metsInfo.getType())) {
                divType.setTYPE("PERIODICAL_PAGE");
            } else {
                divType.setTYPE("MONOGRAPH_PAGE");
            }
            for (FileType fileType : fileTypes) {
                Fptr fptr = new Fptr();
                fptr.setFILEID(fileType);
                divType.getFptr().add(fptr);
            }
            mapType.setDiv(divType);
            saveAmdSec(amdSecMets);
            FileType fileType = prepareFileType(this.getSeq(), "TECHMDGRP");
            metsInfo.fileGrpMap.get("TECHMDGRP").getFile().add(fileType);
        }

        metsInfo.physDivType.setTYPE(this.metsInfo.getType());

        addFileToMetsDiv(metsInfo.physDivType);
        setStruct(parent.getElementId(), mets);
    }

    /**
     * Prepares a mets FileType element for a file
     * 
     * @param seq
     * @param metsStreamName
     * @return
     */
    private FileType prepareFileType(int seq, String metsStreamName) throws MetsExportException {
        String streamName = streamMapping.get(metsStreamName);
        FileType fileType = new FileType();
        fileType.setCHECKSUMTYPE("MD5");
        fileType.setSEQ(seq);
        fileType.setMIMETYPE(mimeTypes.get(streamName));
        fileTypes.add(fileType);
        InputStream is = null;
        fileType.setID(streamMappingPrefix.get(metsStreamName) + "_" + getFileId());
        if (fileNames.get(streamName) instanceof String) {
            String fileNameOriginal = (String) fileNames.get(streamName);
            int lastIndex = fileNameOriginal.lastIndexOf("/");
            int preLastIndex = fileNameOriginal.substring(1, lastIndex).lastIndexOf("/");
            String fileName = metsInfo.getPath() + fileNameOriginal.substring(preLastIndex + 2);
            File file = new File(fileName);
            fileType.setSIZE(file.length());
            try {
                is = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                throw new MetsExportException("File not found:" + fileName, false, e);
            }
        }
        if (fileNames.get(streamName) instanceof byte[]) {
            byte[] bytes = (byte[]) fileNames.get(streamName);
            is = new ByteArrayInputStream(bytes);
            fileType.setSIZE(Long.valueOf(bytes.length));
        }
        String outputFileName = fileType.getID() + "." + MimeType.getExtension(mimeTypes.get(streamName));
        String fullOutputFileName = outputDirectory + "/" + streamMappingFile.get(metsStreamName) + "/" + outputFileName;
        outputFileNames.put(metsStreamName, fullOutputFileName);
        try {
            FileMD5Info fileMD5Info = MetsUtils.getDigestAndCopy(is, new FileOutputStream(fullOutputFileName));
            fileMD5Info.setFileName("./" + streamMappingFile.get(metsStreamName) + "/" + outputFileName);
            fileType.setCHECKSUM(fileMD5Info.getMd5());
            metsInfo.addFile(fileMD5Info);
        } catch (Exception e) {
            LOG.log(Level.SEVERE, "Unable to process file " + fullOutputFileName, e);
            throw new MetsExportException("Unable to process file " + fullOutputFileName, false, e);
        }

        FLocat flocat = new FLocat();
        flocat.setLOCTYPE("URL");
        flocat.setHref("./" + streamMappingFile.get(metsStreamName) + "/" + outputFileName);
        fileType.getFLocat().add(flocat);

        return fileType;

    }

    /**
     * Setter for pageIndex attribute
     * 
     * @param pageIndex
     */
    private void setPageIndex(String pageIndex) {
        this.pageIndex = pageIndex;
    }

    /**
     * Setter for pageNumber attribute
     * 
     * @param pageNumber
     */
    private void setPageNumber(String pageNumber) {
        this.pageNumber = pageNumber;
    }

    /**
     * Inserts the struct element into mets
     * 
     * @param fromId
     * @param mets
     */
    private void setStruct(String fromId, Mets mets) throws MetsExportException {
        SmLink smLink = new SmLink();
        smLink.setFrom(parent.getElementId());
        smLink.setTo(getPageId());
        if (mets.getStructLink() == null) {
            mets.setStructLink(new MetsType.StructLink());
        }
        mets.getStructLink().getSmLinkOrSmLinkGrp().add(smLink);
    }
}