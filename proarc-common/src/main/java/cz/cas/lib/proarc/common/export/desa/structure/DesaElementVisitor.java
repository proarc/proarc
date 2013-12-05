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

package cz.cas.lib.proarc.common.export.desa.structure;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import net.lingala.zip4j.core.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.ZipParameters;
import net.lingala.zip4j.util.Zip4jConstants;

import org.w3c.dom.Element;

import com.yourmediashelf.fedora.generated.foxml.DatastreamType;

import cz.cas.lib.proarc.common.export.desa.Const;
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.MetsUtils.NamespacePrefixMapperImpl;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
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
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

/**
 * Visitor class for creating mets document out of Desa objects
 * 
 * @author eskymo
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
                LOG.log(Level.INFO, "File:" + zipFileName + " exists, so it was deleted");
            }
            ZipFile zipFile = new ZipFile(zipFileName);
            ZipParameters zip4jZipParameters = new ZipParameters();
            zip4jZipParameters.setCompressionMethod(Zip4jConstants.COMP_DEFLATE);
            zip4jZipParameters.setCompressionLevel(Zip4jConstants.DEFLATE_LEVEL_NORMAL);
            zipFile.addFiles(fileList, zip4jZipParameters);
            LOG.log(Level.INFO, "Zip archive created:" + zipFileName + " for " + desaElement.getElementType());
        } catch (ZipException e) {
            LOG.log(Level.SEVERE, "Unable to create a zip file:" + zipFileName, e);
            throw new MetsExportException(desaElement.getOriginalPid(), "Unable to create a zip file:" + zipFileName, false, e);
        }

    }

    /**
     * Returns a label for LOGICAL div from DC stream
     * 
     * @param desaElement
     * @return
     * @throws MetsExportException
     */
    private String getLabelFromDC(IDesaElement desaElement) throws MetsExportException {
        List<Element> descriptor = desaElement.getDescriptor();
        return MetsUtils.xPathEvaluateString(descriptor, "*[local-name()='dc']/*[local-name()='title']");
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
            tmpFileFolder = File.createTempFile("tmp" + desaElement.getDesaContext().getPackageID() + desaElement.getElementID(), ".tmp");
            tmpFileFolder.delete();
            tmpFileFolder = new File(tmpFileFolder.getAbsolutePath());
        } catch (IOException e) {
            LOG.log(Level.SEVERE, "Unable to create a temp file for:" + desaElement.getElementID());
            throw new MetsExportException(desaElement.getOriginalPid(), "Unable to create a temp folder", false, e);
        }
        tmpFileFolder.mkdir();
        LOG.log(Level.INFO, "TMP folder:" + tmpFileFolder.getAbsolutePath() + " created for:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")");
        return tmpFileFolder;
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
            marshaller.setProperty("com.sun.xml.internal.bind.namespacePrefixMapper", new NamespacePrefixMapperImpl());
            marshaller.marshal(mets, outputFile);
            try {
                MetsUtils.validateAgainstXSD(outputFile, Mets.class.getResourceAsStream("mets.xsd"));
            } catch (MetsExportException ex) {
                LOG.log(Level.WARNING, "Invalid mets.xml for:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")");
                desaElement.getDesaContext().getMetsExportException().addException(desaElement.getOriginalPid(), "Invalid mets !", true, ex.exceptionList.get(0).getEx());
            }
        } catch (Exception e) {
            LOG.log(Level.SEVERE, "Unable to save mets file:" + outputFile.getAbsolutePath());
            throw new MetsExportException("Unable to save mets file:" + outputFile.getAbsolutePath(), false, e);
        }
    }

    /**
     * Inserts a document element into a mets
     * 
     * @param desaElement
     * @param suffix
     * @throws MetsExportException
     */
    private void insertDerDocument(IDesaElement desaElement, String suffix) throws MetsExportException {
        if (suffix == null) {
            suffix = "0001";
        }
        File tmpFolder = createTempFolder(desaElement);
        File outputMets = new File(tmpFolder.getAbsolutePath() + "/mets.xml");
        Mets mets = prepareMets(desaElement);
        DivType divType = new DivType();
        divType.setLabel(getLabelFromDC(desaElement));
        divType.setTYPE("record");
        divType.getDMDID().add(mets.getDmdSec().get(0));

        FileSec fileSec = new FileSec();
        mets.setFileSec(fileSec);
        FileGrp fileGrp = new FileGrp();
        fileGrp.setUSE("Original");
        fileSec.getFileGrp().add(fileGrp);
        fileGrp.setID("GRP_0001");
        StructMapType structMapType = mets.getStructMap().get(0);
        structMapType.setDiv(divType);

        int fileOrder = 0;
        ArrayList<File> fileList = new ArrayList<File>();
        for (DesaElement fileElement : desaElement.getChildren()) {
            fileOrder++;
            DatastreamType rawDS = FoxmlUtils.findDatastream(fileElement.getSourceObject(), "RAW");
            byte[] fileContent = new byte[0];
            String mimeType = "empty";
            if (rawDS != null) {
                fileContent = rawDS.getDatastreamVersion().get(0).getBinaryContent();
                mimeType = rawDS.getDatastreamVersion().get(0).getMIMETYPE();
            } else {
                LOG.log(Level.WARNING, "RAW datastream is null for:" + desaElement.getOriginalPid() + " - skipping");
                desaElement.getDesaContext().getMetsExportException().addException(fileElement.getOriginalPid(), "RAW datastream is missing", false, null);
                continue;
            }
            FileOutputStream fos;
            String outputFileName = getFileName(fileElement);
            /*
             * Generates a filename if it's not provided from the original
             * document
             */
            if ((outputFileName == null) || (outputFileName.trim().length() == 0)) {
                outputFileName = "file_" + String.format("%04d", fileOrder) + "." + MetsUtils.getMimeToExtension().getProperty(mimeType);
                LOG.log(Level.INFO, "importFile was not specified for:" + fileElement.getOriginalPid() + " new name was generated:" + outputFileName);
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
            fileType.setID(desaElement.getDesaContext().getPackageID() + "_" + suffix + "_" + String.format("%04d", fileOrder));
            fileType.setCHECKSUMTYPE("MD5");
            fileType.setCHECKSUM(fileMd5Info.getMd5());
            FLocat flocat = new FLocat();
            flocat.setLOCTYPE("URL");
            flocat.setHref(outputFileName);
            fileType.getFLocat().add(flocat);
            fileGrp.getFile().add(fileType);
            Fptr fptr = new Fptr();
            fptr.setFILEID(fileType);
            divType.getFptr().add(fptr);
        }
        saveMets(mets, outputMets, desaElement);
        desaElement.setZipName(desaElement.getDesaContext().getPackageID() + "_" + suffix);
        fileList.add(outputMets);
        String zipFileName = desaElement.getDesaContext().getOutputPath() + "/" + desaElement.getDesaContext().getPackageID() + "_" + suffix + ".zip";
        zip(zipFileName, fileList, desaElement);
    }

    /**
     * Insert a folder element into a mets
     * 
     * @param desaElement
     * @throws MetsExportException
     */
    private void insertDerFolder(IDesaElement desaElement) throws MetsExportException {
        Mets mets = prepareMets(desaElement);
        DivType divType = new DivType();
        divType.setTYPE("file");
        divType.setLabel(getLabelFromDC(desaElement));
        divType.getDMDID().add(mets.getDmdSec().get(0));
        StructMapType structMapType = mets.getStructMap().get(0);
        structMapType.setDiv(divType);
        int documentOrder = 0;
        for (DesaElement documentElement : desaElement.getChildren()) {
            documentOrder++;
            insertDerDocument(documentElement, String.format("%04d", documentOrder));
            DivType documentDiv = new DivType();
            documentDiv.setTYPE("record");
            documentDiv.setLabel(getLabelFromDC(documentElement));
            Mptr mptr = new Mptr();
            mptr.setLOCTYPE("OTHER");
            mptr.setOTHERLOCTYPE("internal_reference");
            mptr.setHref(documentElement.getZipName());
            documentDiv.getMptr().add(mptr);
            divType.getDiv().add(documentDiv);
        }
        File tmpFolder = createTempFolder(desaElement);
        File outputMets = new File(tmpFolder.getAbsolutePath() + "/mets.xml");
        saveMets(mets, outputMets, desaElement);
        String zipFileName = desaElement.getDesaContext().getOutputPath() + "/" + desaElement.getDesaContext().getPackageID() + "_FILE.zip";
        ArrayList<File> fileList = new ArrayList<File>();
        fileList.add(outputMets);
        zip(zipFileName, fileList, desaElement);
    }

    /*
     * (non-Javadoc)
     * 
     * @see cz.cas.lib.proarc.common.export.desa.structure.IDesaElementVisitor#
     * insertIntoMets
     * (cz.cas.lib.proarc.common.export.desa.structure.IDesaElement)
     */
    @Override
    public void insertIntoMets(IDesaElement desaElement) throws MetsExportException {
        LOG.log(Level.INFO, "Inserting into Mets:" + desaElement.getOriginalPid() + "(" + desaElement.getElementType() + ")");
        if (Const.DOCUMENT.equalsIgnoreCase(desaElement.getElementType())) {
            insertDerDocument(desaElement, null);
        } else if (Const.FOLDER.equalsIgnoreCase(desaElement.getElementType())) {
            insertDerFolder(desaElement);
        } else
            throw new MetsExportException(desaElement.getOriginalPid(), "Unknown type:" + desaElement.getElementType() + " model:" + desaElement.getModel(), false, null);
        if (desaElement.getDesaContext().getMetsExportException().exceptionList.size() > 0) {
            throw desaElement.getDesaContext().getMetsExportException();
        }
    }
}
