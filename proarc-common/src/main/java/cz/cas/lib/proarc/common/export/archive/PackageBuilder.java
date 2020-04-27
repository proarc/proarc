/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.export.archive;

import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.export.mets.FileMD5Info;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils.ControlGroup;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DisseminationHandler;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.DivType.Fptr;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.FileType.FLocat;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap.XmlData;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType.FileSec;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.MetsType.MetsHdr;
import cz.cas.lib.proarc.mets.MetsType.MetsHdr.Agent;
import cz.cas.lib.proarc.mets.StructMapType;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import javax.ws.rs.core.Response;
import javax.xml.bind.JAXB;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.io.IOUtils;

/**
 * Builds resulting METS package and a corresponding folder layout.
 *
 * @author Jan Pokorsky
 */
public class PackageBuilder {

    /**
     * A {@link MdWrap#setMDTYPE(java.lang.String) } helper.
     * @see <a href='http://www.loc.gov/standards/mets/docs/mets.v1-9.html#mdWrap'>mdWrap</a>
     */
    public enum MdType { DC, MODS }

    public static final String METS_FILENAME = "mets.xml";
    /** The type of the structural map of other objects like devices. */
    public static final String STRUCTMAP_OTHERS_TYPE = "OTHERS";
    /** The type of the structural map of digital objects. */
    public static final String STRUCTMAP_PHYSICAL_TYPE = "PHYSICAL";
    /** The ID of the {@code div} containing a list of devices. */
    public static final String DIV_DEVICE_LIST_ID = "DIV_DEVICES";

    private File pkgFolder;
    private URI pkgFolderUri;
    private final File parentFolder;
    private final DatatypeFactory xmlTypes;
    private Mets mets;
    private StructMapType othersStructMap;
    private final Transformer domTransformer;
    private final HashMap<String, DivType> pid2PhysicalDiv;

    public PackageBuilder(File targetFolder) {
        this.parentFolder = targetFolder;
        this.pid2PhysicalDiv = new HashMap<String, DivType>();
        try {
            this.xmlTypes = DatatypeFactory.newInstance();
            this.domTransformer = TransformerFactory.newInstance().newTransformer();
            this.domTransformer.setOutputProperty(OutputKeys.INDENT, "yes");
            this.domTransformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
        } catch (DatatypeConfigurationException ex) {
            throw new IllegalStateException(ex);
        } catch (TransformerConfigurationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    public void prepare(List<DigitalObjectElement> objectPath, LocalObject lobj, AppConfiguration appConfig) throws MetsExportException {
        DigitalObjectElement entry = objectPath.get(0);

        // create package folder
        pkgFolder = new File(parentFolder, FoxmlUtils.pidAsUuid(entry.getPid()));
        if (!pkgFolder.mkdir()) {
            throw new IllegalStateException("The package folder already exists: " + pkgFolder);
        }
        pkgFolderUri = pkgFolder.toURI();

        DigitalObject digitalObject = lobj.getDigitalObject();
        MetsHdr metsHdr = new MetsHdr();
        // XXX should we use rather actual date?
        // for now use modified date as create day to later decide whether fedora contains same or updated object
//        metsHdr.setCREATEDATE(xmlTypes.newXMLGregorianCalendar());
        metsHdr.setCREATEDATE(getXmlDate(digitalObject, FoxmlUtils.PROPERTY_CREATEDATE));
//        metsHdr.setCREATEDATE(getDate(digitalObject, FoxmlUtils.PROPERTY_CREATEDATE));
        metsHdr.setLASTMODDATE(getXmlDate(digitalObject, FoxmlUtils.PROPERTY_LASTMODIFIED));
        setAgent(metsHdr, "CREATOR", "ORGANIZATION", appConfig.getNdkExportOptions().getCreator());
        setAgent(metsHdr, "ARCHIVIST", "ORGANIZATION", appConfig.getNdkExportOptions().getArchivist());
        Agent agent = new Agent();
        agent.setName("ProArc");
        agent.setROLE("CREATOR");
        agent.setTYPE("OTHER");
//        agent.setTYPE("ORGANIZATION");
       // metsHdr.getAgent().add(agent);

        mets = new Mets();
//        mets.setID(null);
        mets.setLabel1(getPackageLabel(objectPath));
        mets.setMetsHdr(metsHdr);
        mets.setTYPE(entry.getModelId());

        mets.setFileSec(new FileSec());
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

    public void build() {
        JAXB.marshal(mets, new File(pkgFolder, METS_FILENAME));
    }

    public DivType addObject(int index, DigitalObjectElement elm, DigitalObjectElement parentElm) {
        DivType div = new DivType();
        String modelId = elm.getModelId();
        String type = getObjectId(modelId);
        div.setID(String.format("div_%s_%04d", type, index));
        div.getCONTENTIDS().add(elm.getPid());
        div.setLabel3(elm.getItem().getLabel());
        div.setORDER(null);
        div.setTYPE(modelId);

        DivType parent = parentElm == null
                ? null : pid2PhysicalDiv.get(parentElm.getPid());
        if (parent == null) {
            StructMapType structMap = new StructMapType();
            structMap.setDiv(div);
            structMap.setTYPE(STRUCTMAP_PHYSICAL_TYPE);
            structMap.setLabel2("Physical Structure");
            mets.getStructMap().add(structMap);
        } else {
            parent.getDiv().add(div);
        }
        pid2PhysicalDiv.put(elm.getPid(), div);
        return div;
    }

    public DivType addDevice(LocalObject cache) {
        String pid = cache.getPid();
        DivType div = pid2PhysicalDiv.get(pid);
        if (div != null) {
            return div;
        }
        div = new DivType();
        String modelId = DeviceRepository.METAMODEL_ID;
        String type = getObjectId(modelId);
        div.getCONTENTIDS().add(pid);
        div.setLabel3(cache.getLabel());
        div.setORDER(null);
        div.setTYPE(modelId);

        DivType devicesDiv;
        if (othersStructMap == null) {
            othersStructMap = new StructMapType();
            devicesDiv = new DivType();
            devicesDiv.setID(DIV_DEVICE_LIST_ID);
            devicesDiv.setLabel3("List of devices");
            othersStructMap.setDiv(devicesDiv);
            othersStructMap.setTYPE(STRUCTMAP_OTHERS_TYPE);
            othersStructMap.setLabel2("Other objects");
            mets.getStructMap().add(othersStructMap);
        } else {
            devicesDiv = othersStructMap.getDiv();
        }
        div.setID(String.format("div_%s_%04d", type, devicesDiv.getDiv().size() + 1));
        devicesDiv.getDiv().add(div);
        pid2PhysicalDiv.put(pid, div);
        return div;
    }

    public void addFoxmlAsFile(int index, DigitalObjectElement elm, LocalObject obj) throws DigitalObjectException {
        addFoxmlAsFile(index, elm.getModelId(), obj);
    }

    public void addFoxmlAsFile(int index, String modelId, LocalObject obj) throws DigitalObjectException {
        try {
            String uuid = getObjectId(obj.getPid());
            String dsId = "FOXML";
            String modelName = getObjectId(modelId);
            File grpFile = getGroupFile(pkgFolder, dsId,
                    getFilename(dsId, index, modelName, uuid, "xml"));
            DigitalObject dObj = obj.getDigitalObject();
            FoxmlUtils.marshal(new StreamResult(grpFile), dObj, true);
            FileMD5Info fileInfo = getDigest(new BufferedInputStream(new FileInputStream(grpFile)));

            FileGrp fileGrp = getMetsFileGrp(dsId);
            FileType fileType = new FileType();
            fileType.setCHECKSUM(fileInfo.getMd5());
            fileType.setCHECKSUMTYPE("MD5");
            fileType.setCREATED(getXmlDate(dObj, FoxmlUtils.PROPERTY_LASTMODIFIED));
            fileType.setID(String.format("%s_%s_%04d_%s", dsId, modelName, index, uuid));
            fileType.setMIMETYPE("text/xml");
//            fileType.setSEQ(index);
            fileType.setSIZE(fileInfo.getSize());
            fileType.getFLocat().add(createFLocat(grpFile));
            fileGrp.getFile().add(fileType);

            DivType div = pid2PhysicalDiv.get(obj.getPid());
            Fptr fptr = new Fptr();
            fptr.setFILEID(fileType);
            div.getFptr().add(fptr);
        } catch (NoSuchAlgorithmException ex) {
            throw new DigitalObjectException(obj.getPid(), null, ex);
        } catch (IOException ex) {
            throw new DigitalObjectException(obj.getPid(), null, ex);
        }
    }

    public void addStreamAsMdSec(
            int index, DatastreamType dt, String pid, String modelId, MdType mdType
    ) throws DigitalObjectException {
        String uuid = getObjectId(pid);
        String mimetype = dt.getDatastreamVersion().get(0).getMIMETYPE();
        String modelName = getObjectId(modelId);
        DatastreamVersionType ds = dt.getDatastreamVersion().get(0);

        MdSecType mdSec = new MdSecType();
        mdSec.setCREATED(ds.getCREATED());
        mdSec.setID(String.format("DMD_%s_%s_%04d_%s", mdType.name(), modelName, index, uuid));

        MdWrap mdWrap = new MdWrap();
        mdWrap.setMIMETYPE(mimetype);
        mdWrap.setMDTYPE(mdType.name());
        XmlData xmlData = new XmlData();
        xmlData.getAny().addAll(ds.getXmlContent().getAny());
        mdWrap.setXmlData(xmlData);
        mdSec.setMdWrap(mdWrap);
        mets.getDmdSec().add(mdSec);

        DivType div = pid2PhysicalDiv.get(pid);
        div.getDMDID().add(mdSec);
    }

    public void addStreamAsFile(
            int index, DatastreamType dt, String pid, String modelId, DisseminationHandler dHandler
    ) throws DigitalObjectException {
        String dsId = dt.getID();
        String uuid = getObjectId(pid);
        DatastreamVersionType ds = dt.getDatastreamVersion().get(0);
        String mimetype = ds.getMIMETYPE();
        String ext = getMimeFileExtension(mimetype);
        String modelName = getObjectId(modelId);
        File dsFile = getGroupFile(pkgFolder, dsId, getFilename(dsId, index, modelName, uuid, ext));
        FileMD5Info fileInfo = copyStream(pid, dt, ds, dHandler, dsFile);

        // add to fileGrp
        FileGrp fileGrp = getMetsFileGrp(dsId);
        FileType fileType = new FileType();
        fileType.setCHECKSUM(fileInfo.getMd5());
        fileType.setCHECKSUMTYPE("MD5");
        fileType.setCREATED(ds.getCREATED());
        fileType.setID(String.format("%s_%s_%04d_%s", dsId, modelName, index, uuid));
        fileType.setMIMETYPE(mimetype);
//        fileType.setSEQ(index);
        fileType.setSIZE(fileInfo.getSize());
        fileType.getFLocat().add(createFLocat(dsFile));
        fileGrp.getFile().add(fileType);

        DivType div = pid2PhysicalDiv.get(pid);
        Fptr fptr = new Fptr();
        fptr.setFILEID(fileType);
        div.getFptr().add(fptr);
    }

    private FileMD5Info copyStream(String pid,
            DatastreamType dt, DatastreamVersionType ds, DisseminationHandler dHandler,
            File dsFile
    ) throws DigitalObjectException {
        String dsId = dt.getID();
        ControlGroup ctrlGroup = ControlGroup.fromExternal(dt.getCONTROLGROUP());
        FileMD5Info fileInfo;
        try {
            if (ctrlGroup == ControlGroup.INLINE) {
                DOMSource domSource = new DOMSource(ds.getXmlContent().getAny().get(0));
                domTransformer.transform(domSource, new StreamResult(dsFile));
                fileInfo = getDigest(new BufferedInputStream(new FileInputStream(dsFile)));
            } else {
                Response resp = dHandler.getDissemination(null);
                Object entity = resp.getEntity();
                if (entity instanceof InputStream) {
                    fileInfo = MetsUtils.getDigestAndCopy((InputStream) entity, new FileOutputStream(dsFile));
                } else {
                    String msg = "Unsupported entity "
                            + (entity == null ? null : entity.getClass().getName());
                    throw new DigitalObjectException(pid, null, dsId, msg, null);
                }
            }
            return fileInfo;
        } catch (TransformerException ex) {
            throw new DigitalObjectException(pid, null, dsId, null, ex);
        } catch (NoSuchAlgorithmException ex) {
            throw new DigitalObjectException(pid, null, dsId, null, ex);
        } catch (IOException ex) {
            throw new DigitalObjectException(pid, null, dsId, null, ex);
        }
    }

    private FLocat createFLocat(File dsFile) {
        FLocat fLocat = new FLocat();
        fLocat.setLOCTYPE("URL");
        fLocat.setHref("./" + pkgFolderUri.relativize(dsFile.toURI()).toASCIIString());
        return fLocat;
    }

    private FileGrp getMetsFileGrp(String dsId) {
        List<FileGrp> fileGrps = mets.getFileSec().getFileGrp();
        for (FileGrp fileGrp : fileGrps) {
            if (dsId.equals(fileGrp.getID())) {
                return fileGrp;
            }
        }
        FileGrp fileGrp = new FileGrp();
        fileGrp.setID(dsId);
        fileGrps.add(fileGrp);
        return fileGrp;
    }

    private File getGroupFile(File parent, String grpId, String filename) {
        File dsFolder = new File(parent, grpId);
        dsFolder.mkdirs();
        File dsFile = new File(dsFolder, filename);
        if (dsFile.exists()) {
            throw new IllegalStateException("File exists: " + dsFile);
        }
        return dsFile;
    }

    static String getFilename(String datastream, int index, String model, String name, String ext) {
        return String.format("%s_%s_%04d_%s.%s", datastream.toLowerCase(), model, index, name, ext);
    }

    private static String getMimeFileExtension(String mime) {
        try {
            return MetsUtils.getMimeToExtension().getProperty(mime);
        } catch (MetsExportException ex) {
            throw new IllegalStateException(ex.getCause().getMessage(), ex);
        }
    }

    static String getObjectId(String pid) {
        return pid.substring(pid.indexOf(':') + 1);
    }

    private XMLGregorianCalendar getXmlDate(DigitalObject dobj, String name) {
        PropertyType createProp = FoxmlUtils.findProperty(dobj, name);
        if (createProp != null) {
            String value = createProp.getVALUE();
            if (value != null && !value.isEmpty()) {
                return xmlTypes.newXMLGregorianCalendar(value);
            }
        }
        return xmlTypes.newXMLGregorianCalendar();
    }

    private static String getPackageLabel(List<DigitalObjectElement> objectPath) {
        StringBuilder sb = new StringBuilder();
        for (DigitalObjectElement elm : objectPath) {
            if (sb.length() > 0) {
                sb.append(", ");
            }
            sb.append(elm.getItem().getLabel());
        }
        return sb.toString();
    }

    private static FileMD5Info getDigest(InputStream is) throws NoSuchAlgorithmException, IOException {
        try {
            FileMD5Info fileInfo = MetsUtils.getDigest(is);
            is.close();
            is = null;
            return fileInfo;
        } finally {
            IOUtils.closeQuietly(is);
        }
    }

}
