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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.dublincore.DcUtils;
import cz.cas.lib.proarc.common.export.Kramerius4Export;
import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

/**
 * Class that represents the element of Mets export
 *
 * @author Robert Simonovsky
 *
 */
public class MetsElement implements IMetsElement {
    public final List<Element> descriptor;
    public final String model;
    private final MetsContext metsContext;
    private final String originalPid;
    private final Logger LOG = Logger.getLogger(MetsElement.class.getName());
    private MetsElement parent;
    private final List<MetsElement> children = new ArrayList<MetsElement>();
    private final List<Element> relsExt;
    private final DigitalObject sourceObject;
    public final List<Element> modsStream;
    public final XMLGregorianCalendar createDate;
    public final String label;
    public MdSecType modsMetsElement;
    private FileType altoFile;

    @Override
    public FileType getAltoFile() {
        return altoFile;
    }

    @Override
    public void setAltoFile(FileType altoFile) {
        this.altoFile = altoFile;
    }

    @Override
    public MdSecType getModsMetsElement() {
        return modsMetsElement;
    }

    @Override
    public void setModsMetsElement(MdSecType modsMetsElement) {
        this.modsMetsElement = modsMetsElement;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets2.structure.IMetsElement#getLabel()
     */
    @Override
    public String getLabel() {
        return label;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets2.structure.IMetsElement#getCreateDate
     * ()
     */
    @Override
    public XMLGregorianCalendar getCreateDate() {
        return createDate;
    }

    /*
     * (non-Javadoc)
     *
     * @see cz.cas.lib.proarc.common.export.mets2.structure.IMetsElement#
     * getLastUpdateDate()
     */
    @Override
    public XMLGregorianCalendar getLastUpdateDate() {
        return lastUpdateDate;
    }

    public final XMLGregorianCalendar lastUpdateDate;

    private final String elementType;
    private String elementID;
    private String modsElementID;

    @Override
    public void setModsElementID(String modsElementID) {
        this.modsElementID = modsElementID;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets2.structure.IMetsElement#getModsElementID
     * ()
     */
    @Override
    public String getModsElementID() {
        return modsElementID;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getMetsContext
     * ()
     */
    @Override
    public MetsContext getMetsContext() {
        return metsContext;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getElementType
     * ()
     */
    @Override
    public String getElementType() {
        return elementType;
    }

    @Override
    public Map<String, String> getModsIdentifiers() throws MetsExportException {
        Map<String, String> result = new HashMap<String, String>();
        String XPATH = "*[local-name()='mods']";
        if (this.modsStream != null) {
            Node descNode = MetsUtils.xPathEvaluateNode(MetsUtils.removeModsCollection(this.modsStream), XPATH);
            if (descNode != null) {
                NodeList nodeList = descNode.getChildNodes();
                for (int a = 0; a < nodeList.getLength(); a++) {
                    Node node = nodeList.item(a);
                    if ("identifier".equalsIgnoreCase(node.getLocalName())) {
                        result.put(node.getAttributes().getNamedItem("type").getNodeValue(), node.getTextContent());
                    }
                }
            }
        }
        return result;
    }

    /**
     * Validates Mods and Dc against xsd schema
     *
     * @throws MetsExportException
     */
    private void validateDCMODS() throws MetsExportException {
        List<String> validationErrors;

        if (this.descriptor != null) {
            Document dcDoc = MetsUtils.getDocumentFromList(this.descriptor);
            try {
                validationErrors = MetsUtils.validateAgainstXSD(dcDoc, OaiDcType.class.getResourceAsStream("dc_oai.xsd"));
            } catch (Exception ex) {
                throw new MetsExportException(this.getOriginalPid(), "Error while validating DC for:" + this.getOriginalPid() + "(" + this.getElementType() + ")", false, ex);
            }

            if (validationErrors.size() > 0) {
                MetsExportException metsException = new MetsExportException(this.getOriginalPid(), "Invalid DC in BIBLIO_MODS for:" + this.getOriginalPid() + "(" + this.getElementType() + ")", false, null);
                metsException.getExceptions().get(0).setValidationErrors(validationErrors);
                throw metsException;
            }
        }
        Document modsDoc = MetsUtils.getDocumentFromList(this.modsStream);
        try {
            if ("3.5".equals(this.modsStream.get(0).getAttribute("version"))) {
                validationErrors = MetsUtils.validateAgainstXSD(modsDoc, ModsDefinition.class.getResourceAsStream("mods-3-5.xsd"));
            } else {
                validationErrors = MetsUtils.validateAgainstXSD(modsDoc, ModsDefinition.class.getResourceAsStream("mods.xsd"));
            }
        } catch (Exception ex) {
            throw new MetsExportException(this.getOriginalPid(), "Error while validating MODS for:" + this.getOriginalPid() + "(" + this.getElementType() + ")", false, ex);
        }

        if (validationErrors.size() > 0) {
            MetsExportException metsException = new MetsExportException(this.getOriginalPid(), "Invalid MODS for:" + this.getOriginalPid() + "(" + this.getElementType() + ")", false, null);
            metsException.getExceptions().get(0).setValidationErrors(validationErrors);
            throw metsException;
        }
    }

    /**
     * Constructor
     *
     * @param digitalObject
     * @param parent
     * @param metsContext
     * @param fillChildren
     * @throws MetsExportException
     */
    public MetsElement(DigitalObject digitalObject, Object parent, MetsContext metsContext, boolean fillChildren) throws MetsExportException {
        this.metsContext = metsContext;
        this.sourceObject = digitalObject;
        this.originalPid = digitalObject.getPID();
        metsContext.getPidElements().put(this.originalPid, this);
        try {
            this.createDate = DatatypeFactory.newInstance().newXMLGregorianCalendar(MetsUtils.getProperty(Const.FEDORA_CREATEDATE, digitalObject.getObjectProperties().getProperty()));
            this.lastUpdateDate = DatatypeFactory.newInstance().newXMLGregorianCalendar(MetsUtils.getProperty(Const.FEDORA_LASTMODIFIED, digitalObject.getObjectProperties().getProperty()));
        } catch (DatatypeConfigurationException ex) {
            throw new MetsExportException(this.getOriginalPid(), "Unable to set create/lastModDate", false, ex);
        }

        this.label = MetsUtils.getProperty(Const.FEDORA_LABEL, digitalObject.getObjectProperties().getProperty());

        this.relsExt = FoxmlUtils.findDatastream(digitalObject, "RELS-EXT").getDatastreamVersion().get(0).getXmlContent().getAny();
        if (FoxmlUtils.findDatastream(digitalObject, "BIBLIO_MODS") != null) {
            this.modsStream = MetsUtils.removeSchemaLocation(MetsUtils.removeModsCollection(FoxmlUtils.findDatastream(digitalObject, "BIBLIO_MODS").getDatastreamVersion().get(0).getXmlContent().getAny()));
        } else {
            this.modsStream = null;
        }

        Kramerius4Export.removeNils(modsStream.get(0));

        model = MetsUtils.getModel(relsExt);
        this.elementType = Const.typeMap.get(model);

        if (!Const.PAGE.equals(elementType)) {
            NdkMapper mapper = NdkMapper.get(model.replaceAll("info:fedora/", ""));
            Document modsDocument = MetsUtils.getDocumentFromList(modsStream);
            DOMSource modsDOMSource = new DOMSource(modsDocument);
            ModsDefinition modsDefinition = ModsUtils.unmarshalModsType(modsDOMSource);
            OaiDcType dcType = mapper.toDc(modsDefinition, null);
            DOMResult dcDOMResult = new DOMResult();
            DcUtils.marshal(dcDOMResult, dcType, true);
            this.descriptor = new ArrayList<Element>();
            this.descriptor.add((Element) dcDOMResult.getNode().getFirstChild());
        } else {
            this.descriptor = null;
        }

        validateDCMODS();

        String modsName = Const.typeNameMap.get(this.elementType);
        if (modsName == null) {
            throw new MetsExportException(this.originalPid, "Unable to find mods name for:" + this.elementType, false, null);
        }
        this.elementID = this.elementType + "_" + String.format("%04d", metsContext.addElementId(this.elementType));
        this.modsElementID = elementID.replaceAll(this.elementType, modsName);

        if (Const.ARTICLE.equals(elementType)) {
            this.elementID = elementID.replaceAll(this.elementType, modsName);
        }

        if (Const.SUPPLEMENT.equals(elementType)) {
            this.elementID = elementID.replaceAll(this.elementType, modsName);
        }

        if (parent instanceof MetsElement) {
            this.parent = (MetsElement) parent;
        }

        if (fillChildren) {
            fillChildren();
        }
        if (parent == null) {
            this.parent = initParent();
        }

        if (this.parent == null) {
            metsContext.setRootElement(this);
            LOG.log(Level.FINE, "Root element found:" + getOriginalPid() + "(" + getElementType() + ")");
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getElementID
     * ()
     */
    @Override
    public String getElementID() {
        return elementID;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getParent()
     */
    @Override
    public MetsElement getParent() {
        return parent;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getChildren()
     */
    @Override
    public List<MetsElement> getChildren() {
        return children;
    }

    /**
     * Inits the parent element of current element
     *
     * @return
     * @throws MetsExportException
     */
    private MetsElement initParent() throws MetsExportException {
        String parentId;
        if (metsContext.getFedoraClient() != null) {
            parentId = MetsUtils.getParent(originalPid, metsContext.getRemoteStorage());
            LOG.fine("Parent found from Fedora:" + parentId);
        } else {
            parentId = MetsUtils.getParent(originalPid, metsContext.getFsParentMap());
            LOG.fine("Parent found from Local:" + parentId);
        }

        if (parentId == null) {
            LOG.fine("Parent not found - returning null");
            return null;
        }

        DigitalObject parentObject = null;
        if (metsContext.getFedoraClient() != null) {
            parentObject = MetsUtils.readRelatedFoXML(parentId, metsContext.getFedoraClient());
        } else {
            parentObject = MetsUtils.readRelatedFoXML(metsContext.getPath(), parentId);
        }
        MetsElement parentInit = new MetsElement(parentObject, null, metsContext, false);
        parentInit.children.add(this);
        return parentInit;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getDescriptor
     * ()
     */
    @Override
    public List<Element> getDescriptor() {
        return descriptor;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getModel()
     */
    @Override
    public String getModel() {
        return model;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getOriginalPid
     * ()
     */
    @Override
    public String getOriginalPid() {
        return originalPid;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getRelsExt()
     */
    @Override
    public List<Element> getRelsExt() {
        return relsExt;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getSourceObject
     * ()
     */
    @Override
    public DigitalObject getSourceObject() {
        return sourceObject;
    }

    /**
     * Static method for instantiating an Element
     *
     * @param object
     * @param parent
     * @param metsContext
     * @param withChildren
     * @return
     * @throws MetsExportException
     */
    public static MetsElement getElement(DigitalObject object, MetsElement parent, MetsContext metsContext, boolean withChildren) throws MetsExportException {
        List<Element> relsExt = FoxmlUtils.findDatastream(object, "RELS-EXT").getDatastreamVersion().get(0).getXmlContent().getAny();
        String model = MetsUtils.getModel(relsExt);
        String type = Const.typeMap.get(model);
        if (type == null) {
            throw new MetsExportException(object.getPID(), "Unknown model:" + model, false, null);
        }
        return new MetsElement(object, parent, metsContext, withChildren);
    }

    /**
     * Generates children of this element
     *
     */
    @Override
    public void fillChildren() throws MetsExportException {
        Node node = MetsUtils.xPathEvaluateNode(relsExt, "*[local-name()='RDF']/*[local-name()='Description']");
        NodeList hasPageNodes = node.getChildNodes();
        for (int a = 0; a < hasPageNodes.getLength(); a++) {
            if (MetsUtils.hasReferenceXML(hasPageNodes.item(a).getNodeName())) {
                Node rdfResourceNode = hasPageNodes.item(a).getAttributes().getNamedItem("rdf:resource");
                String fileName = rdfResourceNode.getNodeValue();

                DigitalObject object = null;
                if (metsContext.getFedoraClient() != null) {
                    object = MetsUtils.readRelatedFoXML(fileName, metsContext.getFedoraClient());
                } else {
                    object = MetsUtils.readRelatedFoXML(metsContext.getPath(), fileName);
                }
                MetsElement child = new MetsElement(object, this, metsContext, true);
                this.children.add(child);
                LOG.log(Level.FINE, "Child found for:" + getOriginalPid() + "(" + getElementType() + ") - " + child.getOriginalPid() + "(" + child.getElementType() + ")");
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#accept(cz
     * .cas.lib.proarc.common.export.mets.structure.IMetsElementVisitor)
     */
    @Override
    public void accept(IMetsElementVisitor metsVisitor) throws MetsExportException {
        metsVisitor.insertIntoMets(this);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.mets.structure.IMetsElement#getModsStream
     * ()
     */
    @Override
    public List<Element> getModsStream() {
        return this.modsStream;
    }
}
