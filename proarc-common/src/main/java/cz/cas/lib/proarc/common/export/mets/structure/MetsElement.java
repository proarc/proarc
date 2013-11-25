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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

/**
 * Java class representing simple Mets element (Title, Volume,..)
 * 
 * @author Robert Simonovsky
 * 
 */
public class MetsElement {
    private final String id;
    private static Logger LOG = Logger.getLogger(MetsElement.class.getName());

    /**
     * Return uuid of element
     * 
     * @return
     */
    public String getId() {
        return id;
    }

    protected String originalPID;
    protected List<Element> DCstream;
    protected List<Element> MODSstream;
    protected List<Element> RELExtstream;
    public MdSecType modsMetsElement;
    private final Integer modOrder;
    public String type;
    protected MetsElement parent;
    protected MetsInfo metsInfo;
    public List<MetsElement> children = new ArrayList<MetsElement>();

    /**
     * 
     * Collects all identifiers for mods element
     * 
     * @return
     */
    public Map<String, String> getModsIdentifiers() throws MetsExportException {
        Map<String, String> result = new HashMap<String, String>();
        String XPATH = "*[local-name()='mods']";
        Node descNode = MetsUtils.xPathEvaluateNode(MetsUtils.removeModsCollection(MODSstream), XPATH);
        NodeList nodeList = descNode.getChildNodes();
        for (int a = 0; a < nodeList.getLength(); a++) {
            Node node = nodeList.item(a);
            if ("identifier".equalsIgnoreCase(node.getLocalName())) {
                result.put(node.getAttributes().getNamedItem("type").getNodeValue(), node.getNodeValue());
            }
        }
        return result;

    }

    /**
     * Returns the mets id string for element
     * 
     * @return
     */
    public String getElementId() throws MetsExportException {
        /* monographUnit */
        if (MetsUtils.isMultiUnitMonograph(this)) {
            return "TITLE_" + String.format("%04d", modOrder);
        }
        return MetsUtils.getModName(type) + "_" + String.format("%04d", modOrder);
    }

    /**
     * Inserts the element into a mets div
     * 
     * @param parentDiv
     * @return
     */
    public DivType insertIntoDiv(DivType parentDiv) throws MetsExportException {
        DivType elementDivType = new DivType();
        if (Const.VOLUME.equalsIgnoreCase(this.type) && (MetsUtils.isMultiUnitMonograph(this))) {
            elementDivType.setTYPE("MONOGRAPH");
        } else if (Const.MONOGRAPHUNIT.equalsIgnoreCase(this.type)) {
            elementDivType.setTYPE("VOLUME");
        } else
            elementDivType.setTYPE(this.type);

        elementDivType.setID(getElementId());
        elementDivType.getDMDID().add(this.modsMetsElement);
        elementDivType.setLabel(metsInfo.getLabel());
        if (parentDiv != null) {
            parentDiv.getDiv().add(elementDivType);
        }
        return elementDivType;
    }

    /**
     * Returns an element for digital object
     * 
     * @param object
     * @param path
     * @param parent
     * @param metsInfo
     * @param withChildren
     * @return
     */
    protected static MetsElement getElement(DigitalObject object, Object parent, MetsInfo metsInfo, boolean withChildren) throws MetsExportException {
        MetsElement result = null;
        String type = MetsUtils.getTypeModel(object, metsInfo);

        if (Const.PERIODICAL_TITLE.equalsIgnoreCase(type)) {
            result = new MetsElement(object, parent, withChildren, metsInfo);
        }

        if (Const.PERIODICAL_VOLUME.equalsIgnoreCase(type)) {
            result = new MetsElement(object, parent, withChildren, metsInfo);
        }

        if (Const.ISSUE.equalsIgnoreCase(type)) {
            result = new MetsElement(object, parent, withChildren, metsInfo);
        }

        if (Const.PAGE.equalsIgnoreCase(type)) {
            result = new Page(object, parent, withChildren, metsInfo);
        }

        if (Const.PICTURE.equalsIgnoreCase(type)) {
            result = new IntPart(object, parent, withChildren, metsInfo);
        }

        if (Const.ARTICLE.equalsIgnoreCase(type)) {
            result = new IntPart(object, parent, withChildren, metsInfo);
        }

        if (Const.VOLUME.equalsIgnoreCase(type)) {
            result = new MetsElement(object, parent, withChildren, metsInfo);

        }
        if (Const.MONOGRAPHUNIT.equalsIgnoreCase(type)) {
            result = new MetsElement(object, parent, withChildren, metsInfo);
        }
        return result;
    }

    /**
     * Inits parent tree of the element
     * 
     * @param object
     * @return
     */
    private MetsElement initParent(DigitalObject object) throws MetsExportException {
        String parentId;
        if (metsInfo.fedoraClient != null) {
            parentId = MetsUtils.getParent(id, metsInfo.remoteStorage);
            LOG.info("Parent found from Fedora:" + parentId);
        } else {
            parentId = MetsUtils.getParent(id);
            LOG.info("Parent found from Local:" + parentId);
        }

        if (parentId == null) {
            return null;
        }

        DigitalObject parentObject = null;
        if (metsInfo.fedoraClient != null) {
            parentObject = MetsUtils.readRelatedFoXML(parentId, metsInfo.fedoraClient);
        } else {
            parentObject = MetsUtils.readRelatedFoXML(metsInfo.getPath(), parentId);
        }
        MetsElement parentInit = getElement(parentObject, null, metsInfo, false);
        return parentInit;
    }

    /**
     * Registers child of the element
     */
    private void registerChild() {
        this.parent.children.add(this);
        if (this.parent.metsInfo != null) {
            this.metsInfo = this.parent.metsInfo;
        }
    }

    /**
     * Constructor of mets element
     * 
     * @param object
     * @param path
     * @param parent
     * @param withChildren
     * @param metsInfo
     */
    public MetsElement(DigitalObject object, Object parent, boolean withChildren, MetsInfo metsInfo) throws MetsExportException {
        this.metsInfo = metsInfo;
        originalPID = object.getPID();
        metsInfo.pidElements.put(originalPID, this);
        if (metsInfo.fedoraClient != null) {
            DCstream = MetsUtils.getDataStreams(metsInfo.fedoraClient, object.getPID(), "DC");
            MODSstream = MetsUtils.getDataStreams(metsInfo.fedoraClient, object.getPID(), "BIBLIO_MODS");
            RELExtstream = MetsUtils.getDataStreams(metsInfo.fedoraClient, object.getPID(), "RELS-EXT");
        } else {
            DCstream = MetsUtils.getDataStreams(object.getDatastream(), "DC");
            MODSstream = MetsUtils.getDataStreams(object.getDatastream(), "BIBLIO_MODS");
            RELExtstream = MetsUtils.getDataStreams(object.getDatastream(), "RELS-EXT");
        }
        this.id = MetsUtils.getObjectId(RELExtstream);
        this.type = MetsUtils.getTypeModel(RELExtstream);
        this.modOrder = metsInfo.getModOrder(type);

        if (parent == null) {
            parent = initParent(object);
        }

        this.parent = (MetsElement) parent;

        if (this.parent != null) {
            registerChild();
        } else {
            metsInfo.rootElement = this;
            metsInfo.setLabel(MetsUtils.getProperty(Const.FEDORA_LABEL, object.getObjectProperties().getProperty()));
            try {
                metsInfo.setCreateDate(DatatypeFactory.newInstance().newXMLGregorianCalendar(MetsUtils.getProperty(Const.FEDORA_CREATEDATE, object.getObjectProperties().getProperty())));
                metsInfo.setLastModDate(DatatypeFactory.newInstance().newXMLGregorianCalendar(MetsUtils.getProperty(Const.FEDORA_LASTMODIFIED, object.getObjectProperties().getProperty())));
            } catch (DatatypeConfigurationException e) {
                throw new MetsExportException("Unable to set dates", false, e);
            }
        }
        if (withChildren) {
            fillChildren();
        }
    }

    /**
     * Inserts an element into mets
     * 
     * @param mets
     * @param withChildren
     * @param outputDirectory
     */
    public void insertIntoMets(Mets mets, boolean withChildren, String outputDirectory) throws MetsExportException {
        if ((parent != null) && (parent.modsMetsElement == null)) {
            parent.insertIntoMets(mets, false, outputDirectory);
        }
        modsMetsElement = MetsUtils.createMdSec("MODSMD_" + getElementId(), "MODS", "text/xml", MODSstream);
        mets.getDmdSec().add(MetsUtils.createMdSec("DCMD_" + getElementId(), "DC", "text/xml", DCstream));
        mets.getDmdSec().add(modsMetsElement);
        if (withChildren) {
            for (MetsElement element : children) {
                element.insertIntoMets(mets, withChildren, outputDirectory);
            }
        }
        Document docMods = MetsUtils.getDocumentFromList(MODSstream);
        Document docDC = MetsUtils.getDocumentFromList(DCstream);
        try {
            MetsUtils.validateAgainstXSD(docMods, ModsDefinition.class.getResourceAsStream("mods.xsd"));
        } catch (MetsExportException ex) {
            LOG.log(Level.WARNING, "Invalid MODS xml:" + this.getElementId() + "/" + this.originalPID);
            metsInfo.metsExportException.addException(this.originalPID, "Invalid MODS xml for " + this.originalPID, true, ex.exceptionList.get(0).getEx());
            LOG.log(Level.INFO, MetsUtils.documentToString(docMods));
        }
        try {
            MetsUtils.validateAgainstXSD(docDC, OaiDcType.class.getResourceAsStream("dc_oai.xsd"));
        } catch (MetsExportException ex) {
            LOG.log(Level.WARNING, "Invalid DC xml:" + this.getElementId() + "/" + this.originalPID);
            LOG.log(Level.INFO, MetsUtils.documentToString(docDC));
            metsInfo.metsExportException.addException(this.originalPID, "Invalid DC xml for " + this.originalPID, true, ex.exceptionList.get(0).getEx());
        }
    }

    /**
     * Generates children of this element
     * 
     */
    protected void fillChildren() throws MetsExportException {
        Node node = MetsUtils.xPathEvaluateNode(RELExtstream, "*[local-name()='RDF']/*[local-name()='Description']");
        NodeList hasPageNodes = node.getChildNodes();
        for (int a = 0; a < hasPageNodes.getLength(); a++) {
            if (MetsUtils.hasReferenceXML(hasPageNodes.item(a).getNodeName())) {
                Node rdfResourceNode = hasPageNodes.item(a).getAttributes().getNamedItem("rdf:resource");
                String fileName = rdfResourceNode.getNodeValue();

                DigitalObject object = null;
                if (metsInfo.fedoraClient != null) {
                    object = MetsUtils.readRelatedFoXML(fileName, metsInfo.fedoraClient);
                } else {
                    object = MetsUtils.readRelatedFoXML(metsInfo.getPath(), fileName);
                }
                getElement(object, this, metsInfo, true);
            }
        }
    }
}
