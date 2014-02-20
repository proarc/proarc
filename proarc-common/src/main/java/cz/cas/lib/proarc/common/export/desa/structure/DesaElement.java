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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.export.desa.Const;
import cz.cas.lib.proarc.common.export.desa.DesaContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;

/**
 * Class that represents the element of Desa export
 *
 * @author Robert Simonovsky
 *
 */
public class DesaElement implements IDesaElement {
    public final List<Element> descriptor;
    public final String model;
    private final DesaContext desaContext;
    private final String originalPid;
    private final Logger LOG = Logger.getLogger(DesaElement.class.getName());
    private DesaElement parent;
    private final List<DesaElement> children = new ArrayList<DesaElement>();
    private final List<Element> relsExt;
    private final DigitalObject sourceObject;
    private String idSIPVersion;
    private final String descriptorType;

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getDescriptorType
     * ()
     */
    @Override
    public String getDescriptorType() {
        return descriptorType;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getIdSIPVersion
     * ()
     */
    @Override
    public String getIdSIPVersion() {
        return idSIPVersion;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#setIdSIPVersion
     * (java.lang.String)
     */
    @Override
    public void setIdSIPVersion(String idSIPVersion) {
        this.idSIPVersion = idSIPVersion;
    }

    private final String elementType;
    private final String elementID;
    private String zipName;

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getZipName()
     */
    @Override
    public String getZipName() {
        return zipName;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#setZipName
     * (java.lang.String)
     */
    @Override
    public void setZipName(String zipName) throws MetsExportException {
        this.zipName = zipName;
        if (!checkZipFileOK()) {
            LOG.severe("Invalid identifier for " + this.getOriginalPid() + " filename is not valid:" + zipName);
            throw new MetsExportException(this.getOriginalPid(), "Identifier is not valid - generated output file name is:" + zipName, false, null);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getDesaContext
     * ()
     */
    @Override
    public DesaContext getDesaContext() {
        return desaContext;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getElementType
     * ()
     */
    @Override
    public String getElementType() {
        return elementType;
    }

    /**
     * Constructor
     *
     * @param digitalObject
     * @param parent
     * @param desaContext
     * @param fillChildren
     * @throws MetsExportException
     */
    public DesaElement(DigitalObject digitalObject, Object parent, DesaContext desaContext, boolean fillChildren) throws MetsExportException {
        this.desaContext = desaContext;
        this.sourceObject = digitalObject;
        this.relsExt = FoxmlUtils.findDatastream(digitalObject, "RELS-EXT").getDatastreamVersion().get(0).getXmlContent().getAny();
        DatastreamVersionType biblioVersion = FoxmlUtils.findDatastream(digitalObject, "BIBLIO_MODS").getDatastreamVersion().get(0);
        this.descriptor = biblioVersion.getXmlContent().getAny();
        this.descriptorType = biblioVersion.getFORMATURI();
        model = MetsUtils.getModel(relsExt);
        this.elementType = Const.typeMap.get(model);
        this.elementID = this.elementType + desaContext.addElementId(this.elementType);
        this.originalPid = digitalObject.getPID();
        if (parent instanceof DesaElement) {
            this.parent = (DesaElement) parent;
        }
        if (fillChildren) {
            fillChildren();
        }
        if (parent == null) {
            if (Const.FILE.equals(this.getElementType())) {
                this.parent = initParent();
            }
        }

        if (this.parent == null) {
            desaContext.setRootElement(this);
            LOG.log(Level.FINE, "Root element found:" + getOriginalPid() + "(" + getElementType() + ")");
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getElementID
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
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getParent()
     */
    @Override
    public DesaElement getParent() {
        return parent;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getChildren()
     */
    @Override
    public List<DesaElement> getChildren() {
        return children;
    }

    /**
     * Inits the parent element of current element
     *
     * @return
     * @throws MetsExportException
     */
    private DesaElement initParent() throws MetsExportException {
        String parentId;
        if (desaContext.getFedoraClient() != null) {
            parentId = MetsUtils.getParent(originalPid, desaContext.getRemoteStorage());
            LOG.fine("Parent found from Fedora:" + parentId);
        } else {
            parentId = MetsUtils.getParent(originalPid, desaContext.getFsParentMap());
            LOG.fine("Parent found from Local:" + parentId);
        }

        if (parentId == null) {
            LOG.fine("Parent not found - returning null");
            return null;
        }

        DigitalObject parentObject = null;
        if (desaContext.getFedoraClient() != null) {
            parentObject = MetsUtils.readRelatedFoXML(parentId, desaContext.getFedoraClient());
        } else {
            parentObject = MetsUtils.readRelatedFoXML(desaContext.getPath(), parentId);
        }
        DesaElement parentInit = new DesaElement(parentObject, null, desaContext, false);
        parentInit.children.add(this);
        return parentInit;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getDescriptor
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
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getModel()
     */
    @Override
    public String getModel() {
        return model;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getOriginalPid
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
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getRelsExt()
     */
    @Override
    public List<Element> getRelsExt() {
        return relsExt;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#getSourceObject
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
     * @param desaContext
     * @param withChildren
     * @return
     * @throws MetsExportException
     */
    public static DesaElement getElement(DigitalObject object, DesaElement parent, DesaContext desaContext, boolean withChildren) throws MetsExportException {
        List<Element> relsExt = FoxmlUtils.findDatastream(object, "RELS-EXT").getDatastreamVersion().get(0).getXmlContent().getAny();
        String model = MetsUtils.getModel(relsExt);
        String type = Const.typeMap.get(model);
        if (type == null) {
            throw new MetsExportException(object.getPID(), "Unknown model:" + model, false, null);
        }
        return new DesaElement(object, parent, desaContext, withChildren);
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
                if (desaContext.getFedoraClient() != null) {
                    object = MetsUtils.readRelatedFoXML(fileName, desaContext.getFedoraClient());
                } else {
                    object = MetsUtils.readRelatedFoXML(desaContext.getPath(), fileName);
                }
                DesaElement child = new DesaElement(object, this, desaContext, true);
                this.children.add(child);
                LOG.log(Level.FINE, "Child found for:" + getOriginalPid() + "(" + getElementType() + ") - " + child.getOriginalPid() + "(" + child.getElementType() + ")");
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#accept(cz
     * .cas.lib.proarc.common.export.desa.structure.IDesaElementVisitor)
     */
    @Override
    public void accept(IDesaElementVisitor desaVisitor) throws MetsExportException {
        desaVisitor.insertIntoMets(this, null);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * cz.cas.lib.proarc.common.export.desa.structure.IDesaElement#accept(cz
     * .cas.lib.proarc.common.export.desa.structure.IDesaElementVisitor)
     */
    @Override
    public void accept(IDesaElementVisitor desaVisitor, HashMap<String, String> desaProps) throws MetsExportException {
        LOG.info("Export visitor accepted for:" + this.getOriginalPid() + "(" + this.getElementType() + ")");
        desaVisitor.insertIntoMets(this, desaProps);
    }

    /**
     * Checks if fileName is correct
     *
     * @param folder
     */
    private boolean checkZipFileOK() {
        File file = null;
        try {
            file = new File(this.getDesaContext().getOutputPath() + File.separator + this.getZipName() + ".zip");
            LOG.fine("Checking if fileName is OK:" + file.getAbsolutePath());
            if (file.exists()) {
                LOG.log(Level.SEVERE, "File:" + file.getAbsolutePath() + " already exists !");
                return false;
            }
            file.createNewFile();
            file.delete();
            return true;
        } catch (IOException e) {
            LOG.log(Level.WARNING, "file:" + file.getAbsolutePath() + " is not valid filename", e);
            return false;
        }

    }
}
