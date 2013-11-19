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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.MutableSeq;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.mets.AreaType;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.DivType.Fptr;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType;
import cz.cas.lib.proarc.mets.StructLinkType.SmLink;

/**
 * 
 * Java representation of Internal part mets object
 * 
 * @author Robert Simonovsky
 * 
 */
public class IntPart extends MetsElement {

    protected byte[] structStream;
    protected byte[] ocrStream;
    protected String label;
    MutableSeq seq = new MutableSeq();
    private static Logger LOG = Logger.getLogger(IntPart.class.getName());

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
     * Adds the internal elements into the mets div
     * 
     * @param parentType
     */
    private void addInternalElements(DivType parentType) {
        List<IntPartInfo> partInfoList = parseAltoInfo(MetsUtils.getDocumentFromBytes(structStream));
        for (IntPartInfo partInfo : partInfoList) {
            seq.add(1);
            DivType divType = new DivType();
            divType.setTYPE(partInfo.getType());
            try {
                divType.setORDER(new BigInteger(partInfo.getOrder()));
            } catch (NumberFormatException ex) {
                LOG.log(Level.WARNING, partInfo.getOrder() + " is not a number in  object " + this.originalPID);
            }
            String number = String.format("%04d", seq.get());

            /**
             * if an internal element is part of article, then the ID is
             * inherited
             */
            if ("ARTICLE".equalsIgnoreCase(parent.type)) {
                divType.setID(parent.getElementId() + "_" + number);
            } else {
                divType.setID(getElementId() + "_" + number);
            }
            Fptr fptr = new Fptr();
            AreaType area = new AreaType();
            Page refPage = (Page) metsInfo.pidElements.get(partInfo.getAltoPID());
            area.setFILEID(refPage.getALTOfile());
            area.setBEGIN(partInfo.getBegin());
            area.setBETYPE("IDREF");
            fptr.setArea(area);
            divType.getFptr().add(fptr);
            parentType.getDiv().add(divType);
        }
    }

    /* Fills the "isOnPage" structure */
    private void fillIsOnPage() {
        Node node = MetsUtils.xPathEvaluateNode(RELExtstream, "*[local-name()='RDF']/*[local-name()='Description']");
        NodeList hasPageNodes = node.getChildNodes();
        for (int a = 0; a < hasPageNodes.getLength(); a++) {
            if (hasPageNodes.item(a).getNodeName().equalsIgnoreCase(Const.ISONPAGE)) {
                String fileName = hasPageNodes.item(a).getAttributes().getNamedItem("rdf:resource").getNodeValue();
                Page page = (Page) metsInfo.pidElements.get(fileName.substring(fileName.indexOf("/") + 1));
                SmLink smLink = new SmLink();
                smLink.setFrom(getElementId());
                smLink.setTo(page.getPageId());
                if (metsInfo.mets.getStructLink() == null) {
                    metsInfo.mets.setStructLink(new MetsType.StructLink());
                }
                metsInfo.mets.getStructLink().getSmLinkOrSmLinkGrp().add(smLink);
            }
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
    public void insertIntoMets(Mets mets, boolean withChildren, String outputDirectory) {
        super.insertIntoMets(mets, withChildren, outputDirectory);
        fillIsOnPage();
    };

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.kramerius.importFoXML.structure.MetsElement#insertIntoDiv(org.kramerius
     * .mets.DivType)
     */
    @Override
    public DivType insertIntoDiv(DivType parentDiv) {
        DivType elementDivType = new DivType();
        if (parent.type.equalsIgnoreCase("ARTICLE")) {
            this.seq = ((IntPart) parent).seq;
            seq.add(1);
            // TODO elementDivType.setORDER
            // BigInteger(partInfo.getOrder()));
            String number = String.format("%04d", seq.get());
            elementDivType.setID(parent.getElementId() + "_" + number);
        } else {
            elementDivType.setID(getElementId());
            elementDivType.setORDER(BigInteger.valueOf(seq.get()));
        }

        elementDivType.setLabel(this.getLabel());
        elementDivType.setTYPE(this.type);
        elementDivType.getDMDID().add(this.modsMetsElement);

        parentDiv.getDiv().add(elementDivType);
        addInternalElements(elementDivType);
        return elementDivType;
    }

    /**
     * Internal part constructor
     * 
     * @param object
     * @param path
     * @param parent
     * @param withChildren
     * @param metsInfo
     */
    public IntPart(DigitalObject object, Object parent, boolean withChildren, MetsInfo metsInfo) {
        super(object, parent, withChildren, metsInfo);
        if (metsInfo.fedoraClient != null) {
            structStream = MetsUtils.getBinaryDataStreams(metsInfo.fedoraClient, object.getPID(), "STRUCT_MAP");
            ocrStream = MetsUtils.getBinaryDataStreams(metsInfo.fedoraClient, object.getPID(), "TEXT_OCR");
        } else {
            structStream = MetsUtils.getBinaryDataStreams(object.getDatastream(), "STRUCT_MAP");
            ocrStream = MetsUtils.getBinaryDataStreams(object.getDatastream(), "TEXT_OCR");
        }
        this.setLabel(MetsUtils.getProperty(Const.FEDORA_LABEL, object.getObjectProperties().getProperty()));
    }

    /**
     * 
     * Returns a label of internal element - it's used for specifying the DIV
     * label
     * 
     * @return
     */
    public String getLabel() {
        return label;
    }

    /**
     * 
     * Sets the internal part label
     * 
     * @param label
     */
    public void setLabel(String label) {
        this.label = label;
    }
}