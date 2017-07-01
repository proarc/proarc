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

package cz.cas.lib.proarc.common.export.mets;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.apache.commons.codec.binary.Hex;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.response.FedoraResponse;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;

import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mets.info.Info;
import cz.cas.lib.proarc.mets.info.Info.Checksum;
import cz.cas.lib.proarc.mets.info.Info.Itemlist;
import cz.cas.lib.proarc.mets.info.Info.Titleid;
import cz.cas.lib.proarc.mets.info.Info.Validation;

/**
 * @author Robert Simonovsky
 *
 *         Utility class
 *
 */
public class MetsUtils {

    private static Logger LOG = Logger.getLogger(MetsUtils.class.getName());
    private static Properties mimeToExtension = new Properties();

    /**
     * Retuns an XMLGregorianCalendar representation of current date
     *
     * @return
     * @throws MetsExportException
     */
    public static XMLGregorianCalendar getCurrentDate() throws MetsExportException {
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
        return calendar;
    }

    /**
     * Returns the properties for mapping Mime type to file extension
     *
     * @return
     * @throws MetsExportException
     */
    public static Properties getMimeToExtension() throws MetsExportException {
        if (mimeToExtension.isEmpty()) {
            try {
                mimeToExtension.loadFromXML(MetsUtils.class.getResourceAsStream("mimeToExt.xml"));
            } catch (Exception e) {
                throw new MetsExportException("Unable to read mime type mapping", false, e);
            }
        }
        return mimeToExtension;
    }

    private static void findChildPSPs(DigitalObject dObj, MetsContext ctx, List<String> psps, String parentType) throws MetsExportException {
        List<Element> relsExt = FoxmlUtils.findDatastream(dObj, "RELS-EXT").getDatastreamVersion().get(0).getXmlContent().getAny();
        Node node = MetsUtils.xPathEvaluateNode(relsExt, "*[local-name()='RDF']/*[local-name()='Description']");

        NodeList hasPageNodes = node.getChildNodes();
        for (int a = 0; a < hasPageNodes.getLength(); a++) {
            if (MetsUtils.hasReferenceXML(hasPageNodes.item(a).getNodeName())) {
                Node rdfResourceNode = hasPageNodes.item(a).getAttributes().getNamedItem("rdf:resource");
                String fileName = rdfResourceNode.getNodeValue();

                DigitalObject object = null;
                if (ctx.getFedoraClient() != null) {
                    object = MetsUtils.readRelatedFoXML(fileName, ctx.getFedoraClient());
                } else {
                    object = MetsUtils.readRelatedFoXML(ctx.getPath(), fileName);
                }
                relsExt = FoxmlUtils.findDatastream(object, "RELS-EXT").getDatastreamVersion().get(0).getXmlContent().getAny();
                String model = MetsUtils.getModel(relsExt);
                String elementType = Const.typeMap.get(model);

                if (Const.PSPElements.contains(elementType)) {
                    if (((Const.MONOGRAPH_UNIT.equals(parentType) || (Const.ISSUE.equals(parentType)))) && (Const.SUPPLEMENT.equals(elementType))) {
                        // do not add
                    } else {
                        psps.add(object.getPID());
                    }
                } else {
                    findChildPSPs(object, ctx, psps, elementType);
                }
            }
        }
    }

    public static List<String> findPSPPIDs(String pid, MetsContext ctx, boolean fillChildren) throws MetsExportException {
        List<String> result = new ArrayList<String>();
        DigitalObject dObj;
        if (ctx.getFedoraClient() != null) {
            dObj = readFoXML(pid, ctx.getFedoraClient());
        } else {
            dObj = readFoXML(ctx.getPath() + File.separator + pid + ".xml");
        }
        // List<Element> relsExt = FoxmlUtils.findDatastream(dObj,
        // "RELS-EXT").getDatastreamVersion().get(0).getXmlContent().getAny();
        // String model = MetsUtils.getModel(relsExt);
        // String elementType = Const.typeMap.get(model);

        String parentId = pid;
        String parentModel = null;
        String parentType = null;
        List<Element> parentRels = null;
        DigitalObject parentdbObj = null;
        String firstParentType = null;

        // if (ctx.getFedoraClient() != null) {
        // parentId = MetsUtils.getParent(pid, ctx.getRemoteStorage());
        // } else {
        // parentId = MetsUtils.getParent(pid, ctx.getFsParentMap());
        // }
        //
        // if (Const.PSPElements.contains(parentType)) {
        // result.add(e)
        // }

        while (parentId != null) {
            if (ctx.getFedoraClient() != null) {
                parentdbObj = readFoXML(parentId, ctx.getFedoraClient());
            } else {
                parentdbObj = readFoXML(ctx.getPath() + File.separator + parentId + ".xml");
            }
            parentRels = FoxmlUtils.findDatastream(parentdbObj, "RELS-EXT").getDatastreamVersion().get(0).getXmlContent().getAny();
            parentModel = MetsUtils.getModel(parentRels);
            parentType = Const.typeMap.get(parentModel);

            if ((parentId.equals(pid)) && (firstParentType == null)) {
                firstParentType = parentType;
            }

            String oldParentId = parentId;

            if (ctx.getFedoraClient() != null) {
                parentId = MetsUtils.getParent(parentId, ctx.getRemoteStorage());
            } else {
                parentId = MetsUtils.getParent(parentId, ctx.getFsParentMap());
            }

            if (Const.PSPElements.contains(parentType)) {
                if (Const.SUPPLEMENT.equals(parentType)) {
                    if (parentId != null) {
                        DigitalObject parentdbObjSupp;
                        if (ctx.getFedoraClient() != null) {
                            parentdbObjSupp = readFoXML(parentId, ctx.getFedoraClient());
                        } else {
                            parentdbObjSupp = readFoXML(ctx.getPath() + File.separator + parentId + ".xml");
                        }
                        List<Element> parentRelsSupp = FoxmlUtils.findDatastream(parentdbObjSupp, "RELS-EXT").getDatastreamVersion().get(0).getXmlContent().getAny();
                        String parentTypeSupp = Const.typeMap.get(MetsUtils.getModel(parentRelsSupp));
                        if (Const.MONOGRAPH_UNIT.equals(parentTypeSupp) || (Const.ISSUE.equals(parentTypeSupp))) {
                            // do not add an PSP for Supplement under monograph
                            // unit or issue
                        } else {
                            result.add(oldParentId);
                        }
                    }
                } else {
                    result.add(oldParentId);
                }
            }
        }

        if (fillChildren) {
            findChildPSPs(dObj, ctx, result, firstParentType);
        }

        return result;
    }

    /**
     *
     * Converts byte array to hex string
     *
     * @param byteArray
     * @return
     */
    public static String byteToHex(byte[] byteArray) {
        StringBuffer result = new StringBuffer();
        for (byte b : byteArray) {
            result.append(String.format("%02X", b));
        }
        return result.toString();
    }

    /**
     *
     * Returns a file name (content location) from the datastream
     *
     * @param elements
     * @return
     */
    public static String getFileNameFromStream(List<Element> elements) throws MetsExportException {
        if (elements == null) {
            return null;
        }
        return MetsUtils.xPathEvaluateString(elements, "*[local-name()='datastreamVersion']/*[local-name()='contentLocation'/@REF");
    }

    /**
     *
     * Returns a mime type attribute from datastream
     *
     * @param elements
     * @return
     */
    public static String getMimeFromStream(List<Element> elements) throws MetsExportException {
        if (elements == null) {
            return null;
        }
        return MetsUtils.xPathEvaluateString(elements, "*[local-name()='datastreamVersion']/@MIMETYPE");
    }

    /**
     *
     * Returns a property value from a list of properties
     *
     * @param name
     * @param properties
     * @return
     */
    public static String getProperty(String name, java.util.List<PropertyType> properties) throws MetsExportException {
        if (name == null) {
            throw new MetsExportException("Name is null");
        }
        if (properties == null) {
            throw new MetsExportException("Properties is null");
        }
        for (PropertyType property : properties) {
            if (name.equalsIgnoreCase(property.getNAME())) {
                return property.getVALUE();
            }
        }
        throw new MetsExportException("Property " + name + " not found");
    }

    /**
     * Removes the schemaLocation attribute
     *
     * @param elements
     * @return
     */
    public static List<Element> removeSchemaLocation(List<Element> elements) {
        if (elements.size() > 0) {
            Element element = elements.get(0);
            element.removeAttribute("xsi:schemaLocation");
            element.removeAttribute("schemaLocation");
        }
        return elements;
    }

    /**
     *
     * Removes the top element "modsCollection" from the xml
     *
     * @param elements
     * @return
     */
    public static List<Element> removeModsCollection(List<Element> elements) {
        if (elements.size() > 0) {
            if ("mods:modsCollection".equalsIgnoreCase(elements.get(0).getNodeName())) {
                NodeList nl = elements.get(0).getChildNodes();
                List<Element> result = new ArrayList<Element>();
                result.add((Element) nl.item(0));
                return result;
            } else {
                return elements;
            }
        }
        return null;
    }

    /**
     *
     * Returns a datastream of given type
     *
     * @param datastreams
     * @param type
     * @return
     */
    public static List<Element> getDataStreams(List<DatastreamType> datastreams, String type) {
        for (DatastreamType streamType : datastreams) {
            if (streamType.getID().startsWith(type)) {
                List<DatastreamVersionType> dsVersions = streamType.getDatastreamVersion();
                for (DatastreamVersionType dsVersion : dsVersions) {
                    XmlContentType dcContent = dsVersion.getXmlContent();
                    List<Element> elements = dcContent.getAny();
                    return elements;
                }
            }
        }
        return null;
    }

    /**
     *
     * Returns a datastream of given type from binary representation
     *
     * @param datastreams
     * @param type
     * @return
     */
    public static byte[] getBinaryDataStreams(List<DatastreamType> datastreams, String type) {
        for (DatastreamType streamType : datastreams) {
            if (streamType.getID().startsWith(type)) {
                List<DatastreamVersionType> dsVersions = streamType.getDatastreamVersion();
                for (DatastreamVersionType dsVersion : dsVersions) {
                    return dsVersion.getBinaryContent();
                }
            }
        }
        return null;
    }

    /**
     * Method for identifying dataStream name
     *
     * @param dataStream
     * @param streamName
     * @return
     */
    public static boolean equalDataStreams(String dataStream, String streamName) {
        if (dataStream.equalsIgnoreCase(streamName)) {
            return true;
        }
        if (dataStream.startsWith(streamName + ".")) {
            return true;
        }
        String datastreamIMG = "IMG_" + streamName;
        if (dataStream.equalsIgnoreCase(datastreamIMG)) {
            return true;
        }
        if (dataStream.startsWith(datastreamIMG + ".")) {
            return true;
        }
        return false;
    }

    /**
     *
     * Generates an XML document from list of elements
     *
     * @param elements
     * @return
     */
    public static Document getDocumentFromList(List<Element> elements) throws MetsExportException {
        Document document = null;
        try {
            DocumentBuilderFactory builder = DocumentBuilderFactory.newInstance();
            builder.setValidating(true);
            builder.setNamespaceAware(true);
            document = builder.newDocumentBuilder().newDocument();
        } catch (ParserConfigurationException e1) {
            throw new MetsExportException("Error while getting document from list", false, e1);
        }

        for (Element element : elements) {
            Node newNode = element.cloneNode(true);
            document.adoptNode(newNode);
            document.appendChild(newNode);
        }
        return document;
    }

    /**
     *
     * Returns a string from the xml document defined by the Xpath
     *
     * @param elements
     * @param xPath
     * @return
     */
    public static String xPathEvaluateString(List<Element> elements, String xPath) throws MetsExportException {
        XPath xpathObject = XPathFactory.newInstance().newXPath();

        Document document = getDocumentFromList(elements);
        try {
            return xpathObject.compile(xPath).evaluate(document);
        } catch (XPathExpressionException e) {
            throw new MetsExportException("Error while evaluating xPath:" + xPath, false, e);
        }
    }

    /**
     *
     * Returns a node from the xml document defined by the Xpath
     *
     * @param elements
     * @param xPath
     * @return
     */
    public static Node xPathEvaluateNode(List<Element> elements, String xPath) throws MetsExportException {
        Document document = null;
        try {
            document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
        } catch (ParserConfigurationException e1) {
            throw new MetsExportException("Error while evaluating xPath " + xPath, false, e1);
        }

        for (Element element : elements) {
            Node newNode = element.cloneNode(true);
            document.adoptNode(newNode);
            document.appendChild(newNode);
        }
        XPath xpathObject = XPathFactory.newInstance().newXPath();

        try {
            return (Node) xpathObject.compile(xPath).evaluate(document, XPathConstants.NODE);
        } catch (XPathExpressionException e) {
            throw new MetsExportException("Error while evaluating xPath " + xPath, false, e);
        }
    }

    /**
     *
     * Returns a model of the document
     *
     * @param relExtStream
     * @return
     */
    public static String getModel(List<Element> relExtStream) throws MetsExportException {
        Node hasPageNodes = MetsUtils.xPathEvaluateNode(relExtStream, "*[local-name()='RDF']/*[local-name()='Description']/*[local-name()='hasModel']");
        String model = hasPageNodes.getAttributes().getNamedItem("rdf:resource").getNodeValue();
        return model;
    }

    /**
     *
     * Returns a dataStream from Fedora for given pid
     *
     * @param fedoraClient
     * @param pid
     * @param streamName
     * @return
     * @throws MetsExportException
     */
    public static List<Element> getDataStreams(FedoraClient fedoraClient, String pid, String streamName) throws MetsExportException {
        try {
            FedoraResponse response = FedoraClient.getDatastreamDissemination(pid, streamName).execute(fedoraClient);
            InputStream is = response.getEntityInputStream();
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(is);
            List<Element> elements = new ArrayList<Element>();
            elements.add(doc.getDocumentElement());
            return elements;
        } catch (Exception ex) {
            throw new MetsExportException("Error while getting stream " + streamName + " from " + pid, false, ex);
        }
    }

    /**
     *
     * Copies inputStream to outputStream
     *
     * @param is
     * @param os
     * @throws IOException
     */
    public static void copyStream(InputStream is, OutputStream os) throws IOException {
        byte[] buffer = new byte[1024];
        int len;
        while ((len = is.read(buffer)) != -1) {
            os.write(buffer, 0, len);
        }
        is.close();
    }

    /**
     *
     * Returns the byteArray of the specified datastream from fedora
     *
     * @param fedoraClient
     * @param pid
     * @param streamName
     * @return
     * @throws MetsExportException
     */
    public static byte[] getBinaryDataStreams(FedoraClient fedoraClient, IMetsElement metsElement, String streamName) throws MetsExportException {
        try {
            DatastreamType rawDS = FoxmlUtils.findDatastream(metsElement.getSourceObject(), streamName);
            if (rawDS != null) {
                FedoraResponse response = FedoraClient.getDatastreamDissemination(metsElement.getOriginalPid(), streamName).execute(fedoraClient);
                InputStream is = response.getEntityInputStream();
                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                copyStream(is, bos);
                bos.close();
                return bos.toByteArray();
            } else {
                return null;
            }
        } catch (Exception ex) {
            throw new MetsExportException(metsElement.getOriginalPid(), "Error while getting stream " + streamName + " from " + metsElement.getElementType(), false, ex);
        }
    }

    /**
     *
     * Prepares a logical/physical structure divs in mets
     *
     * @param mets
     * @param label
     * @param type
     * @return
     */
    public static DivType createStructureDiv(Mets mets, String label, String type) {
        StructMapType structType = new StructMapType();
        mets.getStructMap().add(structType);
        structType.setLabel2(label);
        structType.setTYPE(type);
        DivType divType = new DivType();
        structType.setDiv(divType);
        divType.setLabel(mets.getLabel1());
        return divType;
    }

    /**
     *
     * Inits the file groups in mets
     *
     * @param mets
     * @return
     */
    public static HashMap<String, FileGrp> initFileGroups() {
        FileGrp MCimagesGRP = new FileGrp();
        MCimagesGRP.setID("MC_IMGGRP");
        MCimagesGRP.setUSE("Images");
        // mets.getFileSec().getFileGrp().add(MCimagesGRP);

        FileGrp UCimageGrp = new FileGrp();
        UCimageGrp.setID("UC_IMGGRP");
        UCimageGrp.setUSE("Images");
        // mets.getFileSec().getFileGrp().add(UCimageGrp);

        FileGrp AltoGRP = new FileGrp();
        AltoGRP.setID("ALTOGRP");
        AltoGRP.setUSE("Layout");
        // mets.getFileSec().getFileGrp().add(AltoGRP);

        FileGrp TxtGRP = new FileGrp();
        TxtGRP.setID("TXTGRP");
        TxtGRP.setUSE("Text");
        // mets.getFileSec().getFileGrp().add(TxtGRP);

        FileGrp TechMDGrp = new FileGrp();
        TechMDGrp.setID("TECHMDGRP");
        TechMDGrp.setUSE("Technical Metadata");
        // mets.getFileSec().getFileGrp().add(TechMDGrp);

        HashMap<String, FileGrp> fileGrpMap = new HashMap<String, FileGrp>();
        fileGrpMap.put("UC_IMGGRP", UCimageGrp);
        fileGrpMap.put("MC_IMGGRP", MCimagesGRP);
        fileGrpMap.put("ALTOGRP", AltoGRP);
        fileGrpMap.put("TXTGRP", TxtGRP);
        fileGrpMap.put("TECHMDGRP", TechMDGrp);
        return fileGrpMap;
    }

    /**
     *
     * Reads and unmarshalls Digital Object
     *
     * @param path
     * @return
     */
    public static DigitalObject readFoXML(String path) throws MetsExportException {
        DigitalObject foXMLObject;
        File file = new File(path);
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(DigitalObject.class);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            foXMLObject = (DigitalObject) unmarshaller.unmarshal(file);

            return foXMLObject;
        } catch (JAXBException e) {
            throw new MetsExportException("Unable to read FoXML document " + path, false, e);
        }
    }

    /**
     *
     * Reads and unmarshalls Digital Object from Fedora
     *
     * @param path
     * @return
     */
    public static DigitalObject readFoXML(String uuid, FedoraClient client) throws MetsExportException {
        DigitalObject foXMLObject = null;
        if (uuid.startsWith("info:fedora/")) {
            uuid = uuid.substring(uuid.indexOf("/") + 1);
        }
        LOG.log(Level.FINE, "Reading document from Fedora:" + uuid);
        try {
            FedoraResponse response = FedoraClient.getObjectXML(uuid).execute(client);
            JAXBContext jaxbContext = JAXBContext.newInstance(DigitalObject.class);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            foXMLObject = (DigitalObject) unmarshaller.unmarshal(response.getEntityInputStream());
        } catch (Exception e) {
            throw new MetsExportException("Unable to get " + uuid + " from Fedora", false, e);
        }
        return foXMLObject;
    }

    /**
     *
     * Transforms the xml document to a string
     *
     * @param doc
     * @return
     */
    public static String documentToString(Document doc) throws MetsExportException {
        try {
            StringWriter sw = new StringWriter();
            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer transformer = tf.newTransformer();
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
            transformer.setOutputProperty(OutputKeys.METHOD, "xml");
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");

            transformer.transform(new DOMSource(doc), new StreamResult(sw));
            return sw.toString();
        } catch (TransformerException ex) {
            throw new MetsExportException("Error converting Document to String", false, ex);
        }
    }

    /**
     *
     * Validates given document agains an XSD schema
     *
     * @param document
     * @param xsd
     * @return
     */
    public static List<String> validateAgainstXSD(Document document, InputStream xsd) throws Exception {
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        factory.setResourceResolver(MetsLSResolver.getInstance());
        Schema schema = factory.newSchema(new StreamSource(xsd));
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        DOMSource domSource = new DOMSource(document);
        StreamResult sResult = new StreamResult();
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        sResult.setOutputStream(bos);
        transformer.transform(domSource, sResult);
        InputStream is = new ByteArrayInputStream(bos.toByteArray());
        DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
        dbfactory.setValidating(false);
        dbfactory.setNamespaceAware(true);
        dbfactory.setSchema(schema);
        DocumentBuilder documentBuilder = dbfactory.newDocumentBuilder();
        ValidationErrorHandler errorHandler = new ValidationErrorHandler();
        documentBuilder.setErrorHandler(errorHandler);
        documentBuilder.parse(is);
        return errorHandler.getValidationErrors();
    }

    /**
     *
     * Validates given XML file against an XSD schema
     *
     * @param file
     * @param xsd
     * @return
     */
    public static List<String> validateAgainstXSD(File file, InputStream xsd) throws Exception {
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        factory.setResourceResolver(MetsLSResolver.getInstance());
        Schema schema = factory.newSchema(new StreamSource(xsd));
        DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
        dbfactory.setValidating(false);
        dbfactory.setNamespaceAware(true);
        dbfactory.setSchema(schema);
        DocumentBuilder documentBuilder = dbfactory.newDocumentBuilder();
        ValidationErrorHandler errorHandler = new ValidationErrorHandler();
        documentBuilder.setErrorHandler(errorHandler);
        documentBuilder.parse(file);
        return errorHandler.getValidationErrors();
    }

    /**
     *
     * Indicates if the "has..." is used for defining children
     *
     * @param name
     * @return
     */
    public static boolean hasReferenceXML(String name) {
        if (Const.HASINTCOMPPART.equalsIgnoreCase(name)) {
            return true;
        }
        if (Const.HASISSUE.equalsIgnoreCase(name)) {
            return true;
        }
        if (Const.HASMEMBER.equalsIgnoreCase(name)) {
            return true;
        }
        if (Const.HASPAGE.equalsIgnoreCase(name)) {
            return true;
        }
        if (Const.HASUNIT.equalsIgnoreCase(name)) {
            return true;
        }
        if (Const.HASVOLUME.equalsIgnoreCase(name)) {
            return true;
        }
        return false;
    }

     /* Return a valid identifier for mets document removes whitespaces and if an
     * identifier does not start with a letter it adds a prefix
     *
     * @param identifier
     * @return
     */
    public static String validateIdentifier(String identifier) {
        identifier = removeNonAlpabetChars(identifier);
        if (!(identifier.toUpperCase().substring(0, 1).matches("[A-Z]"))) {
            return "FID_" + identifier;
        } else {
            return identifier;
        }
    }

    /**
     * Returns a string with alphabetical characters only
     *
     * @param inputString
     * @return
     */
    public static String removeNonAlpabetChars(String inputString) {
        String validChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz_-.";
        String output = "";
        for (int a = 0; a < inputString.length(); a++) {
            if (validChars.contains(inputString.substring(a, a + 1))) {
                output = output + inputString.substring(a, a + 1);
            }
        }
        return output;
    }

    public static void addModsIdentifiersRecursive(MetsElement element, Info infoJaxb) throws MetsExportException {
        Map<String, String> identifiers = element.getModsIdentifiers();
        for (String type : identifiers.keySet()) {
            if (Const.allowedIdentifiers.contains(type)) {
                boolean alreadyAdded = false;
                for (Titleid titleId : infoJaxb.getTitleid()) {
                    if ((titleId.getType().equals(type)) && (titleId.getValue().equals(identifiers.get(type)))) {
                        alreadyAdded = true;
                        break;
                    }
                }
                if (!alreadyAdded) {
                    Titleid titleId = new Titleid();
                    titleId.setType(type);
                    titleId.setValue(identifiers.get(type));
                    infoJaxb.getTitleid().add(titleId);
                }
            }
        }

        for (MetsElement child : element.getChildren()) {
            addModsIdentifiersRecursive(child, infoJaxb);
        }
    }

    /**
     *
     * Generates and saves info.xml
     *
     * @param path
     * @param mets
     */
    public static void saveInfoFile(String path, MetsContext metsContext, String md5, String fileMd5Name, File metsFile) throws MetsExportException {
        File infoFile = new File(path + File.separator + metsContext.getPackageID() + File.separator + "info_" + metsContext.getPackageID() + ".xml");
            GregorianCalendar c = new GregorianCalendar();
            c.setTime(new Date());
        XMLGregorianCalendar date2;
        try {
            date2 = DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
        } catch (DatatypeConfigurationException e1) {
            throw new MetsExportException("Error while generating info.xml file", false, e1);
        }
            Info infoJaxb = new Info();
            infoJaxb.setCreated(date2);
            infoJaxb.setMainmets("./" + metsFile.getName());
            Checksum checkSum = new Checksum();
            checkSum.setChecksum(md5);
            checkSum.setType("MD5");
        addModsIdentifiersRecursive(metsContext.getRootElement(), infoJaxb);
            checkSum.setValue(fileMd5Name);
            infoJaxb.setChecksum(checkSum);
            Validation validation = new Validation();
            validation.setValue("W3C-XML");
            validation.setVersion(Float.valueOf("0.0"));
            infoJaxb.setValidation(validation);
            infoJaxb.setCreator(metsContext.getOptions().getCreator());
            infoJaxb.setPackageid(metsContext.getPackageID());
            if (Const.PERIODICAL_TITLE.equalsIgnoreCase(metsContext.getRootElement().getElementType())) {
                infoJaxb.setMetadataversion((float) 1.5);
            } else {
                infoJaxb.setMetadataversion((float) 1.1);
            }
            Itemlist itemList = new Itemlist();
            infoJaxb.setItemlist(itemList);
            itemList.setItemtotal(BigInteger.valueOf(metsContext.getFileList().size()));
            List<FileMD5Info> fileList = metsContext.getFileList();
        long size = 0;
            for (FileMD5Info fileName : fileList) {
                itemList.getItem().add(fileName.getFileName().replaceAll(Matcher.quoteReplacement(File.separator), "/"));
                size += fileName.getSize();
            }
            int infoTotalSize = (int) (size/1024);
        infoJaxb.setSize(infoTotalSize);
            try {
                JAXBContext jaxbContext = JAXBContext.newInstance(Info.class);
                Marshaller marshaller = jaxbContext.createMarshaller();
            // SchemaFactory factory =
            // SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            // factory.setResourceResolver(MetsLSResolver.getInstance());
            // Schema schema = factory.newSchema(new
            // StreamSource(Info.class.getResourceAsStream("info.xsd")));
            // marshaller.setSchema(schema);
                marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
                marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
                marshaller.marshal(infoJaxb, infoFile);
            } catch (Exception ex) {
                throw new MetsExportException("Error while generating info.xml", false, ex);
            }

        List<String> validationErrors;
        try {
            validationErrors = MetsUtils.validateAgainstXSD(infoFile, Info.class.getResourceAsStream("info.xsd"));
        } catch (Exception e) {
            throw new MetsExportException("Error while validating info.xml", false, e);
        }

        if (validationErrors.size() > 0) {
            MetsExportException metsException = new MetsExportException("Invalid info file:" + infoFile.getAbsolutePath(), false, null);
            metsException.getExceptions().get(0).setValidationErrors(validationErrors);
            for (String error : validationErrors) {
                LOG.fine(error);
            }
            throw metsException;
        }
    }

    /**
     *
     * Returns an ObjectID from the rels-ext stream
     *
     * @param relExtElements
     * @return
     */
    public static String getObjectId(List<Element> relExtElements) throws MetsExportException {
        String XPATH = "*[local-name()='RDF']/*[local-name()='Description']";
        Node descNode = xPathEvaluateNode(relExtElements, XPATH);
        String ID = descNode.getAttributes().getNamedItem("rdf:about").getNodeValue();
        return ID.substring(ID.indexOf("/") + 1);
    }

    /**
     *
     * Reads referenced object from Fedora
     *
     * @param uuid
     * @param client
     * @return
     */
    public static DigitalObject readRelatedFoXML(String uuid, FedoraClient client) throws MetsExportException {
        DigitalObject object = readFoXML(uuid, client);
        return object;
    }

    /**
     *
     * Reads referenced object from file
     *
     * @param path
     * @param fileName
     * @return
     */
    public static DigitalObject readRelatedFoXML(String path, String fileName) throws MetsExportException {
        String fileNameInternal = path + fileName.substring(fileName.lastIndexOf(":") + 1) + ".xml";
        DigitalObject object = readFoXML(fileNameInternal);
        return object;
    }

    /**
     *
     * Generates an MD5 checksum and copies a file (image) to defined
     * OutputStream
     *
     * @param is
     * @param os
     * @return
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    public static FileMD5Info getDigestAndCopy(InputStream is, OutputStream os) throws NoSuchAlgorithmException, IOException {
        MessageDigest md = MessageDigest.getInstance("MD5");
        md.reset();
        byte[] bytes = new byte[2048];
        int numBytes;
        long totalBytes = 0;
        while ((numBytes = is.read(bytes)) > 0) {
            totalBytes += numBytes;
            md.update(bytes, 0, numBytes);
            os.write(bytes, 0, numBytes);
        }
        byte[] digest = md.digest();
        os.close();
        is.close();
        String result = new String(Hex.encodeHex(digest));
        return new FileMD5Info(result, totalBytes);
    }

    /**
     *
     * Generates an MD5 checksum OutputStream
     *
     * @param is
     * @param os
     * @return
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    public static FileMD5Info getDigest(InputStream is) throws NoSuchAlgorithmException, IOException {
        MessageDigest md = MessageDigest.getInstance("MD5");
        md.reset();
        byte[] bytes = new byte[2048];
        int numBytes;
        long totalBytes = 0;
        while ((numBytes = is.read(bytes)) > 0) {
            totalBytes += numBytes;
            md.update(bytes, 0, numBytes);
        }
        byte[] digest = md.digest();
        String result = new String(Hex.encodeHex(digest));
        return new FileMD5Info(result, totalBytes);
    }

    /**
     * Returns parent pid from Resource index
     *
     * @param uuid
     * @param remoteStorage
     * @return
     */
    public static String getParent(String uuid, RemoteStorage remoteStorage) throws MetsExportException {
        List<Item> referrers;
        try {
            referrers = remoteStorage.getSearch().findReferrers(uuid);
        } catch (Exception e) {
            throw new MetsExportException("Error while finding parent for:" + uuid, false, e);
        }
        if (referrers.size() > 1) {
            throw new MetsExportException("More referrers for pid:" + uuid, false);
        }
        if (referrers.size() == 0) {
            return null;
        }
        return referrers.get(0).getPid();
    }

    /**
     *
     * Mock method for simulation of resource index
     *
     * @param uuid
     * @return
     */
    public static String getParent(String uuid, Map<String, String> fileSystemParents) {
        String result = fileSystemParents.get(uuid);
        LOG.log(Level.FINE, "Parent from FS for :" + uuid + " found:" + result);
        return result;
    }

    /**
     *
     * Checks if a monograph is MultiUnit
     *
     * @param monograph
     * @return
     */
    public static boolean isMultiUnitMonograph(MetsElement monograph) {
        if (Const.VOLUME.equals(monograph.getElementType())) {
            for (MetsElement element : monograph.getChildren()) {
                if (Const.MONOGRAPH_UNIT.equalsIgnoreCase(element.getElementType())) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     *
     * Generates a document from a byte array
     *
     * @param bytes
     * @return
     */
    public static Document getDocumentFromBytes(byte[] bytes) throws MetsExportException {
        if (bytes == null) {
            return null;
        }

        DocumentBuilder builder;
        try {
            builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new MetsExportException("Error while creating DocumentBuilder", false, e);
        }
        Document document;
        try {
            document = builder.parse(new ByteArrayInputStream(bytes));
        } catch (Exception e) {
            throw new MetsExportException("Error while parsing document", false, e);
        }
        return document;
    }

    /**
     * Deletes a folder
     *
     * @param folder
     */
    public static void deleteFolder(File folder) {
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
}