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

package cz.cas.lib.proarc.common.export.mets;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
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
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.response.FedoraResponse;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;

import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsInfo;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.mets.DivType;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap;
import cz.cas.lib.proarc.mets.MdSecType.MdWrap.XmlData;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType.FileSec;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.StructMapType;
import cz.cas.lib.proarc.mets.info.Info;
import cz.cas.lib.proarc.mets.info.Info.Checksum;
import cz.cas.lib.proarc.mets.info.Info.Itemlist;
import cz.cas.lib.proarc.mets.info.Info.Titleid;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

/**
 * @author Robert Simonovsky
 *
 *         Utility class
 *
 */
public class MetsUtils {

    private static Logger LOG = Logger.getLogger(MetsUtils.class.getName());
    private static HashMap<String, String> typeMap = new HashMap<String, String>();
    private static HashMap<String, String> modMap = new HashMap<String, String>();
    private static Properties mimeToExtension = new Properties();

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
                LOG.log(Level.SEVERE, "Unable to read mime type mapping", e);
                throw new MetsExportException("Unable to read mime type mapping", false, e);
            }
        }
        return mimeToExtension;
    }

    static {
        typeMap.put("info:fedora/model:periodicalvolume", Const.PERIODICAL_VOLUME);
        typeMap.put("info:fedora/model:page", Const.PAGE);
        typeMap.put("info:fedora/model:periodical", Const.PERIODICAL_TITLE);
        typeMap.put("info:fedora/model:monograph", Const.VOLUME);
        typeMap.put("info:fedora/model:picture", Const.PICTURE);
        typeMap.put("info:fedora/model:article", Const.ARTICLE);
        typeMap.put("info:fedora/model:periodicalitem", Const.ISSUE);
        typeMap.put("info:fedora/model:monographunit", Const.MONOGRAPHUNIT);

        modMap.put(Const.PERIODICAL_VOLUME, "VOLUME");
        modMap.put(Const.PERIODICAL_TITLE, "TITLE");
        modMap.put(Const.ARTICLE, "ART");
        modMap.put(Const.PICTURE, "PICT");
        modMap.put(Const.MONOGRAPHUNIT, "VOLUME");
        modMap.put(Const.ISSUE, "ISSUE");
        modMap.put(Const.VOLUME, "VOLUME");
        modMap.put(Const.PAGE, "PAGE");
    }

    /**
     *
     * Method used for retrieving a document type from the rels-ext stream
     *
     * @param relExtStream
     * @return
     */
    public static String getTypeModel(List<Element> relExtStream) throws MetsExportException {
        String result = typeMap.get(MetsUtils.getModel(relExtStream));
        if (result == null) {
            LOG.log(Level.SEVERE, "Unknown model:" + MetsUtils.getModel(relExtStream));
            throw new MetsExportException("Unknown model:" + MetsUtils.getModel(relExtStream));
        }
        return result;
    }

    /**
     *
     * Method used for retrieving the name of the mod element for selected
     * document type
     *
     * @param type
     * @return
     */
    public static String getModName(String type) throws MetsExportException {
        String result = modMap.get(type);
        if (result == null) {
            LOG.log(Level.SEVERE, "Unknown mod type:" + type);
            throw new MetsExportException("Unknown mod type:" + type);
        }
        return result;
    }

    /**
     *
     * Method used for retrieving a document type from digital object
     *
     * @param object
     * @return
     */
    public static String getTypeModel(DigitalObject object, MetsInfo metsInfo) throws MetsExportException {
        String result = typeMap.get(MetsUtils.getModel(object, metsInfo));
        if (result == null) {
            LOG.log(Level.SEVERE, "Unknown model:" + MetsUtils.getModel(object, metsInfo));
            throw new MetsExportException("Unknown model:" + MetsUtils.getModel(object, metsInfo));
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
            LOG.log(Level.SEVERE, "Name cannot be null");
            throw new MetsExportException("Name is null");
        }
        if (properties == null) {
            LOG.log(Level.SEVERE, "Properties cannot be null");
            throw new MetsExportException("Properties is null");
        }
        for (PropertyType property : properties) {
            if (name.equalsIgnoreCase(property.getNAME())) {
                return property.getVALUE();
            }
        }
        LOG.log(Level.SEVERE, "Property " + name + " not found");
        throw new MetsExportException("Property " + name + " not found");
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
            LOG.log(Level.SEVERE, "Error while getting document from list", e1);
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
            LOG.log(Level.SEVERE, "Unable to find:" + xPath + " in " + MetsUtils.documentToString(MetsUtils.getDocumentFromList(elements)), e);
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
            LOG.log(Level.SEVERE, "Error while evaluating xPath " + xPath, e1);
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
            LOG.log(Level.SEVERE, "Error while evaluating xPath " + xPath, e);
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
            LOG.log(Level.SEVERE, "Error while getting stream " + streamName + " from " + pid, ex);
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

    public static byte[] getBinaryDataStreams(FedoraClient fedoraClient, String pid, String streamName) throws MetsExportException {
        try {
            FedoraResponse response = FedoraClient.getDatastreamDissemination(pid, streamName).execute(fedoraClient);
            InputStream is = response.getEntityInputStream();
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            copyStream(is, bos);
            bos.close();
            return bos.toByteArray();
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, pid, ex);
            throw new MetsExportException("Error while getting stream " + streamName + " from " + pid, false, ex);
        }
    }

    /**
     *
     * Returns a model of the document
     *
     * @param relExtStream
     * @return
     */
    private static String getModel(DigitalObject object, MetsInfo metsInfo) throws MetsExportException {
        List<Element> relStream = null;
        if (metsInfo.fedoraClient != null) {
            relStream = MetsUtils.getDataStreams(metsInfo.fedoraClient, object.getPID(), "RELS-EXT");
        } else {
            relStream = MetsUtils.getDataStreams(object.getDatastream(), "RELS-EXT");
        }

        return getModel(relStream);
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
    public static HashMap<String, FileGrp> initFileGroups(Mets mets) {
        if (mets.getFileSec() == null) {
            FileSec fileSec = new FileSec();
            mets.setFileSec(fileSec);
        }

        FileGrp MCimagesGRP = new FileGrp();
        MCimagesGRP.setID("MC_IMGGRP");
        MCimagesGRP.setUSE("Images");
        mets.getFileSec().getFileGrp().add(MCimagesGRP);

        FileGrp UCimageGrp = new FileGrp();
        UCimageGrp.setID("UC_IMGGRP");
        UCimageGrp.setUSE("Images");
        mets.getFileSec().getFileGrp().add(UCimageGrp);

        FileGrp AltoGRP = new FileGrp();
        AltoGRP.setID("ALTOGRP");
        AltoGRP.setUSE("Layout");
        mets.getFileSec().getFileGrp().add(AltoGRP);

        FileGrp TxtGRP = new FileGrp();
        TxtGRP.setID("TXTGRP");
        TxtGRP.setUSE("Text");
        mets.getFileSec().getFileGrp().add(TxtGRP);

        FileGrp TechMDGrp = new FileGrp();
        TechMDGrp.setID("TECHMDGRP");
        TechMDGrp.setUSE("Technical Metadata");
        mets.getFileSec().getFileGrp().add(TechMDGrp);

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
            LOG.log(Level.SEVERE, "Unable to read foXML document " + path);
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
            LOG.log(Level.SEVERE, "Unable to get:" + uuid + " from Fedora", e);
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
            LOG.log(Level.SEVERE, "Error converting Document to String", ex);
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
    public static boolean validateAgainstXSD(Document document, InputStream xsd) throws MetsExportException {
        try {
            SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
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
            documentBuilder.setErrorHandler(new ErrorHandler() {
                @Override
                public void warning(SAXParseException exception) throws SAXException {
                    LOG.log(Level.WARNING, "Validation error", exception);
                }

                @Override
                public void fatalError(SAXParseException exception) throws SAXException {
                    LOG.log(Level.SEVERE, "Error while parsing document", exception);
                    throw new SAXException(exception);
                }

                @Override
                public void error(SAXParseException exception) throws SAXException {
                    LOG.log(Level.SEVERE, "Error while parsing document", exception);
                    throw new SAXException(exception);
                }
            });
            documentBuilder.parse(is);
            return true;
        } catch (Exception ex) {
            LOG.log(Level.WARNING, "Error while validating document", ex);
            throw new MetsExportException("Error validating document", true, ex);
        }
    }

    /**
     *
     * Validates given XML file against an XSD schema
     *
     * @param file
     * @param xsd
     * @return
     */
    public static boolean validateAgainstXSD(File file, InputStream xsd) throws MetsExportException {
        try {
            SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = factory.newSchema(new StreamSource(xsd));
            DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
            dbfactory.setValidating(false);
            dbfactory.setNamespaceAware(true);
            dbfactory.setSchema(schema);
            DocumentBuilder documentBuilder = dbfactory.newDocumentBuilder();
            documentBuilder.setErrorHandler(new ErrorHandler() {
                @Override
                public void warning(SAXParseException exception) throws SAXException {
                    LOG.log(Level.WARNING, "Error while validating document", exception);
                    throw new SAXException(exception);
                }

                @Override
                public void fatalError(SAXParseException exception) throws SAXException {
                    LOG.log(Level.WARNING, "Error while validating document", exception);
                    throw new SAXException(exception);

                }

                @Override
                public void error(SAXParseException exception) throws SAXException {
                    LOG.log(Level.WARNING, "Error while validating document", exception);
                    throw new SAXException(exception);
                }
            });
            documentBuilder.parse(file);
            return true;
        } catch (Exception ex) {
            LOG.log(Level.WARNING, "Error while validating document", ex);
            throw new MetsExportException("Error validating document", true, ex);
        }
    }

    /**
     *
     * Generates and saves info.xml
     *
     * @param path
     * @param mets
     */
    private static void saveInfoFile(String path, MetsInfo mets, String md5, String fileMd5Name, long fileSize) throws MetsExportException {
        File infoFile = new File(path + File.separator + "info.xml");
        try {
            GregorianCalendar c = new GregorianCalendar();
            c.setTime(new Date());
            XMLGregorianCalendar date2 = DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
            Info infoJaxb = new Info();
            infoJaxb.setCreated(date2);
            Checksum checkSum = new Checksum();
            checkSum.setChecksum(md5);
            checkSum.setType("MD5");
            Map<String, String> identifiers = mets.rootElement.getModsIdentifiers();
            for (String type : identifiers.keySet()) {
                Titleid titleId = new Titleid();
                titleId.setType(type);
                titleId.setValue(identifiers.get(type));
                infoJaxb.getTitleid().add(titleId);
            }
            checkSum.setValue(fileMd5Name);
            infoJaxb.setChecksum(checkSum);
            infoJaxb.setCreator("ProARC");
            infoJaxb.setPackageid(mets.getPackageId());
            infoJaxb.setMetadataversion("1.1");
            Itemlist itemList = new Itemlist();
            infoJaxb.setItemlist(itemList);
            itemList.setItemtotal(BigInteger.valueOf(mets.getFileList().size()));
            List<FileMD5Info> fileList = mets.getFileList();
            int size = (int) fileSize;
            for (FileMD5Info fileName : fileList) {
                itemList.getItem().add(fileName.getFileName());
                size += fileName.getSize();
            }
            infoJaxb.setSize(size / 1024);
            try {
                JAXBContext jaxbContext = JAXBContext.newInstance(Info.class);
                Marshaller marshaller = jaxbContext.createMarshaller();
                marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
                marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
                // marshaller.setProperty("com.sun.xml.internal.bind.namespacePrefixMapper",
                // new NamespacePrefixMapperImpl());
                marshaller.marshal(infoJaxb, infoFile);
            } catch (Exception ex) {
                LOG.log(Level.SEVERE, "Error while generating info.xml", ex);
                throw new MetsExportException("Error while generating info.xml", false, ex);
            }
            MetsUtils.validateAgainstXSD(infoFile, Info.class.getResourceAsStream("info.xsd"));
        } catch (Exception e) {
            LOG.log(Level.SEVERE, "Error while creating info.xml", e);
            throw new MetsExportException("Error while creating info.xml", false, e);
        }
    }

    /**
     *
     * Saves mets file
     *
     * @param path
     * @param mets
     */
    public static void saveMets(String path, MetsInfo mets) throws MetsExportException {
        String fileName = "/METS_" + mets.getPackageId() + ".xml";
        FileMD5Info fileMD5Info = new FileMD5Info("." + fileName);
        mets.addFile(fileMD5Info);
        File file = new File(path + fileName);
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class, OaiDcType.class, ModsDefinition.class);
            Marshaller marshaller = jaxbContext.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
            marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, "http://www.w3.org/2001/XMLSchema-instance http://www.w3.org/2001/XMLSchema.xsd http://www.loc.gov/METS/ http://www.loc.gov/standards/mets/mets.xsd http://www.loc.gov/mods/v3 http://www.loc.gov/standards/mods/mods.xsd http://www.openarchives.org/OAI/2.0/oai_dc/ http://www.openarchives.org/OAI/2.0/oai_dc.xsd");
            // marshaller.setProperty("com.sun.xml.internal.bind.namespacePrefixMapper",
            // new NamespacePrefixMapperImpl());
            marshaller.marshal(mets.getMets(), file);
            MessageDigest md;
            try {
                md = MessageDigest.getInstance("MD5");
            } catch (NoSuchAlgorithmException e) {
                LOG.log(Level.SEVERE, "Unable to create MD5 hash", e);
                throw new MetsExportException("Unable to create MD5 hash", false, e);
            }
            md.reset();
            MetsUtils.validateAgainstXSD(file, Mets.class.getResourceAsStream("mets.xsd"));
            InputStream is;
            try {
                is = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                LOG.log(Level.SEVERE, "Unable to open file" + file.getAbsolutePath(), e);
                throw new MetsExportException("Unable to open file" + file.getAbsolutePath(), false, e);
            }
            byte[] bytes = new byte[2048];
            int numBytes;
            try {
                while ((numBytes = is.read(bytes)) != -1) {
                    md.update(bytes, 0, numBytes);
                }
            } catch (IOException e) {
                LOG.log(Level.SEVERE, "Unable to generate MD5 hash", e);
                throw new MetsExportException("Unable to generate MD5 hash", false, e);
            }
            byte[] digest = md.digest();
            String result = new String(Hex.encodeHex(digest));
            String fileMd5Name = "/MD5_" + mets.getPackageId() + ".md5";
            File fileMd5 = new File(path + fileMd5Name);
            mets.addFile(new FileMD5Info("." + fileMd5Name));
            OutputStreamWriter osw = new OutputStreamWriter(new FileOutputStream(fileMd5));
            osw.write(result);
            osw.close();
            is.close();
            saveInfoFile(path, mets, result, fileMd5Name, file.length());
        } catch (Exception e) {
            LOG.log(Level.SEVERE, "Error while saving METS file", e);
            throw new MetsExportException("Error while saving METS file", false, e);
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

    public static MdSecType createMdSec(String ID, String type, String mime, Collection<? extends Object> xmlData) {
        MdSecType typeMods = new MdSecType();
        typeMods.setID(ID);
        MdWrap mdWrap = new MdWrap();
        mdWrap.setMDTYPE(type);
        mdWrap.setMIMETYPE(mime);
        typeMods.setMdWrap(mdWrap);
        XmlData xmlDataElement = new XmlData();
        xmlDataElement.getAny().addAll(xmlData);
        mdWrap.setXmlData(xmlDataElement);
        return typeMods;
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
        int totalBytes = 0;
        while ((numBytes = is.read(bytes)) > 0) {
            totalBytes += numBytes;
            md.update(bytes, 0, numBytes);
            os.write(bytes, 0, numBytes);
        }
        byte[] digest = md.digest();
        os.close();
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
            LOG.log(Level.SEVERE, "Error while finiding parent for:" + uuid, e);
            throw new MetsExportException("Error while finding parent for:" + uuid, false, e);
        }
        if (referrers.size() > 1) {
            LOG.log(Level.SEVERE, "More referrers for pid:" + uuid);
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
        LOG.log(Level.INFO, "Parent from FS for :" + uuid + " found:" + result);
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
        if (Const.VOLUME.equals(monograph.type)) {
            for (MetsElement element : monograph.children) {
                if (Const.MONOGRAPHUNIT.equalsIgnoreCase(element.type)) {
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
            LOG.log(Level.SEVERE, "Error while creating DocumentBuilder", e);
            throw new MetsExportException("Error while creating DocumentBuilder", false, e);
        }
        Document document;
        try {
            document = builder.parse(new ByteArrayInputStream(bytes));
        } catch (Exception e) {
            LOG.log(Level.SEVERE, "Error while parsing document", e);
            throw new MetsExportException("Error while parsing document", false, e);
        }
        return document;
    }
}