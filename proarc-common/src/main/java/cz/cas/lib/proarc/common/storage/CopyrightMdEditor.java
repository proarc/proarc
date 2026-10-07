package cz.cas.lib.proarc.common.storage;

import cz.cas.lib.proarc.foxml.management.DatastreamProfile;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Locale;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.stream.StreamResult;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

/** Stores the standalone copyrightMD document, without a METS wrapper. */
public class CopyrightMdEditor {
    public static final String ID = "COPYRIGHTMD";
    public static final String NS = "http://www.cdlib.org/inside/diglib/copyrightMD";
    private static final Set<String> COPYRIGHT_STATUSES = Set.of("copyrighted", "pd", "pd_expired", "pd_holder", "unknown");
    private static final Set<String> COUNTRY_CODES = Set.of(Locale.getISOCountries());
    private static final Schema SCHEMA = createSchema();
    private final ProArcObject object;
    private final XmlStreamEditor editor;

    public CopyrightMdEditor(ProArcObject object) {
        this.object = object;
        this.editor = object.getEditor(profile());
    }

    public static DatastreamProfile profile() {
        return FoxmlUtils.managedProfile(ID, NS, "Copyright metadata (copyrightMD).");
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    public String readAsString() throws DigitalObjectException {
        Source source = editor.read();
        if (source == null) { return null; }
        try {
            DOMResult result = new DOMResult();
            transformerFactory().newTransformer().transform(source, result);
            Document document = result.getNode() instanceof Document ? (Document) result.getNode() : result.getNode().getOwnerDocument();
            validate(document.getDocumentElement());
            return toXml(document.getDocumentElement());
        } catch (Exception e) {
            throw new DigitalObjectException(object.getPid(), "Cannot read copyrightMD", e);
        }
    }

    public void write(String xml, long timestamp, String message) throws DigitalObjectException {
        Element copyright = parse(xml);
        XmlStreamEditor.EditorResult result = editor.createResult();
        try {
            transformerFactory().newTransformer().transform(new DOMSource(copyright), result);
        } catch (Exception e) {
            throw new DigitalObjectException(object.getPid(), "Cannot write copyrightMD", e);
        }
        editor.write(result, timestamp, message);
    }

    public void delete(long timestamp, String message) throws DigitalObjectException {
        if (timestamp != getLastModified()) {
            throw new DigitalObjectConcurrentModificationException(object.getPid());
        }
        object.purgeDatastream(ID, message);
    }

    public static Element parse(String xml) throws DigitalObjectException {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
            factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
            Element root = factory.newDocumentBuilder().parse(new InputSource(new StringReader(xml))).getDocumentElement();
            validate(root);
            return root;
        } catch (Exception e) {
            throw new DigitalObjectException(null, "Invalid copyrightMD: " + e.getMessage(), e);
        }
    }

    public static String toXml(Element root) throws DigitalObjectException {
        try {
            StringWriter result = new StringWriter();
            transformerFactory().newTransformer().transform(new DOMSource(root), new StreamResult(result));
            return result.toString();
        } catch (Exception e) {
            throw new DigitalObjectException(null, "Cannot serialize copyrightMD", e);
        }
    }

    private static TransformerFactory transformerFactory() throws Exception {
        TransformerFactory factory = TransformerFactory.newInstance();
        factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
        factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
        factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
        return factory;
    }

    private static Schema createSchema() {
        try {
            SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            factory.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            factory.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
//            return factory.newSchema(CopyrightMdEditor.class.getResource("copyrightMD.xsd"));
            return factory.newSchema(CopyrightMdEditor.class.getResource("/cz/cas/lib/proarc/copyrightmd/copyrightMD.xsd"));
        } catch (Exception e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private static void validate(Element root) throws Exception {
        SCHEMA.newValidator().validate(new DOMSource(root));
        if (!COPYRIGHT_STATUSES.contains(root.getAttribute("copyright.status"))) {
            throw new IllegalArgumentException("Unsupported NDK copyright.status");
        }
        validateNdkCardinality(root);
    }

    private static void validateNdkCardinality(Element parent) {
        Map<String, Integer> counts = new HashMap<>();
        for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling()) {
            if (!(node instanceof Element)) { continue; }
            Element child = (Element) node;
            String name = child.getLocalName();
            boolean repeatable = Set.of("note", "general.note", "contact", "creator.person", "creator.corporate", "services").contains(name);
            if (counts.merge(name, 1, Integer::sum) > 1 && !repeatable) {
                throw new IllegalArgumentException("Repeated copyrightMD element: " + name);
            }
            if (name.startsWith("year.") && !child.getTextContent().isBlank()
                    && !child.getTextContent().matches("[0-9]{4}")) {
                throw new IllegalArgumentException("Expected YYYY in " + name);
            }
            if (child.hasAttribute("iso.code") && !COUNTRY_CODES.contains(child.getAttribute("iso.code"))) {
                throw new IllegalArgumentException("Expected ISO alpha-2 country code");
            }
            validateNdkCardinality(child);
        }
    }
}
