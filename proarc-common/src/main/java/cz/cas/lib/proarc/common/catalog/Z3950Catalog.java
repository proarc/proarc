/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.cas.lib.proarc.common.catalog;

import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.config.CatalogQueryField;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import cz.cas.lib.proarc.common.xml.Transformers;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.DateOtherDefinition;
import cz.cas.lib.proarc.mods.ModsCollectionDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusSupplied;
import cz.cas.lib.proarc.z3950.Z3950Client;
import cz.cas.lib.proarc.z3950.Z3950ClientException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Z39.50 metadata provider.
 *
 * @author Jan Pokorsky
 */
public final class Z3950Catalog implements BibliographicCatalog {

    public static final String TYPE = "Z3950Catalog";
    /**
     * Configuration property name to define database.
     */
    static final String PROPERTY_BASE = "base";
    /**
     * Configuration property name to override char set of returned records.
     */
    static final String PROPERTY_RECORD_CHARSET = "recordCharset";
    /**
     * Configuration property name to define field's query.
     */
    static final String PROPERTY_FIELD_QUERY = "query";

    private static final Logger LOG = Logger.getLogger(Z3950Catalog.class.getName());
    private Transformers transformers = new Transformers();
    private final Z3950Client client;
    private final String host;
    private final String base;
    private final int port;
    private final Charset recordCharset;
    /**
     * fieldId -> field
     */
    private final Map<String, Z3950Field> fields;

    public static Z3950Catalog get(CatalogConfiguration c) {
        if (c == null || !TYPE.equals(c.getType())) {
            return null;
        }
        String host = null;
        int port = -1;
        String url = c.getUrl();
        if (url != null) {
            try {
                URI uri = new URI(url);
                host = uri.getHost();
                if (host == null || host.isEmpty()) {
                    LOG.log(Level.SEVERE, "Missing host in URL.\n{0}", c);
                    return null;
                }
                port = uri.getPort();
                if (port == -1) {
                    LOG.log(Level.SEVERE, "Missing port in URL.\n{0}", c);
                    return null;
                }
            } catch (URISyntaxException ex) {
                LOG.log(Level.SEVERE, c.toString(), ex);
                return null;
            }
        }
        String base = c.getProperty(PROPERTY_BASE);
        String recordCharset = c.getProperty(PROPERTY_RECORD_CHARSET);
        Charset charset = null;
        if (recordCharset != null) {
            try {
                charset = Charset.forName(recordCharset);
            } catch (Exception ex) {
                LOG.log(Level.SEVERE, c.toString(), ex);
            }
        }

        Map<String, Z3950Field> fields = readFields(c);
        return new Z3950Catalog(host, port, base, charset, fields);
    }

    static Map<String, Z3950Field> readFields(CatalogConfiguration c) {
        Map<String, Z3950Field> fields = new HashMap<String, Z3950Field>();
        List<CatalogQueryField> queryFields = c.getQueryFields();
        for (CatalogQueryField queryField : queryFields) {
            fields.put(queryField.getName(), new Z3950Field(queryField));
        }
        return fields;
    }

    public Z3950Catalog(String host, int port, String base, Charset recordCharset, Map<String, Z3950Field> fields) {
        this.host = host;
        this.port = port;
        this.base = base;
        this.recordCharset = recordCharset;
        client = new Z3950Client(host, port, base);
        this.fields = fields;
    }

    @Override
    public List<MetadataItem> find(String catalog, String fieldId, String value, Locale locale) throws TransformerException, IOException {
        String query = buildQuery(fieldId, value);
        LOG.fine(query);
        if (query == null) {
            return Collections.emptyList();
        }
        ArrayList<MetadataItem> result = new ArrayList<MetadataItem>();
        int index = 1;
        try {
            for (byte[] content : client.search(query)) {
                String charset = recordCharset == null ? null : recordCharset.name();
                String marc21 = new String(content, charset == null ? "UTF-8" : charset);
                marc21 = marc21.replaceAll("LDR", "");
                //content = marc21.getBytes(charset);
                if (LOG.isLoggable(Level.FINE)) {
                    marc21 = new String(content, charset == null ? "UTF-8" : charset);
                    LOG.fine(marc21);
                }


                Document marcXml = Z3950Client.toMarcXml(content, charset);
                if (LOG.isLoggable(Level.FINE)) {
                    StringBuilder sb = new StringBuilder();
                    transformers.dump(new DOMSource(marcXml), sb);
                    LOG.fine(sb.toString());
                }
                MetadataItem item = createResponse(index++, catalog, marcXml, locale);
                result.add(item);
            }
            return result;
        } catch (Z3950ClientException ex) {
            throw new IOException(ex);
        } finally {
            client.close();
        }
    }

    Charset getRecordCharset() {
        return recordCharset;
    }

    String getHost() {
        return host;
    }

    String getBase() {
        return base;
    }

    int getPort() {
        return port;
    }

    private String buildQuery(String fieldId, String value) {
        String query = null;
        Z3950Field field = fields.get(fieldId);
        if (field != null && field.getQuery() != null) {
            return field.getQuery().replace("%s", value);
        }
        // default queries
        if ("issn".equals(fieldId)) {
            query = String.format("@attrset bib-1 @attr 1=8 \"%s\"", value);
        } else if ("isbn".equals(fieldId)) {
            query = String.format("@attrset bib-1 @attr 1=7 \"%s\"", value);
        }
        return query;
    }

    private MetadataItem createResponse(int entryIdx, String catalog, Document marcXml, Locale locale)
            throws TransformerException, UnsupportedEncodingException {


        Source marcxmlSrc = new DOMSource(marcXml);
        byte[] modsBytes = transformers.transformAsBytes(
                marcxmlSrc, Transformers.Format.MarcxmlAsMods3);
        modsBytes = repairModsBytes(modsBytes, marcXml);
        byte[] modsHtmlBytes = modsAsHtmlBytes(new StreamSource(new ByteArrayInputStream(modsBytes)), locale);
        byte[] modsTitleBytes = transformers.transformAsBytes(
                new StreamSource(new ByteArrayInputStream(modsBytes)),
                Transformers.Format.ModsAsTitle);
        if (modsTitleBytes != null && modsTitleBytes.length == 0) {
            modsTitleBytes = transformers.transformAsBytes(new StreamSource(new ByteArrayInputStream(modsBytes)), Transformers.Format.ModsAsAuthorityTitle);
        }

        return new MetadataItem(entryIdx, catalog, new String(modsBytes, "UTF-8"),
                repairHtml(new String(modsHtmlBytes, "UTF-8")), new String(modsTitleBytes, "UTF-8"));
    }

    private byte[] repairModsBytes(byte[] modsBytes, Document marcXml) throws UnsupportedEncodingException {
        String modsAsString = new String(modsBytes, "UTF-8");
        ModsCollectionDefinition modsCollection = ModsUtils.unmarshal(modsAsString, ModsCollectionDefinition.class);
        List<String> couples = new ArrayList<>();
        int updateNode = 0;

        if (containsNode(marcXml, "260")) {
            couples = findAndSplitNode(marcXml, "260");
            updateNode = 260;
        } else if (containsNode(marcXml, "264")) {
            couples = findAndSplitNode(marcXml, "264");
            updateNode = 264;
        }

        if (couples.size() < 2) {
            return modsBytes;
        } else { //knav monografie isbn 80-200-0953-1
            if (260 == updateNode) {
                modsAsString = repairMods(modsCollection, couples);
                return modsAsString.getBytes(StandardCharsets.UTF_8);
            } else if (264 == updateNode) { // knav monografie - name: History of nanotechnology from pre-historic to modern times; isbn: 978-1-119-46008-4
                modsAsString = repairMods_264(modsCollection, couples);
                return modsAsString.getBytes(StandardCharsets.UTF_8);
            } else {
                return modsBytes;
            }
        }
    }

    private String repairMods_264(ModsCollectionDefinition modsCollection, List<String> couples) {
        List<OriginInfoDefinition> fixedOriginInfo = new ArrayList<>();
        ModsDefinition mods = modsCollection.getMods().get(0);
        for (String couple : couples) {
            OriginInfoDefinition newOriginInfo = null;
            if (couple.contains("a")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    if (oldOriginInfo.getEventType() != null) {
                        for (PlaceDefinition oldPlace : oldOriginInfo.getPlace()) {
                            boolean delete = false;
                            for (PlaceTermDefinition oldPlaceTerm : oldPlace.getPlaceTerm()) {
                                if (oldPlaceTerm.getAuthority() == null) {
                                    PlaceDefinition newPlace = new PlaceDefinition();
                                    newPlace.getPlaceTerm().add(oldPlaceTerm);
                                    if (newOriginInfo == null) {
                                        newOriginInfo = new OriginInfoDefinition();
                                        newOriginInfo.setEventType(oldOriginInfo.getEventType());
                                    }
                                    newOriginInfo.getPlace().add(newPlace);
                                    oldPlace.getPlaceTerm().remove(oldPlace);
                                    delete = true;
                                    break;
                                }
                            }
                            if (delete == true) {
                                oldOriginInfo.getPlace().remove(oldPlace);
                                break;
                            }
                        }
                    }
                }
            }
            if (couple.contains("b")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    if (oldOriginInfo.getEventType() != null) {
                        for (StringPlusLanguagePlusSupplied oldPublisher : oldOriginInfo.getPublisher()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getPublisher().add(oldPublisher);
                            oldOriginInfo.getPublisher().remove(oldPublisher);
                            break;
                        }
                    }
                }
            }
            if (couple.contains("c")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    if (oldOriginInfo.getEventType() != null) {
                        for (DateDefinition oldDate : oldOriginInfo.getDateIssued()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getDateIssued().add(oldDate);
                            oldOriginInfo.getDateIssued().remove(oldDate);
                            break;
                        }
                        for (DateDefinition oldDate : oldOriginInfo.getDateCreated()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getDateCreated().add(oldDate);
                            oldOriginInfo.getDateCreated().remove(oldDate);
                            break;
                        }
                        for (DateDefinition oldDate : oldOriginInfo.getCopyrightDate()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getCopyrightDate().add(oldDate);
                            oldOriginInfo.getCopyrightDate().remove(oldDate);
                            break;
                        }
                        for (DateOtherDefinition oldDate : oldOriginInfo.getDateOther()) {
                            if (newOriginInfo == null) {
                                newOriginInfo = new OriginInfoDefinition();
                                newOriginInfo.setEventType(oldOriginInfo.getEventType());
                            }
                            newOriginInfo.getDateOther().add(oldDate);
                            oldOriginInfo.getDateOther().remove(oldDate);
                            break;
                        }
                    }
                }
            }
            if (newOriginInfo != null) {
                fixedOriginInfo.add(newOriginInfo);
            }
        }
        mods.getOriginInfo().addAll(fixedOriginInfo);
        cleanOriginInfo(mods);
        return ModsUtils.toXml(mods, true);
    }

    private void cleanOriginInfo(ModsDefinition mods) {
        ListIterator<OriginInfoDefinition> iterator = mods.getOriginInfo().listIterator();
        while (iterator.hasNext()) {
            OriginInfoDefinition originInfo = iterator.next();
            if (originInfo.getEventType() != null &&
                    originInfo.getPlace().isEmpty() &&
                    originInfo.getPublisher().isEmpty() &&
                    originInfo.getDateCreated().isEmpty() &&
                    originInfo.getDateIssued().isEmpty() &&
                    originInfo.getCopyrightDate().isEmpty() &&
                    originInfo.getDateOther().isEmpty()) {
                iterator.remove();
            }
        }
    }

    private String repairMods(ModsCollectionDefinition modsCollection, List<String> couples) {
        List<OriginInfoDefinition> fixedOriginInfo = new ArrayList<>();
        ModsDefinition mods = modsCollection.getMods().get(0);
        for (String couple : couples) {
            OriginInfoDefinition newOriginInfo = null;
            if (couple.contains("a")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    for (PlaceDefinition oldPlace : oldOriginInfo.getPlace()) {
                        boolean delete = false;
                        for (PlaceTermDefinition oldPlaceTerm : oldPlace.getPlaceTerm()) {
                            if (oldPlaceTerm.getAuthority() == null) {
                                PlaceDefinition newPlace = new PlaceDefinition();
                                newPlace.getPlaceTerm().add(oldPlaceTerm);
                                if (newOriginInfo == null) {
                                    newOriginInfo = new OriginInfoDefinition();
                                }
                                newOriginInfo.getPlace().add(newPlace);
                                oldPlace.getPlaceTerm().remove(oldPlace);
                                delete = true;
                                break;
                            }
                        }
                        if (delete == true) {
                            oldOriginInfo.getPlace().remove(oldPlace);
                            break;
                        }
                    }
                }
            }
            if (couple.contains("b")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    for (StringPlusLanguagePlusSupplied oldPublisher : oldOriginInfo.getPublisher()) {
                        if (newOriginInfo == null) {
                            newOriginInfo = new OriginInfoDefinition();
                        }
                        newOriginInfo.getPublisher().add(oldPublisher);
                        oldOriginInfo.getPublisher().remove(oldPublisher);
                        break;
                    }
                }
            }
            if (couple.contains("c")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    for (DateDefinition oldDate : oldOriginInfo.getDateIssued()) {
                        if (newOriginInfo == null) {
                            newOriginInfo = new OriginInfoDefinition();
                        }
                        newOriginInfo.getDateIssued().add(oldDate);
                        oldOriginInfo.getDateIssued().remove(oldDate);
                        break;
                    }
                }
            }

            if (couple.contains("g")) {
                for (OriginInfoDefinition oldOriginInfo : mods.getOriginInfo()) {
                    for (DateDefinition oldDate : oldOriginInfo.getDateCreated()) {
                        if (newOriginInfo == null) {
                            newOriginInfo = new OriginInfoDefinition();
                        }
                        newOriginInfo.getDateCreated().add(oldDate);
                        oldOriginInfo.getDateCreated().remove(oldDate);
                        break;
                    }
                }
            }
            if (newOriginInfo != null) {
                fixedOriginInfo.add(newOriginInfo);
            }
        }
        mods.getOriginInfo().addAll(fixedOriginInfo);
        return ModsUtils.toXml(mods, true);
    }

    private List<String> findAndSplitNode(Document marcXml, String tagValue) {
        List<String> couples = new ArrayList<>();
        try {
            XPathFactory xPathFactory = ProarcXmlUtils.defaultXPathFactory();
            XPath xPath = xPathFactory.newXPath();
            xPath.setNamespaceContext(new SimpleNamespaceContext().add("m", "http://www.loc.gov/MARC21/slim"));
            XPathExpression originInfoPath = xPath.compile("m:collection/m:record/m:datafield[@tag=" + tagValue + "]");
            Node node = (Node) originInfoPath.evaluate(marcXml, XPathConstants.NODE);
            List<String> listOfSubelements = new ArrayList<>();
            if (node != null && node.hasChildNodes()) {
                NodeList listOfNodes = node.getChildNodes();
                for (int i = 0; i < listOfNodes.getLength(); i++) {
                    Node subelement = listOfNodes.item(i);
                    if (subelement.getAttributes() != null && subelement.getAttributes().getNamedItem("code") != null) {
                        listOfSubelements.add(subelement.getAttributes().getNamedItem("code").getNodeValue());
                    }
                }
            }
            int position = 0;
            for (String subelement : listOfSubelements) {
                if (couples.isEmpty()) {
                    couples.add(subelement);
                } else {
                    if (couples.get(position).contains(subelement)) {
                        couples.add(subelement);
                        position++;
                    } else {
                        String element = couples.get(position);
                        couples.set(position, element + subelement);
                    }
                }
            }
        } catch (XPathExpressionException e) {
            LOG.warning("Impossible to parse node with tag " + tagValue + " from downloaded marcXml");
            e.printStackTrace();
        }
        return couples;
    }


    private boolean containsNode(Document marcXml, String tagValue) {
        Node node = null;
        try {
            XPathFactory xPathFactory = ProarcXmlUtils.defaultXPathFactory();
            XPath xPath = xPathFactory.newXPath();
            xPath.setNamespaceContext(new SimpleNamespaceContext().add("m", "http://www.loc.gov/MARC21/slim"));
            XPathExpression originInfoPath = xPath.compile("m:collection/m:record/m:datafield[@tag=" + tagValue + "]");
            node = (Node) originInfoPath.evaluate(marcXml, XPathConstants.NODE);
        } catch (XPathExpressionException e) {
            LOG.warning("Impossible to parse node with tag " + tagValue + " from downloaded marcXml");
            e.printStackTrace();
        } finally {
            return node != null;
        }
    }

    private String repairHtml(String s) {
        s = s.replaceAll("\\n", "");
        s = s.replaceAll("\\r", "");
        s = replaceMoreSpace(s);
        s = s.replace(") </b>", ") ");
        s = s.replace("( ", "</b>( ");
        s = s.replace("( ", " (");
        s = s.replace("=\" ", " = ");
        s = s.replace("\"", "");
        s = repairGeographicCode(s);
        return s;
    }

    private String repairGeographicCode(String s) {
        s = s.replace("e-xr---", "Česko");
        s = s.replace("e-xr-cc", "Čechy");
        s = s.replace("e-xr-pg", "Praha (Česko : kraj)");
        s = s.replace("e-xr-st", "Středočeský kraj");
        s = s.replace("e-xr-kr", "Královéhradecký kraj");
        s = s.replace("e-xr-pa", "Pardubický kraj");
        s = s.replace("e-xr-us", "Ústecký kraj");
        s = s.replace("e-xr-li", "Liberecký kraj");
        s = s.replace("e-xr-pl", "Plzeňský kraj");
        s = s.replace("e-xr-ka", "Karlovarský kraj");
        s = s.replace("e-xr-jc", "Jihočeský kraj");
        s = s.replace("e-xr-jm", "Jihomoravský kraj");
        s = s.replace("e-xr-zl", "Zlínský kraj");
        s = s.replace("e-xr-vy", "Vysočina");
        s = s.replace("e-xr-mo", "Moravskoslezský kraj");
        s = s.replace("e-xr-ol", "Olomoucký kraj");
        s = s.replace("e-xr-mr", "Morava");
        s = s.replace("e-xr-sl", "Slezsko (Česko)");
        return s;
    }

    private String replaceMoreSpace(String s) {
        while (s.contains("  ")) {
            s = s.replace("  ", " ");
        }
        return s;
    }

    private byte[] modsAsHtmlBytes(Source source, Locale locale) throws TransformerException {
        byte[] modsHtmlBytes = transformers.transformAsBytes(
                source, Transformers.Format.ModsAsHtml, ModsUtils.modsAsHtmlParameters(locale));
        return modsHtmlBytes;
    }

    public static final class Z3950Field extends CatalogQueryField {

        public Z3950Field(CatalogQueryField cqField) {
            super(cqField.getName(), cqField.getProperties());
        }

        public String getQuery() {
            return getProperties().getString(PROPERTY_FIELD_QUERY);
        }
    }

}
