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
import cz.cas.lib.proarc.common.xml.Transformers;
import cz.cas.lib.proarc.z3950.Z3950Client;
import cz.cas.lib.proarc.z3950.Z3950ClientException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Document;

/**
 * Z39.50 metadata provider.
 *
 * @author Jan Pokorsky
 */
public final class Z3950Catalog implements BibliographicCatalog {

    public static final String TYPE = "Z3950Catalog";
    /** Configuration property name to define database. */
    static final String PROPERTY_BASE = "base";
    /** Configuration property name to override char set of returned records. */
    static final String PROPERTY_RECORD_CHARSET = "recordCharset";
    /** Configuration property name to define field's query. */
    static final String PROPERTY_FIELD_QUERY = "query";
    
    private static final Logger LOG = Logger.getLogger(Z3950Catalog.class.getName());
    private Transformers transformers = new Transformers();
    private final Z3950Client client;
    private final String host;
    private final String base;
    private final int port;
    private final Charset recordCharset;
    /** fieldId -> field */
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
                if (LOG.isLoggable(Level.FINE)) {
                    String marc21 = new String(content, charset == null ? "UTF-8" : charset);
                    LOG.fine(marc21);
                }

                Document marcXml = Z3950Client.toMarcXml(content, charset);
                if (LOG.isLoggable(Level.FINE)) {
                    StringBuilder sb = new StringBuilder();
                    transformers.dump(new DOMSource(marcXml), sb);
                    LOG.fine(sb.toString());
                }
                MetadataItem item = createResponse(index++, catalog, new DOMSource(marcXml), locale);
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

    private MetadataItem createResponse(int entryIdx, String catalog, Source marcxmlSrc, Locale locale)
            throws TransformerException, UnsupportedEncodingException {



        byte[] modsBytes = transformers.transformAsBytes(
                marcxmlSrc, Transformers.Format.MarcxmlAsMods3);
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

    private String repairHtml(String s) {
        s = s.replaceAll("\\n", "");
        s = s.replaceAll("\\r", "");
        s = replaceMoreSpace(s);
        s = s.replace(") </b>", ") ");
        s = s.replace("( ", "</b>( ");
        s = s.replace("( ", " (");
        s = s.replace("=\" ", " = ");
        s = s.replace("\"", "");
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
