/*
 * Copyright (C) 2021 Lukas Sykora
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
import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.ClientBuilder;
import jakarta.ws.rs.client.WebTarget;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.logging.LoggingFeature;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.w3c.dom.Document;

import static cz.cas.lib.proarc.common.catalog.CatalogUtils.repairHtml;
import static cz.cas.lib.proarc.common.catalog.CatalogUtils.repairModsBytes;

/**
 * Alma metadata provider.
 *
 * @author Lukas Sykora
 */
public final class AlmaCatalog implements BibliographicCatalog {

    public static final String TYPE = "ALMA";
    /**
     * Configuration property name to define database.
     */
    static final String PROPERTY_APIKEY = "apikey";
    /**
     * Configuration property name to override char set of returned records.
     */
    static final String PROPERTY_RECORD_CHARSET = "recordCharset";

    private static final Logger LOG = Logger.getLogger(AlmaCatalog.class.getName());
    private Transformers transformers;
    private final String url;
    private final String apikey;
    private final Charset recordCharset;
    private Client httpClient;

    private static Templates ALMA_MARC_XSLT;
    /**
     * fieldId -> field
     */
    private final Map<String, AlmaField> fields;

    public static AlmaCatalog get(CatalogConfiguration c, String customTemplatePath) {
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
            } catch (URISyntaxException ex) {
                LOG.log(Level.SEVERE, c.toString(), ex);
                return null;
            }
        }
        String apikey = c.getProperty(PROPERTY_APIKEY);
        String recordCharset = c.getProperty(PROPERTY_RECORD_CHARSET);
        Charset charset = null;
        if (recordCharset != null) {
            try {
                charset = Charset.forName(recordCharset);
            } catch (Exception ex) {
                LOG.log(Level.SEVERE, c.toString(), ex);
            }
        }

        Map<String, AlmaField> fields = readFields(c);
        return new AlmaCatalog(url, apikey, charset, fields, customTemplatePath);
    }

    static Map<String, AlmaField> readFields(CatalogConfiguration c) {
        Map<String, AlmaField> fields = new HashMap<String, AlmaField>();
        List<CatalogQueryField> queryFields = c.getQueryFields();
        for (CatalogQueryField queryField : queryFields) {
            fields.put(queryField.getName(), new AlmaField(queryField));
        }
        return fields;
    }

    public AlmaCatalog(String url, String apikey, Charset recordCharset, Map<String, AlmaField> fields, String customTemplatePath) {
        this.url = url;
        this.apikey = apikey;
        this.recordCharset = recordCharset;
        this.fields = fields;
        this.transformers = new Transformers(customTemplatePath);
    }

    @Override
    public List<MetadataItem> find(String catalog, String fieldId, String value, Locale locale) throws TransformerException, IOException {
        WebTarget query = buildQuery(fieldId, value);
        String serverResponse = findRecord(query);
        ArrayList<MetadataItem> result = new ArrayList<>();
        try {
            JSONObject response = new JSONObject(serverResponse);
            JSONArray records = response.getJSONArray("bib");
            int index = 1;
            for (int i = 0; i < records.length(); i++) {
                JSONObject json = records.getJSONObject(i);
                if ("marc21".equals(json.getString("record_format"))) {
                    String marc = getMarcResult(json);
                    DOMResult marcResult = transformAlmaResponse(new StreamSource(new StringReader(updateMarc(marc))), new DOMResult());
                    if (marcResult != null) {
                        MetadataItem item = createResponse(index++, catalog, new DOMSource(marcResult.getNode()), locale);
                        result.add(item);
                    }
                } else {
                    LOG.log(Level.WARNING, "Expected record format \"marc21\", get \"" + json.getString("record_format") + "\".");
                    throw new IOException("Očekávaný formát záznamu \"marc21\", ze serveru dostal \"" + json.getString("record_format") + "\".");
                }
            }
        } catch (JSONException e) {
            LOG.log(Level.WARNING, e.getMessage());
        }
        return result;
    }

    <T extends Result> T transformAlmaResponse(StreamSource streamSource, T domResult) throws TransformerException {
        Transformer t = getAlma2MarcXslt().newTransformer();
        XslErrorListener errorListener = new XslErrorListener();
        t.setErrorListener(errorListener);
        try {
            t.transform(streamSource, domResult);
            return domResult;
        } catch (TransformerException ex) {
            // ignore ID not found
            if (errorListener.containError(XslErrorListener.ERR_ID_DOESNOT_EXIST)) {
                return null;
            } else if (!errorListener.getMessages().isEmpty()) {
                throw new TransformerException(errorListener.getMessages().toString(), ex);
            }
            throw ex;
        }
    }

    Templates getAlma2MarcXslt() throws TransformerConfigurationException {
        return createAlma2MarcXslt();
    }

    private static Templates createAlma2MarcXslt() throws TransformerConfigurationException {
        if (ALMA_MARC_XSLT == null) {
            String xsltSrc = OaiCatalog.class.getResource("/xml/Alma2MARC21slim.xsl").toExternalForm();
            ALMA_MARC_XSLT = TransformerFactory.newInstance().newTemplates(new StreamSource(xsltSrc));
        }
        return ALMA_MARC_XSLT;
    }

    private String getMarcResult(JSONObject json) throws JSONException {
        if (json != null) {
            String marc21 = json.getString("anies");
            if (marc21 != null && !marc21.isEmpty()) {
                return marc21;
            }
        }
        return null;
    }

    private String findRecord(WebTarget query) {
        if (query == null) {
            return null;
        }
        String result = query.request().get(String.class);
        return result;
    }

    private WebTarget buildQuery(String fieldId, String value) {
        return getClient()
                .target(url)
                .queryParam("apikey", apikey)
                .queryParam("format", "json")
                .queryParam(fieldId, value);
    }

    private Client getClient() {
        if (httpClient == null) {
            httpClient = createClient();
        }
        return httpClient;
    }

    private Client createClient() {
        ClientBuilder builder = ClientBuilder.newBuilder();
        if (LOG.isLoggable(Level.FINEST)) {
            builder.register(new LoggingFeature(LOG));
        }
        Client client = builder
                .property(ClientProperties.FOLLOW_REDIRECTS, true)
                .property(ClientProperties.CONNECT_TIMEOUT, 2 * 60 * 1000) // 2 min
                .property(ClientProperties.READ_TIMEOUT, 1 * 60 * 1000) // 1 min
                .build();
        return client;
    }

    Charset getRecordCharset() {
        return recordCharset;
    }

    String getUrl() {
        return url;
    }

    String getApikey() {
        return apikey;
    }

    private MetadataItem createResponse(int entryIdx, String catalog, Source marcxmlSrc, Locale locale)
            throws TransformerException, UnsupportedEncodingException {
        byte[] modsBytes = transformers.transformAsBytes(
                marcxmlSrc, Transformers.Format.MarcxmlAsMods3);
        modsBytes = repairModsBytes(modsBytes, (Document) ((DOMSource) marcxmlSrc).getNode());
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

    private String updateMarc(String marc) {
        marc = marc.replace("[", "");
        marc = marc.replace("\\", "");
        marc = marc.replace("]", "");
        marc = marc.replace("\"<", "<");
        marc = marc.replace(">\"", ">");
        marc = marc.replace("<record>", "<records xmlns:m=\"http://www.loc.gov/MARC21/slim\"><m:record>");
        marc = marc.replace("<record xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:discovery=\"http://purl.org/dc/elements/1.1/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">", "<records xmlns:m=\"http://www.loc.gov/MARC21/slim\"><m:record>");
        marc = marc.replace("</record>", "</m:record></records>");
        marc = marc.replace("leader>", "m:leader>");
        marc = marc.replace("controlfield", "m:controlfield");
        marc = marc.replace("datafield", "m:datafield");
        marc = marc.replace("subfield", "m:subfield");
        return marc;
    }

    private byte[] modsAsHtmlBytes(Source source, Locale locale) throws TransformerException {
        byte[] modsHtmlBytes = transformers.transformAsBytes(
                source, Transformers.Format.ModsAsHtml, ModsUtils.modsAsHtmlParameters(locale));
        return modsHtmlBytes;
    }

    public static final class AlmaField extends CatalogQueryField {

        public AlmaField(CatalogQueryField cqField) {
            super(cqField.getName(), cqField.getProperties());
        }
    }

    private static final class XslErrorListener implements ErrorListener {

        static final String ERR_INVALID_METADATA_FORMAT = "Invalid metadata format:";
        static final String ERR_ID_DOESNOT_EXIST = "idDoesNotExist:";

        private final List<String> messages = new ArrayList<String>();

        public List<String> getMessages() {
            return messages;
        }

        public boolean containError(String err) {
            for (String message : messages) {
                if (message.startsWith(err)) {
                    return true;
                }
            }
            return false;
        }

        @Override
        public void warning(TransformerException exception) throws TransformerException {
            messages.add(exception.getMessage().trim());
        }

        @Override
        public void error(TransformerException exception) throws TransformerException {
            messages.add(exception.getMessage().trim());
        }

        @Override
        public void fatalError(TransformerException exception) throws TransformerException {
            throw exception;
        }
    }
}
