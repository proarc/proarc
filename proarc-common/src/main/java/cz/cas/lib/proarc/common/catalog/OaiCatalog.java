/*
 * Copyright (C) 2015 Jan Pokorsky
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

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.filter.HTTPBasicAuthFilter;
import com.sun.jersey.api.client.filter.LoggingFilter;
import com.sun.jersey.core.util.MultivaluedMapImpl;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.xml.Transformers;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.core.MultivaluedMap;
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

/**
 * The catalog can query OAI repositories with
 * <a href='http://www.openarchives.org/OAI/openarchivesprotocol.html#GetRecord'>GetRecord</a> queries.
 *
 * @author Jan Pokorsky
 */
public class OaiCatalog implements BibliographicCatalog {

    public static final String TYPE = "OAICatalog";
    /**
     * The field name used to query OAI repository.
     */
    static final String FIELD_ID = "id";
    /**
     * The optional prefix for the identifier in GetRecord queries.
     */
    static final String PROPERTY_IDENTIFIER_PREFIX = "identifierPrefix";
    /**
     * The metadata prefix for GetRecord queries.
     */
    static final String PROPERTY_METADATA_PREFIX = "metadataPrefix";
    private static Templates OAI_MARC_XSLT;
    private static final Logger LOG = Logger.getLogger(OaiCatalog.class.getName());

    private Client httpClient;
    private final String url;
    private String user;
    private String password;
    private final String metadataPrefix;
    private String identifierPrefix;
    private final Transformers transformers;
    private boolean debug;

    public static OaiCatalog get(CatalogConfiguration c) {
        if (c == null || !TYPE.equals(c.getType())) {
            return null;
        }
        String url = c.getUrl();
        String metadataPrefix = c.getProperty(PROPERTY_METADATA_PREFIX);
        OaiCatalog cat = new OaiCatalog(url, metadataPrefix);
        cat.setIdentifierPrefix(c.getProperty(PROPERTY_IDENTIFIER_PREFIX, null));
        cat.setUser(c.getProperty(CatalogConfiguration.PROPERTY_USER, null));
        cat.setPassword(c.getProperty(CatalogConfiguration.PROPERTY_PASSWD, null));
        cat.setDebug(c.getDebug());
        return cat;
    }

    public OaiCatalog(String url, String metadataPrefix) {
        this(url, metadataPrefix, null);
    }

    public OaiCatalog(String url, String metadataPrefix, String identifierPrefix) {
        this.url = url;
        this.metadataPrefix = metadataPrefix;
        this.identifierPrefix = identifierPrefix;
        this.transformers = new Transformers();
    }

    public void setDebug(boolean debug) {
        this.debug = debug;
    }

    public void setIdentifierPrefix(String identifierPrefix) {
        this.identifierPrefix = identifierPrefix;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    public List<MetadataItem> find(String fieldName, String value, Locale locale) throws TransformerException, IOException {
        WebResource query = buildOaiQuery(fieldName, value);
        String oaiResponse = findOaiRecord(query);
        ArrayList<MetadataItem> result = new ArrayList<MetadataItem>();
        if (oaiResponse != null) {
            DOMResult marcResult = transformOaiResponse(
                    new StreamSource(new StringReader(oaiResponse)), new DOMResult());
            if (marcResult != null) {
                MetadataItem item = createResponse(0, new DOMSource(marcResult.getNode()), locale);
                result.add(item);
            }
        }
        return result;
    }

    public String findOaiRecord(String id) {
        return findOaiRecord(buildOaiQuery(id));
    }

    String findOaiRecord(WebResource query) {
        if (query == null) {
            return null;
        }
        String result = query.get(String.class);
        return result;
    }

    WebResource buildOaiQuery(String fieldName, String value) {
        WebResource query = null;
        if (FIELD_ID.equals(fieldName)) {
            query = buildOaiQuery(value);
        }
        return query;
    }

    WebResource buildOaiQuery(String id) {
        if (identifierPrefix != null && !id.startsWith(identifierPrefix)) {
            id = identifierPrefix + id;
        }
        MultivaluedMap<String, String> params = new MultivaluedMapImpl();
        params.add("verb", "GetRecord");
        params.add("identifier", id);
        params.add("metadataPrefix", metadataPrefix);
        WebResource query = getClient().resource(url).queryParams(params);
        if (debug || LOG.isLoggable(Level.FINEST)) {
            query.addFilter(new LoggingFilter(System.out));
        }
        return query;
    }

    private Client getClient() {
        if (httpClient == null) {
            httpClient = createClient();
        }
        return httpClient;
    }

    private Client createClient() {
        Client client = Client.create();
        client.setFollowRedirects(Boolean.TRUE);
        client.setConnectTimeout(2 * 60 * 1000); // 2 min
        client.setReadTimeout(2 * 60 * 1000); // 2 min
        if (user != null) {
            client.addFilter(new HTTPBasicAuthFilter(user, password));
        }
        return client;
    }

    private MetadataItem createResponse(int entryIdx, Source marcxmlSrc, Locale locale)
            throws TransformerException, UnsupportedEncodingException {

        byte[] modsBytes = transformers.transformAsBytes(
                marcxmlSrc, Transformers.Format.MarcxmlAsMods3);
        byte[] modsHtmlBytes = modsAsHtmlBytes(new StreamSource(new ByteArrayInputStream(modsBytes)), locale);
        byte[] modsTitleBytes = transformers.transformAsBytes(
                new StreamSource(new ByteArrayInputStream(modsBytes)),
                Transformers.Format.ModsAsTitle);
        return new MetadataItem(entryIdx, new String(modsBytes, "UTF-8"),
                new String(modsHtmlBytes, "UTF-8"), new String(modsTitleBytes, "UTF-8"));
    }

    private byte[] modsAsHtmlBytes(Source source, Locale locale) throws TransformerException {
        byte[] modsHtmlBytes = transformers.transformAsBytes(
                source, Transformers.Format.ModsAsHtml, ModsUtils.modsAsHtmlParameters(locale));
        return modsHtmlBytes;
    }

    /**
     * @return the result metadata or {@code null} for empty result.
     */
    <T extends Result> T transformOaiResponse(Source src, T dst) throws TransformerException {
        Transformer t = getOai2MarcXslt().newTransformer();
        XslErrorListener errorListener = new XslErrorListener();
        t.setErrorListener(errorListener);
        try {
            t.transform(src, dst);
            return dst;
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

    Templates getOai2MarcXslt() throws TransformerConfigurationException {
        return createOai2MarcXslt();
    }

    private static Templates createOai2MarcXslt() throws TransformerConfigurationException {
        if (OAI_MARC_XSLT == null) {
            String xsltSrc = OaiCatalog.class.getResource("/xml/Oai2MARC21slim.xsl").toExternalForm();
            OAI_MARC_XSLT = TransformerFactory.newInstance().newTemplates(new StreamSource(xsltSrc));
        }
        return OAI_MARC_XSLT;
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
