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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.xml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.URIResolver;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 * Transforms MARCXML and MODS documents to various {@link Format formats}.
 *
 * @author Jan Pokorsky
 * @see <a href='http://www.loc.gov/standards/marcxml/'>MARC 21 XML Schema</a>
 * @see <a href='http://www.loc.gov/standards/mods/mods-conversions.html'>MODS Conversions</a>
 */
public final class Transformers {

    private static final Map<Format, String> FORMAT2XSL;
    private static final Map<Format, Templates> FORMAT2TEMPLATES;
    private static final Logger LOG = Logger.getLogger(Transformers.class.getName());

//    private static final String DC_RDF_XSL_PATH = "http://www.loc.gov/standards/marcxml/xslt/MARC21slim2RDFDC.xsl";
    private static final String MODS_3_XSL_PATH = "http://www.loc.gov/standards/mods/v3/MARC21slim2MODS3-4.xsl";
    private static final String OAIMARC2MARC21slim_XSL_PATH = "http://www.loc.gov/standards/marcxml/xslt/OAIMARC2MARC21slim.xsl";
    private static final String MARC21slim2HTML_XSL_PATH = "http://www.loc.gov/standards/marcxml/xslt/MARC21slim2HTML.xsl";
    private static final String MODS2HTML_XSL_PATH = "http://www.loc.gov/standards/mods/mods.xsl";
    private static final String MODS2TITLE_XSL_PATH = "/xml/mods2Title.xsl";
    private static final String MODS2AUTHORITYTITLE_XSL_PATH = "/xml/mods2AuthorityTitle.xsl";
    private static final String MODS2FEDORA_LABEL_XSL_PATH = "/xml/mods2FedoraLabel.xsl";
    private static final String ALEPHXSERVERFIX_XSL_PATH = "/xml/alephOaiMarcFix.xsl";

    static {
        FORMAT2TEMPLATES = new EnumMap<Format, Templates>(Format.class);
        FORMAT2XSL = new EnumMap<Format, String>(Format.class);
//        FORMAT2XSL.put(Format.MarcxmlAsDcRdf, DC_RDF_XSL_PATH);
        FORMAT2XSL.put(Format.MarcxmlAsMods34, MODS_3_XSL_PATH);
        FORMAT2XSL.put(Format.MarcxmlAsMods3, MODS_3_XSL_PATH);
        FORMAT2XSL.put(Format.OaimarcAsMarc21slim, OAIMARC2MARC21slim_XSL_PATH);
        FORMAT2XSL.put(Format.MarcxmlAsHtml, MARC21slim2HTML_XSL_PATH);
        FORMAT2XSL.put(Format.ModsAsHtml, MODS2HTML_XSL_PATH);
        FORMAT2XSL.put(Format.ModsAsTitle, MODS2TITLE_XSL_PATH);
        FORMAT2XSL.put(Format.ModsAsAuthorityTitle, MODS2AUTHORITYTITLE_XSL_PATH);
        FORMAT2XSL.put(Format.ModsAsFedoraLabel, MODS2FEDORA_LABEL_XSL_PATH);
        FORMAT2XSL.put(Format.AlephOaiMarcFix, ALEPHXSERVERFIX_XSL_PATH);
    }

    public Transformers(String customTemplatePath) {
        initTemplates(customTemplatePath);
    }

    public Source transform(Source input, Format format) throws TransformerException {
        return transform(input, format, Collections.<String, Object>emptyMap());
    }

    public Source transform(Source input, Format format, Map<String, Object> params) throws TransformerException {
        return toSource(transformAsBytes(input, format, params));
    }

    public byte[] transformAsBytes(Source input, Format format) throws TransformerException {
        return transformAsBytes(input, format, Collections.<String, Object>emptyMap());
    }

    public byte[] transformAsBytes(Source input, Format format, Map<String, Object> params) throws TransformerException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        Result output = new StreamResult(buffer);
        Transformer t = createTransformer(format);
        for (Map.Entry<String, Object> param : params.entrySet()) {
            t.setParameter(param.getKey(), param.getValue());
        }
        t.transform(input, output);
        return buffer.toByteArray();
    }

    public Source toSource(byte[] buffer) {
        return new StreamSource(new ByteArrayInputStream(buffer));
    }

    public Source dump(Source source, StringBuilder dump) {
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer t = factory.newTransformer();
            ByteArrayOutputStream buffer = new ByteArrayOutputStream();
            t.transform(source, new StreamResult(buffer));
            dump.append(buffer.toString("UTF-8"));
            return new StreamSource(new ByteArrayInputStream(buffer.toByteArray()));
        } catch (TransformerException ex) {
            throw new IllegalStateException(ex);
        } catch (UnsupportedEncodingException ex) {
            throw new IllegalStateException(ex);
        }
    }

    public Source dump2Temp(Source source, String filename) {
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer t = factory.newTransformer();
            ByteArrayOutputStream buffer = new ByteArrayOutputStream();
            t.transform(source, new StreamResult(buffer));

//            t.transform(new StreamSource(new ByteArrayInputStream(buffer.toByteArray())),
//                    new StreamResult(new File("/tmp/aleph/" + filename)));

            return new StreamSource(new ByteArrayInputStream(buffer.toByteArray()));
        } catch (TransformerException ex) {
            throw new IllegalStateException(ex);
        }
    }

    static Source getXsl(Format format, URIResolver resolver) throws TransformerException {
        String path = FORMAT2XSL.get(format);
        return resolver.resolve(path, path);
    }

    private static Templates createTemplates(Format recordFormat, String customTemplatePath) throws TransformerException {
        TransformerFactory factory = TransformerFactory.newInstance();
//        factory.setAttribute("debug", true);
        SimpleResolver resolver = new SimpleResolver(customTemplatePath);
        factory.setURIResolver(resolver);
        Templates templates = factory.newTemplates(getXsl(recordFormat, resolver));
        return templates;
    }

    private static Transformer createTransformer(Format recordFormat) throws TransformerConfigurationException {
        Templates templates = FORMAT2TEMPLATES.get(recordFormat);
        if (templates == null) {
            throw new TransformerConfigurationException("Cannot transform " + recordFormat);
        }
        return templates.newTransformer();
    }

    private static void initTemplates(String customTemplatePath) {
        for (Map.Entry<Format, String> entry : FORMAT2XSL.entrySet()) {
            try {
                Templates templates = createTemplates(entry.getKey(), customTemplatePath);
                FORMAT2TEMPLATES.put(entry.getKey(), templates);
            } catch (TransformerException ex) {
                LOG.log(Level.SEVERE, entry.getValue(), ex);
            }
        }
    }

    /**
     * This allows to run Transformers offline.
     */
    static final class SimpleResolver implements URIResolver {

        /** mapping to offline resources */
        private static final Map<String, String> CATALOG = new HashMap<String, String>();

        private final String customPath;

        static {
            CATALOG.put(MODS_3_XSL_PATH, "/xml/MARC21slim2MODS3.xsl");
//            CATALOG.put(DC_RDF_XSL_PATH, "/xslts/MARC21slim2RDFDC.xsl");
            CATALOG.put(OAIMARC2MARC21slim_XSL_PATH, "/xml/OAIMARC2MARC21slim.xsl");
            CATALOG.put(MARC21slim2HTML_XSL_PATH, "/xml/MARC21slim2HTML.xsl");
            CATALOG.put(MODS2HTML_XSL_PATH, "/xml/mods2html.xsl");
            CATALOG.put("http://www.loc.gov/standards/mods/modsDictionary.xml", "/xml/modsDictionary.xml");
            CATALOG.put("http://www.loc.gov/standards/marcxml/xslt/MARC21slimUtils.xsl", "/xml/MARC21slimUtils.xsl");
            CATALOG.put(MODS2TITLE_XSL_PATH, MODS2TITLE_XSL_PATH);
            CATALOG.put(MODS2AUTHORITYTITLE_XSL_PATH, MODS2AUTHORITYTITLE_XSL_PATH);
            CATALOG.put(MODS2FEDORA_LABEL_XSL_PATH, MODS2FEDORA_LABEL_XSL_PATH);
            CATALOG.put(ALEPHXSERVERFIX_XSL_PATH, ALEPHXSERVERFIX_XSL_PATH);
        }

        public SimpleResolver(String customTemplatePath) {
            this.customPath = customTemplatePath;
        }

        public Source resolve(String href, String base) throws TransformerException {
            if (MODS_3_XSL_PATH.equals(href) && (customPath != null)) {
                File file = new File(customPath);
                if (file.exists() && file.isFile()) {
                    return new StreamSource(file);
                }
            }

            String path = CATALOG.get(href);
            if (path == null) {
                path = "/xml/" + href;
            }
            URL resource = SimpleResolver.class.getResource(path);
            if (resource != null) {
//                Transformers.LOG.info(String.format("\nhref: %s, \nbase: %s, \npath: %s, \nres:  %s", href, base, path, resource));
                return new StreamSource(resource.toExternalForm());
            }

            // delegates to system resolver
            return null;
        }

    }

    public enum Format {

        /**
         * Use {@link #MarcxmlAsMods3} instead.
         * @deprecated
         */
        @Deprecated
        MarcxmlAsMods34,

        /** The latest MODS 3 version. */
        MarcxmlAsMods3,

        /*MarcxmlAsDcRdf,*/
        OaimarcAsMarc21slim,
        MarcxmlAsHtml,
        ModsAsHtml,
        ModsAsTitle,
        ModsAsFedoraLabel,
        ModsAsAuthorityTitle,
        AlephOaiMarcFix;

    }

}
