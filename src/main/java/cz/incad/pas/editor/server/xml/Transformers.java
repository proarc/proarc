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
package cz.incad.pas.editor.server.xml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
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
//    private static final String MODS_33_XSL_PATH = "http://www.loc.gov/standards/mods/v3/MARC21slim2MODS3-3.xsl";
    private static final String MODS_34_XSL_PATH = "http://www.loc.gov/standards/mods/v3/MARC21slim2MODS3-4.xsl";
    private static final String OAIMARC2MARC21slim_XSL_PATH = "http://www.loc.gov/standards/marcxml/xslt/OAIMARC2MARC21slim.xsl";
    private static final String MARC21slim2HTML_XSL_PATH = "http://www.loc.gov/standards/marcxml/xslt/MARC21slim2HTML.xsl";
    private static final String MODS2HTML_XSL_PATH = "http://www.loc.gov/standards/mods/mods.xsl";

    static {
        FORMAT2TEMPLATES = new EnumMap<Format, Templates>(Format.class);
        FORMAT2XSL = new EnumMap<Format, String>(Format.class);
//        FORMAT2XSL.put(Format.MarcxmlAsDcRdf, DC_RDF_XSL_PATH);
//        FORMAT2XSL.put(Format.MarcxmlAsMods33, MODS_33_XSL_PATH);
        FORMAT2XSL.put(Format.MarcxmlAsMods34, MODS_34_XSL_PATH);
        FORMAT2XSL.put(Format.OaimarcAsMarc21slim, OAIMARC2MARC21slim_XSL_PATH);
        FORMAT2XSL.put(Format.MarcxmlAsHtml, MARC21slim2HTML_XSL_PATH);
        FORMAT2XSL.put(Format.ModsAsHtml, MODS2HTML_XSL_PATH);
        initTemplates();
    }

    public Source transform(Source input, Format format) throws TransformerException {
        return toSource(transformAsBytes(input, format));
    }

    public byte[] transformAsBytes(Source input, Format format) throws TransformerException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        Result output = new StreamResult(buffer);
        Transformer t = createTransformer(format);
        t.transform(input, output);
        return buffer.toByteArray();
    }

    public Source toSource(byte[] buffer) {
        return new StreamSource(new ByteArrayInputStream(buffer));
    }

    static Source getXsl(Format format, URIResolver resolver) throws TransformerException {
        String path = FORMAT2XSL.get(format);
        return resolver.resolve(path, path);
    }

    private static Templates createTemplates(Format recordFormat) throws TransformerException {
        TransformerFactory factory = TransformerFactory.newInstance();
//        factory.setAttribute("debug", true);
        SimpleResolver resolver = new SimpleResolver();
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

    private static void initTemplates() {
        for (Map.Entry<Format, String> entry : FORMAT2XSL.entrySet()) {
            try {
                Templates templates = createTemplates(entry.getKey());
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

        static {
            CATALOG.put(MODS_34_XSL_PATH, "/xml/MARC21slim2MODS3-4.xsl");
//            CATALOG.put(MODS_33_XSL_PATH,"/xslts/MARC21slim2MODS3-3.xsl");
//            CATALOG.put(DC_RDF_XSL_PATH, "/xslts/MARC21slim2RDFDC.xsl");
            CATALOG.put(OAIMARC2MARC21slim_XSL_PATH, "/xml/OAIMARC2MARC21slim.xsl");
            CATALOG.put(MARC21slim2HTML_XSL_PATH, "/xml/MARC21slim2HTML.xsl");
            CATALOG.put(MODS2HTML_XSL_PATH, "/xml/mods2html.xsl");
            CATALOG.put("http://www.loc.gov/standards/mods/modsDictionary.xml", "/xml/modsDictionary.xml");
            CATALOG.put("http://www.loc.gov/standards/marcxml/xslt/MARC21slimUtils.xsl", "/xslts/MARC21slimUtils.xsl");
        }

        @Override
        public Source resolve(String href, String base) throws TransformerException {
            String path = CATALOG.get(href);
            if (path == null) {
                path = "/xslts/" + href;
            }
            InputStream stream = SimpleResolver.class.getResourceAsStream(path);
            if (stream != null) {
                return new StreamSource(stream, href);
            }

            // delegates to system resolver
            return null;
        }

    }

    public enum Format {

        /*MarcxmlAsMods33,*/ MarcxmlAsMods34, /*MarcxmlAsDcRdf,*/ OaimarcAsMarc21slim, MarcxmlAsHtml, ModsAsHtml;
    }

}
