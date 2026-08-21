/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.ocr;

import cz.cas.lib.proarc.common.process.export.mets.MetsLSResolver;
import cz.cas.lib.proarc.common.process.imports.ImportProfile;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.foxml.management.DatastreamProfile;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.xml.sax.SAXException;

/**
 * ALTO data stream.
 *
 * @author Jan Pokorsky
 */
public final class AltoDatastream {

    public static final String ALTO_ID = "ALTO";
    public static final String ALTO_LABEL = "ALTO for this object";
    public static final String ALTO_FORMAT_URI = "http://www.loc.gov/standards/alto/ns-v2#";
    private static final Map<String, String> ALTO_SCHEMA_PATHS = createSchemaPaths();
    public static final List<String> SUPPORTED_VERSIONS = List.copyOf(ALTO_SCHEMA_PATHS.keySet());
    public static ImportProfile config;

    public AltoDatastream(ImportProfile config) {
        this.config = config;
    }

    public static DatastreamProfile altoProfile() {
        return FoxmlUtils.managedProfile(ALTO_ID, ALTO_FORMAT_URI, ALTO_LABEL);
    }

    /**
     * Adds ALTO content to a fedora object
     * @param fo fedora object
     * @param altoUri OCR
     * @param msg log message
     * @throws DigitalObjectException failure
     */
    public static void importAlto(ProArcObject fo, URI altoUri, String msg) throws DigitalObjectException {
        try {
            if (!isAlto(altoUri)) {
                throw new DigitalObjectException(fo.getPid(),
                        String.format("%s: missing expected ALTO version: %s",
                                altoUri.toASCIIString(), AltoDatastream.ALTO_FORMAT_URI),
                        null);
            }
        } catch (Exception ex) {
            throw new DigitalObjectException(fo.getPid(), altoUri.toASCIIString(), ex);
        }
        XmlStreamEditor editor = fo.getEditor(altoProfile());
        editor.write(altoUri, editor.getLastModified(), msg);
    }

    /**
     * Checks whether URI content contains proper ALTO data.
     * @param alto URI
     * @throws IOException failure
     */
    static boolean isAlto(URI alto) throws IOException, SAXException {
        return validSchema(getSchemas(), alto);
        //getSchema().newValidator().validate(new StreamSource(alto.toASCIIString()));
    }

    private static boolean validSchema(List<Schema> schemas, URI alto) throws IOException, SAXException {
        for (Schema schema : schemas) {
            try {
                schema.newValidator().validate(new StreamSource(alto.toASCIIString()));
                return true;
            } catch (SAXException ex) {
                // Try the next supported ALTO schema.
            }
        }
        return false;
    }

    public static List<Schema> getSchemas() throws SAXException {
        String version = config == null ? null : config.getAltoFileVersion();
        if (version == null || version.isBlank()) {
            return getSchemasList();
        }

        String schemaPath = ALTO_SCHEMA_PATHS.get(version);
        if (schemaPath == null) {
            throw new SAXException("Unsupported ALTO version '" + version
                    + "'. Supported versions: " + String.join(", ", SUPPORTED_VERSIONS));
        }
        return List.of(createSchema(schemaPath));
    }

    public static List<Schema> getSchemasList() throws SAXException {
        List<Schema> schemas = new ArrayList<>();
        for (String schemaPath : ALTO_SCHEMA_PATHS.values()) {
            schemas.add(createSchema(schemaPath));
        }
        return schemas;
    }

    private static Schema createSchema(String schemaPath) throws SAXException {
        SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        schemaFactory.setResourceResolver(MetsLSResolver.getInstance());
        return schemaFactory.newSchema(AltoDatastream.class.getResource(schemaPath));
    }

    private static Map<String, String> createSchemaPaths() {
        Map<String, String> schemaPaths = new LinkedHashMap<>();
        schemaPaths.put("2.0", "/xml/alto/alto-v2.0.xsd");
        schemaPaths.put("2.1", "/xml/alto/alto-v2.1.xsd");
        schemaPaths.put("3.0", "/xml/alto/alto-v3.0.xsd");
        schemaPaths.put("3.1", "/xml/alto/alto-v3.1.xsd");
        schemaPaths.put("4.0", "/xml/alto/alto-v4.0.xsd");
        schemaPaths.put("4.1", "/xml/alto/alto-v4.1.xsd");
        schemaPaths.put("4.2", "/xml/alto/alto-v4.2.xsd");
        schemaPaths.put("4.3", "/xml/alto/alto-v4.3.xsd");
        schemaPaths.put("4.4", "/xml/alto/alto-v4.4.xsd");
        return Collections.unmodifiableMap(schemaPaths);
    }
}
