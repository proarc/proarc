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

import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.export.mets.MetsLSResolver;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import java.io.IOException;
import java.net.URI;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import cz.cas.lib.proarc.common.imports.ImportProfile;
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
    private static Schema ALTO_SCHEMA;
    private static final String ALTO_SCHEMA_PATH_20 = "/xml/alto-v2.0.xsd";
    private static final String ALTO_SCHEMA_PATH_21 = "/xml/alto-v2.1.xsd";
    private static final String ALTO_SCHEMA_PATH_30 = "/xml/alto-v3.0.xsd";
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
    public static void importAlto(FedoraObject fo, URI altoUri, String msg) throws DigitalObjectException {
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
        getSchema().newValidator().validate(new StreamSource(alto.toASCIIString()));
        return true;
    }

    public static Schema getSchema() throws SAXException {
        if (ALTO_SCHEMA == null) {
            SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            schemaFactory.setResourceResolver(MetsLSResolver.getInstance());
            if (config != null) {
                switch (config.getAltoFileVersion()) {
                    case "2.0":
                        ALTO_SCHEMA = schemaFactory.newSchema(AltoDatastream.class.getResource(ALTO_SCHEMA_PATH_20));
                        break;
                    case "2.1":
                        ALTO_SCHEMA = schemaFactory.newSchema(AltoDatastream.class.getResource(ALTO_SCHEMA_PATH_21));
                        break;
                    case "3.0":
                        ALTO_SCHEMA = schemaFactory.newSchema(AltoDatastream.class.getResource(ALTO_SCHEMA_PATH_30));
                        break;
                }
            } else {
                ALTO_SCHEMA = schemaFactory.newSchema(AltoDatastream.class.getResource(ALTO_SCHEMA_PATH_21));
            }
        }
        return ALTO_SCHEMA;
    }

}
