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

import jakarta.ws.rs.client.WebTarget;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.glassfish.jersey.uri.UriComponent;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.xmlunit.builder.Input;
import org.xmlunit.xpath.JAXPXPathEngine;
import org.w3c.dom.Node;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 *
 * @author Jan Pokorsky
 */
public class OaiCatalogTest {

    public OaiCatalogTest() {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testOaiResponseTransformation() throws Exception {
        OaiCatalog c = new OaiCatalog("", "", null);
        String srcUrl = OaiCatalogTest.class.getResource("oaiResponse.xml").toExternalForm();
        StreamResult result = c.transformOaiResponse(new StreamSource(srcUrl), new StreamResult(new StringWriter()));
        assertNotNull(result);

        String marc = result.getWriter().toString();

        Map<String, String> namespaces = new HashMap<>();
        namespaces.put("marc", "http://www.loc.gov/MARC21/slim");

        JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
        xpathEngine.setNamespaceContext(namespaces);

        Iterable<Node> nodes = xpathEngine.selectNodes("/marc:record", Input.fromString(marc).build());

        assertFalse(nodes.iterator().hasNext());
    }

    @Test
    public void testOaiIdNotExistResponseTransformation() throws Exception {
        OaiCatalog c = new OaiCatalog("", "", null);
        String srcUrl = OaiCatalogTest.class.getResource("oaiIdNotExistResponse.xml").toExternalForm();
        StringWriter resultWriter = new StringWriter();
        StreamResult result = c.transformOaiResponse(new StreamSource(srcUrl), new StreamResult(resultWriter));
        assertNull(result, resultWriter.toString());
    }

    @Test
    public void testOaiErrorResponseTransformation() throws Exception {
        OaiCatalog c = new OaiCatalog("", "", null);
        String srcUrl = OaiCatalogTest.class.getResource("oaiErrorResponse.xml").toExternalForm();
        try {
            c.transformOaiResponse(new StreamSource(srcUrl), new StreamResult(new StringWriter()));
            fail();
        } catch (TransformerException result) {
            String msg = result.getMessage();
            assertTrue(msg.contains("cannotDisseminateFormat"), msg);
        }
    }

    @Test
    public void testOaiInvalidResponseTransformation() throws Exception {
        OaiCatalog c = new OaiCatalog("", "", null);
        String srcUrl = OaiCatalogTest.class.getResource("oaiInvalidResponse.xml").toExternalForm();
        try {
            c.transformOaiResponse(new StreamSource(srcUrl), new StreamResult(new StringWriter()));
            fail();
        } catch (TransformerException result) {
            String msg = result.getMessage();
            assertTrue(msg.contains("Invalid metadata format: http://www.openarchives.org/OAI/2.0/oai_dc/"), msg);
        }
    }

    @Test
    public void testIdentifierPrefix() throws Exception {
        String url = "http://arXiv.org/oai2";
        String metadataPrefix = "marc21";
        String identifierPrefix = "oai:arXiv.org:quant-ph/";
        OaiCatalog c = new OaiCatalog(url, metadataPrefix, identifierPrefix);
        WebTarget wr = c.buildOaiQuery(OaiCatalog.FIELD_ID, "4");
        String resultQuery = wr.getUri().toString();
        String encIdParam = "identifier=" + UriComponent.encode(
                "oai:arXiv.org:quant-ph/4",
                UriComponent.Type.QUERY_PARAM_SPACE_ENCODED);
        assertTrue(resultQuery.contains(encIdParam), resultQuery);

        // do not prefix ID with prefix
        wr = c.buildOaiQuery(OaiCatalog.FIELD_ID, "oai:arXiv.org:quant-ph/4");
        resultQuery = wr.getUri().toString();
        assertTrue(resultQuery.contains(encIdParam), resultQuery);

        // no prefix
        c.setIdentifierPrefix(null);
        wr = c.buildOaiQuery(OaiCatalog.FIELD_ID, "4");
        resultQuery = wr.getUri().toString();
        assertTrue(resultQuery.contains("identifier=4"), resultQuery);
    }
}
