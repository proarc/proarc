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

import java.io.StringWriter;
import java.util.HashMap;
import javax.ws.rs.client.WebTarget;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.custommonkey.xmlunit.SimpleNamespaceContext;
import org.custommonkey.xmlunit.XMLAssert;
import org.custommonkey.xmlunit.XMLUnit;
import org.glassfish.jersey.uri.UriComponent;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class OaiCatalogTest {

    public OaiCatalogTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testOaiResponseTransformation() throws Exception {
        OaiCatalog c = new OaiCatalog("", "", null);
        String srcUrl = OaiCatalogTest.class.getResource("oaiResponse.xml").toExternalForm();
        StreamResult result = c.transformOaiResponse(new StreamSource(srcUrl), new StreamResult(new StringWriter()));
        assertNotNull(result);

        HashMap<String, String> namespaces = new HashMap<String, String>();
        namespaces.put("marc", "http://www.loc.gov/MARC21/slim");
        XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(namespaces));
        String marc = result.getWriter().toString();
        XMLAssert.assertXpathExists("/marc:record", marc);
    }

    @Test
    public void testOaiIdNotExistResponseTransformation() throws Exception {
        OaiCatalog c = new OaiCatalog("", "", null);
        String srcUrl = OaiCatalogTest.class.getResource("oaiIdNotExistResponse.xml").toExternalForm();
        StringWriter resultWriter = new StringWriter();
        StreamResult result = c.transformOaiResponse(new StreamSource(srcUrl), new StreamResult(resultWriter));
        assertNull(resultWriter.toString(), result);
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
            assertTrue(msg, msg.contains("cannotDisseminateFormat"));
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
            assertTrue(msg, msg.contains("Invalid metadata format: http://www.openarchives.org/OAI/2.0/oai_dc/"));
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
        assertTrue(resultQuery, resultQuery.contains(encIdParam));

        // do not prefix ID with prefix
        wr = c.buildOaiQuery(OaiCatalog.FIELD_ID, "oai:arXiv.org:quant-ph/4");
        resultQuery = wr.getUri().toString();
        assertTrue(resultQuery, resultQuery.contains(encIdParam));

        // no prefix
        c.setIdentifierPrefix(null);
        wr = c.buildOaiQuery(OaiCatalog.FIELD_ID, "4");
        resultQuery = wr.getUri().toString();
        assertTrue(resultQuery, resultQuery.contains("identifier=4"));
    }

//    @Test
    public void testFindOaiRecord() throws Exception {
        String url = "http://web2.mlp.cz/cgi/oai";
        String metadataPrefix = "marc21";
        String identifierPrefix = "oai:www.mlp.cz:";
        OaiCatalog c = new OaiCatalog(url, metadataPrefix, identifierPrefix);
        String oaiRecord = c.findOaiRecord("4");
        System.out.println(oaiRecord);
        assertNotNull(oaiRecord);
    }

}
