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
package cz.cas.lib.proarc.common.catalog;

import cz.cas.lib.proarc.common.catalog.AlephXServer.Criteria;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import org.apache.commons.configuration.BaseConfiguration;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.io.InputStream;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class AlephXServerTest {

    private static ProxySelector defaultProxy;
    private static List<URI> externalConnections;

    public AlephXServerTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
        defaultProxy = ProxySelector.getDefault();
        // detect external connections
        ProxySelector.setDefault(new ProxySelector() {

            @Override
            public List<Proxy> select(URI uri) {
                externalConnections.add(uri);
                return defaultProxy.select(uri);
            }

            @Override
            public void connectFailed(URI uri, SocketAddress sa, IOException ioe) {
                defaultProxy.connectFailed(uri, sa, ioe);
            }
        });
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
        ProxySelector.setDefault(defaultProxy);
    }

    @Before
    public void setUp() {
        externalConnections = new ArrayList<URI>();
    }

    @After
    public void tearDown() {
        assertTrue(externalConnections.toString(), externalConnections.isEmpty());
    }

    @Test
    public void testFindResponse() throws Exception {
        InputStream xmlIS = AlephXServerTest.class.getResourceAsStream("alephXServerFindResponse.xml");
        assertNotNull(xmlIS);
        try {
            AlephXServer server = new AlephXServer("http://dummyUrl");
            AlephXServer.FindResponse found = server.createFindResponse(xmlIS);
            assertNotNull(found);
            assertEquals("183719", found.getNumber());
            assertEquals(1, found.getRecordCount());
            assertEquals(1, found.getEntryCount());
        } finally {
            xmlIS.close();
        }
    }
    
    @Test
    public void testDetailResponse() throws Exception {
        InputStream xmlIS = AlephXServerTest.class.getResourceAsStream("alephXServerDetailResponse.xml");
        assertNotNull(xmlIS);

        try {
            AlephXServer server = new AlephXServer("http://dummyUrl");
            List<MetadataItem> details = server.createDetailResponse(xmlIS, null);
            assertNotNull(server);
            assertEquals(1, details.size());
            MetadataItem detail = details.get(0);
            assertEquals(1, detail.getId());
            assertEquals("Rozpravy Československé akademie věd", detail.getTitle());
            String mods = detail.getMods();
//            System.out.println(mods);
            assertTrue("MODS", mods != null && mods.trim().length() > 10);
            String preview = detail.getPreview();
            assertTrue("MODS", preview != null && preview.trim().length() > 10);
        } finally {
            xmlIS.close();
        }
    }

    @Test
    public void testSetQuery() throws Exception {
        Criteria issnCriteria = Criteria.get("issn", "ISSNVALUE");
        URI result = AlephXServer.setQuery(new URI("http://aleph.nkp.cz/X?base=nkc"),
                issnCriteria.toUrlParams(), true);
        assertEquals("http://aleph.nkp.cz/X?base=nkc&op=find&request=ssn=ISSNVALUE", result.toASCIIString());
        System.out.println("URI: " + result.toASCIIString());
    }

    @Test
    public void testSetFurtherQuery() throws Exception {
        Criteria issnCriteria = Criteria.get("issn", "ISSNVALUE");
        URI result = AlephXServer.setQuery(new URI("http://aleph.nkp.cz/X?base=nkc"),
                issnCriteria.toUrlParams(), false);
        assertEquals("http://aleph.nkp.cz/X?op=find&request=ssn=ISSNVALUE", result.toASCIIString());
        System.out.println("URI: " + result.toASCIIString());
    }

    @Test
    public  void testCustomConfiguration() throws Exception {

        final String id = "aleph_nkp";

        CatalogConfiguration c = new CatalogConfiguration(id, "", new BaseConfiguration() {{
            addProperty(CatalogConfiguration.PROPERTY_URL, "http://aleph.nkp.cz/X?base=nkc");
            addProperty(CatalogConfiguration.PROPERTY_NAME, "test");
            addProperty(CatalogConfiguration.PROPERTY_TYPE, AlephXServer.TYPE);
            addProperty(CatalogConfiguration.PROPERTY_FIELDS, "sg");
            addProperty(CatalogConfiguration.FIELD_PREFIX + ".sg.title", "Short Signature");
            addProperty(CatalogConfiguration.FIELD_PREFIX + ".sg.query", "sg");
        }});

        AlephXServer result = AlephXServer.get(c);

        assertNotNull(result);
        assertEquals("op=find&request=sg=test", AlephXServer.Criteria.get("sg", "test").toUrlParams());
        assertEquals(null, AlephXServer.Criteria.get("sig", "test"));
    }



//    @Test
//    public void testFind() throws Exception {
//        System.out.println("find");
//        String issn = "";
//        AlephXServer instance = new AlephXServer();
//        List expResult = null;
//        List result = instance.find(issn);
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
}
