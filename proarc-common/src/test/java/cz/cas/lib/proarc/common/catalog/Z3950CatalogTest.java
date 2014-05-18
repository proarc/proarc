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

import cz.cas.lib.proarc.common.catalog.Z3950Catalog.Z3950Field;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.configuration.BaseConfiguration;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Assume;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class Z3950CatalogTest {

    public Z3950CatalogTest() {
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
    public void testGet() {
        final String host = "localhost";
        final int port = 9991;
        final String base = "testbase";
        final String recordCharset = "UTF-8";
        final String id = "z3950";

        CatalogConfiguration c = new CatalogConfiguration(id, "", new BaseConfiguration() {{
            addProperty(CatalogConfiguration.PROPERTY_URL, "tcp://" + host + ":" + port);
            addProperty(CatalogConfiguration.PROPERTY_NAME, "test");
            addProperty(CatalogConfiguration.PROPERTY_TYPE, Z3950Catalog.TYPE);
            addProperty(Z3950Catalog.PROPERTY_BASE, "testbase");
            addProperty(Z3950Catalog.PROPERTY_RECORD_CHARSET, "UTF-8");
        }});
        Z3950Catalog result = Z3950Catalog.get(c);
        assertNotNull(result);
        assertEquals(host, result.getHost());
        assertEquals(port, result.getPort());
        assertEquals(base, result.getBase());
        assertEquals(Charset.forName(recordCharset), result.getRecordCharset());
    }

    @Test
    public void testFind() throws Exception {
        String host = System.getProperty("Z3950CatalogTest.host");
        String port = System.getProperty("Z3950CatalogTest.port");
        String base = System.getProperty("Z3950CatalogTest.base");
        String recordCharset = System.getProperty("Z3950CatalogTest.recordCharset");
        Assume.assumeNotNull(host, port, base);

        String fieldName = "issn";
        String value = "0231-5904";
        Locale locale = null;
        Z3950Catalog instance = new Z3950Catalog(host, Integer.parseInt(port), base,
                recordCharset == null ? null : Charset.forName(recordCharset),
                new HashMap<String, Z3950Field>()
                );
        List<MetadataItem> result = instance.find(fieldName, value, locale);
        assertFalse(result.isEmpty());
    }

    @Test
    public void testReadFields() {
        final String catalogId = "catalogId";
        CatalogConfiguration c = new CatalogConfiguration(catalogId, "", new BaseConfiguration() {{
            addProperty(CatalogConfiguration.PROPERTY_FIELDS, "field1,field2 , field3  ");
            addProperty(CatalogConfiguration.FIELD_PREFIX + '.' + "field1" + '.' + Z3950Catalog.PROPERTY_FIELD_QUERY, "query1");
            addProperty(CatalogConfiguration.FIELD_PREFIX + '.' + "field2" + '.' + Z3950Catalog.PROPERTY_FIELD_QUERY, "query2");
        }});
        Map<String, Z3950Field> result = Z3950Catalog.readFields(c);
        assertNotNull(result);
        assertEquals(3, result.size());
        Z3950Field field1 = result.get("field1");
        assertNotNull("field1", field1);
        assertEquals("query1", field1.getQuery());
        Z3950Field field2 = result.get("field2");
        assertNotNull("field2", field2);
        assertEquals("query2", field2.getQuery());
        Z3950Field field3 = result.get("field3");
        assertNotNull("field3", field3);
        assertNull(field3.getQuery());
    }
}
