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

import java.util.List;
import java.util.Locale;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assume;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class DigitizationRegistryCatalogTest {

    private static String url;
    private static String user;
    private static String passwd;

    public DigitizationRegistryCatalogTest() {
    }

    @BeforeClass
    public static void setUpClass() {
        url = System.getProperty("DigitizationRegistryCatalogTest.url");
//        System.out.println("URL: " + url);
        user = System.getProperty("DigitizationRegistryCatalogTest.user");
        passwd = System.getProperty("DigitizationRegistryCatalogTest.passwd");
        Assume.assumeNotNull(url);
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

    /**
     * Test of find method, of class DigitizationRegistryCatalog.
     */
    @Test
    public void testRemoteFind() throws Exception {
        String fieldName = "barcode";
        String value = "26005405857";
        Locale locale = new Locale("cs");
        DigitizationRegistryCatalog instance = new DigitizationRegistryCatalog(url, user, passwd);
        List<MetadataItem> details = instance.find(null, fieldName, value, locale);
        assertEquals(1, details.size());
        MetadataItem detail = details.get(0);
        assertEquals(1, detail.getId());
        assertEquals("Rozpravy Československé akademie věd", detail.getTitle());
        String mods = detail.getMods();
        System.out.println(mods);
        assertTrue("MODS", mods != null && mods.trim().length() > 10);
        String preview = detail.getPreview();
        assertTrue("MODS", preview != null && preview.trim().length() > 10);
    }
}
