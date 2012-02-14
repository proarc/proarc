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
package cz.incad.pas.editor.server.catalog;

import cz.incad.pas.editor.server.marc.MarcUtilsTest;
import cz.incad.pas.editor.server.rest.MetadataCatalogResource.MetadataItem;
import java.io.InputStream;
import java.util.List;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class AlephXServerTest {

    public AlephXServerTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testFindResponse() throws Exception {
        InputStream xmlIS = MarcUtilsTest.class.getResourceAsStream("/cz/incad/pas/editor/server/catalog/alephXServerFindResponse.xml");
        assertNotNull(xmlIS);
        try {
            AlephXServer server = new AlephXServer();
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
        InputStream xmlIS = MarcUtilsTest.class.getResourceAsStream("/cz/incad/pas/editor/server/catalog/alephXServerDetailResponse.xml");
        assertNotNull(xmlIS);

        try {
            AlephXServer server = new AlephXServer();
            List<MetadataItem> details = server.createDetailResponse(xmlIS);
            assertNotNull(server);
            assertEquals(1, details.size());
            MetadataItem detail = details.get(0);
            assertEquals(1, detail.getId());
            assertEquals("Česká komora lehkých obvodových plášťů.", detail.getTitle());
            String mods = detail.getMods();
            assertTrue("MODS", mods != null && mods.trim().length() > 10);
            String preview = detail.getPreview();
            assertTrue("MODS", preview != null && preview.trim().length() > 10);
        } finally {
            xmlIS.close();
        }
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
