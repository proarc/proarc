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
package cz.cas.lib.proarc.common.mods.custom;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.cas.lib.proarc.common.mods.custom.PageMapper.Page;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.custommonkey.xmlunit.XMLAssert;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 *
 * @author Jan Pokorsky
 */
public class PageMapperTest {

    public PageMapperTest() {
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
    public void testRead() {
        ModsDefinition mods = ModsUtils.unmarshal(PageMapperTest.class.getResource("page_mods.xml"), ModsDefinition.class);

        PageMapper instance = new PageMapper();
        Page result = instance.map(mods);
        assertNotNull(result);
        assertEquals("1", result.getIndex());
        assertEquals("[1]", result.getNumber());
        assertEquals("Blank", result.getType());
        assertEquals("note", result.getNote());
        List<IdentifierItem> identifiers = result.getIdentifiers();
        assertEquals(2, identifiers.size());
        assertEquals("issn", identifiers.get(0).getType());
        assertEquals("issn value", identifiers.get(0).getValue());
        assertEquals("uuid", identifiers.get(1).getType());
        assertEquals("1", identifiers.get(1).getValue());
    }

    @Test
    public void testReadEmptyDocument() throws Exception {
        ModsDefinition root = new ModsDefinition();

        PageMapper instance = new PageMapper();
        Page result = instance.map(root);
        assertNotNull(result);
        assertEquals(null, result.getIndex());
        assertEquals(null, result.getNumber());
        assertEquals(null, result.getType());
        assertEquals(null, result.getNote());
        assertEquals(Collections.emptyList(), result.getIdentifiers());
    }

    @Test
    public void testWriteUpdate() throws Exception {
        ModsDefinition mods = ModsUtils.unmarshal(PageMapperTest.class.getResource("page_mods.xml"), ModsDefinition.class);
        Page page = new Page();
        page.setNote("note updated");
        page.setIndex("2");
        page.setNumber("[2]");
        page.setType("TitlePage");
        page.setIdentifiers(Arrays.asList(
                new IdentifierItem(1, "uuid", "1 updated"),
                new IdentifierItem(0, "issn", "issn value updated"),
                new IdentifierItem("isbn", "isbn value inserted")));

        PageMapper instance = new PageMapper();
        ModsDefinition result = instance.map(mods, page);
        String resultXml = ModsUtils.toXml(result, true);
//        System.out.println(resultXml);

        ModsDefinition expectedMods = ModsUtils.unmarshal(PageMapperTest.class.getResource("page_mods_updated.xml"), ModsDefinition.class);
        String expectedXml = ModsUtils.toXml(expectedMods, true);
//        System.out.println(expectedXml);
        XMLAssert.assertXMLEqual(expectedXml, resultXml);
    }

    @Test
    public void testWriteRead() throws Exception {
        Page page = new Page();
        page.setIndex("1");
        page.setNumber("[1]");
        page.setType("normalPage");
        page.setNote("note");
        page.setIdentifiers(Arrays.asList(new IdentifierItem(null, "uuid", "1")));

        PageMapper handler = new PageMapper();
        ModsDefinition mods = new ModsDefinition();
        handler.map(mods, page);

        String dump = ModsUtils.toXml(mods, true);

        ModsDefinition resultMods = ModsUtils.unmarshal(dump, ModsDefinition.class);
        PageMapper resultHandler = new PageMapper();
        Page result = resultHandler.map(resultMods);
        assertNotNull(result);
        assertEquals(page.getIndex(), result.getIndex());
        assertEquals(page.getNumber(), result.getNumber());
        assertEquals(page.getType(), result.getType());
        assertEquals(page.getNote(), result.getNote());
        assertEquals(Arrays.asList(new IdentifierItem(0, "uuid", "1")), result.getIdentifiers());
    }
}
