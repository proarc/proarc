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
package cz.cas.lib.proarc.common.object.oldprint;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.cas.lib.proarc.common.mods.custom.PageMapperTest;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper.Page;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 *
 * @author Jan Pokorsky
 */
public class OldPrintPageMapperTest {

    public OldPrintPageMapperTest() {
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
    public void testToJsonObject() {
        OldPrintPageMapper mapper = new OldPrintPageMapper();
        ModsDefinition mods = ModsUtils.unmarshal(PageMapperTest.class.getResource("page_mods.xml"), ModsDefinition.class);
        Page page = mapper.toJsonObject(mods, null);
        assertNotNull(page);
        assertEquals("1", page.getIndex());
        assertEquals("[1]", page.getNumber());
        assertEquals("Blank", page.getType());
        assertEquals("note", page.getNote());
        List<IdentifierItem> identifiers = page.getIdentifiers();
        assertEquals(2, identifiers.size());
        assertEquals("issn", identifiers.get(0).getType());
        assertEquals("issn value", identifiers.get(0).getValue());
        assertEquals("uuid", identifiers.get(1).getType());
        assertEquals("1", identifiers.get(1).getValue());
    }

    @Test
    public void testToJsonObjectJaxb() {
        OldPrintPageMapper mapper = new OldPrintPageMapper();
        ModsDefinition mods = ModsUtils.unmarshal(PageMapperTest.class.getResource("page_mods.xml"), ModsDefinition.class);
        Page page = mapper.toJsonObject(mods, null);
        assertNotNull(page);
        assertEquals("1", page.getIndex());
        assertEquals("[1]", page.getNumber());
        assertEquals("Blank", page.getType());
        assertEquals("note", page.getNote());
        List<IdentifierItem> identifiers = page.getIdentifiers();
        assertEquals(2, identifiers.size());
        assertEquals("issn", identifiers.get(0).getType());
        assertEquals("issn value", identifiers.get(0).getValue());
        assertEquals("uuid", identifiers.get(1).getType());
        assertEquals("1", identifiers.get(1).getValue());
    }

    @Test
    public void testFromJsonObject() throws Exception {
        OldPrintPageMapper mapper = new OldPrintPageMapper();
        Context ctx = new Context("uuid:1");
        Page page = new Page();
        page.setIndex("1");
        page.setNumber("[1]");
        page.setType("NormalPage");
        page.setNote("note");
        page.setIdentifiers(Arrays.asList(new IdentifierItem(null, "uuid", "1")));

        ModsDefinition mods = mapper.toMods(page, ctx);
        assertNotNull(mods);
        TypeOfResourceDefinition resultType = mods.getTypeOfResource().get(0);
        assertEquals("text", resultType.getValue());
        assertEquals("yes", resultType.getManuscript());

        Page result = mapper.toJsonObject(mods, ctx);
        assertNotNull(result);
        assertEquals(page.getIndex(), result.getIndex());
        assertEquals(page.getNumber(), result.getNumber());
        assertEquals(page.getType(), result.getType());
        assertEquals(page.getNote(), result.getNote());
        assertEquals(Arrays.asList(new IdentifierItem(null, "uuid", "1")), result.getIdentifiers());
    }

    @Test
    public void testGetPageTypeLabel() {
        Locale.setDefault(Locale.ENGLISH);

        assertEquals("Front Endpaper", OldPrintPageMapper.getPageTypeLabel("FrontEndpaper", Locale.ENGLISH));
        assertEquals("Přední předsádka", OldPrintPageMapper.getPageTypeLabel("FrontEndpaper", new Locale("cs")));

        Locale.setDefault(new Locale("cs", "CZ"));

        assertEquals("Front Endpaper", OldPrintPageMapper.getPageTypeLabel("FrontEndpaper", Locale.ENGLISH));
        assertEquals("Přední předsádka", OldPrintPageMapper.getPageTypeLabel("FrontEndpaper", new Locale("cs")));
    }
}
