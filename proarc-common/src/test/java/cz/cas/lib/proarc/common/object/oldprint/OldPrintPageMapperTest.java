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
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 *
 * @author Jan Pokorsky
 */
public class OldPrintPageMapperTest {

    public OldPrintPageMapperTest() {
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
    public void testToJsonObject() {
        OldPrintPageMapper mapper = new OldPrintPageMapper();
        ModsDefinition mods = ModsUtils.unmarshal(PageMapperTest.class.getResource("page_mods.xml"), ModsDefinition.class);
//        Page page = mapper.toJsonObject(mods, null);
        assertNotNull(mods);
        assertEquals("1", mapper.getIndex(mods));
        assertEquals("[1]", mapper.getNumber(mods));
        assertEquals("Blank", mapper.getType(mods));
        assertEquals("note", mapper.getPhysicalDescription(mods));
        List<IdentifierDefinition> identifiers = mapper.getIdentifiers(mods);
        assertEquals(2, identifiers.size());
        assertEquals("issn", identifiers.get(0).getTypeString());
        assertEquals("issn value", identifiers.get(0).getValue());
        assertEquals("uuid", identifiers.get(1).getTypeString());
        assertEquals("1", identifiers.get(1).getValue());
    }

    @Test
    public void testToJsonObjectJaxb() {
        OldPrintPageMapper mapper = new OldPrintPageMapper();
        ModsDefinition mods = ModsUtils.unmarshal(PageMapperTest.class.getResource("page_mods.xml"), ModsDefinition.class);
//        Page page = mapper.toJsonObject(mods, null);
        assertNotNull(mods);
        assertEquals("1", mapper.getIndex(mods));
        assertEquals("[1]", mapper.getNumber(mods));
        assertEquals("Blank", mapper.getType(mods));
        assertEquals("note", mapper.getPhysicalDescription(mods));
        List<IdentifierDefinition> identifiers = mapper.getIdentifiers(mods);
        assertEquals(2, identifiers.size());
        assertEquals("issn", identifiers.get(0).getTypeString());
        assertEquals("issn value", identifiers.get(0).getValue());
        assertEquals("uuid", identifiers.get(1).getTypeString());
        assertEquals("1", identifiers.get(1).getValue());
    }

    @Test
    public void testFromJsonObject() throws Exception {
        OldPrintPageMapper mapper = new OldPrintPageMapper();
        Context ctx = new Context("uuid:1");
        Page page = new Page();
        page.setIndex("1");
        page.setNumber("[1]");
        page.setType("normalPage");
        page.setPhysicalDescription("note");
        page.setIdentifiers(Arrays.asList(new IdentifierItem(null, "uuid", "1")));

        ModsDefinition mods = mapper.toMods(page, ctx);
        assertNotNull(mods);
        TypeOfResourceDefinition resultType = mods.getTypeOfResource().get(0);
        assertEquals("text", resultType.getValue());
        assertEquals("yes", resultType.getManuscript());

//        Page result = mapper.toJsonObject(mods, ctx);
        assertNotNull(mods);
        assertEquals(page.getIndex(), mapper.getIndex(mods));
        assertEquals(page.getNumber(), mapper.getNumber(mods));
        assertEquals(page.getType(), mapper.getType(mods));
        assertEquals(page.getPhysicalDescription(), mapper.getPhysicalDescription(mods));
        IdentifierDefinition idenfier = new IdentifierDefinition();
        idenfier.setTypeString("uuid");
        idenfier.setValue("1");
        assertEquals(Arrays.asList(idenfier), mapper.getIdentifiers(mods));
    }

    @Test
    public void testGetPageTypeLabel() {
        Locale.setDefault(Locale.ENGLISH);

        assertEquals("Front End Paper", OldPrintPageMapper.getPageTypeLabel("frontEndPaper", Locale.ENGLISH));
        assertEquals("Přední předsádka (Front End Paper)", OldPrintPageMapper.getPageTypeLabel("frontEndPaper", new Locale("cs")));

        Locale.setDefault(new Locale("cs", "CZ"));

        assertEquals("Front End Paper", OldPrintPageMapper.getPageTypeLabel("frontEndPaper", Locale.ENGLISH));
        assertEquals("Přední předsádka (Front End Paper)", OldPrintPageMapper.getPageTypeLabel("frontEndPaper", new Locale("cs")));
    }
}
