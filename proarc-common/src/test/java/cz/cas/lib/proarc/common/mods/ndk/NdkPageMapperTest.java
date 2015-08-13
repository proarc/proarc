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
package cz.cas.lib.proarc.common.mods.ndk;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.cas.lib.proarc.common.mods.custom.PageMapperTest;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper.Page;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class NdkPageMapperTest {

    public NdkPageMapperTest() {
    }

    @Test
    public void testToJsonObject() {
        NdkPageMapper mapper = new NdkPageMapper();
        ModsDefinition mods = ModsUtils.unmarshal(PageMapperTest.class.getResource("page_mods.xml"),
                ModsDefinition.class);
        Page result = mapper.toJsonObject(mods, new Context("uuid:test"));
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
    public void testFromJsonObject() throws Exception {
        NdkPageMapper mapper = new NdkPageMapper();
        Context ctx = new Context("uuid:1");
        Page page = new Page();
        page.setIndex("1");
        page.setNumber("[1]");
        page.setType("NormalPage");
        page.setNote("note");
        page.setIdentifiers(Arrays.asList(new IdentifierItem(null, "uuid", "1")));

        ModsDefinition mods = mapper.toMods(page, ctx);
        assertNotNull(mods);
        Page result = mapper.toJsonObject(mods, ctx);
        assertNotNull(result);
        assertEquals(page.getIndex(), result.getIndex());
        assertEquals(page.getNumber(), result.getNumber());
        assertEquals(page.getType(), result.getType());
        assertEquals(page.getNote(), result.getNote());
        assertEquals(Arrays.asList(new IdentifierItem(null, "uuid", "1")), result.getIdentifiers());
    }

    @Test
    public void testCreatePage() throws Exception {
        NdkPageMapper mapper = new NdkPageMapper();
        ModsDefinition mods = mapper.createPage("uuid:test", "1", "[1]", "NormalPage");
        assertNotNull(mods);
        Page result = mapper.toJsonObject(mods, new Context("dummy"));
        assertNotNull(result);
        assertEquals("1", result.getIndex());
        assertEquals("[1]", result.getNumber());
        assertEquals("NormalPage", result.getType());
        assertNull(result.getNote());
        assertEquals(Arrays.asList(new IdentifierItem(null, "uuid", "test")), result.getIdentifiers());
    }

}
