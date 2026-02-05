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
import cz.cas.lib.proarc.common.mods.custom.MonographUnitMapper.MonographUnit;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.Arrays;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 *
 * @author Jan Pokorsky
 */
public class MonographUnitMapperTest {

    @Test
    public void testRead() {
        ModsDefinition mods = ModsUtils.unmarshal(MonographUnitMapperTest.class.getResource("monograph_unit_mods.xml"), ModsDefinition.class);
        MonographUnitMapper instance = new MonographUnitMapper();
        MonographUnit result = instance.map(mods);
        assertEquals("Zbirka 1", result.getNumber());
        assertNull(result.getNote());
    }

    @Test
    public void testWrite() {
        ModsDefinition mods = new ModsDefinition();
        MonographUnit munit = new MonographUnit();
        munit.setIdentifiers(Arrays.asList(new IdentifierItem("type", "IDENTIFIER")));
        munit.setNumber("NUMBER");
        munit.setNote("NOTE");

        MonographUnitMapper instance = new MonographUnitMapper();
        instance.map(mods, munit);
        String toXml = ModsUtils.toXml(mods, true);
        System.out.println(toXml);
        MonographUnit result = instance.map(ModsUtils.unmarshal(toXml, ModsDefinition.class));
        assertNotNull(result);
        assertEquals(Arrays.asList(new IdentifierItem(0, "type", "IDENTIFIER")), result.getIdentifiers());
        assertEquals("NUMBER", result.getNumber());
        assertEquals("NOTE", result.getNote());

    }
}
