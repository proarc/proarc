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
import cz.cas.lib.proarc.common.mods.custom.PeriodicalVolumeMapper.PeriodicalVolume;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import java.util.Arrays;
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
public class PeriodicalVolumeMapperTest {

    public PeriodicalVolumeMapperTest() {
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
    public void testRead() throws Exception {
        ModsType mods = ModsUtils.unmarshal(PeriodicalVolumeMapperTest.class.getResource("volume_mods.xml"), ModsType.class);
        PeriodicalVolumeMapper instance = new PeriodicalVolumeMapper();
        PeriodicalVolume result = instance.map(mods);
        assertNotNull(result);
        assertEquals("1", result.getVolumeNumber());
        assertEquals("1893", result.getYear());
        assertEquals("nekvalitní a poškozená předloha;", result.getNote());
    }

    @Test
    public void testWriteRead() throws Exception {
        ModsType mods = new ModsType();
        PeriodicalVolume value = new PeriodicalVolume();
        value.setVolumeNumber("1");
        value.setYear("1893");
        value.setNote("note");
        value.setIdentifiers(Arrays.asList(new IdentifierItem(null, "uuid", "1")));

        PeriodicalVolumeMapper instance = new PeriodicalVolumeMapper();
        instance.map(mods, value);
        String dump = ModsUtils.toXml(mods, true);
//        System.out.println(dump);

        PeriodicalVolume result = instance.map(ModsUtils.unmarshal(dump, ModsType.class));
        assertNotNull(result);
        assertEquals(value.getVolumeNumber(), result.getVolumeNumber());
        assertEquals(value.getYear(), result.getYear());
        assertEquals(value.getNote(), result.getNote());
        assertEquals(Arrays.asList(new IdentifierItem(0, "uuid", "1")), result.getIdentifiers());
    }
}
