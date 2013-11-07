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

import cz.cas.lib.proarc.common.mods.custom.ClassificationMapper.ClassificationItem;
import cz.cas.lib.proarc.common.mods.custom.ClassificationMapper.ClassificationItem.Type;
import cz.cas.lib.proarc.common.mods.custom.ClassificationMapper.ClassificationPair;
import cz.fi.muni.xkremser.editor.server.mods.ClassificationType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import java.util.Arrays;
import java.util.List;
import org.hamcrest.core.Is;
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
public class ClassificationMapperTest {

    public ClassificationMapperTest() {
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
    public void testReadItems() {
        ModsType mods = new ModsType();
        mods.getModsGroup().add(classificationType(Type.DDC.getText(), "ddc[0]"));
        mods.getModsGroup().add(classificationType(Type.DDC.getText(), "ddc[1]"));
        mods.getModsGroup().add(classificationType(Type.UDC.getText(), "udc[2]"));
        mods.getModsGroup().add(classificationType(null, "unknown[3]"));
        mods.getModsGroup().add(classificationType(Type.UDC.getText(), "udc[4]"));
        ClassificationMapper instance = new ClassificationMapper();
        List<ClassificationItem> expResult = Arrays.asList(
            new ClassificationItem(0, Type.DDC, "ddc[0]"),
            new ClassificationItem(1, Type.DDC, "ddc[1]"),
            new ClassificationItem(2, Type.UDC, "udc[2]"),
            new ClassificationItem(3, Type.OTHER, null),
            new ClassificationItem(4, Type.UDC, "udc[4]")
        );
        List<ClassificationItem> result = instance.map(mods);
        assertThat(result, Is.is(expResult));
    }

    @Test
    public void testReadPairs() {
        ModsType mods = new ModsType();
        mods.getModsGroup().add(classificationType(Type.DDC.getText(), "ddc[0]"));
        mods.getModsGroup().add(classificationType(Type.DDC.getText(), "ddc[1]"));
        mods.getModsGroup().add(classificationType(Type.UDC.getText(), "udc[2]"));
        mods.getModsGroup().add(classificationType(null, "unknown[3]"));
        mods.getModsGroup().add(classificationType(Type.UDC.getText(), "udc[4]"));
        ClassificationMapper instance = new ClassificationMapper();
        List<ClassificationPair> expResult = Arrays.asList(
            new ClassificationPair("ddc[0]", 0, null, null),
            new ClassificationPair("ddc[1]", 1, "udc[2]", 2),
            new ClassificationPair(null, null, "udc[4]", 4)
        );
        List<ClassificationPair> result = instance.mapPairs(mods);
        assertThat(result, Is.is(expResult));
    }

    @Test
    public void testWritePairs() {
        ModsType mods = new ModsType();
        mods.getModsGroup().add(classificationType(Type.DDC.getText(), "ddc[0]"));
        mods.getModsGroup().add(classificationType(Type.DDC.getText(), "ddc[1]"));
        mods.getModsGroup().add(classificationType(Type.UDC.getText(), "udc[2]"));
        mods.getModsGroup().add(classificationType(null, "unknown[3]"));
        mods.getModsGroup().add(classificationType(Type.UDC.getText(), "udc[4]"));
        List<ClassificationPair> updates = Arrays.asList(
            new ClassificationPair("ddc[0]-updated", 0, "udc[new][1]", null), // update + insert
//            new ClassificationPair("ddc[1]-removed", 1, "udc[2]", 2), // delete
            new ClassificationPair(null, null, "udc[4]", 4), // nothing
            new ClassificationPair("ddc[new][5]", null, "udc[new][6]", null) // insert
        );
        ClassificationMapper instance = new ClassificationMapper();
        instance.mapPairs(mods, updates);
        List<ClassificationItem> result = instance.map(mods);
        List<ClassificationItem> expected = Arrays.asList(
                new ClassificationItem(0, Type.DDC, "ddc[0]-updated"),
                new ClassificationItem(1, Type.UDC, "udc[new][1]"),
                new ClassificationItem(2, Type.DDC, null),
                new ClassificationItem(3, Type.UDC, "udc[4]"),
                new ClassificationItem(4, Type.DDC, "ddc[new][5]"),
                new ClassificationItem(5, Type.UDC, "udc[new][6]"),
                new ClassificationItem(6, Type.OTHER, null)
                );
        assertThat(result, Is.is(expected));
    }

    private static ClassificationType classificationType(String type, String value) {
        ClassificationType c = new ClassificationType();
        c.setAuthority(type);
        c.setValue(value);
        return c;
    }

}
