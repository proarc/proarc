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
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 * @author Jan Pokorsky
 */
public class ClassificationMapperTest {

    public ClassificationMapperTest() {
    }

    @BeforeAll
    public static void setUpClass() throws Exception {
    }

    @AfterAll
    public static void tearDownClass() throws Exception {
    }

    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testReadItems() {
        ModsDefinition mods = new ModsDefinition();
        mods.getClassification().add(classificationType(Type.DDC.getText(), "ddc[0]"));
        mods.getClassification().add(classificationType(Type.DDC.getText(), "ddc[1]"));
        mods.getClassification().add(classificationType(Type.UDC.getText(), "udc[2]"));
        mods.getClassification().add(classificationType(null, "unknown[3]"));
        mods.getClassification().add(classificationType(Type.UDC.getText(), "udc[4]"));
        ClassificationMapper instance = new ClassificationMapper();
        List<ClassificationItem> expResult = Arrays.asList(
                new ClassificationItem(0, Type.DDC, "ddc[0]"),
                new ClassificationItem(1, Type.DDC, "ddc[1]"),
                new ClassificationItem(2, Type.UDC, "udc[2]"),
                new ClassificationItem(3, Type.OTHER, null),
                new ClassificationItem(4, Type.UDC, "udc[4]")
        );
        List<ClassificationItem> result = instance.map(mods);
        assertEquals(result, expResult);
    }

    @Test
    public void testReadPairs() {
        ModsDefinition mods = new ModsDefinition();
        mods.getClassification().add(classificationType(Type.DDC.getText(), "ddc[0]"));
        mods.getClassification().add(classificationType(Type.DDC.getText(), "ddc[1]"));
        mods.getClassification().add(classificationType(Type.UDC.getText(), "udc[2]"));
        mods.getClassification().add(classificationType(null, "unknown[3]"));
        mods.getClassification().add(classificationType(Type.UDC.getText(), "udc[4]"));
        ClassificationMapper instance = new ClassificationMapper();
        List<ClassificationPair> expResult = Arrays.asList(
                new ClassificationPair("ddc[0]", 0, null, null),
                new ClassificationPair("ddc[1]", 1, "udc[2]", 2),
                new ClassificationPair(null, null, "udc[4]", 4)
        );
        List<ClassificationPair> result = instance.mapPairs(mods);
        assertEquals(result, expResult);
    }

    @Test
    public void testWritePairs() {
        ModsDefinition mods = new ModsDefinition();
        mods.getClassification().add(classificationType(Type.DDC.getText(), "ddc[0]"));
        mods.getClassification().add(classificationType(Type.DDC.getText(), "ddc[1]"));
        mods.getClassification().add(classificationType(Type.UDC.getText(), "udc[2]"));
        mods.getClassification().add(classificationType(null, "unknown[3]"));
        mods.getClassification().add(classificationType(Type.UDC.getText(), "udc[4]"));
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
        assertEquals(result, expected);
    }

    private static ClassificationDefinition classificationType(String type, String value) {
        ClassificationDefinition c = new ClassificationDefinition();
        c.setAuthority(type);
        c.setValue(value);
        return c;
    }

}
