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

import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.fi.muni.xkremser.editor.server.mods.IdentifierType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.RecordInfoType;
import cz.fi.muni.xkremser.editor.server.mods.TitleInfoType;
import java.util.Arrays;
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
public class IdentifierMapperTest {

    public IdentifierMapperTest() {
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
    public void testMap_ModsType() {
        ModsType mods = new ModsType();
        List<Object> modsGroup = mods.getModsGroup();
        modsGroup.add(new TitleInfoType());
        modsGroup.addAll(Arrays.asList(
                new TitleInfoType(),
                create("uuid", "1"),
                create("issn", "issn-1"),
                new RecordInfoType(),
                create("issn", "issn-2")
                ));

        IdentifierMapper instance = new IdentifierMapper();
        IdentifierItem[] expResult = {
            new IdentifierItem(0, "uuid", "1"),
            new IdentifierItem(1, "issn", "issn-1"),
            new IdentifierItem(2, "issn", "issn-2")
        };
        List result = instance.map(mods);
        assertArrayEquals(expResult, result.toArray());
    }

    private static IdentifierType create(String type, String value) {
        IdentifierType ident = new IdentifierType();
        ident.setType(type);
        ident.setValue(value);
        return ident;
    }

    @Test
    public void testMap_ModsType_List() {
        ModsType mods = new ModsType();
        List<Object> modsGroup = mods.getModsGroup();
        final TitleInfoType titleInfoType = new TitleInfoType();
        final RecordInfoType recordInfoType = new RecordInfoType();
        modsGroup.addAll(Arrays.asList(
                titleInfoType,
                create("uuid", "1"),
                create("toDelete", "1"),
                create("issn", "issn-1"),
                recordInfoType));

        List<IdentifierItem> updates = Arrays.asList(
                new IdentifierItem(null, "uuid", "insert"),
                new IdentifierItem(2, "isbn", "isbn-1"),
                new IdentifierItem(0, "uuid", "2"),
                new IdentifierItem(null, "uuid", "append")
                );

        IdentifierMapper instance = new IdentifierMapper();
        Object[] expResult = {
            titleInfoType,
            create("uuid", "insert"),
            create("isbn", "isbn-1"),
            create("uuid", "2"),
            create("uuid", "append"),
            recordInfoType,
        };
        instance.map(mods, updates);
        List<Object> result = mods.getModsGroup();

//        System.out.println(Arrays.toString(result.toArray()));
        assertEquals(expResult.length, result.size());
        assertEquals(expResult[0], result.get(0));
        assertIdentifiersEquals(expResult[1], result.get(1));
        assertIdentifiersEquals(expResult[2], result.get(2));
        assertIdentifiersEquals(expResult[3], result.get(3));
        assertIdentifiersEquals(expResult[4], result.get(4));
        assertEquals(expResult[5], result.get(5));
    }

    public static boolean equals(Object i1, Object i2) {
        return (i1 == null) ? i2 != null : i1.equals(i2);
    }

    public static boolean assertIdentifiersEquals(Object i1, Object i2) {
        return assertIdentifiersEquals((IdentifierType) i1, (IdentifierType) i2);
    }
    
    public static boolean assertIdentifiersEquals(IdentifierType i1, IdentifierType i2) {
        if (equals(i1, i2)) {
            return true;
        } else {
            throw new AssertionError(String.format("expected: %s\nresult: %s", toString(i1), toString(i2)));
        }
    }

    private static boolean equals(IdentifierType i1, IdentifierType i2) {
        if (i1 == null) {
            return i2 == null;
        }
        if (!equals(i1.getValue(), i2.getValue())) {
            return false;
        }
        if (!equals(i1.getType(), i2.getType())) {
            return false;
        }
        return true;
    }

    private static String toString(IdentifierType i) {
        return i == null ? null : String.format("IdentifierType[%s, %s]", i.getType(), i.getValue());
    }

}
