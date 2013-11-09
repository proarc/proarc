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

import cz.fi.muni.xkremser.editor.server.mods.IdentifierType;
import cz.fi.muni.xkremser.editor.server.mods.RecordInfoType;
import cz.fi.muni.xkremser.editor.server.mods.TitleInfoType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.hamcrest.core.Is;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class MapperUtilsTest {

    public MapperUtilsTest() {
    }

    @Test
    public void testRemove() {
        TitleInfoType obj1 = new TitleInfoType();
        TitleInfoType obj2 = new TitleInfoType();
        IdentifierType obj3 = new IdentifierType();
        TitleInfoType obj4 = new TitleInfoType();
        IdentifierType obj5 = new IdentifierType();
        List<Object> list = new ArrayList<Object>(Arrays.asList(obj1, obj2, obj3, obj4, obj5));
        List<Object> expected = Arrays.<Object>asList(obj1, obj2, obj4);

        Class type = IdentifierType.class;
        MapperUtils.remove(list, type);
        assertThat(expected, Is.is(list));
    }

    @Test
    public void testUpdate() {
        TitleInfoType obj1 = new TitleInfoType();
        IdentifierType obj2 = new IdentifierType();
        IdentifierType obj3 = new IdentifierType(); // delete
        IdentifierType obj4 = new IdentifierType();
        RecordInfoType obj5 = new RecordInfoType();
        List<Object> list = new ArrayList<Object>(Arrays.asList(obj1, obj2, obj3, obj4, obj5));

        IdentifierType objInsertedBefore4 = new IdentifierType();
        IdentifierType objAddedAfter2 = new IdentifierType();
        List<IdentifierType> updates = Arrays.asList(objInsertedBefore4, obj4, obj2, objAddedAfter2);
        List<Object> expected = Arrays.<Object>asList(obj1, objInsertedBefore4, obj4, obj2, objAddedAfter2, obj5);

        MapperUtils.update(list, updates, IdentifierType.class);
        assertThat(expected, Is.is(list));
    }

}
