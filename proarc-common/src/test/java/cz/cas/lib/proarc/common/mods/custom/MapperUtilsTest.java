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

import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 * @author Jan Pokorsky
 */
public class MapperUtilsTest {

    public MapperUtilsTest() {
    }

    @Test
    public void testRemove() {
        TitleInfoDefinition obj1 = new TitleInfoDefinition();
        TitleInfoDefinition obj2 = new TitleInfoDefinition();
        IdentifierDefinition obj3 = new IdentifierDefinition();
        TitleInfoDefinition obj4 = new TitleInfoDefinition();
        IdentifierDefinition obj5 = new IdentifierDefinition();
        List<Object> list = new ArrayList<Object>(Arrays.asList(obj1, obj2, obj3, obj4, obj5));
        List<Object> expected = Arrays.<Object>asList(obj1, obj2, obj4);

        Class type = IdentifierDefinition.class;
        MapperUtils.remove(list, type);
        assertEquals(expected, list);
    }

}
