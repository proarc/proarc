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
package cz.cas.lib.proarc.common.i18n;

import java.util.Locale;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class JsonValueMapTest {

    public JsonValueMapTest() {
    }

    @Test
    public void testFromBundle() {
        BundleName bundle = BundleName.CEJSH_ROLES;
        JsonValueMap result = JsonValueMap.fromBundle(bundle, Locale.ENGLISH);
        assertNotNull(result);
        assertEquals(bundle.getValueMapId(), result.getMapId());
        assertEquals("aut", result.getValues().get(0).get("value"));
    }

    @Test
    public void testFromBundle_InvalidBundle() {
        BundleName bundle = BundleName.MODS_IDENTIFIER_TYPES;
        JsonValueMap result = JsonValueMap.fromBundle(bundle, Locale.ENGLISH);
        assertNotNull(result);
        assertEquals(bundle.getValueMapId(), result.getMapId());
        assertTrue(result.getValues().isEmpty());
    }

}
