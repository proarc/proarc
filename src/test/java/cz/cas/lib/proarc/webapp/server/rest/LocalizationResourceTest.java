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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.webapp.server.rest.LocalizationResource.Item;
import cz.incad.pas.editor.shared.rest.LocalizationResourceApi.BundleName;
import java.util.EnumSet;
import java.util.Set;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class LocalizationResourceTest {

    public LocalizationResourceTest() {
    }

    @Test
    public void testGetBundle() {
        Set<BundleName> bundles = EnumSet.of(BundleName.MODS_PAGE_TYPES);
        String locale = "cs";
        boolean sorted = true;
        LocalizationResource instance = new LocalizationResource(null);
        SmartGwtResponse<Item> result = instance.getBundle(bundles, locale, sorted);
        assertNotNull(result);
        assertFalse(result.getData().isEmpty());
        System.out.println(result.getData());
    }
}
