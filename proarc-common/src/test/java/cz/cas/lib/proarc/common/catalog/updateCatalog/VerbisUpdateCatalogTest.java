/*
 * Copyright (C) 2026 Lukas Sykora
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

package cz.cas.lib.proarc.common.catalog.updateCatalog;

import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import org.apache.commons.configuration2.BaseConfiguration;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_UPDATE_FIELD;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_UPDATE_SUBFIELD_APP;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_UPDATE_SUBFIELD_DIGITALIZED;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_UPDATE_SUBFIELD_OBJECT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class VerbisUpdateCatalogTest {

    @Test
    public void createUpdateRecordJsonReusesNewSubfields() {
        CatalogConfiguration catalog = createCatalogConfiguration();

        JSONObject json = new JSONObject(VerbisUpdateCatalog.createUpdateRecordJson(catalog, "123456", "uuid:abc"));

        assertEquals("f1:123456", json.getJSONArray("reusing").getJSONObject(0).getString("id"));

        JSONObject fieldValue = json.getJSONArray("value").getJSONObject(0);
        assertTrue(fieldValue.has("reusing"));
        assertEquals("D.856", fieldValue.getJSONArray("reusing").getJSONObject(0).getString("type"));
        assertEquals("D.856.x", fieldValue.getJSONArray("reusing").getJSONObject(0).getJSONObject("havingField").getString("type"));
        assertEquals("proarcId", fieldValue.getJSONArray("reusing").getJSONObject(0).getJSONObject("havingField").getString("value"));

        JSONArray subfields = fieldValue.getJSONObject("set").getJSONArray("value");
        assertEquals(3, subfields.length());
        assertReusedSubfield(subfields.getJSONObject(0), "D.856.x", "proarcId");
        assertReusedSubfield(subfields.getJSONObject(1), "D.856.y", "uuid:abc");
        assertReusedSubfield(subfields.getJSONObject(2), "D.856.d", "zdigitalizováno");
    }

    private static void assertReusedSubfield(JSONObject subfield, String type, String value) {
        assertTrue(subfield.has("reusing"));
        assertEquals(type, subfield.getJSONArray("reusing").getJSONObject(0).getString("type"));
        assertEquals(type, subfield.getJSONObject("set").getString("type"));
        assertEquals(value, subfield.getJSONObject("set").getString("value"));
    }

    private static CatalogConfiguration createCatalogConfiguration() {
        BaseConfiguration properties = new BaseConfiguration();
        properties.addProperty(PROPERTY_UPDATE_FIELD, "856");
        properties.addProperty(PROPERTY_UPDATE_SUBFIELD_APP, "x");
        properties.addProperty(PROPERTY_UPDATE_SUBFIELD_OBJECT, "y");
        properties.addProperty(PROPERTY_UPDATE_SUBFIELD_DIGITALIZED, "d");
        return new CatalogConfiguration("test", "catalog.test", properties);
    }
}
