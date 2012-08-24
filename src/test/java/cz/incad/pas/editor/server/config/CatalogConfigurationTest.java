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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.config;

import cz.incad.pas.editor.server.catalog.BibliographicCatalog;
import cz.incad.pas.editor.server.config.CatalogConfiguration.CatalogProperties;
import java.util.List;
import org.apache.commons.configuration.BaseConfiguration;
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
public class CatalogConfigurationTest {

    private BaseConfiguration conf;
    private String catalog1ExtraOption;

    public CatalogConfigurationTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        conf = new BaseConfiguration();
        // catalog1
        String prefix = CatalogConfiguration.CATALOG_PREFIX + '.' + "catalog1";
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_NAME, "catalog1Name");
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_URL, "catalog1URL");
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_TYPE, "catalog1Type");
        catalog1ExtraOption = prefix + '.' + "extraOption";
        conf.addProperty(catalog1ExtraOption, "catalog1ExtraOption");
        // Invalid catalog
        prefix = CatalogConfiguration.CATALOG_PREFIX + '.' + "catalogInvalid";
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_NAME, "catalogInvalidName");
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_TYPE, "catalogInvalidType");
        // Not listed catalog
        prefix = CatalogConfiguration.CATALOG_PREFIX + '.' + "catalogNotListed";
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_NAME, "catalogNotListedName");
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_URL, "catalogNotListedURL");
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_TYPE, "catalogNotListedType");
        // catalog2
        prefix = CatalogConfiguration.CATALOG_PREFIX + '.' + "catalog2";
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_NAME, "catalog2Name");
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_URL, "catalog2URL");
        conf.addProperty(prefix + '.' + CatalogProperties.PROPERTY_TYPE, "catalog2Type");
        // catalogs declaration
        conf.addProperty(CatalogConfiguration.PROPERTY_CATALOGS, "catalog1, catalogInvalid, catalogMissing, catalog2");
        conf.addProperty("dummyProperty", "dummy");
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testGetCatalogs() {
        CatalogConfiguration instance = new CatalogConfiguration(conf);
        List<CatalogProperties> catalogs = instance.getCatalogs();
        assertEquals(2, catalogs.size());
        assertEquals("catalog1", catalogs.get(0).getId());
        assertEquals("catalog1Name", catalogs.get(0).getName());
        assertEquals("catalog1URL", catalogs.get(0).getUrl());
        assertEquals("catalog1Type", catalogs.get(0).getType());
        assertEquals("catalog1ExtraOption", catalogs.get(0).getProperties().get(catalog1ExtraOption));

        assertEquals("catalog2", catalogs.get(1).getId());
        assertEquals("catalog2Name", catalogs.get(1).getName());
        assertEquals("catalog2URL", catalogs.get(1).getUrl());
        assertEquals("catalog2Type", catalogs.get(1).getType());
    }

    @Test
    public void testFindCatalog() {
        CatalogConfiguration instance = new CatalogConfiguration(conf);
        BibliographicCatalog bcatalog = instance.findCatalog("catalog1");
        assertNull(bcatalog);
    }

    @Test
    public void testFindConfig() {
        CatalogConfiguration instance = new CatalogConfiguration(conf);
        CatalogProperties catalog1 = instance.findConfig("catalog1");
        assertEquals("catalog1", catalog1.getId());

        CatalogProperties catalogMissing = instance.findConfig("catalogMissing");
        assertNull(catalogMissing);
    }
}
