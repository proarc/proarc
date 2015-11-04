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
package cz.cas.lib.proarc.common.workflow;

import cz.cas.lib.proarc.common.catalog.Z3950Catalog;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.workflow.model.PhysicalMaterial;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.io.IOUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class PhysicalMaterialBuilderTest {

    public PhysicalMaterialBuilderTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testBuild() throws Exception {
        CatalogConfiguration c = new CatalogConfiguration("testCatalogId", "", new BaseConfiguration() {{
            addProperty(CatalogConfiguration.PROPERTY_URL, "tcp://localhost:9991");
            addProperty(CatalogConfiguration.PROPERTY_NAME, "test");
            addProperty(CatalogConfiguration.PROPERTY_TYPE, Z3950Catalog.TYPE);
        }});
        String xml = IOUtils.toString(WorkflowManagerTest.class.getResource("rdczmods.xml"));
        PhysicalMaterial pm = new PhysicalMaterialBuilder().build(xml, c);
        assertEquals(xml, pm.getMetadata());
        assertEquals(c.getUrl(), pm.getSource());
        assertEquals("Nová vlna = : La nouvelle vague : Truffaut, Godard, Chabrol, Rohmer, Rivette", pm.getLabel());

    }

}
