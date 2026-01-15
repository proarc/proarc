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
import java.nio.charset.StandardCharsets;
import org.apache.commons.configuration2.BaseConfiguration;
import org.apache.commons.io.IOUtils;
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
public class PhysicalMaterialBuilderTest {

    public PhysicalMaterialBuilderTest() {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testBuild() throws Exception {
        CatalogConfiguration c = new CatalogConfiguration("testCatalogId", "", new BaseConfiguration() {{
            addProperty(CatalogConfiguration.PROPERTY_URL, "tcp://localhost:9991");
            addProperty(CatalogConfiguration.PROPERTY_NAME, "test");
            addProperty(CatalogConfiguration.PROPERTY_TYPE, Z3950Catalog.TYPE);
        }});
        String xml = IOUtils.toString(WorkflowManagerTest.class.getResource("rdczmods.xml"), StandardCharsets.UTF_8);
        PhysicalMaterial pm = new PhysicalMaterialBuilder().build(xml, c);
        assertEquals(xml, pm.getMetadata());
        assertEquals(c.getUrl(), pm.getSource());
        assertEquals("Nová vlna = : La nouvelle vague : Truffaut, Godard, Chabrol, Rohmer, Rivette", pm.getLabel());

    }

}
