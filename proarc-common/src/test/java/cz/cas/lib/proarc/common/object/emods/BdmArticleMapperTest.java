/*
 * Copyright (C) 2017 Lukas Sykora
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
package cz.cas.lib.proarc.common.object.emods;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import java.io.StringReader;
import java.util.List;
import javax.xml.transform.stream.StreamSource;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Lukas Sykora
 */
public class BdmArticleMapperTest {

    public BdmArticleMapperTest() {
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
    public void testCreateMods() {
        String xml = "<mods version='3.5' xmlns='http://www.loc.gov/mods/v3'>"
                + "<physicalDescription> <form>bez média</form> </physicalDescription>"
                + "<physicalDescription> <form>bez média</form> </physicalDescription>"
                + "<physicalDescription> <form>počítač</form> </physicalDescription>"
                + "<physicalDescription> <form>jiný</form> </physicalDescription>"
                + "<physicalDescription> <form>jiný</form> </physicalDescription>"
                + "<physicalDescription> <form>audio</form> </physicalDescription>"
                + "</mods>";
        ModsDefinition mods = ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xml)));
        BdmArticleMapper mapper = new BdmArticleMapper();
        Context ctx = new Context("uuid:testId");
        mapper.createMods(mods, ctx);

        int rdamediaSum = 0; // sum of all PhysicalDescription element, where authority is "rdamedia"
        int rdacarrierSum = 0; // sum of all PhysicalDescription element, where authority is "rdacarrier"
        int otherSum = 0; // sum of all PhysicalDescription element, where authority differes from "rdamedia" and "rdacarrier"
        for (PhysicalDescriptionDefinition pd : mods.getPhysicalDescription()) {
            String authority = pd.getForm().get(0).getAuthority();
            if ("rdamedia".equals(authority)) {
                rdamediaSum++;
            } else if ("rdacarrier".equals(authority)) {
                rdacarrierSum++;
            } else {
                otherSum++;
            }
        }
        assertEquals(9, mods.getPhysicalDescription().size()); // sum of all PhysicalDescription element
        assertEquals(5, rdamediaSum);
        assertEquals(3, rdacarrierSum);
        assertEquals(1, otherSum);
        assertEquals(mods.getPhysicalDescription().size(), rdamediaSum + rdacarrierSum + otherSum); // sum of all PhysicalDescription element
    }
}
