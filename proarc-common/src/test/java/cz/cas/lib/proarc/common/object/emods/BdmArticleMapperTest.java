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
import javax.xml.transform.stream.StreamSource;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 * @author Lukas Sykora
 */
public class BdmArticleMapperTest {

    public BdmArticleMapperTest() {
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

        /*  According to the issue #609, the original xml will be enriched by 3 elements
         *  that are newly generated. For values calculation see issue #609.
         */
        assertEquals(9, mods.getPhysicalDescription().size()); // sum of all PhysicalDescription element
        assertEquals(5, rdamediaSum);
        assertEquals(3, rdacarrierSum);
        assertEquals(1, otherSum);
        assertEquals(mods.getPhysicalDescription().size(), rdamediaSum + rdacarrierSum + otherSum); // sum of all PhysicalDescription element
    }
}
