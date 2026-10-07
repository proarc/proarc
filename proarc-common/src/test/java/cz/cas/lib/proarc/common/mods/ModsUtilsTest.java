/*
 * Copyright (C) 2026
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
package cz.cas.lib.proarc.common.mods;

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.RoleDefinition;
import cz.cas.lib.proarc.mods.RoleTermDefinition;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Validator;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ModsUtilsTest {

    @Test
    public void testMods38Schema() throws Exception {
        String xml = "<mods xmlns=\"http://www.loc.gov/mods/v3\" version=\"3.8\">"
                + "<originInfo><agent type=\"corporate\"><namePart>Publisher</namePart></agent></originInfo>"
                + "</mods>";
        Validator validator = ModsUtils.getSchema().newValidator();
        validator.validate(new StreamSource(new ByteArrayInputStream(xml.getBytes(StandardCharsets.UTF_8))));
    }

    @Test
    public void testSchemaSelection() throws Exception {
        try (InputStream schema38 = ModsUtils.getSchemaAsStream("3.8");
                InputStream schema37 = ModsUtils.getSchemaAsStream("3.7")) {
            assertArrayEquals(schema38.readAllBytes(), schema37.readAllBytes());
        }
        assertThrows(IllegalArgumentException.class, () -> ModsUtils.getSchemaAsStream("3.9"));
    }

    @Test
    public void testAgentRoleTermByEventType() {
        assertEquals("publisher", ModsUtils.getAgentRoleTerm(null));
        assertEquals("publisher", ModsUtils.getAgentRoleTerm(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION));
        assertEquals("producer", ModsUtils.getAgentRoleTerm(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION));
        assertEquals("distributor", ModsUtils.getAgentRoleTerm(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION));
        assertEquals("manufacturer", ModsUtils.getAgentRoleTerm(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE));
        assertEquals("custom", ModsUtils.getAgentRoleTerm("custom"));
    }

    @Test
    public void testSetAgentRole() {
        NameDefinition agent = new NameDefinition();

        ModsUtils.setAgentRole(agent, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION);
        assertEquals("producer", agent.getRole().get(0).getRoleTerm().get(0).getValue());

        ModsUtils.setAgentRole(agent, null);
        assertEquals(1, agent.getRole().size());
        assertEquals(1, agent.getRole().get(0).getRoleTerm().size());
        assertEquals("publisher", agent.getRole().get(0).getRoleTerm().get(0).getValue());
    }

    @Test
    public void testSetAgentRolePreservesCodedRoleTerm() {
        NameDefinition agent = new NameDefinition();
        RoleDefinition role = new RoleDefinition();
        RoleTermDefinition codedRoleTerm = new RoleTermDefinition();
        codedRoleTerm.setType(CodeOrText.CODE);
        codedRoleTerm.setValue("pbl");
        role.getRoleTerm().add(codedRoleTerm);
        agent.getRole().add(role);

        ModsUtils.setAgentRole(agent, null);

        assertEquals(2, role.getRoleTerm().size());
        assertEquals("pbl", role.getRoleTerm().get(0).getValue());
        assertEquals("publisher", role.getRoleTerm().get(1).getValue());
    }

}
