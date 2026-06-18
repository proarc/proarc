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

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.cas.lib.proarc.common.mods.custom.PeriodicalIssueMapper.PeriodicalIssue;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.Arrays;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 *
 * @author Jan Pokorsky
 */
public class PeriodicalIssueMapperTest {

    public PeriodicalIssueMapperTest() {
    }

    @BeforeAll
    public static void setUpClass() throws Exception {
    }

    @AfterAll
    public static void tearDownClass() throws Exception {
    }

    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testRead() throws Exception {
        ModsDefinition mods = ModsUtils.unmarshal(PeriodicalIssueMapperTest.class.getResource("issue_mods.xml"), ModsDefinition.class);
        PeriodicalIssueMapper instance = new PeriodicalIssueMapper();
        PeriodicalIssue result = instance.map(mods);
        assertNotNull(result);
        assertEquals("1", result.getIssueNumber());
        assertEquals("2", result.getIssueSortingNumber());
        assertEquals("16.12.1893", result.getIssueDate());
        assertEquals("note", result.getNote());
    }

    @Test
    public void testWriteRead() throws Exception {
        ModsDefinition mods = new ModsDefinition();
        PeriodicalIssue value = new PeriodicalIssue();
        value.setIssueNumber("1");
        value.setIssueSortingNumber("2");
        value.setIssueDate("16.12.1893");
        value.setNote("note");
        value.setIdentifiers(Arrays.asList(new IdentifierMapper.IdentifierItem(null, "uuid", "1")));

        PeriodicalIssueMapper instance = new PeriodicalIssueMapper();
        instance.map(mods, value);

        String dump = ModsUtils.toXml(mods, true);
//        System.out.println(dump);

        PeriodicalIssue result = instance.map(ModsUtils.unmarshal(dump, ModsDefinition.class));
        assertNotNull(result);
        // XXX issue 43: sotingNumber == number
        assertEquals(value.getIssueSortingNumber(), result.getIssueNumber());
        assertEquals(value.getIssueSortingNumber(), result.getIssueSortingNumber());
        assertEquals(value.getIssueDate(), result.getIssueDate());
        assertEquals(value.getNote(), result.getNote());
        assertEquals(Arrays.asList(new IdentifierItem(0, "uuid", "1")), result.getIdentifiers());
    }

}
