/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.urnnbn;

import cz.cas.lib.proarc.mods.ModsDefinition;
import java.io.StringReader;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static jakarta.xml.bind.JAXB.unmarshal;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 * @author Jan Pokorsky
 */
public class ResolverUtilsTest {


    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testGetOriginator() {
        final String xml = "<mods xmlns='http://www.loc.gov/mods/v3'>"
                + "<name type='corporate'><namePart>Corporate</namePart></name>"
                + "<name type='personal' usage='primary'>"
                    + "<namePart type='family'>PrimaryFamily</namePart>"
                    + "<namePart type='given'>PrimaryGiven</namePart>"
                    + "<namePart type='date'>Primary2002</namePart>"
                    + "<namePart type='termsOfAddress'>PrimaryTermsOfAddress</namePart>"
                + "</name>"
                + "<name type='personal'>"
                    + "<namePart>FullName</namePart>"
                    + "<namePart type='given'>Given</namePart>"
                    + "<namePart type='date'>2001</namePart>"
                    + "<namePart type='termsOfAddress'>TermsOfAddress</namePart>"
                    + "<namePart type='family'>Family</namePart>"
                + "</name>"
                + "<name type='personal'>"
                    + "<namePart type='family'>Family2</namePart>"
                    + "<namePart type='given'>Given2</namePart>"
                    + "<namePart type='date'>2002</namePart>"
                    + "<namePart type='termsOfAddress'>TermsOfAddress2</namePart>"
                + "</name>"
                + "</mods>";
        ModsDefinition mods = unmarshal(new StringReader(xml), ModsDefinition.class);
        String originator = ResolverUtils.getOriginator("personal", false, mods);
        assertEquals("FullName, Family, Given; Family2, Given2", originator);

        originator = ResolverUtils.getOriginator("personal", true, mods);
        assertEquals("PrimaryFamily, PrimaryGiven", originator);

        originator = ResolverUtils.getOriginator("corporate", null, mods);
        assertEquals("Corporate", originator);
    }
}