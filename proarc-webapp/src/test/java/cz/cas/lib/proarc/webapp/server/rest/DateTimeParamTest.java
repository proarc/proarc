/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.server.rest;

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
public class DateTimeParamTest {

    public DateTimeParamTest() {
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
    public void testFullDateTime() {
        DateTimeParam param = new DateTimeParam("2012-10-30T00:00:00.000+0000");
        assertNotNull(param.toDate());
        assertNotNull(param.toTimestamp());
    }

    @Test
    public void testDateTime() {
        DateTimeParam param = new DateTimeParam("20130725T22:00:00");
        assertNotNull(param.toDate());
        assertNotNull(param.toTimestamp());
    }

    @Test
    public void testDateTimeWithDashes() {
        DateTimeParam param = new DateTimeParam("2013-07-25T22:00:00");
        assertNotNull(param.toDate());
        assertNotNull(param.toTimestamp());
    }

    @Test
    public void testDate() {
        DateTimeParam param = new DateTimeParam("2013-07-25");
        assertNotNull(param.toDate());
        assertNotNull(param.toTimestamp());
    }

    @Test(expected = RestException.class)
    public void testTime() {
        DateTimeParam param = new DateTimeParam("22:00:00");
        assertNotNull(param.toDate());
        assertNotNull(param.toTimestamp());
    }

}