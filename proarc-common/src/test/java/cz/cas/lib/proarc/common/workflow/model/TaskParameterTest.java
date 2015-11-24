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
package cz.cas.lib.proarc.common.workflow.model;

import java.math.BigDecimal;
import java.sql.Timestamp;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class TaskParameterTest {

    @Test
    public void testDateTime() {
        TaskParameter tp = new TaskParameter().addValueDateTime(new Timestamp(System.currentTimeMillis()));
        Timestamp now = tp.getValueDateTime();
        assertEquals(ValueType.DATETIME, tp.getValueType());
        assertEquals(now, tp.getValueDateTime());

        tp = new TaskParameter().addValue(ValueType.DATETIME, "2011-01-13");
        assertEquals(ValueType.DATETIME, tp.getValueType());
        assertEquals("2011-01-13T00:00:00.000Z", tp.getValue());
        assertNotNull(tp.getValueDateTime());
    }

    @Test
    public void testNumber() {
        TaskParameter tp = new TaskParameter().addValue(ValueType.NUMBER, "1");
        // DO NOT use equals for BigDecimal!
        assertEquals(0, new BigDecimal("1.00").compareTo(new BigDecimal("1.0")));
        assertEquals(0, BigDecimal.ONE.compareTo(tp.getValueNumber()));
        assertEquals("1", tp.getValue());

        // test zero in format 0E-9 as it is fetched by Postgres; JDK bug 6480539
        tp = new TaskParameter().addValue(ValueType.NUMBER, "0E-9");
        assertEquals(0, BigDecimal.ZERO.compareTo(tp.getValueNumber()));
        assertEquals("0", tp.getValue());
}

    @Test
    public void testBooleanTrue() {
        TaskParameter tp = new TaskParameter().addValue(ValueType.NUMBER, "true");
        // DO NOT use equals for BigDecimal!
        assertEquals(0, BigDecimal.ONE.compareTo(tp.getValueNumber()));
        assertEquals("1", tp.getValue());
    }

    @Test
    public void testBooleanFalse() {
        TaskParameter tp = new TaskParameter().addValue(ValueType.NUMBER, "false");
        // DO NOT use equals for BigDecimal!
        assertEquals(0, BigDecimal.ZERO.compareTo(tp.getValueNumber()));
        assertEquals("0", tp.getValue());
    }

}
