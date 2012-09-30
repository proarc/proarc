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
package cz.incad.pas.editor.server.rest;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Test {@link JacksonProvider} configuration to comply with SmartGWT expectations.
 *
 * @author Jan Pokorsky
 */
public class JacksonProviderTest {

    public JacksonProviderTest() {
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
    public void testWrapRootValue() throws IOException {
        Record obj = new Record("value", null);
        String json = toJson(obj);
        assertTrue(json, json.startsWith("{\"record\":{"));
    }

    @Test
    public void testMapNonNullValues() throws IOException {
        Record obj = new Record("value", null);
        String json = toJson(obj);
        assertTrue(json, json.contains("\"field\":\"value\""));
        assertFalse(json, json.contains("nullField"));
    }

    @Test
    public void testDateAsDateNotTimestamp() throws IOException {
        GregorianCalendar date = new GregorianCalendar(2012, 9, 30);
        date.setTimeZone(TimeZone.getTimeZone("GMT"));
        Record obj = new Record("value", null, date.getTime());
        String json = toJson(obj);
        assertTrue(json, json.contains("\"dateField\":\"2012-10-30T00:00:00.000+0000\""));
    }

    @Test
    public void testArray() throws IOException {
        ArrayHolder obj = new ArrayHolder(Arrays.asList(
                new Record("value[0]", null),
                new Record("value[1]", null)
                ));
        String json = toJson(obj);
        assertTrue(json, json.startsWith("{\"ArrayHolder\":{\"records\":[{\"field\":\"value[0]\""));
    }

    /**
     * Default Jersey provider skips square brackets in case of single item array.
     * Jackson fixes it in default configuration.
     */
    @Test
    public void testSingleItemArray() throws IOException {
        ArrayHolder obj = new ArrayHolder(Arrays.asList(
                new Record("value[0]", null)
                ));
        String json = toJson(obj);
        assertTrue(json, json.startsWith("{\"ArrayHolder\":{\"records\":[{\"field\":\"value[0]\""));
    }

    private static String toJson(Object obj) throws IOException {
        JacksonProvider jp = new JacksonProvider();
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        jp.writeTo(obj, obj.getClass(), null, null, MediaType.APPLICATION_JSON_TYPE, null, out);
        String json = out.toString("UTF-8");
        return json;
    }

    @XmlRootElement(name = "record")
    @XmlAccessorType(XmlAccessType.FIELD)
    static class Record {

        private String field;

        private String nullField;

        private Date dateField;

        public Record(String field, String nullField) {
            this(field, nullField, new Date());
        }

        public Record(String field, String nullField, Date dateField) {
            this.field = field;
            this.nullField = nullField;
            this.dateField = dateField;
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    static class ArrayHolder {

        private List<Record> records;
        
        public ArrayHolder(List<Record> records) {
            this.records = records;
        }
    }
}
