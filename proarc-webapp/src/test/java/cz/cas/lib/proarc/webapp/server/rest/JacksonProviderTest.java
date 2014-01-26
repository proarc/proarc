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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.dao.Batch.State;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.dublincore.DcUtils;
import cz.cas.lib.proarc.common.export.desa.DesaServices;
import cz.cas.lib.proarc.common.export.desa.sip2desa.nomen.Nomenclatures;
import cz.cas.lib.proarc.common.export.desa.sip2desa.nomen.Nomenclatures.RecCls;
import cz.cas.lib.proarc.common.export.desa.sip2desa.nomen.Nomenclatures.RecCls.RecCl;
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URL;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.stream.StreamResult;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
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
    public void testDublinCoreRecord() throws Exception {
        ObjectMapper om = new JacksonProvider().locateMapper(DublinCoreRecord.class, MediaType.APPLICATION_JSON_TYPE);
        om.configure(DeserializationConfig.Feature.UNWRAP_ROOT_VALUE, true);
        URL resource = JacksonProviderTest.class.getResource("dc_test.xml");
        assertNotNull(resource);
        OaiDcType dc = JAXB.unmarshal(resource, OaiDcType.class);
        DublinCoreRecord dr = new DublinCoreRecord(dc, System.currentTimeMillis(), "uuid:1");
        String toJson = om.writeValueAsString(dr);
//        System.out.println(toJson);

        DublinCoreRecord ndr = om.readValue(toJson, DublinCoreRecord.class);
        assertEquals(dr.getBatchId(), ndr.getBatchId());
        assertEquals(dr.getPid(), ndr.getPid());
        assertEquals(dr.getTimestamp(), ndr.getTimestamp());
        assertEquals(dr.getDc().getIdentifiers(), ndr.getDc().getIdentifiers());
        assertEquals(dr.getDc().getTitles(), ndr.getDc().getTitles());
        assertEquals(dr.getDc().getCreators(), ndr.getDc().getCreators());

        StringWriter toXml = new StringWriter();
        DcUtils.marshal(new StreamResult(toXml), ndr.getDc(), true);
        toXml.flush();
//        System.out.println("---");
//        System.out.println(toXml);
}

    @Test
    public void testNomenclatureAsValueMap() throws Exception {
        Nomenclatures n = new Nomenclatures();
        n.setRecCls(new RecCls());
        List<RecCl> recCls = n.getRecCls().getRecCl();
        RecCl recCl = new RecCl();
        recCl.setFullyQcc("FullyQcc");
        recCls.add(recCl);
        DesaServices desaServices = new DesaServices(null);
        List<ValueMap> valueMap = desaServices.getValueMap(n, "test");
        SmartGwtResponse<ValueMap> sgr = new SmartGwtResponse<ValueMap>(valueMap);

        ObjectMapper om = new JacksonProvider().locateMapper(SmartGwtResponse.class, MediaType.APPLICATION_JSON_TYPE);
        om.configure(DeserializationConfig.Feature.UNWRAP_ROOT_VALUE, true);
        String toJson = om.writeValueAsString(sgr);
        System.out.println(toJson);
        assertTrue(toJson.contains("\"data\":[{\"mapId\":\"test.rec-cl\",\"values\":[{\"fullyQcc\":\"FullyQcc\"}]}]}"));
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

    @Test
    public void testMixedInAnnotations() throws Exception {
        BatchView b = new BatchView();
        b.setCreate(Timestamp.valueOf("1991-01-19 00:00:01"));
        b.setFolder("folder/");
        b.setId(1);
        b.setParentPid("parenPid");
        b.setState(State.LOADED.name());
        b.setTimestamp(new Timestamp(System.currentTimeMillis()));
        b.setTitle("title");
        b.setUserId(10);
        b.setUsername("username");
        String json = toJson(b);
        assertTrue(json, json.contains("\"description\":\"title\""));
        assertTrue(json, json.contains("\"create\":\"1991-01"));
        assertFalse(json, json.contains("\"folder\":"));
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
