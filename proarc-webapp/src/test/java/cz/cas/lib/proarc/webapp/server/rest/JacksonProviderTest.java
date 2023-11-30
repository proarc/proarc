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

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.dao.Batch.State;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.dublincore.DcUtils;
import cz.cas.lib.proarc.common.process.export.desa.DesaServices;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.object.DesDesaPlugin.DesMetadataHandler;
import cz.cas.lib.proarc.common.object.DesDesaPlugin.DesObjectWrapper;
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.common.object.emods.BdmArticleMapper.BdmModsWrapper;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler.ModsWrapper;
import cz.cas.lib.proarc.desa.nomenclature.Nomenclatures;
import cz.cas.lib.proarc.desa.nomenclature.Nomenclatures.RecCls;
import cz.cas.lib.proarc.desa.nomenclature.Nomenclatures.RecCls.RecCl;
import cz.cas.lib.proarc.mix.ImageCaptureMetadataType;
import cz.cas.lib.proarc.mix.ImageCaptureMetadataType.ScannerCapture;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixType;
import cz.cas.lib.proarc.mix.MixUtils;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.nsesss2.Dokument;
import cz.cas.lib.proarc.nsesss2.NsesssUtils;
import cz.cas.lib.proarc.nsesss2.Spis;
import cz.cas.lib.proarc.nsesss2.TLogicky;
import cz.cas.lib.proarc.nsesss2.TZpusobVyrizeni;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URL;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;
import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.stream.StreamResult;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
        ObjectMapper om = new JacksonProvider().getContext(DublinCoreRecord.class);
        om.configure(DeserializationFeature.UNWRAP_ROOT_VALUE, true);
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
    public void testMix() throws Exception {
        ObjectMapper om = new JacksonProvider().getContext(MixType.class);
        om.configure(DeserializationFeature.UNWRAP_ROOT_VALUE, true);
        Mix mix = new Mix();
        ScannerCapture scanner = new ScannerCapture();
        scanner.setScannerManufacturer(MixUtils.stringType("ScannerManufacturer"));
        ImageCaptureMetadataType imageCaptureMetadata = new ImageCaptureMetadataType();
        imageCaptureMetadata.setScannerCapture(scanner);
        mix.setImageCaptureMetadata(imageCaptureMetadata);
        String toJson = om.writeValueAsString(mix);
//        System.out.println(toJson);

        Mix result = om.readValue(toJson, Mix.class);
        assertNotNull(result);
        assertEquals("ScannerManufacturer", result.getImageCaptureMetadata().getScannerCapture().getScannerManufacturer().getValue());
//        System.out.println(MixUtils.toXml(result, true));
    }

    @Test
    public void testBdmModsDefinition() throws Exception {
        String toJson = "{\"BdmModsWrapper\":{\n" +
"    \"mods\":{\n" +
"        \"identifier\":{\n" +
"            \"value\":\"8232b78c-345b-40ae-9dc0-17bbb101d304\", \n" +
"            \"type\":\"uuid\"\n" +
"        }, \n" +
"        \"version\":\"3.5\", \n" +
"        \"relatedItem\":{\n" +
"            \"part\":{\n" +
"                \"extent\":{\n" +
"                    \"start\":{\n" +
"                        \"value\":\"1\"\n" +
"                    }, \n" +
"                    \"end\":{\n" +
"                        \"value\":\"2\"\n" +
"                    }\n" +
"                }\n" +
"            }\n" +
"        }\n" +
"    }, \n" +
"    \"reviewed\":true\n" +
"}}";
        ObjectMapper om = new JacksonProvider().getContext(BdmModsWrapper.class);
        om.configure(DeserializationFeature.UNWRAP_ROOT_VALUE, true);
//        System.out.println(toJson);

        BdmModsWrapper result = om.readValue(toJson, BdmModsWrapper.class);
        assertNotNull(result);
        ModsDefinition resultMods = result.getMods();
        assertNotNull(resultMods);
        String toXml = ModsUtils.toXml(resultMods, true);
        IdentifierDefinition resultPid = resultMods.getIdentifier().get(0);
        assertEquals(toXml, "uuid", resultPid.getType());
        assertEquals(toXml, "8232b78c-345b-40ae-9dc0-17bbb101d304", resultPid.getValue());
        assertFalse(toXml, resultMods.getRelatedItem().isEmpty());
        assertEquals(toXml, "1", resultMods.getRelatedItem().get(0).getPart().get(0).getExtent().get(0).getStart().getValue());
//        System.out.println("---");
//        System.out.println(toXml);
    }

    @Test
    public void testModsDefinition() throws Exception {
        ObjectMapper om = new JacksonProvider().getContext(ModsWrapper.class);
        om.configure(DeserializationFeature.UNWRAP_ROOT_VALUE, true);
        ModsDefinition mods = ModsStreamEditor.defaultMods("uuid:test");
        String toJson = om.writeValueAsString(new ModsWrapper(mods));
//        System.out.println(toJson);

        ModsWrapper result = om.readValue(toJson, ModsWrapper.class);
        assertNotNull(result);
        ModsDefinition resultMods = result.getMods();
        assertNotNull(resultMods);
        IdentifierDefinition resultPid = resultMods.getIdentifier().get(0);
        assertEquals("uuid", resultPid.getType());
        assertEquals("test", resultPid.getValue());
//        System.out.println("---");
//        System.out.println(ModsUtils.toXml(resultMods, true));
    }

    @Test
    public void testNsesss2WrappedInternalDocument() throws Exception {
        ObjectMapper om = new JacksonProvider().getContext(DesObjectWrapper.class);
        om.configure(DeserializationFeature.UNWRAP_ROOT_VALUE, true);
        Dokument nsesss = NsesssUtils.defaultInternalDokument();
        String toJson = om.writeValueAsString(new DesObjectWrapper(DesMetadataHandler.mapToJson(nsesss)));
//        System.out.println(toJson);
        DesObjectWrapper result = om.readValue(toJson, DesObjectWrapper.class);
        assertNotNull(result);
        assertNotNull(result.getDokument());
//        System.out.println("---");
//        System.out.println(NsesssUtils.toXml(result.getDokument(), true));
    }

    @Test
    public void testNsesss2WrappedSpis() throws Exception {
        ObjectMapper om = new JacksonProvider().getContext(Spis.class);
        om.configure(DeserializationFeature.UNWRAP_ROOT_VALUE, true);
        Spis spis = NsesssUtils.defaultSpis();
        String toJson = om.writeValueAsString(new DesObjectWrapper(spis));
//        System.out.println(toJson);
        DesObjectWrapper wrapper = om.readValue(toJson, DesObjectWrapper.class);
        assertNotNull(wrapper);
        Spis result = wrapper.getSpis();
        assertNotNull(result);
        assertEquals("ERMS", result.getEvidencniUdaje().getEvidence().getNazevEvidenceDokumentu());
        assertEquals(TZpusobVyrizeni.VYŘÍZENÍ_DOKUMENTEM, result.getEvidencniUdaje().getVyrizeniUzavreni().getZpusob());
        assertEquals("Vyplývá ze spisového plánu organizace",
                result.getEvidencniUdaje().getVyrazovani().getSkartacniRezim().getOduvodneni());
        assertEquals("Uzavření spisu",
                result.getEvidencniUdaje().getVyrazovani().getSkartacniRezim().getSpousteciUdalost());
        assertEquals(TLogicky.ANO, result.getEvidencniUdaje().getManipulace().getAnalogovyDokument().ANO);
//        System.out.println("---");
//        System.out.println(NsesssUtils.toXml(spisResult, true));
    }

    @Test
    public void testReadNsesss2WrappedSpis() throws Exception {
        String toJson = "{ \"Spis\":{ \"EvidencniUdaje\":{ \"Identifikace\":{ \"Identifikator\":[ { \"value\":\"\", \"zdroj\":\"ERMS\" } ] }, \"Popis\":{ \"Nazev\":\"Název\", \"KlicovaSlova\":{ \"KlicoveSlovo\":[ \"slovo2\" ] } }, \"Evidence\":{ \"NazevEvidenceDokumentu\":\"ERMS\" }, \"Puvod\":{ \"DatumVytvoreni\":{ \"value\":\"2013-12-31T23:00:00.000+0000\" } }, \"VyrizeniUzavreni\":{ \"Zpusob\":\"vyřízení dokumentem\" }, \"Vyrazovani\":{ \"SkartacniRezim\":{ \"Oduvodneni\":\"Vyplývá ze spisového plánu organizace\", \"SkartacniLhuta\":1, \"SpousteciUdalost\":\"Uzavření spisu\" } }, \"Manipulace\":{ \"AnalogovyDokument\":\"ano\" } }, \"ID\":\"1\" }}";
        ObjectMapper om = new JacksonProvider().getContext(DesObjectWrapper.class);
//        om.configure(DeserializationFeature.UNWRAP_ROOT_VALUE, true);
        DesObjectWrapper result = om.readValue(toJson, DesObjectWrapper.class);
        assertNotNull(result.getSpis());
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

        ObjectMapper om = new JacksonProvider().getContext(SmartGwtResponse.class);
        om.configure(DeserializationFeature.UNWRAP_ROOT_VALUE, true);
        String toJson = om.writeValueAsString(sgr);
//        System.out.println(toJson);
        assertTrue(toJson.contains("\"data\":[{\"mapId\":\"test.rec-cl\",\"values\":[{\"fullyQcc\":\"FullyQcc\"}]}]}"));
    }

    /**
     * Tests read without mix in annotations and write with them.
     */
    @Test
    public void testMapSearchViewItemToJson() throws Exception {
        String input = "{\"pid\":\"pid\",\"model\":\"model:test\",\"k0\":1}";
        SearchViewItem item = JsonUtils.createObjectMapper().readValue(input, SearchViewItem.class);
        String json = toJson(item);
//        System.out.println(json);
        assertNotNull(json);
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
        b.setUserName("username");
        String json = toJson(b);
        assertTrue(json, json.contains("\"description\":\"title\""));
        assertTrue(json, json.contains("\"create\":\"1991-01"));
        assertFalse(json, json.contains("\"folder\":"));
    }

    private static String toJson(Object obj) throws IOException {
        ObjectMapper om = new JacksonProvider().getContext(obj == null ? null : obj.getClass());
        return om.writeValueAsString(obj);
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
