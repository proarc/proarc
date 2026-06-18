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
package cz.cas.lib.proarc.common.xml;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.PageMapperTest;
import cz.cas.lib.proarc.common.process.export.mets.ValidationErrorHandler;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Validator;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.xml.sax.InputSource;
import org.xmlunit.builder.DiffBuilder;
import org.xmlunit.builder.Input;
import org.xmlunit.diff.Diff;
import org.xmlunit.xpath.JAXPXPathEngine;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class TransformersTest {

    private static final Logger LOG = Logger.getLogger(TransformersTest.class.getName());
    private static ProxySelector defaultProxy;
    private static List<URI> externalConnections;

    public TransformersTest() {
    }

    @BeforeAll
    public static void setUpClass() throws Exception {
        defaultProxy = ProxySelector.getDefault();
        // detect external connections
        ProxySelector.setDefault(new ProxySelector() {

            @Override
            public List<Proxy> select(URI uri) {
                externalConnections.add(uri);
                return defaultProxy.select(uri);
            }

            @Override
            public void connectFailed(URI uri, SocketAddress sa, IOException ioe) {
                defaultProxy.connectFailed(uri, sa, ioe);
            }
        });
    }

    @AfterAll
    public static void tearDownClass() throws Exception {
    }

    @BeforeEach
    public void setUp() {
        externalConnections = new ArrayList<URI>();
    }

    @AfterEach
    public void tearDown() {
        assertTrue(externalConnections.isEmpty(), () -> externalConnections.toString());
    }

    @Test
    public void testMarcAsMods() throws Exception {
        InputStream goldenIS = TransformersTest.class.getResourceAsStream("alephXServerDetailResponseAsMods.xml");
        assertNotNull(goldenIS);
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("alephXServerDetailResponseAsMarcXml.xml");// from test
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
//            System.out.println(new String(contents, "UTF-8"));

            Diff diff = DiffBuilder.compare(new InputSource(goldenIS))
                    .withTest(new InputSource(new ByteArrayInputStream(contents)))
                    .ignoreWhitespace()
                    .ignoreComments()
                    .checkForSimilar()
                    .build();

            assertFalse(diff.hasDifferences(), diff.toString());
        } finally {
            close(xmlIS);
            close(goldenIS);
        }
    }

    /**
     * Tests mapping of 910a(sigla) as {@code <physicalLocation>} and 910b(signatura) as {@code <shelfLocator>}.
     */
    @Test
    public void testMarcAsMods_Issue32() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);

            Map<String, String> namespaces = new HashMap<>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            assertEquals("HKA001", xpathEngine.evaluate("/m:mods/m:location/m:physicalLocation[1]", Input.fromString(xmlResult).build()));

            assertEquals("HKA001", xpathEngine.evaluate("/m:mods/m:location/m:physicalLocation[1]", Input.fromString(xmlResult).build()));
            assertEquals("test sigla", xpathEngine.evaluate("/m:mods/m:location/m:physicalLocation[2]", Input.fromString(xmlResult).build()));
            assertEquals("2", xpathEngine.evaluate("count(/m:mods/m:location/m:physicalLocation)", Input.fromString(xmlResult).build()));
            assertEquals("54 487", xpathEngine.evaluate("/m:mods/m:location/m:shelfLocator[1]", Input.fromString(xmlResult).build()));
            assertEquals("test signatura", xpathEngine.evaluate("/m:mods/m:location/m:shelfLocator[2]", Input.fromString(xmlResult).build()));
            assertEquals("2", xpathEngine.evaluate("count(/m:mods/m:location/m:shelfLocator)", Input.fromString(xmlResult).build()));
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Tests mapping of fields 310a and 008/18 to {@code frequency@authority}.
     * See issue 118 and 181.
     */
    @Test
    public void testMarcAsMods_FrequencyAuthority_Issue181() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("frequencyAuthority.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);

            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            assertTrue(xpathEngine.evaluate("/m:mods/m:originInfo/m:frequency[1]/@authority", Input.fromString(xmlResult).build()).isEmpty());
            assertEquals("2x ročně", xpathEngine.evaluate("/m:mods/m:originInfo/m:frequency[1]", Input.fromString(xmlResult).build()));
            assertEquals("marcfrequency", xpathEngine.evaluate("/m:mods/m:originInfo/m:frequency[2]/@authority", Input.fromString(xmlResult).build()));
            assertEquals("Semiannual", xpathEngine.evaluate("/m:mods/m:originInfo/m:frequency[2]", Input.fromString(xmlResult).build()));
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Tests mapping of fields 600, 610, 611, 630, 648, 650, 651 indicator_9 $2 to {@code subject@authority}.
     * See issue 182.
     */
    @Test
    public void testMarcAsMods_SubjectAuthority_Issue182() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc_subject_65X_X9.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);
            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            // 600
            assertFalse(xpathEngine.evaluate("/m:mods/m:subject[@authority='czenas']/m:name[@type='personal']/m:namePart[text()='Novák, A. Jiří']", Input.fromString(xmlResult).build()).isEmpty());
            // 650
            assertFalse(xpathEngine.evaluate("/m:mods/m:subject[@authority='czenas']/m:topic[text()='daňové delikty']", Input.fromString(xmlResult).build()).isEmpty());
            assertFalse(xpathEngine.evaluate("/m:mods/m:subject[@authority='eczenas']/m:topic[text()='tax delinquency']", Input.fromString(xmlResult).build()).isEmpty());
            // 651
            assertFalse(xpathEngine.evaluate("/m:mods/m:subject[@authority='czenas']/m:geographic[text()='Česko']", Input.fromString(xmlResult).build()).isEmpty());
            assertFalse(xpathEngine.evaluate("/m:mods/m:subject[@authority='eczenas']/m:geographic[text()='Czechia']", Input.fromString(xmlResult).build()).isEmpty());
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Tests mapping of field 653 indicator_9 $a to {@code subject/topic@lang}.
     * See issue 185.
     * See issue 433.
     */
    @Test
    public void testMarcAsMods_SubjectTopic_Issue185_Issue433() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc_subject_65X_X9.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);

            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            // 653
            assertFalse(xpathEngine.evaluate("/m:mods/m:subject[not(@authority)]/m:topic[text()='kočky' and @lang='cze']", Input.fromString(xmlResult).build()).isEmpty());
            assertFalse(xpathEngine.evaluate("/m:mods/m:subject[not(@authority)]/m:topic[text()='cats' and @lang='eng']", Input.fromString(xmlResult).build()).isEmpty());
            assertTrue(xpathEngine.evaluate("/m:mods/m:subject/m:name/m:namePart[text()='kočky']", Input.fromString(xmlResult).build()).isEmpty());
            assertTrue(xpathEngine.evaluate("/m:mods/m:subject/m:name/m:namePart[text()='cats']", Input.fromString(xmlResult).build()).isEmpty());
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Test mapping of 072#7 $x to {@code subject/topic} and $a/$9 to {@code classification}.
     * See issue 303.
     */
    @Test
    public void testMarcAsMods_Conspectus_Issue303() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);

            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            assertEquals("Umění", xpathEngine.evaluate("/m:mods/m:subject[@authority='Konspekt']/m:topic", Input.fromString(xmlResult).build()));
            assertEquals("7.01/.09", xpathEngine.evaluate("/m:mods/m:classification[@authority='udc' and @edition='Konspekt']", Input.fromString(xmlResult).build()));
            assertEquals("21", xpathEngine.evaluate("/m:mods/m:classification[@authority='Konspekt']", Input.fromString(xmlResult).build()));
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Tests mapping of field 100,700 $7 to {@code name@authorityURI} and {@code name@valueURI}.
     * See issue 305.
     */
    @Test
    public void testMarcAsMods_AuthorityId_Issue305() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc_subject_65X_X9.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);

            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            // test 100 1# $a Kocina, Jan, $d 1960- $4 aut $7 xx0113245
            assertFalse(xpathEngine.evaluate("/m:mods/m:name[@type='personal'"
                    + " and @authorityURI='http://aut.nkp.cz'"
                    + " and @valueURI='http://aut.nkp.cz/xx0113245']"
                    + "/m:namePart[@type='family' and text()='Kocina']"
                    + "/../m:namePart[@type='given' and text()='Jan']"
                    + "/../m:namePart[@type='date' and text()='1960-']"
                    + "/../m:role/m:roleTerm[text()='aut']", Input.fromString(xmlResult).build()).isEmpty());
            // test 700 1# $a Honzík, Bohumil, $d 1972- $4 aut $7 jn20020422016
            assertFalse(xpathEngine.evaluate("/m:mods/m:name[@type='personal'"
                    + " and @authorityURI='http://aut.nkp.cz'"
                    + " and @valueURI='http://aut.nkp.cz/jn20020422016']"
                    + "/m:namePart[@type='family' and text()='Honzík']"
                    + "/../m:namePart[@type='given' and text()='Bohumil']"
                    + "/../m:namePart[@type='date' and text()='1972-']"
                    + "/../m:role/m:roleTerm[text()='aut']", Input.fromString(xmlResult).build()).isEmpty());
            // test 700 1# $a Test Without AuthorityId $d 1972- $4 aut
            assertFalse(xpathEngine.evaluate("/m:mods/m:name[@type='personal'"
                    + " and not(@authorityURI)"
                    + " and not(@valueURI)]"
                    + "/m:namePart[text()='Test Without AuthorityId']"
                    + "/../m:namePart[@type='date' and text()='1972-']"
                    + "/../m:role/m:roleTerm[text()='aut']", Input.fromString(xmlResult).build()).isEmpty());

            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Tests mapping of field 510 $c to {@code part/detail/number}.
     * See issue 306.
     */
    @Test
    public void testMarcAsMods_RelatedItemPartDetailNumber_Issue306() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc_subject_65X_X9.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);

            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            // test 510 4# $a Knihopis 1 $c K01416
            assertFalse(xpathEngine.evaluate("/m:mods/m:relatedItem[@type='isReferencedBy']"
                    + "/m:titleInfo/m:title[text()='Knihopis 1']", Input.fromString(xmlResult).build()).isEmpty());
            assertEquals("K01416", xpathEngine.evaluate("/m:mods/m:relatedItem[@type='isReferencedBy']"
                    + "/m:titleInfo/m:title[text()='Knihopis 1']/../../m:part/m:detail[@type='part']/m:number", Input.fromString(xmlResult).build()));

            // test 510 0# $a Knihopis 2
            assertFalse(xpathEngine.evaluate("/m:mods/m:relatedItem[@type='isReferencedBy']"
                    + "/m:titleInfo/m:title[text()='Knihopis 2']", Input.fromString(xmlResult).build()).isEmpty());
            assertTrue(xpathEngine.evaluate("/m:mods/m:relatedItem[@type='isReferencedBy']"
                    + "/m:titleInfo/m:title[text()='Knihopis 2']/../../m:part", Input.fromString(xmlResult).build()).isEmpty());
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Tests mapping of field 787 to {@code relatedItem}.
     * See issue 313.
     */
    @Test
    public void testMarcAsMods_RelatedItem_Issue313() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc_relatedItem_787.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);

            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            // test 78708 |i Recenze na: |a Čeřovský, Jan |t Jak jsme zachraňovali svět aneb Půl století ve službách ochrany přírody |d Praha : Nakladatelství Academia, 2014 |4 kniha
            assertEquals(
                    "Jak jsme zachraňovali svět aneb Půl století ve službách ochrany přírody", xpathEngine.evaluate(
                            "/m:mods/m:relatedItem[not(@type) and @displayLabel='Recenze na:']"
                                    + "/m:titleInfo/m:title/text()", Input.fromString(xmlResult).build()));
            assertEquals(
                    "Čeřovský, Jan", xpathEngine.evaluate(
                            "/m:mods/m:relatedItem[not(@type) and @displayLabel='Recenze na:']"
                                    + "/m:name/m:namePart/text()", Input.fromString(xmlResult).build()));
            assertEquals(
                    "Praha : Nakladatelství Academia, 2014", xpathEngine.evaluate(
                            "/m:mods/m:relatedItem[not(@type) and @displayLabel='Recenze na:']"
                                    + "/m:originInfo/m:publisher/text()", Input.fromString(xmlResult).build()));
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Tests mapping of field 520 and subfield $9 to {@code abstract@lang}.
     * See issue 434.
     */
    @Test
    public void testMarcAsMods_AbstractLang_Issue434() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc_subject_65X_X9.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);

            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            assertFalse(xpathEngine.evaluate("/m:mods/m:abstract[@lang='cze' and @type='Abstract' and text()='Text cze']", Input.fromString(xmlResult).build()).isEmpty());
            assertFalse(xpathEngine.evaluate("/m:mods/m:abstract[@lang='eng' and @type='Abstract' and text()='Text eng']", Input.fromString(xmlResult).build()).isEmpty());
            assertFalse(xpathEngine.evaluate("/m:mods/m:abstract[not(@lang) and @type='Abstract' and text()='Text no lang']", Input.fromString(xmlResult).build()).isEmpty());
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Tests mapping of field 264_4 to {@code originInfo}.
     * See issue 298.
     */
    @Test
    public void testMarcAsMods_Mapping264_ind4_Issue298() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc_originInfo_264_ind4.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);

            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("m", ModsConstants.NS);

            JAXPXPathEngine xpathEngine = new JAXPXPathEngine();
            xpathEngine.setNamespaceContext(namespaces);

            assertFalse(xpathEngine.evaluate("/m:mods/m:originInfo[@eventType='copyright']", Input.fromString(xmlResult).build()).isEmpty());
            assertEquals("Praha", xpathEngine.evaluate("/m:mods/m:originInfo[@eventType='copyright']/m:place/m:placeTerm", Input.fromString(xmlResult).build()));
            assertEquals("Albatros", xpathEngine.evaluate("/m:mods/m:originInfo[@eventType='copyright']/m:publisher", Input.fromString(xmlResult).build()));
            assertEquals("2015", xpathEngine.evaluate("/m:mods/m:originInfo[@eventType='copyright']/m:copyrightDate", Input.fromString(xmlResult).build()));
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    @Test
    public void testOaiMarcAsMarc() throws Exception {
        InputStream goldenIS = TransformersTest.class.getResourceAsStream("alephXServerDetailResponseAsMarcXml.xml");
        assertNotNull(goldenIS);
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("alephXServerDetailResponseAsOaiMarc.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.OaimarcAsMarc21slim);
            assertNotNull(contents);
//            System.out.println(new String(contents, "UTF-8"));

            Diff diff = DiffBuilder.compare(new InputSource(goldenIS))
                    .withTest(new InputSource(new ByteArrayInputStream(contents)))
                    .ignoreWhitespace()
                    .ignoreComments()
                    .checkForSimilar()
                    .build();

            assertFalse(diff.hasDifferences(), diff.toString());

        } finally {
            close(xmlIS);
            close(goldenIS);
        }
    }

    @Test
    public void testAlephXServerDetailNamespaceFix() throws Exception {
        InputStream goldenIS = TransformersTest.class.getResourceAsStream("alephXServerDetailResponseFixed.xml");
        assertNotNull(goldenIS);
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("../catalog/alephXServerDetailResponse.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.AlephOaiMarcFix);
            assertNotNull(contents);
//            System.out.println(new String(contents, "UTF-8"));

            Diff diff = DiffBuilder.compare(new InputSource(goldenIS))
                    .withTest(new InputSource(new ByteArrayInputStream(contents)))
                    .ignoreWhitespace()
                    .ignoreComments()
                    .checkForSimilar()
                    .build();

            assertFalse(diff.hasDifferences(), diff.toString());

        } finally {
            close(xmlIS);
            close(goldenIS);
        }
    }

    @Test
    public void testModsAsHtml() throws Exception {
//        XMLUnit.setNormalizeWhitespace(true);
//        InputStream goldenIS = TransformersTest.class.getResourceAsStream("alephXServerDetailResponseAsMods.xml");
//        assertNotNull(goldenIS);
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("alephXServerDetailResponseAsMods.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);
        Map<String, Object> params = ModsUtils.modsAsHtmlParameters(Locale.ENGLISH);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.ModsAsHtml, params);
            assertNotNull(contents);
//            System.out.println(new String(contents, "UTF-8"));
//            XMLAssert.assertXMLEqual(new InputSource(goldenIS), new InputSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
//            close(goldenIS);
        }
    }

    @Test
    public void testModsAsFedoraLabel_Page() throws Exception {
        assertEquals("[1], Blank",
                modsAsFedoraLabel(PageMapperTest.class.getResourceAsStream("page_mods.xml"), "model:page"));
    }

    @Test
    public void testModsAsFedoraLabel_Issue() throws Exception {
        assertEquals("1",
                modsAsFedoraLabel(PageMapperTest.class.getResourceAsStream("issue_mods.xml"), "model:periodicalitem"));
    }

    @Test
    public void testModsAsFedoraLabel_Volume() throws Exception {
        assertEquals("1893, 1",
                modsAsFedoraLabel(PageMapperTest.class.getResourceAsStream("volume_mods.xml"), "model:periodicalvolume"));
    }

    /**
     * Tests label with date but missing volume number.
     */
    @Test
    public void testModsAsFedoraLabel_Volume_issue222() throws Exception {
        assertEquals("1893",
                modsAsFedoraLabel(PageMapperTest.class.getResourceAsStream("volume_mods_issue222.xml"), "model:periodicalvolume"));
    }

    @Test
    public void testModsAsFedoraLabel_Periodical() throws Exception {
        assertEquals("MTITLE[0]: STITLE[0]",
                modsAsFedoraLabel(PageMapperTest.class.getResourceAsStream("periodical_mods.xml"), "model:periodical"));
    }

    @Test
    public void testModsAsFedoraLabel_Empty() throws Exception {
        String label = ModsUtils.getLabel(new ModsDefinition(), "model:page");
        assertEquals("?", label);
    }

    private String modsAsFedoraLabel(InputStream xmlIS, String model) throws Exception {
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers(null);
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("MODEL", model);
        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.ModsAsFedoraLabel, params);
            assertNotNull(contents);
            String label = new String(contents, "UTF-8");
//            System.out.println(label);
            return label;
        } finally {
            close(xmlIS);
        }
    }

    private void validateMods(Source source) throws Exception {
        Validator v = ModsUtils.getSchema().newValidator();
        ValidationErrorHandler handler = new ValidationErrorHandler();
        v.setErrorHandler(handler);
        v.validate(source);
        List<String> errors = handler.getValidationErrors();
        assertTrue(errors.isEmpty(), () -> errors.toString());
    }

    private static void close(InputStream is) {
        if (is != null) {
            try {
                is.close();
            } catch (IOException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
    }

}
