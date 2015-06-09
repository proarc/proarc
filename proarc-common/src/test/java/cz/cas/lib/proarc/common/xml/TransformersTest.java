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

import cz.cas.lib.proarc.common.export.mets.ValidationErrorHandler;
import cz.cas.lib.proarc.common.mods.Mods33Utils;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.PageMapperTest;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
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
import org.custommonkey.xmlunit.SimpleNamespaceContext;
import org.custommonkey.xmlunit.XMLAssert;
import org.custommonkey.xmlunit.XMLUnit;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.xml.sax.InputSource;

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

    @BeforeClass
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

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
        externalConnections = new ArrayList<URI>();
        XMLUnit.setIgnoreWhitespace(true);
    }

    @After
    public void tearDown() {
        assertTrue(externalConnections.toString(), externalConnections.isEmpty());
        XMLUnit.setIgnoreWhitespace(false);
        XMLUnit.setNormalizeWhitespace(false);
    }

    @Test
    public void testMarcAsMods() throws Exception {
        XMLUnit.setNormalizeWhitespace(true);
        InputStream goldenIS = TransformersTest.class.getResourceAsStream("alephXServerDetailResponseAsMods.xml");
        assertNotNull(goldenIS);
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("alephXServerDetailResponseAsMarcXml.xml");// from test
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
//            System.out.println(new String(contents, "UTF-8"));
            XMLAssert.assertXMLEqual(new InputSource(goldenIS), new InputSource(new ByteArrayInputStream(contents)));
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
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
            System.out.println(xmlResult);
            XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(new HashMap() {{
                put("m", ModsConstants.NS);
            }}));
            XMLAssert.assertXpathEvaluatesTo("HKA001", "/m:mods/m:location/m:physicalLocation[1]", xmlResult);
            XMLAssert.assertXpathEvaluatesTo("test sigla", "/m:mods/m:location/m:physicalLocation[2]", xmlResult);
            XMLAssert.assertXpathEvaluatesTo("2", "count(/m:mods/m:location/m:physicalLocation)", xmlResult);
            XMLAssert.assertXpathEvaluatesTo("54 487", "/m:mods/m:location/m:shelfLocator[1]", xmlResult);
            XMLAssert.assertXpathEvaluatesTo("test signatura", "/m:mods/m:location/m:shelfLocator[2]", xmlResult);
            XMLAssert.assertXpathEvaluatesTo("2", "count(/m:mods/m:location/m:shelfLocator)", xmlResult);
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
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);
            XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(new HashMap() {{
                put("m", ModsConstants.NS);
            }}));
            XMLAssert.assertXpathNotExists("/m:mods/m:originInfo/m:frequency[1]/@authority", xmlResult);
            XMLAssert.assertXpathEvaluatesTo("2x ročně", "/m:mods/m:originInfo/m:frequency[1]", xmlResult);
            XMLAssert.assertXpathEvaluatesTo("marcfrequency", "/m:mods/m:originInfo/m:frequency[2]/@authority", xmlResult);
            XMLAssert.assertXpathEvaluatesTo("Semiannual", "/m:mods/m:originInfo/m:frequency[2]", xmlResult);
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
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);
            XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(new HashMap() {{
                put("m", ModsConstants.NS);
            }}));
            // 600
            XMLAssert.assertXpathExists("/m:mods/m:subject[@authority='czenas']/m:name[@type='personal']/m:namePart[text()='Novák, A. Jiří']", xmlResult);
            // 650
            XMLAssert.assertXpathExists("/m:mods/m:subject[@authority='czenas']/m:topic[text()='daňové delikty']", xmlResult);
            XMLAssert.assertXpathExists("/m:mods/m:subject[@authority='eczenas']/m:topic[text()='tax delinquency']", xmlResult);
            // 651
            XMLAssert.assertXpathExists("/m:mods/m:subject[@authority='czenas']/m:geographic[text()='Česko']", xmlResult);
            XMLAssert.assertXpathExists("/m:mods/m:subject[@authority='eczenas']/m:geographic[text()='Czechia']", xmlResult);
            validateMods(new StreamSource(new ByteArrayInputStream(contents)));
        } finally {
            close(xmlIS);
        }
    }

    /**
     * Tests mapping of field 653 indicator_9 $a to {@code subject/topic}.
     * See issue 185.
     */
    @Test
    public void testMarcAsMods_SubjectTopic_Issue185() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marc_subject_65X_X9.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);
            XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(new HashMap() {{
                put("m", ModsConstants.NS);
            }}));
            // 653
            XMLAssert.assertXpathExists("/m:mods/m:subject[not(@authority)]/m:topic[text()='kočky']", xmlResult);
            XMLAssert.assertXpathExists("/m:mods/m:subject[not(@authority)]/m:topic[text()='cats']", xmlResult);
            XMLAssert.assertXpathNotExists("/m:mods/m:subject/m:name/m:namePart[text()='kočky']", xmlResult);
            XMLAssert.assertXpathNotExists("/m:mods/m:subject/m:name/m:namePart[text()='cats']", xmlResult);
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
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods3);
            assertNotNull(contents);
            String xmlResult = new String(contents, "UTF-8");
//            System.out.println(xmlResult);
            XMLUnit.setXpathNamespaceContext(new SimpleNamespaceContext(new HashMap() {{
                put("m", ModsConstants.NS);
            }}));
            // test 510 4# $a Knihopis 1 $c K01416
            XMLAssert.assertXpathExists("/m:mods/m:relatedItem[@type='isReferencedBy']"
                    + "/m:titleInfo/m:title[text()='Knihopis 1']", xmlResult);
            XMLAssert.assertXpathEvaluatesTo("K01416", "/m:mods/m:relatedItem[@type='isReferencedBy']"
                    + "/m:titleInfo/m:title[text()='Knihopis 1']/../../m:part/m:detail[@type='part']/m:number", xmlResult);

            // test 510 0# $a Knihopis 2
            XMLAssert.assertXpathExists("/m:mods/m:relatedItem[@type='isReferencedBy']"
                    + "/m:titleInfo/m:title[text()='Knihopis 2']", xmlResult);
            XMLAssert.assertXpathNotExists("/m:mods/m:relatedItem[@type='isReferencedBy']"
                    + "/m:titleInfo/m:title[text()='Knihopis 2']/../../m:part", xmlResult);
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
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.OaimarcAsMarc21slim);
            assertNotNull(contents);
//            System.out.println(new String(contents, "UTF-8"));
            XMLAssert.assertXMLEqual(new InputSource(goldenIS), new InputSource(new ByteArrayInputStream(contents)));
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
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.AlephOaiMarcFix);
            assertNotNull(contents);
//            System.out.println(new String(contents, "UTF-8"));
            XMLAssert.assertXMLEqual(new InputSource(goldenIS), new InputSource(new ByteArrayInputStream(contents)));
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
        Transformers mt = new Transformers();
        Map<String, Object> params = ModsUtils.modsAsHtmlParameters(Locale.ENGLISH);

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.ModsAsHtml, params);
            assertNotNull(contents);
            System.out.println(new String(contents, "UTF-8"));
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

    /** Tests label with date but missing volume number. */
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
        String label = Mods33Utils.getLabel(new ModsType(), "model:page");
        assertEquals("?", label);
    }

    private String modsAsFedoraLabel(InputStream xmlIS, String model) throws Exception {
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers();
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("MODEL", model);
        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.ModsAsFedoraLabel, params);
            assertNotNull(contents);
            String label = new String(contents, "UTF-8");
            System.out.println(label);
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
        assertTrue(errors.toString(), errors.isEmpty());
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
