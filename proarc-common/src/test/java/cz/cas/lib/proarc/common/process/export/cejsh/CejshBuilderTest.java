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
package cz.cas.lib.proarc.common.process.export.cejsh;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.process.export.cejsh.CejshBuilder.Article;
import cz.cas.lib.proarc.common.process.export.cejsh.CejshBuilder.Issue;
import cz.cas.lib.proarc.common.process.export.cejsh.CejshBuilder.Title;
import cz.cas.lib.proarc.common.process.export.cejsh.CejshBuilder.Volume;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import cz.cas.lib.proarc.common.xml.TransformErrorListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Properties;
import javax.xml.bind.DatatypeConverter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import org.apache.commons.io.Charsets;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 *
 * @author Jan Pokorsky
 */
public class CejshBuilderTest {

    private final AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

    public CejshBuilderTest() throws Exception {
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
    public void testWriteProperties() throws Exception {
        CejshBuilder cb = new CejshBuilder(new CejshConfig(), appConfig.getExportParams());
        File folder = temp.getRoot();
        int articleCount = 3;
        cb.writeProperties(folder, articleCount);
        File file = new File(folder, CejshBuilder.IMPORT_PROPERTIES_FILENAME);
        assertTrue(file.exists());
        Properties props = new Properties();
        props.load(new InputStreamReader(new FileInputStream(file), Charsets.UTF_8));
        assertEquals(String.valueOf(articleCount), props.getProperty(CejshBuilder.PROP_IMPORT_OBJECTS));
        assertEquals("1", props.getProperty(CejshBuilder.PROP_IMPORT_BWMETA_FILES));
        assertEquals("0", props.getProperty(CejshBuilder.PROP_IMPORT_CONTENT_FILES));
        String resultDate = props.getProperty(CejshBuilder.PROP_IMPORT_INFODATE);
        assertNotNull(resultDate);
        String expDated = DatatypeConverter.printDateTime(
                new GregorianCalendar(CejshBuilder.UTC)).substring(0, 14);
        assertEquals(resultDate, expDated, resultDate.substring(0, 14));
    }

    @Test
    public void testMissingXsl() throws Exception {
        CejshConfig conf = new CejshConfig();
        conf.setCejshXslUrl("/???");
        try {
            CejshBuilder cb = new CejshBuilder(conf, appConfig.getExportParams());
            fail();
        } catch (TransformerConfigurationException ex) {
//            System.out.println(ex.getMessage());
        }
    }

    @Test
    public void testCreateCejshXml_TitleVolumeIssue() throws Exception {
        CejshConfig conf = new CejshConfig();
        CejshBuilder cb = new CejshBuilder(conf, appConfig.getExportParams());
        Document articleDoc = cb.getDocumentBuilder().parse(CejshBuilderTest.class.getResource("article_mods.xml").toExternalForm());
        // issn must match some cejsh_journals.xml/cejsh/journal[@issn=$issn]
        final String pkgIssn = "0231-5955";
        Issue issue = new Issue();
        issue.setIssn(pkgIssn);
        issue.setIssueId("uuid-issue");
        issue.setIssueNumber("issue1");
        Volume volume = new Volume();
        volume.setVolumeId("uuid-volume");
        volume.setVolumeNumber("volume1");
        volume.setYear("1985");
        Article article = new Article(null, articleDoc.getDocumentElement(), null);
        cb.setIssue(issue);
        cb.setVolume(volume);

        Document articleCollectionDoc = cb.mergeElements(Collections.singletonList(article));
        DOMSource cejshSource = new DOMSource(articleCollectionDoc);
        DOMResult cejshResult = new DOMResult();
//        dump(cejshSource);

        TransformErrorListener xslError = cb.createCejshXml(cejshSource, cejshResult);
        assertEquals(Collections.emptyList(), xslError.getErrors());
        final Node cejshRootNode = cejshResult.getNode();
//        dump(new DOMSource(cejshRootNode));

        List<String> errors = cb.validateCejshXml(new DOMSource(cejshRootNode));
        assertEquals(Collections.emptyList(), errors);

        XPath xpath = ProarcXmlUtils.defaultXPathFactory().newXPath();
        xpath.setNamespaceContext(new SimpleNamespaceContext().add("b", CejshBuilder.NS_BWMETA105));
        assertNotNull(xpath.evaluate("/b:bwmeta/b:element[@id='bwmeta1.element.ebfd7bf2-169d-476e-a230-0cc39f01764c']", cejshRootNode, XPathConstants.NODE));
        assertEquals("volume1", xpath.evaluate("/b:bwmeta/b:element[@id='bwmeta1.element.uuid-volume']/b:name", cejshRootNode, XPathConstants.STRING));
        assertEquals("issue1", xpath.evaluate("/b:bwmeta/b:element[@id='bwmeta1.element.uuid-issue']/b:name", cejshRootNode, XPathConstants.STRING));
        assertEquals("1985", xpath.evaluate("/b:bwmeta/b:element[@id='bwmeta1.element.9358223b-b135-388f-a71e-24ac2c8422c7-1985']/b:name", cejshRootNode, XPathConstants.STRING));
    }

    @Test
    public void testCreateCejshXml_TitleVolume() throws Exception {
        CejshConfig conf = new CejshConfig();
        CejshBuilder cb = new CejshBuilder(conf, appConfig.getExportParams());
        Document articleDoc = cb.getDocumentBuilder().parse(CejshBuilderTest.class.getResource("article_mods.xml").toExternalForm());
        // issn must match some cejsh_journals.xml/cejsh/journal[@issn=$issn]
        final String pkgIssn = "0231-5955";
        Title title = new Title();
        title.setIssn(pkgIssn);
        Volume volume = new Volume();
        volume.setVolumeId("uuid-volume");
        volume.setVolumeNumber("volume1");
        volume.setYear("1985");
        Article article = new Article(null, articleDoc.getDocumentElement(), null);
        cb.setTitle(title);
        cb.setVolume(volume);

        Document articleCollectionDoc = cb.mergeElements(Collections.singletonList(article));
        DOMSource cejshSource = new DOMSource(articleCollectionDoc);
        DOMResult cejshResult = new DOMResult();
//        dump(cejshSource);

        TransformErrorListener xslError = cb.createCejshXml(cejshSource, cejshResult);
        assertEquals(Collections.emptyList(), xslError.getErrors());
        final Node cejshRootNode = cejshResult.getNode();
//        dump(new DOMSource(cejshRootNode));

        List<String> errors = cb.validateCejshXml(new DOMSource(cejshRootNode));
        assertEquals(Collections.emptyList(), errors);

        XPath xpath = ProarcXmlUtils.defaultXPathFactory().newXPath();
        xpath.setNamespaceContext(new SimpleNamespaceContext().add("b", CejshBuilder.NS_BWMETA105));
        assertNotNull(xpath.evaluate("/b:bwmeta/b:element[@id='bwmeta1.element.ebfd7bf2-169d-476e-a230-0cc39f01764c']", cejshRootNode, XPathConstants.NODE));
        assertEquals("volume1", xpath.evaluate("/b:bwmeta/b:element[@id='bwmeta1.element.uuid-volume']/b:name", cejshRootNode, XPathConstants.STRING));
//        assertEquals("issue1", xpath.evaluate("/b:bwmeta/b:element[@id='bwmeta1.element.uuid-issue']/b:name", cejshRootNode, XPathConstants.STRING));
        assertEquals("1985", xpath.evaluate("/b:bwmeta/b:element[@id='bwmeta1.element.9358223b-b135-388f-a71e-24ac2c8422c7-1985']/b:name", cejshRootNode, XPathConstants.STRING));
    }

    @Test
    public void testCreateCejshElement_UnknownIssn() throws Exception {
        CejshConfig conf = new CejshConfig();
        CejshBuilder cb = new CejshBuilder(conf, appConfig.getExportParams());
        Document articleDoc = cb.getDocumentBuilder().parse(CejshBuilderTest.class.getResource("article_mods.xml").toExternalForm());
        final String pkgIssn = "XXX-XXX";
        Issue issue = new Issue();
        issue.setIssn(pkgIssn);
        issue.setIssueId("uuid-issue");
        issue.setIssueNumber("issue1");
        Volume volume = new Volume();
        volume.setVolumeId("uuid-volume");
        volume.setVolumeNumber("volume1");
        volume.setYear("1985");
        Article article = new Article(null, articleDoc.getDocumentElement(), null);
        cb.setIssue(issue);
        cb.setVolume(volume);
        Document articleCollectionDoc = cb.mergeElements(Collections.singletonList(article));
        DOMResult cejshResult = new DOMResult();
        cb.createCejshXml(new DOMSource(articleCollectionDoc), cejshResult);
//        dump(new DOMSource(cejshResult.getNode()));

        // issn must match some cejsh_journals.xml/cejsh/journal[@issn=$issn]
        assertEquals(1, cb.getTranformationErrors().size());
        assertTrue(cb.getTranformationErrors().get(0), cb.getTranformationErrors().get(0)
                .startsWith("ERROR: Missing journalId"));
    }

    @Test
    public void testCreatePackageName_TitleVolumeIssue() throws Exception {
        CejshConfig cejshConfig = new CejshConfig();
        CejshBuilder cb = new CejshBuilder(cejshConfig, appConfig.getExportParams());
        cb.setTitle(new Title());
        cb.getTitle().setIssn("1111-1111");
        cb.setVolume(new Volume());
        cb.getVolume().setVolumeNumber("2");
        cb.getVolume().setVolumeId("uuid-volume");
        cb.getVolume().setYear("1980");
        cb.setIssue(new Issue());
        cb.getIssue().setIssn("2222-2222");
        cb.getIssue().setIssueNumber("3");
        cb.getIssue().setIssueId("uuid:issue");
        assertEquals("2222-2222_1980_2_3", cb.createPackageName());

        cb.getVolume().setVolumeNumber(null);
        cb.getVolume().setYear(null);
        cb.getIssue().setIssueNumber("");
        assertEquals("2222-2222_NA_NA_NA", cb.createPackageName());
    }

    @Test
    public void testCreatePackageName_TitleVolume() throws Exception {
        CejshConfig cejshConfig = new CejshConfig();
        CejshBuilder cb = new CejshBuilder(cejshConfig, appConfig.getExportParams());
        cb.setTitle(new Title());
        cb.getTitle().setIssn("1111-1111");
        cb.setVolume(new Volume());
        cb.getVolume().setVolumeNumber("2");
        cb.getVolume().setVolumeId("uuid-volume");
        cb.getVolume().setYear("1980");
        assertEquals("1111-1111_1980_2_NA", cb.createPackageName());

        cb.getVolume().setVolumeNumber(null);
        cb.getVolume().setYear(null);
        assertEquals("1111-1111_NA_NA_NA", cb.createPackageName());
    }

    @Test
    public void testWritePackage() throws Exception {
        CejshConfig cejshConfig = new CejshConfig();
        CejshBuilder cb = new CejshBuilder(cejshConfig, appConfig.getExportParams());
        CejshContext ctx = new CejshContext(temp.getRoot(), new CejshStatusHandler(), cejshConfig, appConfig.getExportParams());
        cb.setTitle(new Title());
        cb.getTitle().setIssn("1111-1111");
        cb.setVolume(new Volume());
        cb.getVolume().setVolumeNumber("2");
        cb.getVolume().setVolumeId("uuid-volume");
        cb.getVolume().setYear("1980");
        cb.setIssue(new Issue());
        cb.getIssue().setIssn("0231-5955");
        cb.getIssue().setIssueNumber("3");
        cb.getIssue().setIssueId("uuid-issue");
        Document articleDoc = cb.getDocumentBuilder().parse(CejshBuilderTest.class.getResource("article_mods.xml").toExternalForm());
        Article addArticle = cb.addArticle(articleDoc, DigitalObjectElement.NULL, ctx);
        assertNotNull(addArticle);
        assertTrue(addArticle.isReviewed());
        File resultPkg = cb.writePackage(DigitalObjectElement.NULL, Arrays.asList(addArticle), ctx);
        assertTrue(ctx.getStatus().isOk());
        assertNotNull(resultPkg);
        assertTrue(resultPkg.isDirectory());
        assertTrue(new File(resultPkg.getAbsolutePath() + ".zip").exists());
        assertTrue(new File(resultPkg, CejshBuilder.IMPORT_PROPERTIES_FILENAME).exists());
        assertTrue(new File(new File(resultPkg, CejshBuilder.IMPORTS_NEW_FILENAME), CejshBuilder.P0XML_FILENAME).exists());
    }

    @Test
    public void testAddArticleNotReviewed() throws Exception {
        CejshConfig cejshConfig = new CejshConfig();
        CejshBuilder cb = new CejshBuilder(cejshConfig, appConfig.getExportParams());
        CejshContext ctx = new CejshContext(temp.getRoot(), new CejshStatusHandler(), cejshConfig, appConfig.getExportParams());
        cb.setTitle(new Title());
        cb.getTitle().setIssn("1111-1111");
        cb.setVolume(new Volume());
        cb.getVolume().setVolumeNumber("2");
        cb.getVolume().setVolumeId("uuid-volume");
        cb.getVolume().setYear("1980");
        cb.setIssue(new Issue());
        cb.getIssue().setIssn("0231-5955");
        cb.getIssue().setIssueNumber("3");
        cb.getIssue().setIssueId("uuid-issue");
        Document articleDoc = cb.getDocumentBuilder().parse(CejshBuilderTest.class.getResource("article_not_reviewed_mods.xml").toExternalForm());
        Article addArticle = cb.addArticle(articleDoc, DigitalObjectElement.NULL, ctx);
        assertNotNull(addArticle);
        assertFalse(addArticle.isReviewed());
    }

    private void dump(Source src) throws TransformerException {
        dump(src, new StreamResult(System.out));
    }
    private void dump(Source src, Result res) throws TransformerException {
        Transformer t = TransformerFactory.newInstance().newTransformer();
        t.setOutputProperty(OutputKeys.INDENT, "yes");
        t.transform(src, res);
    }

}
