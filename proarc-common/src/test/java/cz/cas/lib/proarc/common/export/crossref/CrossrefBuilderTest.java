/*
 * Copyright (C) 2016 Jan Pokorsky
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
package cz.cas.lib.proarc.common.export.crossref;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.export.cejsh.CejshBuilderTest;
import cz.cas.lib.proarc.common.xml.TransformErrorListener;
import java.io.File;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.List;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Rule;
import org.w3c.dom.Document;

/**
 *
 * @author Jan Pokorsky
 */
public class CrossrefBuilderTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

    public CrossrefBuilderTest() {
    }

    @Test
    public void testCreateCrossrefXml() throws Exception {
        File targetFolder = temp.getRoot();
        CrossrefBuilder builder = new CrossrefBuilder(targetFolder);
        builder.addPeriodicalTitle("1210-8510", "titleTest", "abbrevTest", "print");
        builder.addVolume("1", null, null);
        builder.addIssue("10", "2010", "uuid");
        Document article = builder.getDocumentBuilder().parse(
                CejshBuilderTest.class.getResource("article_mods.xml").toExternalForm());
        builder.addArticle(article);
        Document articles = builder.mergeArticles();
        StringWriter dump = new StringWriter();
        TransformErrorListener errors = builder.createCrossrefXml(new DOMSource(articles), new StreamResult(dump));
//        System.out.println(dump);
        assertTrue(errors.getErrors().toString(), errors.getErrors().isEmpty());

        List<String> validateErrors = builder.validateCrossref(new StreamSource(new StringReader(dump.toString())));
        assertTrue(validateErrors.toString(), validateErrors.isEmpty());
    }

    @Test
    public void testCreateCrossrefXml_SkippedVolume() throws Exception {
        File targetFolder = temp.getRoot();
        CrossrefBuilder builder = new CrossrefBuilder(targetFolder);
        builder.addPeriodicalTitle("1210-8510", "titleTest", "abbrevTest", "print");
        builder.addIssue("10", "2010", "uuid");
        Document article = builder.getDocumentBuilder().parse(
                CejshBuilderTest.class.getResource("article_mods.xml").toExternalForm());
        builder.addArticle(article);
        Document articles = builder.mergeArticles();
        StringWriter dump = new StringWriter();
        TransformErrorListener errors = builder.createCrossrefXml(new DOMSource(articles), new StreamResult(dump));
//        System.out.println(dump);
        assertTrue(errors.getErrors().toString(), errors.getErrors().isEmpty());

        List<String> validateErrors = builder.validateCrossref(new StreamSource(new StringReader(dump.toString())));
        assertTrue(validateErrors.toString(), validateErrors.isEmpty());
    }

    @Test
    public void testCreateCrossrefXml_SkippedIssue() throws Exception {
        File targetFolder = temp.getRoot();
        CrossrefBuilder builder = new CrossrefBuilder(targetFolder);
        builder.addPeriodicalTitle("1210-8510", "titleTest", "abbrevTest", "print");
        builder.addVolume("1", "20.12.2012", "uuid");
        Document article = builder.getDocumentBuilder().parse(
                CejshBuilderTest.class.getResource("article_mods.xml").toExternalForm());
        builder.addArticle(article);
        Document articles = builder.mergeArticles();
        StringWriter dump = new StringWriter();
        TransformErrorListener errors = builder.createCrossrefXml(new DOMSource(articles), new StreamResult(dump));
//        System.out.println(dump);
        assertTrue(errors.getErrors().toString(), errors.getErrors().isEmpty());

        List<String> validateErrors = builder.validateCrossref(new StreamSource(new StringReader(dump.toString())));
        assertTrue(validateErrors.toString(), validateErrors.isEmpty());
    }

    @Test
    public void testGetSchema() throws Exception {
        assertNotNull(CrossrefBuilder.getCrossrefSchema());
    }

}
