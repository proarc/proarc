/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.xml;

import java.io.ByteArrayOutputStream;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import net.sf.saxon.TransformerFactoryImpl;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

/** Characterizes ProArc-specific mappings while upgrading the LoC stylesheet. */
class Marc21ToMods38CompatibilityTest {

    private static final String MARC_NS = "http://www.loc.gov/MARC21/slim";
    private static final String MODS_NS = "http://www.loc.gov/mods/v3";
    private static Templates legacyTemplate;
    private static Templates mods38Template;

    @BeforeAll
    static void compileStylesheets() throws Exception {
        legacyTemplate = compile("/xml/MARC21slim2MODS3-6.xsl", true);
        mods38Template = compile("/xml/MARC21slim2MODS3.xsl", false);
    }

    @TestFactory
    Stream<DynamicTest> preservesOneHundredLegacyContracts() {
        List<CompatibilityCase> cases = cases();
        assertEquals(100, cases.size());
        return cases.stream().map(testCase -> DynamicTest.dynamicTest(testCase.name, () -> {
            String legacyXml = transform(legacyTemplate, testCase.marc);
            String mods38Xml = transform(mods38Template, testCase.marc);
            String legacy = xpath(legacyXml, testCase.xpath);
            String mods38 = xpath(mods38Xml, testCase.xpath);
            assertFalse(legacy.isEmpty(), "Legacy oracle returned no value for " + testCase.xpath + ": " + legacyXml);
            assertEquals(legacy, mods38);
        }));
    }

    private static List<CompatibilityCase> cases() {
        List<CompatibilityCase> result = new ArrayList<>();
        IntStream.range(0, 10).forEach(i -> {
            String suffix = String.format("%02d", i);
            result.add(aCase("015 cCNB " + suffix,
                    field("015", " ", " ", sub(i % 2 == 0 ? "a" : "z", "cnb" + suffix)),
                    "string(/m:mods/m:identifier[@type='ccnb'][1])"));
            result.add(aCase("072 conspectus " + suffix,
                    field("072", " ", "7", sub("a", "7." + suffix), sub("x", "Topic " + suffix),
                            sub("2", "Konspekt"), sub("9", "Class " + suffix)),
                    i % 2 == 0
                            ? "string(/m:mods/m:subject[@authority='Konspekt']/m:topic)"
                            : "string(/m:mods/m:classification[@authority='Konspekt'])"));
            result.add(aCase("910 location " + suffix,
                    field("910", " ", " ", sub("a", "SIGLA" + suffix), sub("b", "SHELF" + suffix)),
                    i % 2 == 0
                            ? "string(/m:mods/m:location/m:physicalLocation)"
                            : "string(/m:mods/m:location/m:shelfLocator)"));
            result.add(aCase("520 language " + suffix,
                    field("520", Integer.toString(i % 5), "9", sub("a", "Abstract " + suffix),
                            sub("9", i % 2 == 0 ? "cze" : "eng")),
                    "string(/m:mods/m:abstract/@lang)"));
            result.add(aCase("653 local language " + suffix,
                    field("653", "0", i % 2 == 0 ? " " : "9", sub("a", "Topic " + suffix)),
                    "string(/m:mods/m:subject/m:topic/@lang)"));
            result.add(aCase("041 summary language " + suffix,
                    field("041", "0", " ", sub("b", i % 2 == 0 ? "eng" : "ger")),
                    "string(/m:mods/m:language[@objectPart='summary']/m:languageTerm)"));
            result.add(aCase("650 local authority " + suffix,
                    field("650", "0", "9", sub("a", "Subject " + suffix),
                            sub("2", i % 2 == 0 ? "czenas" : "eczenas")),
                    "string(/m:mods/m:subject/@authority)"));
            result.add(aCase("personal name " + suffix,
                    field(i % 2 == 0 ? "100" : "700", "1", " ",
                            sub("a", "Family" + suffix + ", Given" + suffix + ","),
                            sub("d", "19" + suffix + "-"), sub("7", "authority" + suffix)),
                    "string(/m:mods/m:name/m:namePart[@type='family'])"));
            result.add(aCase("510 reference detail " + suffix,
                    field("510", Integer.toString(i % 5), " ", sub("a", "Reference " + suffix),
                            sub("c", "Part " + suffix)),
                    "string(/m:mods/m:relatedItem[@type='isReferencedBy']/m:part/m:detail/m:number)"));
            result.add(aCase("787 related title " + suffix,
                    field("787", "0", "8", sub("i", "Review of:"), sub("a", "Author " + suffix),
                            sub("t", "Related title " + suffix), sub("d", "Prague, 20" + suffix),
                            sub("4", "book")),
                    "string(/m:mods/m:relatedItem/m:titleInfo/m:title)"));
        });
        return result;
    }

    private static CompatibilityCase aCase(String name, String field, String xpath) {
        String marc = "<record xmlns=\"" + MARC_NS + "\">"
                + "<leader>00000nam a2200000 a 4500</leader>"
                + "<controlfield tag=\"008\">240101s2024    xr ||||| |||| 00| 0 cze d</controlfield>"
                + field + "</record>";
        return new CompatibilityCase(name, marc, xpath);
    }

    private static String field(String tag, String ind1, String ind2, String... subfields) {
        return "<datafield tag=\"" + tag + "\" ind1=\"" + ind1 + "\" ind2=\"" + ind2 + "\">"
                + String.join("", subfields) + "</datafield>";
    }

    private static String sub(String code, String value) {
        return "<subfield code=\"" + code + "\">" + value + "</subfield>";
    }

    private static Templates compile(String resourcePath, boolean resolveLegacyInclude) throws Exception {
        URL stylesheet = Marc21ToMods38CompatibilityTest.class.getResource(resourcePath);
        if (stylesheet == null) {
            throw new IllegalStateException("Missing stylesheet " + resourcePath);
        }
        TransformerFactory factory = resolveLegacyInclude
                ? TransformerFactory.newDefaultInstance()
                : new TransformerFactoryImpl();
        if (resolveLegacyInclude) {
            factory.setURIResolver((href, base) -> {
                URL utility = Marc21ToMods38CompatibilityTest.class.getResource("/xml/MARC21slimUtils.xsl");
                return utility == null ? null : new StreamSource(utility.toExternalForm());
            });
        }
        return factory.newTemplates(new StreamSource(stylesheet.toExternalForm()));
    }

    private static String transform(Templates templates, String xml) throws Exception {
        Transformer transformer = templates.newTransformer();
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        transformer.transform(new StreamSource(new StringReader(xml)), new StreamResult(output));
        return output.toString("UTF-8");
    }

    private static String xpath(String xml, String expression) throws Exception {
        javax.xml.xpath.XPath xpath = XPathFactory.newInstance().newXPath();
        xpath.setNamespaceContext(new ModsNamespaceContext());
        return (String) xpath.evaluate(expression, new org.xml.sax.InputSource(new StringReader(xml)), XPathConstants.STRING);
    }

    private static final class CompatibilityCase {
        final String name;
        final String marc;
        final String xpath;

        CompatibilityCase(String name, String marc, String xpath) {
            this.name = name;
            this.marc = marc;
            this.xpath = xpath;
        }
    }

    private static final class ModsNamespaceContext implements NamespaceContext {
        @Override
        public String getNamespaceURI(String prefix) {
            return "m".equals(prefix) ? MODS_NS : XMLConstants.NULL_NS_URI;
        }

        @Override
        public String getPrefix(String namespaceURI) {
            return MODS_NS.equals(namespaceURI) ? "m" : null;
        }

        @Override
        public Iterator<String> getPrefixes(String namespaceURI) {
            return java.util.Collections.singleton(getPrefix(namespaceURI)).iterator();
        }
    }
}
