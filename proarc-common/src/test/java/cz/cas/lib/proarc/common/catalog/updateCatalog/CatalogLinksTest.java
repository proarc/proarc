package cz.cas.lib.proarc.common.catalog.updateCatalog;

import java.util.List;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class CatalogLinksTest {
    @Test
    void verbisReadsOnlyConfiguredObjectValuesMarkedAsProarc() throws Exception {
        String xml = "<record xmlns='http://www.loc.gov/MARC21/slim'>"
                + "<datafield tag='856'><subfield code='x'>proarcId</subfield><subfield code='y'>uuid:one</subfield></datafield>"
                + "<datafield tag='856'><subfield code='x'>otherApp</subfield><subfield code='y'>uuid:other</subfield></datafield>"
                + "<datafield tag='991'><subfield code='x'>proarcId</subfield><subfield code='y'>uuid:wrongField</subfield></datafield>"
                + "</record>";
        var factory = javax.xml.parsers.DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        var document = factory.newDocumentBuilder().parse(new org.xml.sax.InputSource(new java.io.StringReader(xml)));
        assertEquals(List.of("uuid:one"), CatalogLinks.readMarcObjectIds(document, "856", "x", "y"));
    }

    @Test
    void equivalentLegacyViewAndUuidLinksAreIdempotent() {
        assertTrue(CatalogLinks.contains(List.of("https://library/uuid/uuid:one"), "https://library/view/uuid:one"));
        assertFalse(CatalogLinks.contains(List.of("https://other/view/uuid:one"), "https://library/view/uuid:one"));
    }

    @Test
    void conflictingLinksArePreservedInSuccessfulReviewResult() {
        CatalogUpdateResult result = CatalogLinks.result(List.of("https://library/view/uuid:old", "https://library/view/uuid:older"),
                "https://library/view/uuid:new");
        assertTrue(result.warning());
        assertTrue(result.message().contains("uuid:old"));
        assertTrue(result.message().contains("uuid:older"));
        assertTrue(result.message().contains("Nový odkaz: https://library/view/uuid:new"));
        assertFalse(CatalogLinks.result(List.of(), "uuid:new").warning());
    }

    @Test
    void readsAllDistinctCatalogLinksFromMods() {
        String xml = "<mods xmlns='http://www.loc.gov/mods/v3'><location>"
                + "<url>https://library/view/uuid:one</url><url>https://library/view/uuid:one</url>"
                + "<url>https://library/view/uuid:two</url></location></mods>";
        assertEquals(List.of("https://library/view/uuid:one", "https://library/view/uuid:two"), CatalogLinks.read(xml));
        assertEquals(List.of("https://library/view/uuid:one", "https://library/view/uuid:two"),
                CatalogLinks.read("<modsCollection xmlns='http://www.loc.gov/mods/v3'>" + xml + "</modsCollection>"));
    }
}
