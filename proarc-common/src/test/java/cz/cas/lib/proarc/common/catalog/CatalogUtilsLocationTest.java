package cz.cas.lib.proarc.common.catalog;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.xml.Transformers;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CatalogUtilsLocationTest {

    @Test
    void preservesSiglaWithoutShelfLocatorAfterCatalogProcessing() throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>");

        assertEquals(1, mods.getLocation().size());
        assertEquals("ABA007", mods.getLocation().get(0).getPhysicalLocation().get(0).getValue());
        assertTrue(mods.getLocation().get(0).getShelfLocator().isEmpty());
    }

    @Test
    void keepsOneOfRepeatedSiglaOnlyLocations() throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>",
                "<datafield tag=\"910\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"a\">ABA007</subfield></datafield>");

        assertEquals(1, mods.getLocation().size());
        assertEquals("ABA007", mods.getLocation().get(0).getPhysicalLocation().get(0).getValue());
    }

    @Test
    void removesSiglaOnlyLocationWhenAnotherLocationHasUrl() throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>",
                "<datafield tag=\"856\" ind1=\"4\" ind2=\"0\">"
                + "<subfield code=\"u\">https://example.org/item</subfield></datafield>");

        assertEquals(1, mods.getLocation().size());
        assertEquals("https://example.org/item", mods.getLocation().get(0).getUrl().get(0).getValue());
    }

    @Test
    void preservesSiglaOnlyLocationsWithDifferentSiglas() throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>",
                "<datafield tag=\"910\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"a\">BOA001</subfield></datafield>");

        assertEquals(2, mods.getLocation().size());
    }

    @Test
    void preservesSiglaAndShelfLocatorAfterCatalogProcessing() throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>"
                + "<subfield code=\"b\">TEST-SIGNATURA</subfield>");

        assertEquals(1, mods.getLocation().size());
        assertEquals("ABA007", mods.getLocation().get(0).getPhysicalLocation().get(0).getValue());
        assertEquals("TEST-SIGNATURA", mods.getLocation().get(0).getShelfLocator().get(0).getValue());
    }

    @ParameterizedTest
    @ValueSource(strings = {"3", "c"})
    void importsShelfLocatorFrom996WithSiglaFrom910(String code) throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>",
                "<datafield tag=\"996\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"" + code + "\">E 83154</subfield></datafield>");

        assertEquals(1, mods.getLocation().size());
        assertEquals("E 83154", mods.getLocation().get(0).getShelfLocator().get(0).getValue());
        assertEquals("ABA007", mods.getLocation().get(0).getPhysicalLocation().get(0).getValue());
    }

    @Test
    void preservesShelfLocatorsFrom910AndRepeated996Fields() throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>"
                + "<subfield code=\"b\">TITLE-SHELF</subfield>",
                "<datafield tag=\"996\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"3\">COPY-1</subfield></datafield>"
                + "<datafield tag=\"996\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"c\">COPY-2</subfield></datafield>");

        assertEquals(3, mods.getLocation().size());
        assertEquals("TITLE-SHELF", mods.getLocation().get(0).getShelfLocator().get(0).getValue());
        assertEquals("COPY-1", mods.getLocation().get(1).getShelfLocator().get(0).getValue());
        assertEquals("COPY-2", mods.getLocation().get(2).getShelfLocator().get(0).getValue());
    }

    @Test
    void ignores996WithoutNonBlankShelfLocator() throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>",
                "<datafield tag=\"996\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"3\">  </subfield><subfield code=\"c\"></subfield>"
                + "<subfield code=\"b\">26002101049</subfield></datafield>");

        assertEquals(1, mods.getLocation().size());
        assertTrue(mods.getLocation().get(0).getShelfLocator().isEmpty());
        assertEquals("barcode", mods.getIdentifier().get(0).getType());
        assertEquals("26002101049", mods.getIdentifier().get(0).getValue());
    }

    @ParameterizedTest
    @ValueSource(strings = {"996", "980"})
    void importsBarcodesAndIgnoresBlankValues(String tag) throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>",
                "<datafield tag=\"" + tag + "\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"b\">0012345</subfield>"
                + "<subfield code=\"b\"> </subfield></datafield>"
                + "<datafield tag=\"" + tag + "\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"b\">0067890</subfield></datafield>");

        assertEquals(2, mods.getIdentifier().size());
        assertEquals("barcode", mods.getIdentifier().get(0).getType());
        assertEquals("0012345", mods.getIdentifier().get(0).getValue());
        assertEquals("barcode", mods.getIdentifier().get(1).getType());
        assertEquals("0067890", mods.getIdentifier().get(1).getValue());
    }

    @Test
    void preservesShelfLocatorAndBarcodeWithout910() throws Exception {
        ModsDefinition mods = importRecord(null,
                "<datafield tag=\"996\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"c\">COPY-1</subfield>"
                + "<subfield code=\"b\">0012345</subfield></datafield>");

        assertEquals(1, mods.getLocation().size());
        assertTrue(mods.getLocation().get(0).getPhysicalLocation().isEmpty());
        assertEquals("COPY-1", mods.getLocation().get(0).getShelfLocator().get(0).getValue());
        assertEquals("barcode", mods.getIdentifier().get(0).getType());
        assertEquals("0012345", mods.getIdentifier().get(0).getValue());
    }

    @Test
    void removesOnlySiglaOnlyLocationAndPreservesAllShelfLocations() throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>",
                "<datafield tag=\"910\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"a\">ABA007</subfield>"
                + "<subfield code=\"b\">COPY-1</subfield></datafield>"
                + "<datafield tag=\"910\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"a\">BOA001</subfield>"
                + "<subfield code=\"b\">OTHER-SHELF</subfield></datafield>"
                + "<datafield tag=\"996\" ind1=\" \" ind2=\" \">"
                + "<subfield code=\"3\">COPY-1</subfield>"
                + "<subfield code=\"c\">COPY-2</subfield>"
                + "<subfield code=\"b\">26002101049</subfield></datafield>");

        assertEquals(4, mods.getLocation().size());
        assertEquals("ABA007", mods.getLocation().get(0).getPhysicalLocation().get(0).getValue());
        assertEquals(1, mods.getLocation().get(0).getShelfLocator().size());
        assertEquals("COPY-1", mods.getLocation().get(0).getShelfLocator().get(0).getValue());
        assertEquals("COPY-1", mods.getLocation().get(2).getShelfLocator().get(0).getValue());
        assertEquals("COPY-2", mods.getLocation().get(3).getShelfLocator().get(0).getValue());
        assertEquals("BOA001", mods.getLocation().get(1).getPhysicalLocation().get(0).getValue());
        assertEquals(1, mods.getLocation().get(1).getShelfLocator().size());
        assertEquals("OTHER-SHELF", mods.getLocation().get(1).getShelfLocator().get(0).getValue());
        assertEquals("26002101049", mods.getIdentifier().get(0).getValue());
    }

    private ModsDefinition importRecord(String holdings) throws Exception {
        return importRecord(holdings, "");
    }

    @Test
    void removesSiglaOnlyLocationsInCatalogUtilsWhilePreservingShelfLocatorsAndUrls() throws Exception {
        String modsXml = "<mods xmlns=\"http://www.loc.gov/mods/v3\">"
                + "<location><physicalLocation>ABA007</physicalLocation></location>"
                + "<location><shelfLocator>COPY-1</shelfLocator></location>"
                + "<location><physicalLocation>ABA007</physicalLocation>"
                + "<shelfLocator>COPY-1</shelfLocator><shelfLocator>COPY-2</shelfLocator></location>"
                + "<location><physicalLocation>ABA007</physicalLocation>"
                + "<url>https://example.org/item</url></location>"
                + "<location displayLabel=\"Reading room\"><physicalLocation>ABA007</physicalLocation>"
                + "<shelfLocator>ROOM-1</shelfLocator></location></mods>";
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document marc = factory.newDocumentBuilder().parse(new InputSource(new StringReader(
                "<record xmlns=\"http://www.loc.gov/MARC21/slim\"/>")));

        byte[] repaired = CatalogUtils.repairModsBytes(modsXml.getBytes(StandardCharsets.UTF_8), marc);
        ModsDefinition mods = ModsUtils.unmarshal(new String(repaired, StandardCharsets.UTF_8), ModsDefinition.class);

        assertEquals(4, mods.getLocation().size());
        assertEquals(1, mods.getLocation().get(0).getShelfLocator().size());
        assertEquals("COPY-1", mods.getLocation().get(0).getShelfLocator().get(0).getValue());
        assertEquals("COPY-1", mods.getLocation().get(1).getShelfLocator().get(0).getValue());
        assertEquals("COPY-2", mods.getLocation().get(1).getShelfLocator().get(1).getValue());
        assertEquals("https://example.org/item", mods.getLocation().get(2).getUrl().get(0).getValue());
        assertEquals("Reading room", mods.getLocation().get(3).getDisplayLabel());
        assertEquals("ROOM-1", mods.getLocation().get(3).getShelfLocator().get(0).getValue());
    }

    private ModsDefinition importRecord(String holdings, String additionalFields) throws Exception {
        String marc = "<record xmlns=\"http://www.loc.gov/MARC21/slim\">"
                + "<leader>00000nam a2200000 a 4500</leader>"
                + "<datafield tag=\"245\" ind1=\"0\" ind2=\"0\">"
                + "<subfield code=\"a\">Test title</subfield></datafield>"
                + (holdings == null ? "" : "<datafield tag=\"910\" ind1=\" \" ind2=\" \">"
                + holdings + "</datafield>") + additionalFields + "</record>";
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document document = factory.newDocumentBuilder().parse(new InputSource(new StringReader(marc)));
        byte[] transformed = new Transformers(null).transformAsBytes(
                new DOMSource(document), Transformers.Format.MarcxmlAsMods3);
        byte[] repaired = CatalogUtils.repairModsBytes(transformed, document);
        return ModsUtils.unmarshal(new String(repaired, StandardCharsets.UTF_8), ModsDefinition.class);
    }
}
