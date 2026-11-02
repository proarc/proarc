package cz.cas.lib.proarc.common.catalog;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.xml.Transformers;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import org.junit.jupiter.api.Test;
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
    void preservesSiglaAndShelfLocatorAfterCatalogProcessing() throws Exception {
        ModsDefinition mods = importRecord("<subfield code=\"a\">ABA007</subfield>"
                + "<subfield code=\"b\">TEST-SIGNATURA</subfield>");

        assertEquals(1, mods.getLocation().size());
        assertEquals("ABA007", mods.getLocation().get(0).getPhysicalLocation().get(0).getValue());
        assertEquals("TEST-SIGNATURA", mods.getLocation().get(0).getShelfLocator().get(0).getValue());
    }

    private ModsDefinition importRecord(String holdings) throws Exception {
        String marc = "<record xmlns=\"http://www.loc.gov/MARC21/slim\">"
                + "<leader>00000nam a2200000 a 4500</leader>"
                + "<datafield tag=\"245\" ind1=\"0\" ind2=\"0\">"
                + "<subfield code=\"a\">Test title</subfield></datafield>"
                + "<datafield tag=\"910\" ind1=\" \" ind2=\" \">"
                + holdings + "</datafield></record>";
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document document = factory.newDocumentBuilder().parse(new InputSource(new StringReader(marc)));
        byte[] transformed = new Transformers(null).transformAsBytes(
                new DOMSource(document), Transformers.Format.MarcxmlAsMods3);
        byte[] repaired = CatalogUtils.repairModsBytes(transformed, document);
        return ModsUtils.unmarshal(new String(repaired, StandardCharsets.UTF_8), ModsDefinition.class);
    }
}
