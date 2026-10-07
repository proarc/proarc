package cz.cas.lib.proarc.common.storage;

import java.io.File;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.w3c.dom.Element;

import static org.junit.jupiter.api.Assertions.*;

class CopyrightMdEditorTest {
    private static final String XML = "<copyright xmlns='" + CopyrightMdEditor.NS
            + "' copyright.status='copyrighted' publication.status='published'>"
            + "<creator><creator.person><name>Author &amp; editor</name><year.death year.type='approximate'>1980</year.death></creator.person>"
            + "<creator.person><name>Second author</name></creator.person></creator>"
            + "<publication><country.publication iso.code='CZ'>Czechia</country.publication><year.publication>1920</year.publication></publication>"
            + "<general.note>BY-SA</general.note><general.note>Additional information</general.note></copyright>";

    @TempDir File temporaryDirectory;

    @Test void storesOnlyCopyrightAndRoundTripsAttributesAndRepeatedFields() throws Exception {
        LocalStorage storage = new LocalStorage();
        File foxml = new File(temporaryDirectory, "object.xml");
        LocalStorage.LocalObject object = storage.create(foxml);
        CopyrightMdEditor editor = new CopyrightMdEditor(object);
        assertNull(editor.readAsString());
        editor.write(XML, editor.getLastModified(), "test");
        object.flush();
        object = storage.load(object.getPid(), foxml);
        editor = new CopyrightMdEditor(object);
        Element root = CopyrightMdEditor.parse(editor.readAsString());
        assertEquals("copyright", root.getLocalName());
        assertEquals(2, root.getElementsByTagNameNS(CopyrightMdEditor.NS, "creator.person").getLength());
        assertEquals("Author & editor", root.getElementsByTagNameNS(CopyrightMdEditor.NS, "name").item(0).getTextContent());
        assertEquals("approximate", ((Element) root.getElementsByTagNameNS(CopyrightMdEditor.NS, "year.death").item(0)).getAttribute("year.type"));
        assertFalse(editor.readAsString().contains("<mets"));
        CopyrightMdEditor persisted = editor;
        assertThrows(DigitalObjectConcurrentModificationException.class, () -> persisted.write(XML, Long.MIN_VALUE, "stale"));
    }

    @Test void requiresStatusesNamespaceAndNdkCardinality() {
        assertThrows(DigitalObjectException.class, () -> CopyrightMdEditor.parse("<copyright/>"));
        assertThrows(DigitalObjectException.class, () -> CopyrightMdEditor.parse(XML.replace("copyright.status='copyrighted'", "")));
        assertThrows(DigitalObjectException.class, () -> CopyrightMdEditor.parse(XML.replace("copyrighted", "pd_usfed")));
        assertThrows(DigitalObjectException.class, () -> CopyrightMdEditor.parse(XML.replace("</publication>", "</publication><publication/>")));
        assertThrows(DigitalObjectException.class, () -> CopyrightMdEditor.parse(XML.replace("1920", "1920-1921")));
        assertThrows(DigitalObjectException.class, () -> CopyrightMdEditor.parse(XML.replace("approximate", "invalid")));
    }

    @Test void rejectsMetsAndDoctype() {
        assertThrows(DigitalObjectException.class, () -> CopyrightMdEditor.parse("<mets xmlns='http://www.loc.gov/METS/'>" + XML + "</mets>"));
        assertThrows(DigitalObjectException.class, () -> CopyrightMdEditor.parse("<!DOCTYPE copyright [<!ENTITY test SYSTEM 'file:///does-not-exist'>]>" + XML));
    }
}
