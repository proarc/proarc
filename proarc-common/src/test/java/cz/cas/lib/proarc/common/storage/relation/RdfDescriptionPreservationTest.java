/*
 * Copyright (C) 2026 Lukas Sykora
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
package cz.cas.lib.proarc.common.storage.relation;

import cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo;
import java.io.StringReader;
import java.io.StringWriter;
import javax.xml.transform.stream.StreamSource;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class RdfDescriptionPreservationTest {
    @Test void replacementWritesOnlyCurrentPackageAndItsState() throws Exception {
        Rdf rdf = new Rdf("uuid:unit");
        rdf.getDescription().setArchivInfo(record("12", ArchivInfo.State.STORED));
        rdf.getDescription().setArchivInfo(record("23", ArchivInfo.State.UPLOADED));
        String xml = marshal(rdf);
        assertFalse(xml.contains(">12<"));
        assertTrue(xml.contains("archivInfo"));
        assertFalse(xml.contains("preservation"));
        Rdf restored = read(xml);
        assertEquals("23", restored.getDescription().getArchivInfo().getPackageId());
        assertEquals(ArchivInfo.State.UPLOADED, restored.getDescription().getArchivInfo().getState());
    }

    @Test void legacyRepeatedElementsReadAndRewriteOnlyLastPackage() throws Exception {
        String xml = """
                <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                         xmlns:p="http://proarc.lib.cas.cz/relations#">
                  <rdf:Description rdf:about="info:fedora/uuid:unit">
                    <p:preservation rdf:parseType="Resource">
                      <p:provider>cesnet</p:provider><p:packageId>12</p:packageId><p:state>STORED</p:state>
                    </p:preservation>
                    <p:preservation rdf:parseType="Resource">
                      <p:provider>cesnet</p:provider><p:packageId>23</p:packageId><p:state>UPLOADED</p:state>
                    </p:preservation>
                  </rdf:Description>
                </rdf:RDF>
                """;
        Rdf rdf = read(xml);
        assertEquals("23", rdf.getDescription().getArchivInfo().getPackageId());
        assertEquals(ArchivInfo.State.UPLOADED, rdf.getDescription().getArchivInfo().getState());
        String rewritten = marshal(rdf);
        assertFalse(rewritten.contains(">12<"));
        assertTrue(rewritten.contains("archivInfo"));
        assertFalse(rewritten.contains("preservation"));
        assertTrue(rdf.getDescription().getRelations().isEmpty(), "Older entries must not survive as unknown XML relations");
        assertEquals("23", read(rewritten).getDescription().getArchivInfo().getPackageId());
    }

    @Test void clearingPackageOmitsPreservationElement() throws Exception {
        Rdf rdf = new Rdf("uuid:unit");
        rdf.getDescription().setArchivInfo(record("12", ArchivInfo.State.STORED));
        rdf.getDescription().setArchivInfo(null);
        String xml = marshal(rdf);
        assertFalse(xml.contains("preservation"));
        assertFalse(xml.contains("archivInfo"));
        assertNull(read(xml).getDescription().getArchivInfo());
    }

    private static ArchivInfo record(String id, ArchivInfo.State state) {
        ArchivInfo record = new ArchivInfo();
        record.setProvider("cesnet");
        record.setPackageId(id);
        record.setState(state);
        return record;
    }

    private static Rdf read(String xml) {
        return Relations.unmarshal(new StreamSource(new StringReader(xml)), Rdf.class);
    }

    private static String marshal(Rdf rdf) throws Exception {
        StringWriter xml = new StringWriter();
        Relations.defaultMarshaller(true).marshal(rdf, xml);
        return xml.toString();
    }
}
