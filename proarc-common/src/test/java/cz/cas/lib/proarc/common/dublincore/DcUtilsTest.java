/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.dublincore;

import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.StringReader;
import java.net.URL;
import java.util.List;
import javax.xml.transform.stream.StreamSource;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class DcUtilsTest {

    public DcUtilsTest() {
    }

    @Test
    public void testMarshalTitle() {
        OaiDcType dc = new OaiDcType();
        dc.getTitles().add(new ElementType("title", null));
        dc.getTitles().add(new ElementType("", null));
        dc.getTitles().add(new ElementType(null, null));
        String xml = DcUtils.toXml(dc, true);
        assertNotNull(xml);
//        System.out.println(xml);

        OaiDcType result = DcUtils.unmarshal(new StreamSource(new StringReader(xml)), OaiDcType.class);
        assertNotNull(result);
        // test collapse of empty elements
        List<ElementType> expTitles = new OaiDcType().getTitles();
        expTitles.add(new ElementType("title", null));
        assertEquals(expTitles, result.getTitles());
        assertEquals(dc.getTitles(), result.getTitles());
    }

    @Test
    public void testMarshal() {
        OaiDcType dc = createFullDc();
        String xml = DcUtils.toXml(dc, true);
        assertNotNull(xml);
//        System.out.println(xml);

        OaiDcType result = DcUtils.unmarshal(new StreamSource(new StringReader(xml)), OaiDcType.class);
        assertNotNull(result);
        assertEquals(dc.getContributors(), result.getContributors());
        assertEquals(dc.getCoverages(), result.getCoverages());
        assertEquals(dc.getCreators(), result.getCreators());
        assertEquals(dc.getDates(), result.getDates());
        assertEquals(dc.getDescriptions(), result.getDescriptions());
        assertEquals(dc.getFormats(), result.getFormats());
        assertEquals(dc.getIdentifiers(), result.getIdentifiers());
        assertEquals(dc.getLanguages(), result.getLanguages());
        assertEquals(dc.getPublishers(), result.getPublishers());
        assertEquals(dc.getRelations(), result.getRelations());
        assertEquals(dc.getRights(), result.getRights());
        assertEquals(dc.getSources(), result.getSources());
        assertEquals(dc.getSubjects(), result.getSubjects());
        assertEquals(dc.getTitles(), result.getTitles());
        assertEquals(dc.getTypes(), result.getTypes());
    }

    @Test
    public void testUnmarshal() {
        URL resource = DcUtilsTest.class.getResource("oai_dc.xml");
        assertNotNull(resource);
        OaiDcType result = DcUtils.unmarshal(resource, OaiDcType.class);
        assertNotNull(result);
        OaiDcType dc = createFullDc();
        assertEquals(dc.getContributors(), result.getContributors());
        assertEquals(dc.getCoverages(), result.getCoverages());
        assertEquals(dc.getCreators(), result.getCreators());
        assertEquals(dc.getDates(), result.getDates());
        assertEquals(dc.getDescriptions(), result.getDescriptions());
        assertEquals(dc.getFormats(), result.getFormats());
        assertEquals(dc.getIdentifiers(), result.getIdentifiers());
        assertEquals(dc.getLanguages(), result.getLanguages());
        assertEquals(dc.getPublishers(), result.getPublishers());
        assertEquals(dc.getRelations(), result.getRelations());
        assertEquals(dc.getRights(), result.getRights());
        assertEquals(dc.getSources(), result.getSources());
        assertEquals(dc.getSubjects(), result.getSubjects());
        assertEquals(dc.getTitles(), result.getTitles());
        assertEquals(dc.getTypes(), result.getTypes());
    }

    private OaiDcType createFullDc() {
        OaiDcType dc = new OaiDcType();
        dc.getContributors().add(new ElementType("Contributor", "en"));
        dc.getCoverages().add(new ElementType("Coverage", "en"));
        dc.getCreators().add(new ElementType("Creator", "en"));
        dc.getDates().add(new ElementType("Date", "en"));
        dc.getDescriptions().add(new ElementType("Description", "en"));
        dc.getFormats().add(new ElementType("Format", "en"));
        dc.getIdentifiers().add(new ElementType("Identifier", "en"));
        dc.getLanguages().add(new ElementType("Language", "en"));
        dc.getPublishers().add(new ElementType("Publisher", "en"));
        dc.getRelations().add(new ElementType("Relation", "en"));
        dc.getRights().add(new ElementType("Right", "en"));
        dc.getSources().add(new ElementType("Source", "en"));
        dc.getSubjects().add(new ElementType("Subject", "en"));
        dc.getTitles().add(new ElementType("Title", "en"));
        dc.getTypes().add(new ElementType("Type", "en"));
        return dc;
    }

}