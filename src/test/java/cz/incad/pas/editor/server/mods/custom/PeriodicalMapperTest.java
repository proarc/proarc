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
package cz.incad.pas.editor.server.mods.custom;

import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.pas.editor.server.mods.ModsUtils;
import cz.incad.pas.editor.server.mods.custom.PeriodicalMapper.Periodical;
import java.io.StringReader;
import java.util.Arrays;
import org.custommonkey.xmlunit.DetailedDiff;
import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.XMLAssert;
import org.custommonkey.xmlunit.XMLUnit;
import org.hamcrest.core.Is;
import static org.junit.Assert.*;
import org.junit.Test;
import org.xml.sax.InputSource;

/**
 *
 * @author Jan Pokorsky
 */
public class PeriodicalMapperTest {

    private static final String XML =
            "  <mods xmlns='http://www.loc.gov/mods/v3'>"
            // identifiers
            + "<identifier type='type'>IDENTIFIER</identifier>"
            // sigla + shelf locators
            + "<location><physicalLocation>SIGLA</physicalLocation>"
                    + "<shelfLocator>SHELF1</shelfLocator>"
                    + "<shelfLocator>SHELF2</shelfLocator></location>"
            // periodicity + publishers + printers
            + "<originInfo transliteration='publisher'>"
                + "<place><placeTerm type='text'>PLACETERM[0]</placeTerm></place>"
                + "<publisher>PUBLISHER[0]</publisher><dateIssued>DATE[0]</dateIssued>"
            + "</originInfo>"
            + "<originInfo transliteration='printer'>"
                + "<place><placeTerm type='text'>PLACETERM[1]</placeTerm></place>"
                + "<publisher>PRINTER[1]</publisher><dateCreated>DATE[1]</dateCreated>"
            + "</originInfo>"
            + "<originInfo>"
                + "<issuance>continuing</issuance><frequency>daily</frequency>"
            + "</originInfo>"
            // titles
            + "<titleInfo type='alternative'><title>ATITLE[0]</title></titleInfo>"
            + "<titleInfo><title>MTITLE[0]</title><subTitle>STITLE[0]</subTitle></titleInfo>"
            + "<titleInfo type='alternative' displayLabel='Klíčový název'><title>KTITLE[0]</title></titleInfo>"
            + "<titleInfo type='uniform'><title>UNSUPPORTED</title></titleInfo>"
            // authors + contributors
            + "<name type='personal'>"
            + "<namePart type='family'>FamilyAuthor1</namePart>"
            + "<namePart type='given'>GivenAuthor1</namePart>"
            + "<role><roleTerm type='code'>cre</roleTerm><roleTerm type='text'>Author</roleTerm></role>"
            + "</name>"
            // languages
            + "<language><languageTerm authority='iso639-2b' type='code'>cze</languageTerm></language>"
            // classifications
            + "<classification authority='ddc'>DDC</classification>"
            + "<classification authority='udc'>UDC</classification>"
            // keywords
            + "<subject><topic>TOPIC</topic></subject>"
            // physicalDescriptions
            + "<physicalDescription><extent>EXTENT</extent><extent>SIZE</extent></physicalDescription>"
            // recordOrigin
            + "<recordInfo><recordOrigin>RECORDORIGIN</recordOrigin></recordInfo>"
            // note
            + "<note>NOTE</note>"
            + "</mods>";

    @Test
    public void testRead() {
        ModsType mods = ModsUtils.unmarshal(XML, ModsType.class);
        PeriodicalMapper instance = new PeriodicalMapper();
        Periodical result = instance.map(mods);
        Periodical expected = new Periodical();
        expected.setIdentifiers(Arrays.asList(new IdentifierMapper.IdentifierItem(0, "type", "IDENTIFIER")));
        expected.setSigla("SIGLA");
        expected.setShelfLocators(ArrayMapper.toStringItemList(Arrays.asList("SHELF1", "SHELF2")));
        expected.setPeriodicities(ArrayMapper.toStringItemList(Arrays.asList("daily")));
        expected.setPublishers(Arrays.asList(new OriginInfoMapper.PublisherItem(
                0, OriginInfoMapper.PublisherItem.Role.PUBLISHER, "PUBLISHER[0]", "DATE[0]", "PLACETERM[0]")));
        expected.setPrinters(Arrays.asList(new OriginInfoMapper.PublisherItem(
                1, OriginInfoMapper.PublisherItem.Role.PRINTER, "PRINTER[1]", "DATE[1]", "PLACETERM[1]")));
        expected.setTitles(ArrayMapper.toStringItemList(Arrays.asList("MTITLE[0]")));
        expected.setSubtitles(ArrayMapper.toStringItemList(Arrays.asList("STITLE[0]")));
        expected.setAlternativeTitles(ArrayMapper.toStringItemList(Arrays.asList("ATITLE[0]")));
        expected.setKeyTitles(ArrayMapper.toStringItemList(Arrays.asList("KTITLE[0]")));
        expected.setAuthors(Arrays.asList(new NameMapper.NameItem(0, "FamilyAuthor1", "GivenAuthor1", NameMapper.NameItem.NameRole.AUTHOR)));
        expected.setContributors(Arrays.<NameMapper.NameItem>asList());
        expected.setLanguages(Arrays.asList(new LanguageMapper.LanguageItem(0, "cze")));
        expected.setClassifications(Arrays.asList(new ClassificationMapper.ClassificationPair("DDC", 0, "UDC", 1)));
        expected.setKeywords(ArrayMapper.toStringItemList(Arrays.asList("TOPIC")));
        expected.setPhysicalDescriptions(Arrays.asList(new PhysicalDescriptionMapper.ExtentPair("EXTENT", 0, "SIZE", 1)));
        expected.setRecordOrigin("RECORDORIGIN");
        expected.setNote("NOTE");
        assertPeriodicalEquals(expected, result);
        assertEquals(expected, result);
    }

    public static void assertPeriodicalEquals(Periodical expected, Periodical actual) {
        if (expected == null && actual == null) {
            return ;
        }
        assertNotNull(expected);
        assertNotNull(actual);
        assertThat(actual.getIdentifiers(), Is.is(expected.getIdentifiers()));
        assertEquals(expected.getSigla(), actual.getSigla());
        assertThat(actual.getShelfLocators(), Is.is(expected.getShelfLocators()));
        assertThat(actual.getPeriodicities(), Is.is(expected.getPeriodicities()));
        assertThat(actual.getTitles(), Is.is(expected.getTitles()));
        assertThat(actual.getSubtitles(), Is.is(expected.getSubtitles()));
        assertThat(actual.getKeyTitles(), Is.is(expected.getKeyTitles()));
        assertThat(actual.getAlternativeTitles(), Is.is(expected.getAlternativeTitles()));
        assertThat(actual.getIdentifiers(), Is.is(expected.getIdentifiers()));
        assertThat(actual.getAuthors(), Is.is(expected.getAuthors()));
        assertThat(actual.getContributors(), Is.is(expected.getContributors()));
        assertThat(actual.getPrinters(), Is.is(expected.getPrinters()));
        assertThat(actual.getPublishers(), Is.is(expected.getPublishers()));
        assertThat(actual.getLanguages(), Is.is(expected.getLanguages()));
        assertThat(actual.getClassifications(), Is.is(expected.getClassifications()));
        assertThat(actual.getKeywords(), Is.is(expected.getKeywords()));
        assertThat(actual.getPhysicalDescriptions(), Is.is(expected.getPhysicalDescriptions()));
        assertEquals(expected.getRecordOrigin(), actual.getRecordOrigin());
        assertEquals(expected.getNote(), actual.getNote());
    }

    @Test
    // XXX missing asserts
    public void testWrite() throws Exception {
        ModsType mods = new ModsType();
        mods.setVersion("3.4");
        PeriodicalMapper instance = new PeriodicalMapper();
        Periodical p = new Periodical();
        p.setIdentifiers(Arrays.asList(new IdentifierMapper.IdentifierItem("type", "IDENTIFIER")));
        p.setSigla("SIGLA");
        p.setShelfLocators(ArrayMapper.toStringItemList(Arrays.asList("SHELF1", "SHELF2")));
        p.setPeriodicities(ArrayMapper.toStringItemList(Arrays.asList("daily")));
        p.setPublishers(Arrays.asList(new OriginInfoMapper.PublisherItem(
                null, OriginInfoMapper.PublisherItem.Role.PUBLISHER, "PUBLISHER[0]", "DATE[0]", "PLACETERM[0]")));
        p.setPrinters(Arrays.asList(new OriginInfoMapper.PublisherItem(
                null, OriginInfoMapper.PublisherItem.Role.PRINTER, "PRINTER[1]", "DATE[1]", "PLACETERM[1]")));
        p.setTitles(ArrayMapper.toStringItemList(Arrays.asList("MTITLE[0]")));
        p.setSubtitles(ArrayMapper.toStringItemList(Arrays.asList("STITLE[0]")));
        p.setAlternativeTitles(ArrayMapper.toStringItemList(Arrays.asList("ATITLE[0]")));
        p.setKeyTitles(ArrayMapper.toStringItemList(Arrays.asList("KTITLE[0]")));
        p.setAuthors(Arrays.asList(new NameMapper.NameItem("FamilyAuthor1", "GivenAuthor1", NameMapper.NameItem.NameRole.AUTHOR)));
        p.setContributors(Arrays.<NameMapper.NameItem>asList());
        p.setLanguages(Arrays.asList(new LanguageMapper.LanguageItem(null, "cze")));
        p.setClassifications(Arrays.asList(new ClassificationMapper.ClassificationPair("DDC", null, "UDC", null)));
        p.setKeywords(ArrayMapper.toStringItemList(Arrays.asList("TOPIC")));
        p.setPhysicalDescriptions(Arrays.asList(new PhysicalDescriptionMapper.ExtentPair("EXTENT", null, "SIZE", null)));
        p.setRecordOrigin("RECORDORIGIN");
        p.setNote("NOTE");
        instance.map(mods, p);
        String toXml = ModsUtils.toXml(mods, true);
        System.out.println(toXml);
//        XMLUnit.setIgnoreWhitespace(true);
////        String expected = ModsUtils.unmarshal(PeriodicalMapperTest.class.getResource("periodical_mods.xml"), ModsType.class);
////        XMLAssert.assertXMLEqual(
////                new InputSource(PeriodicalMapperTest.class.getResource("periodical_mods.xml").toExternalForm()),
////                new InputSource(new StringReader(toXml)));
//        Diff diff = new Diff(
//                new InputSource(PeriodicalMapperTest.class.getResource("periodical_mods.xml").toExternalForm()),
//                new InputSource(new StringReader(toXml)));
//        DetailedDiff detailedDiff = new DetailedDiff(diff);
//        boolean similar = detailedDiff.similar();
//        assertTrue(detailedDiff.toString(), similar);
////        XMLAssert.assertXMLEqual(diff, true);
////        XMLAssert.assertXMLEqual(expected, toXml);
    }
    
}
