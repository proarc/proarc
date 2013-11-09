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
package cz.cas.lib.proarc.common.mods.custom;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.MonographMapper.Monograph;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import java.util.Arrays;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class MonographMapperTest {

    public MonographMapperTest() {
    }

    @Test
    public void testRead() {
        ModsType mods = ModsUtils.unmarshal(MonographMapperTest.class.getResource("monograph_mods.xml"), ModsType.class);
        MonographMapper instance = new MonographMapper();
        Monograph result = instance.map(mods);
        assertNotNull(result);
        // XXX missing asserts
    }

    @Test
    public void testWrite() {
        ModsType mods = new ModsType();
        Monograph m = new Monograph();
        m.setIdentifiers(Arrays.asList(new IdentifierMapper.IdentifierItem("type", "IDENTIFIER")));
        m.setSigla("SIGLA");
        m.setShelfLocators(ArrayMapper.toStringItemList(Arrays.asList("SHELF1", "SHELF2")));
        m.setPublishers(Arrays.asList(new OriginInfoMapper.PublisherItem(
                null, OriginInfoMapper.PublisherItem.Role.PUBLISHER, "PUBLISHER[0]", "DATE[0]", "PLACETERM[0]")));
        m.setPrinters(Arrays.asList(new OriginInfoMapper.PublisherItem(
                null, OriginInfoMapper.PublisherItem.Role.PRINTER, "PRINTER[1]", "DATE[1]", "PLACETERM[1]")));
        m.setTitles(ArrayMapper.toStringItemList(Arrays.asList("MTITLE[0]")));
        m.setSubtitles(ArrayMapper.toStringItemList(Arrays.asList("STITLE[0]")));
        m.setAlternativeTitles(ArrayMapper.toStringItemList(Arrays.asList("ATITLE[0]")));
        m.setKeyTitles(ArrayMapper.toStringItemList(Arrays.asList("KTITLE[0]")));
        m.setAuthors(Arrays.asList(new NameMapper.NameItem("FamilyAuthor1", "GivenAuthor1", NameMapper.NameItem.NameRole.AUTHOR)));
        m.setContributors(Arrays.<NameMapper.NameItem>asList());
        m.setLanguages(Arrays.asList(new LanguageMapper.LanguageItem(null, "cze")));
        m.setClassifications(Arrays.asList(new ClassificationMapper.ClassificationPair("DDC", null, "UDC", null)));
        m.setKeywords(ArrayMapper.toStringItemList(Arrays.asList("TOPIC")));
        m.setPhysicalDescriptions(Arrays.asList(new PhysicalDescriptionMapper.ExtentPair("EXTENT", null, "SIZE", null)));
        m.setPreservationStateOfArt("PRESERVATIONSTATEOFART");
        m.setPreservationTreatment("PRESERVATIONTREATMENT");
        m.setRecordOrigin("RECORDORIGIN");
        m.setNote("NOTE");

        MonographMapper instance = new MonographMapper();
        instance.map(mods, m);
        System.out.println(ModsUtils.toXml(mods, true));
        // XXX missing asserts
    }

}
