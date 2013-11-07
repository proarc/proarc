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

import cz.cas.lib.proarc.common.mods.custom.OriginInfoMapper.OriginInfoItem;
import cz.cas.lib.proarc.common.mods.custom.OriginInfoMapper.PeriodicityItem;
import cz.cas.lib.proarc.common.mods.custom.OriginInfoMapper.PublisherItem;
import cz.cas.lib.proarc.common.mods.custom.OriginInfoMapper.PublisherItem.Role;
import cz.fi.muni.xkremser.editor.server.mods.DateType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.OriginInfoType;
import cz.fi.muni.xkremser.editor.server.mods.PlaceTermType;
import cz.fi.muni.xkremser.editor.server.mods.PlaceType;
import cz.fi.muni.xkremser.editor.server.mods.StringPlusAuthority;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.hamcrest.core.Is;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class OriginInfoMapperTest {

    ObjectFactory factory = new ObjectFactory();

    public OriginInfoMapperTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testRead() {
        ModsType mods = new ModsType();
        mods.getModsGroup().add(originInfo(Role.PRINTER, "printer[0]", "date0", "place0"));
        mods.getModsGroup().add(originInfo(Role.PRINTER, "printer[1]", "date1", "place1"));
        mods.getModsGroup().add(originInfo(Role.OTHER, "unknown[2]", "date2", "place2"));
        mods.getModsGroup().add(originInfo(Role.PUBLISHER, "publisher[3]", "date3", "place3"));
        mods.getModsGroup().add(originInfo(Role.OTHER, "unknown[4]", null, null));
        mods.getModsGroup().add(originInfo(Role.PUBLISHER, "publisher[5]", "date5", "place5"));
        List<OriginInfoItem> expected = Arrays.<OriginInfoItem>asList(
                new PublisherItem(0, Role.PRINTER, "printer[0]", "date0", "place0"),
                new PublisherItem(1, Role.PRINTER, "printer[1]", "date1", "place1"),
                new PublisherItem(2, Role.OTHER, null, null, null),
                new PublisherItem(3, Role.PUBLISHER, "publisher[3]", "date3", "place3"),
                new PublisherItem(4, Role.OTHER, null, null, null),
                new PublisherItem(5, Role.PUBLISHER, "publisher[5]", "date5", "place5")
                );

        OriginInfoMapper instance = new OriginInfoMapper();
        List<OriginInfoItem> result = instance.map(mods);
        assertThat(result, Is.is(expected));
    }

    @Test
    public void testWrite() {
        ModsType mods = new ModsType();
        mods.getModsGroup().add(originInfo(Role.PRINTER, "printer[0]", "date0", "place0"));
        mods.getModsGroup().add(originInfo(Role.PRINTER, "printer[1]", "date1", "place1"));
        mods.getModsGroup().add(originInfo(Role.OTHER, "unknown[2]", "date2", "place2"));
        mods.getModsGroup().add(originInfo(Role.PUBLISHER, "publisher[3]", "date3", "place3"));
        mods.getModsGroup().add(originInfo(Role.OTHER, "unknown[4]", null, null));
        mods.getModsGroup().add(originInfo(Role.PUBLISHER, "publisher[5]", "date5", "place5"));
        List<PublisherItem> publishers = Arrays.asList(
                new PublisherItem(3, null, "publisher[3]-updated", "date3-updated", "place3-updated"), // update
                new PublisherItem(null, null, "publisher[insert][3-5]", "date-inserted", "place5-inserted"), // insert
                new PublisherItem(5, null, "publisher[5]", "date5", "place5")
                );
        List<PublisherItem> printers = Arrays.asList(
//                new PublisherItem(0, null, "printer[0]", "date0", "place0"), // delete
                new PublisherItem(1, null, "printer[1]", "date1", "place1"),
                new PublisherItem(null, null, "printer[add][1]", "date1", "place1") // add
                );
        List<OriginInfoItem> expected = Arrays.<OriginInfoItem>asList(
                new PeriodicityItem(0, Collections.<String>emptyList(), OriginInfoMapper.ISSUANCE_MONOGRAPHIC),
                new PublisherItem(1, Role.PUBLISHER, "publisher[3]-updated", "date3-updated", "place3-updated"),
                new PublisherItem(2, Role.PUBLISHER, "publisher[insert][3-5]", "date-inserted", "place5-inserted"),
                new PublisherItem(3, Role.PUBLISHER, "publisher[5]", "date5", "place5"),
                new PublisherItem(4, Role.PRINTER, "printer[1]", "date1", "place1"),
                new PublisherItem(5, Role.PRINTER, "printer[add][1]", "date1", "place1"),
                new PublisherItem(6, Role.OTHER, null, null, null),
                new PublisherItem(7, Role.OTHER, null, null, null)
                );
        // write
        OriginInfoMapper instance = new OriginInfoMapper();
        instance.map(mods, publishers, printers, OriginInfoMapper.ISSUANCE_MONOGRAPHIC);

        List<OriginInfoItem> result = instance.map(mods);
        assertThat(result, Is.is(expected));
    }

    @Test
    public void testReadFrequency() {
        ModsType mods = new ModsType();
        mods.getModsGroup().add(originInfo(Role.PRINTER, "printer[0]", "date0", "place0"));
        mods.getModsGroup().add(originInfo(Role.PRINTER, "printer[1]", "date1", "place1"));
        mods.getModsGroup().add(originInfo(Role.OTHER, "unknown[2]", "date2", "place2"));
        mods.getModsGroup().add(originInfo(Role.PUBLISHER, "publisher[3]", "date3", "place3"));
        mods.getModsGroup().add(originInfo(Role.OTHER, "unknown[4]", null, null));
        mods.getModsGroup().add(originInfo(Role.PUBLISHER, "publisher[5]", "date5", "place5"));
        mods.getModsGroup().add(frequency("weekly", "daily"));
        List<OriginInfoItem> expected = Arrays.asList(
                new PublisherItem(0, Role.PRINTER, "printer[0]", "date0", "place0"),
                new PublisherItem(1, Role.PRINTER, "printer[1]", "date1", "place1"),
                new PublisherItem(2, Role.OTHER, null, null, null),
                new PublisherItem(3, Role.PUBLISHER, "publisher[3]", "date3", "place3"),
                new PublisherItem(4, Role.OTHER, null, null, null),
                new PublisherItem(5, Role.PUBLISHER, "publisher[5]", "date5", "place5"),
                new PeriodicityItem(6, Arrays.asList("weekly", "daily"), OriginInfoMapper.ISSUANCE_CONTINUING)
                );

        OriginInfoMapper instance = new OriginInfoMapper();
        List<OriginInfoItem> result = instance.map(mods);
        assertThat(result, Is.is(expected));

        List<PublisherItem> resultPublishers = OriginInfoMapper.filter(result, true, Role.PUBLISHER);
        List<String> resultFreqencies = OriginInfoMapper.getFreqencies(result);

        List<PublisherItem> expectedPublishers = Arrays.asList(
                new PublisherItem(3, Role.PUBLISHER, "publisher[3]", "date3", "place3"),
                new PublisherItem(5, Role.PUBLISHER, "publisher[5]", "date5", "place5")
                );
        List<String> expectedFreqencies = Arrays.asList("weekly", "daily");
        assertThat(resultPublishers, Is.is(expectedPublishers));
        assertThat(resultFreqencies, Is.is(expectedFreqencies));
    }

    @Test
    public void testWriteWithFrequency() {
        ModsType mods = new ModsType();
        mods.getModsGroup().add(originInfo(Role.PRINTER, "printer[0]", "date0", "place0"));
        mods.getModsGroup().add(originInfo(Role.PRINTER, "printer[1]", "date1", "place1"));
        mods.getModsGroup().add(originInfo(Role.OTHER, "unknown[2]", "date2", "place2"));
        mods.getModsGroup().add(originInfo(Role.PUBLISHER, "publisher[3]", "date3", "place3"));
        mods.getModsGroup().add(originInfo(Role.OTHER, "unknown[4]", null, null));
        mods.getModsGroup().add(originInfo(Role.PUBLISHER, "publisher[5]", "date5", "place5"));
        mods.getModsGroup().add(frequency("weekly"));
        List<PublisherItem> publishers = Arrays.asList(
                new PublisherItem(3, null, "publisher[3]-updated", "date3-updated", "place3-updated"), // update
                new PublisherItem(null, null, "publisher[insert][3-5]", "date-inserted", "place5-inserted"), // insert
                new PublisherItem(5, null, "publisher[5]", "date5", "place5")
                );
        List<PublisherItem> printers = Arrays.asList(
//                new PublisherItem(0, null, "printer[0]", "date0", "place0"), // delete
                new PublisherItem(1, null, "printer[1]", "date1", "place1"),
                new PublisherItem(null, null, "printer[add][1]", "date1", "place1") // add
                );
        List<OriginInfoItem> expected = Arrays.asList(
                new PeriodicityItem(0, Arrays.asList("weekly", "daily"), OriginInfoMapper.ISSUANCE_CONTINUING),
                new PublisherItem(1, Role.PUBLISHER, "publisher[3]-updated", "date3-updated", "place3-updated"),
                new PublisherItem(2, Role.PUBLISHER, "publisher[insert][3-5]", "date-inserted", "place5-inserted"),
                new PublisherItem(3, Role.PUBLISHER, "publisher[5]", "date5", "place5"),
                new PublisherItem(4, Role.PRINTER, "printer[1]", "date1", "place1"),
                new PublisherItem(5, Role.PRINTER, "printer[add][1]", "date1", "place1"),
                new PublisherItem(6, Role.OTHER, null, null, null),
                new PublisherItem(7, Role.OTHER, null, null, null)
                );
        // write
        OriginInfoMapper instance = new OriginInfoMapper();
        instance.map(mods, publishers, printers, Arrays.asList("weekly", "daily"), OriginInfoMapper.ISSUANCE_CONTINUING);

        List<OriginInfoItem> result = instance.map(mods);
        assertThat(result, Is.is(expected));
    }

    private OriginInfoType originInfo(Role role, String name, String date, String place) {
        String srole = role == null ? null : role.getText();
        OriginInfoType o = factory.createOriginInfoType();
        o.setTransliteration(srole);
        if (date != null) {
            DateType dateElm = factory.createDateType();
            dateElm.setValue(date);
            if (Role.PUBLISHER == role) {
                o.getPlaceOrPublisherOrDateIssued().add(factory.createOriginInfoTypeDateIssued(dateElm));
            } else if (Role.PRINTER == role) {
                o.getPlaceOrPublisherOrDateIssued().add(factory.createOriginInfoTypeDateCreated(dateElm));
            }
        }
        if (place != null) {
            PlaceType placeElm = factory.createPlaceType();
            PlaceTermType placeTerm = factory.createPlaceTermType();
            placeTerm.setValue(place);
            placeElm.getPlaceTerm().add(placeTerm);
            o.getPlaceOrPublisherOrDateIssued().add(factory.createOriginInfoTypePlace(placeElm));
        }

        if (name != null) {
            o.getPlaceOrPublisherOrDateIssued().add(factory.createOriginInfoTypePublisher(name));
        }
        return o;
    }

    private OriginInfoType frequency(String... values) {
        OriginInfoType o = factory.createOriginInfoType();
        for (String value : values) {
            StringPlusAuthority spa = factory.createStringPlusAuthority();
            spa.setValue(value);
            o.getPlaceOrPublisherOrDateIssued().add(factory.createOriginInfoTypeFrequency(spa));
            o.getPlaceOrPublisherOrDateIssued().add(factory.createOriginInfoTypeIssuance(OriginInfoMapper.ISSUANCE_CONTINUING));
        }
        return o;
    }

}
