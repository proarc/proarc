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
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.IssuanceDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusSupplied;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 * @author Jan Pokorsky
 */
public class OriginInfoMapperTest {

    ObjectFactory factory = new ObjectFactory();

    public OriginInfoMapperTest() {
    }

    @BeforeAll
    public static void setUpClass() throws Exception {
    }

    @AfterAll
    public static void tearDownClass() throws Exception {
    }

    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testRead() {
        ModsDefinition mods = new ModsDefinition();
        mods.getOriginInfo().add(originInfo(Role.PRINTER, "printer[0]", "date0", "place0"));
        mods.getOriginInfo().add(originInfo(Role.PRINTER, "printer[1]", "date1", "place1"));
        mods.getOriginInfo().add(originInfo(Role.OTHER, "unknown[2]", "date2", "place2"));
        mods.getOriginInfo().add(originInfo(Role.PUBLISHER, "publisher[3]", "date3", "place3"));
        mods.getOriginInfo().add(originInfo(Role.OTHER, "unknown[4]", null, null));
        mods.getOriginInfo().add(originInfo(Role.PUBLISHER, "publisher[5]", "date5", "place5"));
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
        assertEquals(result, expected);
    }

    @Test
    public void testWrite() {
        ModsDefinition mods = new ModsDefinition();
        mods.getOriginInfo().add(originInfo(Role.PRINTER, "printer[0]", "date0", "place0"));
        mods.getOriginInfo().add(originInfo(Role.PRINTER, "printer[1]", "date1", "place1"));
        mods.getOriginInfo().add(originInfo(Role.OTHER, "unknown[2]", "date2", "place2"));
        mods.getOriginInfo().add(originInfo(Role.PUBLISHER, "publisher[3]", "date3", "place3"));
        mods.getOriginInfo().add(originInfo(Role.OTHER, "unknown[4]", null, null));
        mods.getOriginInfo().add(originInfo(Role.PUBLISHER, "publisher[5]", "date5", "place5"));
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
        assertEquals(result, expected);
    }

    @Test
    public void testReadFrequency() {
        ModsDefinition mods = new ModsDefinition();
        mods.getOriginInfo().add(originInfo(Role.PRINTER, "printer[0]", "date0", "place0"));
        mods.getOriginInfo().add(originInfo(Role.PRINTER, "printer[1]", "date1", "place1"));
        mods.getOriginInfo().add(originInfo(Role.OTHER, "unknown[2]", "date2", "place2"));
        mods.getOriginInfo().add(originInfo(Role.PUBLISHER, "publisher[3]", "date3", "place3"));
        mods.getOriginInfo().add(originInfo(Role.OTHER, "unknown[4]", null, null));
        mods.getOriginInfo().add(originInfo(Role.PUBLISHER, "publisher[5]", "date5", "place5"));
        mods.getOriginInfo().add(frequency("weekly", "daily"));
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
        assertEquals(result, expected);

        List<PublisherItem> resultPublishers = OriginInfoMapper.filter(result, true, Role.PUBLISHER);
        List<String> resultFreqencies = OriginInfoMapper.getFreqencies(result);

        List<PublisherItem> expectedPublishers = Arrays.asList(
                new PublisherItem(3, Role.PUBLISHER, "publisher[3]", "date3", "place3"),
                new PublisherItem(5, Role.PUBLISHER, "publisher[5]", "date5", "place5")
        );
        List<String> expectedFreqencies = Arrays.asList("weekly", "daily");
        assertEquals(resultPublishers, expectedPublishers);
        assertEquals(resultFreqencies, expectedFreqencies);
    }

    @Test
    public void testWriteWithFrequency() {
        ModsDefinition mods = new ModsDefinition();
        mods.getOriginInfo().add(originInfo(Role.PRINTER, "printer[0]", "date0", "place0"));
        mods.getOriginInfo().add(originInfo(Role.PRINTER, "printer[1]", "date1", "place1"));
        mods.getOriginInfo().add(originInfo(Role.OTHER, "unknown[2]", "date2", "place2"));
        mods.getOriginInfo().add(originInfo(Role.PUBLISHER, "publisher[3]", "date3", "place3"));
        mods.getOriginInfo().add(originInfo(Role.OTHER, "unknown[4]", null, null));
        mods.getOriginInfo().add(originInfo(Role.PUBLISHER, "publisher[5]", "date5", "place5"));
        mods.getOriginInfo().add(frequency("weekly"));
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
        assertEquals(result, expected);
    }

    private OriginInfoDefinition originInfo(Role role, String name, String date, String place) {
        String srole = role == null ? null : role.getText();
        OriginInfoDefinition o = factory.createOriginInfoDefinition();
        o.setTransliteration(srole);
        if (date != null) {
            DateDefinition dateElm = factory.createDateDefinition();
            dateElm.setValue(date);
            if (Role.PUBLISHER == role) {
                o.getDateIssued().add(dateElm);
            } else if (Role.PRINTER == role) {
                o.getDateCreated().add(dateElm);
            }
        }
        if (place != null) {
            PlaceDefinition placeElm = factory.createPlaceDefinition();
            PlaceTermDefinition placeTerm = factory.createPlaceTermDefinition();
            placeTerm.setValue(place);
            placeElm.getPlaceTerm().add(placeTerm);
            o.getPlace().add(placeElm);
        }

        if (name != null) {
            StringPlusLanguagePlusSupplied spl = factory.createStringPlusLanguagePlusSupplied();
            spl.setValue(name);
            o.getPublisher().add(spl);
        }
        return o;
    }

    private OriginInfoDefinition frequency(String... values) {
        OriginInfoDefinition o = factory.createOriginInfoDefinition();
        for (String value : values) {
            StringPlusLanguagePlusAuthority spa = factory.createStringPlusLanguagePlusAuthority();
            spa.setValue(value);
            o.getFrequency().add(spa);

            o.getIssuance().add(IssuanceDefinition.CONTINUING);
        }
        return o;
    }

}
