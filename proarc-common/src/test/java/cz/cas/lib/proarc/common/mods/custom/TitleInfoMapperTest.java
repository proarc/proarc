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
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 * @author Jan Pokorsky
 */
public class TitleInfoMapperTest {

    private static final String XML =
            "  <mods xmlns='http://www.loc.gov/mods/v3'>"
                    + "<titleInfo type='alternative'><title>ATITLE[0][0]</title><title>ATITLE[0][1]</title></titleInfo>"
                    + "<titleInfo><title>MTITLE[1][0]</title><subTitle>STITLE[1][0]</subTitle></titleInfo>"
                    + "<titleInfo type='alternative' displayLabel='Klíčový název'><title>KTITLE[2][0]</title></titleInfo>"
                    + "<titleInfo type='uniform'><title>UNSUPPORTED</title></titleInfo>"
                    + "</mods>";

    @Test
    public void testSetTitles() {
        List<String> newTitles = Arrays.asList("MTITLE[1][0]-update");
        List<String> newSubtitles = Arrays.asList("STITLE[1][new[0]]", "STITLE[1][0]", "STITLE[1][new[2]]");
        List<String> newAltTitles = Arrays.asList("ATITLE[0][new[0]]");
        List<String> newKeyTitles = Arrays.asList("KTITLE[2][0]-update", "KTITLE[2][new[1]]");
        ModsDefinition mods = ModsUtils.unmarshal(XML, ModsDefinition.class);
        TitleInfoMapper instance = new TitleInfoMapper(mods);

        instance.setTitles(newTitles, newSubtitles);
        instance.setKeyTitles(newKeyTitles);
        instance.setAlternativeTitles(newAltTitles);

        TitleInfoMapper read = new TitleInfoMapper(mods);
        List<String> titleResult = read.getTitles();
        List<String> subtitleResult = read.getSubtitles();
        List<String> alternativeResult = read.getAlternativeTitles();
        List<String> keyResult = read.getKeyTitles();

        List<String> titleExpected = Arrays.asList("MTITLE[1][0]-update");
        List<String> subtitleExpected = Arrays.asList("STITLE[1][new[0]]", "STITLE[1][0]", "STITLE[1][new[2]]");
        List<String> alternativeExpected = Arrays.asList("ATITLE[0][new[0]]");
        List<String> keyExpected = Arrays.asList("KTITLE[2][0]-update", "KTITLE[2][new[1]]");

        assertEquals(titleResult, titleExpected);
        assertEquals(subtitleResult, subtitleExpected);
        assertEquals(alternativeResult, alternativeExpected);
        assertEquals(keyResult, keyExpected);
        System.out.println(ModsUtils.toXml(mods, true));
    }

    @Test
    public void testGetTitles() {
        ModsDefinition mods = ModsUtils.unmarshal(XML, ModsDefinition.class);
        TitleInfoMapper instance = new TitleInfoMapper(mods);

        List<String> titleResult = instance.getTitles();
        List<String> subtitleResult = instance.getSubtitles();
        List<String> alternativeResult = instance.getAlternativeTitles();
        List<String> keyResult = instance.getKeyTitles();

        List<String> titleExpected = Arrays.asList("MTITLE[1][0]");
        List<String> subtitleExpected = Arrays.asList("STITLE[1][0]");
        List<String> alternativeExpected = Arrays.asList("ATITLE[0][0]", "ATITLE[0][1]");
        List<String> keyExpected = Arrays.asList("KTITLE[2][0]");

        assertEquals(titleResult, titleExpected);
        assertEquals(subtitleResult, subtitleExpected);
        assertEquals(alternativeResult, alternativeExpected);
        assertEquals(keyResult, keyExpected);
    }

    @Test
    public void testGetEmptyTitles() {
        TitleInfoMapper instance = new TitleInfoMapper(new ModsDefinition());

        List<String> titleResult = instance.getTitles();
        List<String> subtitleResult = instance.getSubtitles();
        List<String> alternativeResult = instance.getAlternativeTitles();
        List<String> keyResult = instance.getKeyTitles();

        List<String> titleExpected = Collections.emptyList();
        List<String> subtitleExpected = Collections.emptyList();
        List<String> alternativeExpected = Collections.emptyList();
        List<String> keyExpected = Collections.emptyList();

        assertEquals(titleResult, titleExpected);
        assertEquals(subtitleResult, subtitleExpected);
        assertEquals(alternativeResult, alternativeExpected);
        assertEquals(keyResult, keyExpected);
    }

}
