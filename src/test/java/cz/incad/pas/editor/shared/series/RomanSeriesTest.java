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
package cz.incad.pas.editor.shared.series;

import cz.incad.pas.editor.shared.series.RomanSeries.Roman;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class RomanSeriesTest {

    public RomanSeriesTest() {
    }

    @Test
    public void testRomanSequence_1_inc1() {
        RomanSeries rs = new RomanSeries(1, 1, true);
        String[] expectations = {"I", "II", "III", "IV", "V", "VI"};
        checkRomanSequence(expectations, rs);
    }

    @Test
    public void testRomanSequence_X_dec1() {
        RomanSeries rs = new RomanSeries("X", -1, false);
        String[] expectations = {"x", "ix", "viii", "vii", "vi", "v", "iv", "iii", "ii", "i", null};
        checkRomanSequence(expectations, rs);
    }

    @Test
    public void testRomanSequence_MML_dec2() {
        RomanSeries rs = new RomanSeries("MML", -2, true);
        String[] expectations = {"MML", "MMXLVIII", "MMXLVI", "MMXLIV", "MMXLII", "MMXL"};
        checkRomanSequence(expectations, rs);
    }

    private void checkRomanSequence(String[] expectations, RomanSeries rs) {
        for (String expectation : expectations) {
            assertEquals(expectation, rs.next());
        }
    }

    @Test
    public void testRoman2Arabic() {
        assertEquals(1, Roman.toArabic("I"));
        assertEquals(1, Roman.toArabic("i"));
        assertEquals(9, Roman.toArabic("IX"));
        assertEquals(9, Roman.toArabic("Ix"));
        assertEquals(900, Roman.toArabic("CM"));
        assertEquals(1900, Roman.toArabic("MCM"));
        assertEquals(1997, Roman.toArabic("MCMXCVII"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRoman2Arabic_Invalid_IM() {
        Roman.toArabic("IM");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRoman2Arabic_Invalid_IIII() {
        Roman.toArabic("IIII");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRoman2Arabic_Empty() {
        Roman.toArabic("");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRoman2Arabic_Null() {
        Roman.toArabic(null);
    }

    @Test
    public void testArabic2Roman() {
        for (int i = 1; i < 10000; i++) {
            String roman = Roman.toRoman(i, true);
            int arabic = Roman.toArabic(roman);
            assertEquals(i, arabic);
//            assertEquals(String.format("%s, %s, %s\n", i, roman, arabic), i, arabic);
        }
    }

    @Test
    public void testValidRoman() {
        assertTrue(Series.validRoman("I"));
        assertTrue(Series.validRoman("i"));
        assertTrue(Series.validRoman("X"));
        assertTrue(Series.validRoman("x"));
        assertTrue(Series.validRoman("L"));
        assertTrue(Series.validRoman("l"));
        assertTrue(Series.validRoman("C"));
        assertTrue(Series.validRoman("c"));
        assertTrue(Series.validRoman("D"));
        assertTrue(Series.validRoman("d"));
        assertTrue(Series.validRoman("M"));
        assertTrue(Series.validRoman("m"));
        assertTrue(Series.validRoman("II"));
        assertTrue(Series.validRoman("III"));
        assertTrue(Series.validRoman("IV"));
        assertTrue(Series.validRoman("V"));
        assertTrue(Series.validRoman("VI"));
        assertTrue(Series.validRoman("VII"));
        assertTrue(Series.validRoman("VIII"));
        assertTrue(Series.validRoman("MMMM"));
    }
    @Test
    public void testInvalidRoman() {
        assertFalse(Series.validRoman(""));
        assertFalse(Series.validRoman(null));
        assertFalse(Series.validRoman("a"));
        assertFalse(Series.validRoman("IA"));
        assertFalse(Series.validRoman("IIII"));
        assertFalse(Series.validRoman("CCCC"));
        assertFalse(Series.validRoman("IM"));
        assertFalse(Series.validRoman("IXM"));
        assertFalse(Series.validRoman("IXC"));
        assertFalse(Series.validRoman("IXD"));
        assertFalse(Series.validRoman("IXIX"));
    }
}
