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
package cz.cas.lib.proarc.webapp.shared.series;

import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class AlphabetSeriesTest {

    public AlphabetSeriesTest() {
    }

    @Test
    public void testTextSequence_A_F() {
        AlphabetSeries rs = new AlphabetSeries("A", 1, true);
        String[] expectations = {"A", "B", "C", "D", "E", "F"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_f_a() {
        AlphabetSeries rs = new AlphabetSeries("f", -1, false);
        String[] expectations = {"f", "e", "d", "c", "b", "a", null};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_Y_AC() {
        AlphabetSeries rs = new AlphabetSeries("Y", 1, true);
        String[] expectations = {"Y", "Z", "AA", "AB", "AC"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_Y_AH_inc3() {
        AlphabetSeries rs = new AlphabetSeries("Y", 3, true);
        String[] expectations = {"Y", "AB", "AE", "AH"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_ac_y() {
        AlphabetSeries rs = new AlphabetSeries("ac", -1, false);
        String[] expectations = {"ac", "ab", "aa", "z", "y"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_ay_bc() {
        AlphabetSeries rs = new AlphabetSeries("ay", 1, false);
        String[] expectations = {"ay", "az", "ba", "bb", "bc"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_BC_AY() {
        AlphabetSeries rs = new AlphabetSeries("BC", -1, true);
        String[] expectations = {"BC", "BB", "BA", "AZ", "AY"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_zy_aac() {
        AlphabetSeries rs = new AlphabetSeries("zy", 1, false);
        String[] expectations = {"zy", "zz", "aaa", "aab", "aac"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_AAC_ZY() {
        AlphabetSeries rs = new AlphabetSeries("AAC", -1, true);
        String[] expectations = {"AAC", "AAB", "AAA", "ZZ", "ZY"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_zy_aae_inc2() {
        AlphabetSeries rs = new AlphabetSeries("zy", 2, false);
        String[] expectations = {"zy", "aaa", "aac", "aae"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testTextSequence_AAE_ZY_dec2() {
        AlphabetSeries rs = new AlphabetSeries("AAE", -2, true);
        String[] expectations = {"AAE", "AAC", "AAA", "ZY"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testSequence_Max_Increment() {
        AlphabetSeries rs = new AlphabetSeries("A", 26, true);
        String[] expectations = {"A", "AA", "BA", "CA"};
        checkTextSequence(expectations, rs);
    }

    @Test
    public void testSequence_Max_Decrement() {
        AlphabetSeries rs = new AlphabetSeries("CA", -26, true);
        String[] expectations = {"CA", "BA", "AA", "A"};
        checkTextSequence(expectations, rs);
    }

    private void checkTextSequence(String[] expectations, AlphabetSeries rs) {
        for (String expectation : expectations) {
            String next = rs.next();
            System.out.println(next);
            assertEquals(expectation, next);
        }
    }

    @Test
    public void testValid() {
        assertTrue(Series.validAlphabet("a"));
        assertTrue(Series.validAlphabet("A"));
        assertTrue(Series.validAlphabet("ab"));
        assertTrue(Series.validAlphabet("aB"));
    }

    @Test
    public void testInvalid() {
        assertFalse(Series.validAlphabet("1"));
        assertFalse(Series.validAlphabet("1A"));
        assertFalse(Series.validAlphabet(" ab"));
        assertFalse(Series.validAlphabet(""));
        assertFalse(Series.validAlphabet(null));
    }
}
