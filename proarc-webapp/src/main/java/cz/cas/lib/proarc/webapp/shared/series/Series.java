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

import com.google.gwt.regexp.shared.RegExp;
//import java.util.regex.Pattern;

/**
 *
 * @author Jan Pokorsky
 */
public final class Series {

    public static final String ALPHABET_REGEXP = "^[a-zA-Z]*$";
    public static final String ROMAN_REGEXP = "^M{0,10}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$";

    private static final RegExp ALPHABET_PATTERN = RegExp.compile(ALPHABET_REGEXP);
//    private static final Pattern ALPHABET_PATTERN = Pattern.compile(ALPHABET_REGEXP);
    private static final RegExp ROMAN_PATTERN = RegExp.compile(Series.ROMAN_REGEXP, "i");
//    private static final Pattern ROMAN_PATTERN = Pattern.compile(ROMAN_REGEXP, Pattern.CASE_INSENSITIVE);

    public static Iterable<String> alphabet(String start, int increment, boolean uppercase) {
        return new AlphabetSeries(start, increment, uppercase);
    }

    public static Iterable<String> arabic(int start, int increment) {
        return new ArabicSeries(start, increment);
    }

    public static Iterable<String> roman(int start, int increment, boolean uppercase) {
        return new RomanSeries(start, increment, uppercase);
    }

    public static Iterable<String> roman(String startAsRoman, int increment, boolean uppercase) {
        return new RomanSeries(startAsRoman, increment, uppercase);
    }

    public static boolean validRoman(String roman) {
        if (roman == null || roman.isEmpty()) {
            return false;
        }
        return ROMAN_PATTERN.test(roman);
//        return ROMAN_PATTERN.matcher(roman).matches();
    }

    public static boolean validAlphabet(String alphabet) {
        if (alphabet == null || alphabet.isEmpty()) {
            return false;
        }
        return ALPHABET_PATTERN.test(alphabet);
    }
}
