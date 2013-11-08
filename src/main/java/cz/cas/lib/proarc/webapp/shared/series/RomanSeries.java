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

import java.util.Iterator;

/**
 * Roman numerals.
 *
 * @author Jan Pokorsky
 */
final class RomanSeries implements Iterator<String>, Iterable<String> {

    private final int start;
    private final int increment;
    private Integer current;
    private final boolean uppercase;

    public RomanSeries(String startAsRoman, int increment, boolean uppercase) {
        this(Roman.toArabic(startAsRoman), increment, uppercase);
    }

    public RomanSeries(int start, int increment, boolean uppercase) {
        if (start < 1) {
            throw new IllegalArgumentException("start < 1: " + start);
        }
        this.start = start;
        this.increment = increment;
        this.uppercase = uppercase;
    }

    @Override
    public String next() {
        if (current == null) {
            current = start;
        } else {
            current += increment;
        }
        return current > 0
                ? Roman.toRoman(current, uppercase)
                : null;
    }

    @Override
    public boolean hasNext() {
        return true;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Iterator<String> iterator() {
        return current == null ? this : new RomanSeries(start, increment, uppercase);
    }
    
    enum Roman {

        M(1000), CM(900), D(500), CD(400), C(100), XC(90), L(50), XL(40),
        X(10), IX(9), V(5), IV(4), I(1);

        private final int decimal;

        Roman(int decimal) {
            this.decimal = decimal;
        }

        public int getDecimal() {
            return decimal;
        }

        public static String toRoman(int decimalValue) {
            return toRoman(decimalValue, true);
        }

        public static String toRoman(int decimalValue, boolean upperCase) {
            if (decimalValue < 1) {
                throw new IllegalArgumentException("decimalValue: " + decimalValue);
            }
            StringBuilder result = new StringBuilder();
            for (Roman roman : values()) {
                int i = decimalValue / roman.getDecimal();
                for (int j = 0; j < i; j++) {
                    result.append(upperCase ? roman.name() : roman.name().toLowerCase());
                }
                decimalValue %= roman.getDecimal();
            }
            return result.toString();
        }

        public static int toArabic(String romanValue) {
            if (!Series.validRoman(romanValue)) {
                throw new IllegalArgumentException("romanValue: " + romanValue);
            }

            romanValue = romanValue.toUpperCase();
            int arabic = 0;

            Roman next = null;
            for (int i = 0; i < romanValue.length(); i++) {
                Roman current = (next == null)
                        ? Roman.valueOf(String.valueOf(romanValue.charAt(i)))
                        : next;
                next = (i + 1 < romanValue.length())
                        ? Roman.valueOf(String.valueOf(romanValue.charAt(i + 1)))
                        : null;

                if (next == null || current.getDecimal() >= next.getDecimal()) {
                    arabic += current.getDecimal();
                } else {
                    current = Roman.valueOf(current.name() + next.name());
                    arabic += current.getDecimal();
                    next = null;
                    i++;
                }
            }

            return arabic;
        }

    }

}
