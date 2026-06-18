/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.common.actions.series;

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import java.util.TreeMap;

/**
 * @author Lukas Sykora
 */
public final class SeriesNumber {

    private static final String ALPHABET_LOWER_SERIES = "ALPHABET_LOWER_SERIES"; // a - z, aa - az, ...
    private static final String ALPHABET_UPPER_SERIES = "ALPHABET_UPPER_SERIES"; // A - Z, AA - AZ, ...
    private static final String ARABIC_SERIES = "ARABIC_SERIES"; // 1, 2, 3, 4, ...
    private static final String ROMAN_LOWER_SERIES = "ROMAN_LOWER_SERIES"; // i, ii, iii, iv, ...
    private static final String ROMAN_UPPER_SERIES = "ROMAN_UPPER_SERIES"; // I, II, III, IV, ...

    private String sequence;
    private String startNumber;
    private String incrementNumber;
    private String prefix;
    private String suffix;
    private boolean useBrackets;
    private int number;
    private int increment;


    private final static TreeMap<Integer, String> map = new TreeMap<Integer, String>();

    static {
        map.put(1000, "M");
        map.put(900, "CM");
        map.put(500, "D");
        map.put(400, "CD");
        map.put(100, "C");
        map.put(90, "XC");
        map.put(50, "L");
        map.put(40, "XL");
        map.put(10, "X");
        map.put(9, "IX");
        map.put(5, "V");
        map.put(4, "IV");
        map.put(1, "I");

    }

    public SeriesNumber(String sequence, String startNumber, String incrementNumber, String prefix, String suffix, boolean useBrackets) {
        this.sequence = sequence;
        this.startNumber = startNumber;
        this.incrementNumber = incrementNumber;
        this.prefix = prefix;
        this.suffix = suffix;
        this.number = -1;
        this.useBrackets = useBrackets;
    }

    public boolean isAllowToUpdateNumber() {
        if (number >= 0) {
            return true;
        }
        if (startNumber != null && !startNumber.isEmpty()) {
            try {
                number = Integer.parseInt(startNumber.replaceAll("[^0-9]", ""));
                return number >= 0;
            } catch (Exception ex) {
                // hodnota nelze rozparsovat, nebude se nic delat
                return false;
            }
        }
        // hodnota nenalezena, nebude se nic delat
        return false;
    }

    private void createIncrement() throws DigitalObjectException {
        if (incrementNumber != null && !incrementNumber.isEmpty()) {
            try {
                int increment = Integer.parseInt(incrementNumber.replaceAll("[^0-9]", ""));
                if (increment >= 0) {
                    this.increment = increment;
                } else {
                    this.increment = 1;
                }
            } catch (Exception ex) {
                throw new DigitalObjectException(null, "Nevyplněná hodnota o kolik se má číslo zvětšovat");
            }
        } else {
            this.increment = 1;
        }
    }

    public String getNormalizedPreffixSuffix(String value) {
        if (value != null) {
            if (value.trim().isEmpty()) {
                value = null;
            }
        }
        return value;
    }

    public String getPrefix() {
        return getNormalizedPreffixSuffix(prefix);
    }

    public String getSuffix() {
        return getNormalizedPreffixSuffix(suffix);
    }

    public String getAlhabeticalNumber(int number) {
        char[] alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray();
        if (number <= alphabet.length) {
            return Character.toString(alphabet[number - 1]);
        }
        return getAlhabeticalNumber(--number / alphabet.length) + alphabet[number % alphabet.length];
        //return number-- <= 0 ? "" : (char) ('A' + number % 26) + getAlhabeticalNumber(number / 26);
    }

    public String getRomanNumber(int number) {
        int l = map.floorKey(number);
        if (number == l) {
            return map.get(number);
        }
        return map.get(l) + getRomanNumber(number - l);
    }

    public String getNextNumber() throws DigitalObjectException {
        createIncrement();
        String returnNumber;
        switch (sequence) {
            case ALPHABET_LOWER_SERIES:
                returnNumber = getAlhabeticalNumber(number).toLowerCase();
                number += increment;
                break;
            case ALPHABET_UPPER_SERIES:
                returnNumber = getAlhabeticalNumber(number).toUpperCase();
                number += increment;
                break;
            case ROMAN_LOWER_SERIES:
                returnNumber = getRomanNumber(number).toLowerCase();
                number += increment;
                break;
            case ROMAN_UPPER_SERIES:
                returnNumber = getRomanNumber(number).toUpperCase();
                number += increment;
                break;
            default:                                        // ARABIC_SERIES
                returnNumber = String.valueOf(number);
                number += increment;
        }
        return returnNumber;
    }

    public String getNextValue() throws DigitalObjectException {
        StringBuilder retVal = new StringBuilder();
        if (useBrackets) {
            retVal.append("[");
        }
        String value = getPrefix();
        if (value != null) {
            retVal.append(value);
        }
        value = getNextNumber();
        if (value != null) {
            retVal.append(value);
        }
        value = getSuffix();
        if (value != null) {
            retVal.append(value);
        }
        if (useBrackets) {
            retVal.append("]");
        }
        return retVal.toString();

    }

}
