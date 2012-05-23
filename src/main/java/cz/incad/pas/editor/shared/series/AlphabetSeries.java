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

import java.util.Iterator;

/**
 * Base-26 series.
 * A, B, ..., Z, AA, AB, ..., AZ, BA, BB, ..., ZZ, AAA
 *
 * @author Jan Pokorsky
 */
final class AlphabetSeries implements Iterator<String>, Iterable<String> {

    private final String start;
    private final int increment;
    private StringBuilder current;
    private final boolean uppercase;
    private final char A;
    private final char Z;

    public AlphabetSeries(String start, int increment, boolean uppercase) {
        if (!Series.validAlphabet(start)) {
            throw new IllegalArgumentException("start: " + start);
        }
        if (start == null || start.isEmpty()) {
            start = String.valueOf(getFirst(uppercase));
        } else {
            start = uppercase ? start.toUpperCase() : start.toLowerCase();
        }

        if (Math.abs(increment) > getLast(uppercase) - getFirst(uppercase) + 1) {
            // for now increment must be max abs(26)
            throw new IllegalArgumentException("increment: " + increment);
        }
        this.start = start;
        this.increment = increment;
        this.uppercase = uppercase;
        this.A = getFirst(uppercase);
        this.Z = getLast(uppercase);
    }

    static boolean isValid(String start) {
        return Series.validAlphabet(start);
    }

    private static char getFirst(boolean uppercase) {
        return uppercase ? 'A' : 'a';
    }

    private static char getLast(boolean uppercase) {
        return uppercase ? 'Z' : 'z';
    }

    private void increment() {
        assert increment > 0;
        int overflow = increment;
        for (int i = current.length() - 1; i >= 0; i--) {
            char c = (char) (current.charAt(i) + overflow);
            if (c > Z) {
                c = (char) (A + c - Z - 1);
                overflow = 1;
                current.setCharAt(i, c);
            } else {
                overflow = 0;
                current.setCharAt(i, c);
                break;
            }
        }
        if (overflow > 0) {
            current.insert(0, A);
        }
    }

    public String decrement() {
        assert increment < 0;
        int overflow = increment;
        for (int i = current.length() - 1; i >= 0; i--) {
            char c = (char) (current.charAt(i) + overflow);
            if (c < A) {
                c = (char) (Z - (A - c - 1));
                overflow = -1;
                current.setCharAt(i, c);
            } else {
                overflow = 0;
                current.setCharAt(i, c);
                break;
            }
        }
        if (overflow < 0) {
            current.deleteCharAt(0);
        }
        return current.toString();
    }

    @Override
    public String next() {
        if (current == null) {
            current = new StringBuilder(start);
        } else {
            if (increment > 0) {
                increment();
            } else if (increment < 0 && current.length() > 0) {
                decrement();
            }
        }
        return current.length() > 0 ? current.toString() : null;
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
        return current == null ? this : new AlphabetSeries(start, increment, uppercase);
    }

}
