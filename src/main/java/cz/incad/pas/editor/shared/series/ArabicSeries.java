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
 * Arabic numerals.
 *
 * @author Jan Pokorsky
 */
final class ArabicSeries implements Iterator<String>, Iterable<String> {
    private final int start;
    private final int increment;
    private int counter;

    public ArabicSeries(int start, int increment) {
        this.start = start;
        this.increment = increment;
        counter = start;
    }

    @Override
    public boolean hasNext() {
        return true;
    }

    @Override
    public String next() {
        int result = counter;
        counter += increment;
        return String.valueOf(result);
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Iterator<String> iterator() {
        return counter == start ? this : new ArabicSeries(start, increment);
    }

}
