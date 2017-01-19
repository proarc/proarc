/*
 * Copyright (C) 2017 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.action;

import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Helper to cache selection of items in a proper order.
 *
 * @author Jan Pokorsky
 */
public class SelectionCache<T> {

    private T[] selection;
    private final Optional<Supplier<T[]>> selector;
    private final Function<T,Integer> indexer;

    public static SelectionCache<ListGridRecord> selector(ListGrid lg) {
        return new SelectionCache<>(Optional.of(() -> lg.getSelectedRecords()), r -> lg.getRecordIndex(r));
    }

    /**
     * The constructor.
     * @param selector an optional selector that supplies an array of selected items
     * @param indexer a mapper of items to indices
     */
    public SelectionCache(Optional<Supplier<T[]>> selector, Function<T,Integer> indexer) {
        this.selector = selector;
        this.indexer = indexer;
    }

    public T[] getSelection() {
        return selection;
    }

    public T[] setSelection() {
        return selector.map(s -> setSelection(s.get())).orElse(null);
    }

    public T[] setSelection(T[] selection) {
        this.selection = sortSelection(selection);
        return this.selection;
    }

    private T[] sortSelection(T[] items) {
        if (items != null && items.length > 1) {
            ArrayList<Entry<T>> entries = new ArrayList<>(items.length);
            boolean isOrdered = true;
            int prevIdx = -1;
            for (T item : items) {
                Entry<T> entry = new Entry<>(indexer.apply(item), item);
                entries.add(entry);
                isOrdered &= prevIdx < entry.index;
                prevIdx = entry.index;
            }
            if (!isOrdered) {
                Collections.sort(entries);
                int i = 0;
                for (Entry<T> entry : entries) {
                    items[i++] = entry.object;
                }
            }
        }
        return items;
    }

    private static class Entry<T> implements Comparable<Entry<T>> {
        private int index;
        private T object;

        public Entry(int index, T object) {
            this.index = index;
            this.object = object;
        }

        @Override
        public int compareTo(Entry<T> o) {
            return index - o.index;
        }

    }

}
