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

import cz.cas.lib.proarc.common.mods.custom.ArrayMapper.ArrayItem;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Helper to handle arrays of the same XML element. The helper is necessary
 * in order to update existing objects properly without loss of data
 * that are not part of edited (mapped) objects.
 *
 * @author Jan Pokorsky
 */
final class ArrayMapper<S, T extends ArrayItem> {

    private final ItemMapper<S, T> itemMapper;

    public ArrayMapper(ItemMapper<S, T> mapper) {
        this.itemMapper = mapper;
    }

    public List<T> map(List<S> l) {
        List<T> result = new ArrayList<T>(l.size());
        int index = 0;
        for (S source : l) {
            T target = itemMapper.map(source);
            target.setArrayIndex(index++);
            result.add(target);
        }
        return result;
    }

    public List<S> map(List<T> newValues, List<S> oldValues) {
        newValues = MapperUtils.noNull(newValues);
        oldValues = MapperUtils.noNull(oldValues);
        List<S> result = new ArrayList<S>(newValues.size());
        for (T item : newValues) {
            Integer index = item.getArrayIndex();
            S origin = null;
            if (index != null && index >= 0) {
                origin = oldValues.get(index);
            }
            S source = itemMapper.map(item, origin);
            result.add(source);
        }
        return result;
    }

    public static ArrayMapper<String, StringItem> stringMapper() {
        return new ArrayMapper<String, StringItem>(new StringMapper());
    }

    public static List<StringItem> toStringItemList(List<String> strings) {
        strings = MapperUtils.noNull(strings);
        ArrayList<StringItem> items = new ArrayList<StringItem>(strings.size());
        for (String s : strings) {
            items.add(new StringItem(s));
        }
        return items;
    }

    public static List<String> toStringList(List<StringItem> items) {
        items = MapperUtils.noNull(items);
        ArrayList<String> strings = new ArrayList<String>(items.size());
        for (StringItem item : items) {
            strings.add(item.getValue());
        }
        return strings;
    }

    public interface ArrayItem {

        Integer getArrayIndex();

        void setArrayIndex(Integer index);

    }

    public interface ItemMapper<S, T extends ArrayItem> {

        T map(S source);
        S map(T item, S origin);
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static final class StringItem implements ArrayItem {

        @XmlElement(name = ModsConstants.FIELD_STRING_VALUE)
        private String value;
        private Integer index;

        public StringItem(String value, Integer index) {
            this.value = value;
            this.index = index;
        }

        public StringItem(String value) {
            this(value, null);
        }

        public StringItem() {
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }

        @Override
        public Integer getArrayIndex() {
            return index;
        }

        @Override
        public void setArrayIndex(Integer index) {
            this.index = index;
        }

        @Override
        public String toString() {
            return String.format("StringItem{value=%s, index=%s}", value, index);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final StringItem other = (StringItem) obj;
            if ((this.value == null) ? (other.value != null) : !this.value.equals(other.value)) {
                return false;
            }
            if (this.index != other.index && (this.index == null || !this.index.equals(other.index))) {
                return false;
            }
            return true;
        }

    }

    private static final class StringMapper implements ItemMapper<String, StringItem> {

        @Override
        public StringItem map(String source) {
            return new StringItem(source);
        }

        @Override
        public String map(StringItem item, String origin) {
            return item.getValue();
        }
        
    }

}
