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

import cz.cas.lib.proarc.common.mods.custom.ArrayMapper.ItemMapper;
import cz.cas.lib.proarc.common.mods.custom.ClassificationMapper.ClassificationItem.Type;
import cz.cas.lib.proarc.mods.ClassificationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * UDC, DDC mapper.
 *
 * <pre>{@code
     <classification authority="ddc">123</classification>
     <classification authority="udc">321</classification>
 }</pre>
 * This should fetch pairs of DDC and UDC classifications in arbitrary order.
 *
 * @author Jan Pokorsky
 */
final class ClassificationMapper {

    private final ArrayMapper<ClassificationDefinition, ClassificationItem> classificationMap =
            new ArrayMapper<>(new ClassificationItemMapper());

    public List<ClassificationPair> mapPairs(ModsDefinition mods) {
        List<ClassificationItem> items = map(mods);
        return toPairs(items);
    }

    public ModsDefinition mapPairs(ModsDefinition mods, List<ClassificationPair> pairs) {
        pairs = MapperUtils.noNull(pairs);
        List<ClassificationItem> items = map(mods);
        List<ClassificationItem> others = filter(items, true, Type.OTHER);
        List<ClassificationItem> newItems = toItems(pairs);
        newItems.addAll(others);
        map(mods, newItems);
        return mods;
    }

    public List<ClassificationItem> map(ModsDefinition mods) {
        List<ClassificationDefinition> classifications = mods.getClassification();
        return classificationMap.map(classifications);
    }

    public ModsDefinition map(ModsDefinition mods, List<ClassificationItem> items) {
        List<ClassificationDefinition> oldies = mods.getClassification();
        List<ClassificationDefinition> news = classificationMap.map(items, oldies);
        oldies.clear();
        oldies.addAll(news);
        return mods;
    }

    static List<ClassificationItem> toItems(List<ClassificationPair> pairs) {
        List<ClassificationItem> items = new ArrayList<>(2 * pairs.size());
        for (ClassificationPair pair : pairs) {
            items.add(new ClassificationItem(pair.getDdcIndex(), Type.DDC, pair.getDdc()));
            items.add(new ClassificationItem(pair.getUdcIndex(), Type.UDC, pair.getUdc()));
        }
        return items;
    }

    static List<ClassificationPair> toPairs(List<ClassificationItem> items) {
        List<ClassificationPair> pairs = new ArrayList<>();
        ClassificationPair pair = null;
        Type previousType = null;
        for (ClassificationItem item : items) {
            if (previousType == item.getType()) {
                pair = null;
            }
            switch (item.getType()) {
                case DDC:
                    if (pair == null) {
                        pair = new ClassificationPair();
                        pair.setDdc(item.getValue());
                        pair.setDdcIndex(item.getArrayIndex());
                        pairs.add(pair);
                    } else {
                        pair.setDdc(item.getValue());
                        pair.setDdcIndex(item.getArrayIndex());
                        pair = null;
                    }
                    break;
                case UDC:
                    if (pair == null) {
                        pair = new ClassificationPair();
                        pair.setUdc(item.getValue());
                        pair.setUdcIndex(item.getArrayIndex());
                        pairs.add(pair);
                    } else {
                        pair.setUdc(item.getValue());
                        pair.setUdcIndex(item.getArrayIndex());
                        pair = null;
                    }
                    break;
                default:
                    pair = null;
            }
            previousType = item.getType();
        }

        return pairs;
    }

    static List<ClassificationItem> filter(List<ClassificationItem> items, boolean include, Type first, Type... rest) {
        return filter(items, EnumSet.of(first, rest), include);
    }

    static List<ClassificationItem> filter(List<ClassificationItem> items, Set<Type> filter, boolean include) {
        List<ClassificationItem> result = new ArrayList<>();

        for (ClassificationItem name : items) {
            boolean contains = filter.contains(name.getType());
            if ((include == true && include == contains) || (include == false && include == contains)) {
                result.add(name);
            }
        }
        return result;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ClassificationPair {
        
        @XmlElement(name = ModsConstants.FIELD_CLASSIFICATION_DDC)
        private String ddc;
        @XmlElement(name = ModsConstants.FIELD_CLASSIFICATION_UDC)
        private String udc;
        private Integer ddcIndex;
        private Integer udcIndex;

        public ClassificationPair() {
        }

        ClassificationPair(String ddc, Integer ddcIndex, String udc, Integer udcIndex) {
            this.ddc = ddc;
            this.udc = udc;
            this.ddcIndex = ddcIndex;
            this.udcIndex = udcIndex;
        }

        public String getDdc() {
            return ddc;
        }

        public void setDdc(String ddc) {
            this.ddc = ddc;
        }

        public Integer getDdcIndex() {
            return ddcIndex;
        }

        public void setDdcIndex(Integer ddcIndex) {
            this.ddcIndex = ddcIndex;
        }

        public String getUdc() {
            return udc;
        }

        public void setUdc(String udc) {
            this.udc = udc;
        }

        public Integer getUdcIndex() {
            return udcIndex;
        }

        public void setUdcIndex(Integer udcIndex) {
            this.udcIndex = udcIndex;
        }

        @Override
        public String toString() {
            return String.format("ClassificationPair{ddc:[v: %s, i: %s], udc:[v: %s, i: %s]}",
                    ddc, ddcIndex, udc, udcIndex);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final ClassificationPair other = (ClassificationPair) obj;
            if ((this.ddc == null) ? (other.ddc != null) : !this.ddc.equals(other.ddc)) {
                return false;
            }
            if ((this.udc == null) ? (other.udc != null) : !this.udc.equals(other.udc)) {
                return false;
            }
            if (this.ddcIndex != other.ddcIndex && (this.ddcIndex == null || !this.ddcIndex.equals(other.ddcIndex))) {
                return false;
            }
            if (this.udcIndex != other.udcIndex && (this.udcIndex == null || !this.udcIndex.equals(other.udcIndex))) {
                return false;
            }
            return true;
        }

    }

    private static final class ClassificationItemMapper implements ItemMapper<ClassificationDefinition, ClassificationItem> {

        private final ObjectFactory factory = new ObjectFactory();

        @Override
        public ClassificationItem map(ClassificationDefinition source) {
            ClassificationItem result = new ClassificationItem();
            Type type = Type.from(source.getAuthority());
            result.setType(type);
            if (type != Type.OTHER) {
                result.setValue(source.getValue());
            }
            
            return result;
        }

        @Override
        public ClassificationDefinition map(ClassificationItem item, ClassificationDefinition origin) {
            if (item.getType() == Type.OTHER) {
                return origin;
            }
            ClassificationDefinition source = origin;
            if (origin == null) {
                source = factory.createClassificationDefinition();
                source.setAuthority(item.getType().getText());
            }

            source.setValue(item.getValue());
            return source;
        }

    }

    public static class ClassificationItem implements ArrayMapper.ArrayItem {

        private String value;
        private Type type;
        private Integer index;

        public ClassificationItem() {
        }

        public ClassificationItem(Integer index, Type type, String value) {
            this.value = value;
            this.type = type;
            this.index = index;
        }

        public Type getType() {
            return type;
        }

        public void setType(Type type) {
            this.type = type;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = MapperUtils.normalize(value);
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
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final ClassificationItem other = (ClassificationItem) obj;
            if ((this.value == null) ? (other.value != null) : !this.value.equals(other.value)) {
                return false;
            }
            if ((this.type == null) ? (other.type != null) : !this.type.equals(other.type)) {
                return false;
            }
            if (this.index != other.index && (this.index == null || !this.index.equals(other.index))) {
                return false;
            }
            return true;
        }

        @Override
        public String toString() {
            return String.format("ClassificationItem{type: %s, value: %s, index: %s}",
                    type, value, index);
        }

        public enum Type {
            DDC("ddc"), UDC("udc"), OTHER("ClassificationItem.OTHER");
            private String text;

            private Type(String text) {
                this.text = text;
            }

            public String getText() {
                return text;
            }

            public static Type from(String s) {
                for (Type type : values()) {
                    if (type.getText().equals(s)) {
                        return type;
                    }
                }
                return OTHER;
            }

        }
    }
}
