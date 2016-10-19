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
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 *
 * @author Jan Pokorsky
 */
public final class IdentifierMapper {
    private final ArrayMapper<IdentifierDefinition, IdentifierItem> mapper =
            new ArrayMapper<>(new IdentifierItemMapper());

    public List<IdentifierItem> map(ModsDefinition mods) {
        List<IdentifierDefinition> identifiers = mods.getIdentifier();
        return mapper.map(identifiers);
    }

    public ModsDefinition map(ModsDefinition mods, List<IdentifierItem> items) {
        items = MapperUtils.noNull(items);
        List<IdentifierDefinition> identifiers = mods.getIdentifier();
        List<IdentifierDefinition> mapped = mapper.map(items, identifiers);
        identifiers.clear();
        identifiers.addAll(mapped);
        return mods;
    }

    private static final class IdentifierItemMapper implements ArrayMapper.ItemMapper<IdentifierDefinition, IdentifierItem> {

        @Override
        public IdentifierItem map(IdentifierDefinition source) {
            return new IdentifierItem(source.getType(), source.getValue());
        }

        @Override
        public IdentifierDefinition map(IdentifierItem item, IdentifierDefinition origin) {
            if (origin == null) {
                origin = new IdentifierDefinition();
            }
            origin.setValue(item.getValue());
            origin.setType(item.getType());
            return origin;
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class IdentifierItem implements ArrayItem {

        @XmlElement(name = ModsConstants.FIELD_IDENTIFIER_TYPE)
        private String type;
        @XmlElement(name = ModsConstants.FIELD_IDENTIFIER_VALUE)
        private String value;
        private Integer index;

        public IdentifierItem(Integer index, String type, String value) {
            this.index = index;
            this.type = type;
            this.value = value;
        }

        public IdentifierItem(String type, String value) {
            this(null, type, value);
        }

        public IdentifierItem() {
        }

        @Override
        public String toString() {
            return String.format("IdentifierItem{type: %s, val: %s, index: %s}", type, value, index);
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
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
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final IdentifierItem other = (IdentifierItem) obj;
            if (this.index != other.index && (this.index == null || !this.index.equals(other.index))) {
                return false;
            }
            if ((this.type == null) ? (other.type != null) : !this.type.equals(other.type)) {
                return false;
            }
            if ((this.value == null) ? (other.value != null) : !this.value.equals(other.value)) {
                return false;
            }
            return true;
        }

    }

}
