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
package cz.incad.pas.editor.server.mods.custom;

import cz.fi.muni.xkremser.editor.server.mods.IdentifierType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.pas.editor.client.ds.mods.IdentifierDataSource;
import cz.incad.pas.editor.server.mods.custom.ArrayMapper.ArrayItem;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 *
 * @author Jan Pokorsky
 */
final class IdentifierMapper {
    private final ArrayMapper<IdentifierType, IdentifierItem> mapper =
            new ArrayMapper<IdentifierType, IdentifierItem>(new IdentifierItemMapper());

    public List<IdentifierItem> map(ModsType mods) {
        List<Object> modsGroup = mods.getModsGroup();
        List<IdentifierType> identifiers = MapperUtils.find(modsGroup, IdentifierType.class);
        return mapper.map(identifiers);
    }

    public ModsType map(ModsType mods, List<IdentifierItem> items) {
        items = MapperUtils.noNull(items);
        List<Object> modsGroup = mods.getModsGroup();
        List<IdentifierType> identifiers = MapperUtils.find(modsGroup, IdentifierType.class);
        List<IdentifierType> mapped = mapper.map(items, identifiers);
        MapperUtils.update(modsGroup, mapped, IdentifierType.class);
        return mods;
    }

    private static final class IdentifierItemMapper implements ArrayMapper.ItemMapper<IdentifierType, IdentifierItem> {

        @Override
        public IdentifierItem map(IdentifierType source) {
            return new IdentifierItem(source.getType(), source.getValue());
        }

        @Override
        public IdentifierType map(IdentifierItem item, IdentifierType origin) {
            if (origin == null) {
                origin = new IdentifierType();
            }
            origin.setValue(item.getValue());
            origin.setType(item.getType());
            return origin;
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class IdentifierItem implements ArrayItem {

        @XmlElement(name = IdentifierDataSource.FIELD_TYPE)
        private String type;
        @XmlElement(name = IdentifierDataSource.FIELD_VALUE)
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
