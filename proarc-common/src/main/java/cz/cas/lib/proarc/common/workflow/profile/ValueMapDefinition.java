/*
 * Copyright (C) 2015 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.workflow.profile;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlID;
import jakarta.xml.bind.annotation.XmlValue;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class ValueMapDefinition {

    @XmlAttribute(name = WorkflowProfileConsts.VALUEMAP_NAME_ATT, required = true)
    @XmlID
    private String id;

    @XmlAttribute(name = WorkflowProfileConsts.VALUEMAP_SOURCE_ATT)
    private String source;

    @XmlElement(name = WorkflowProfileConsts.VALUEMAP_VALUE_EL)
    private List<ValueMapItemDefinition> items;

    public String getId() {
        return id;
    }

    public ValueMapDefinition setId(String id) {
        this.id = id;
        return this;
    }

    public ValueMapSource getSource() {
        return ValueMapSource.fromValue(source);
    }

    public void setSource(String source) {
        this.source = ValueMapSource.fromValue(source).name();
    }

    public List<ValueMapItemDefinition> getItems() {
        if (items == null) {
            items = new ArrayList<ValueMapItemDefinition>();
        }
        return items;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ValueMapItemDefinition {

        @XmlAttribute(name = WorkflowProfileConsts.VALUEMAPITEM_KEY_ATT)
        private String key;

        @XmlValue
        private String value;

        public String getKey() {
            return key;
        }

        public ValueMapItemDefinition setKey(String key) {
            this.key = key;
            return this;
        }

        public String getValue() {
            return value;
        }

        public ValueMapItemDefinition setValue(String value) {
            this.value = value;
            return this;
        }

    }
}
