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
package cz.cas.lib.proarc.common.workflow.model;

import java.math.BigDecimal;

/**
 *
 * @author Jan Pokorsky
 */
public class Material {

    public enum Type {
        FOLDER, DIGITAL_OBJECT, PHYSICAL_DOCUMENT;
    }

    private BigDecimal id;
    /**
     * The description of a material's value
     */
    private String label;
    private String name;
    private String note;
    private String state;
    private Type type;

    public BigDecimal getId() {
        return id;
    }

    public void setId(BigDecimal id) {
        this.id = id;
    }

    /**
     * An alias for {@link #getId()}.
     */
    public BigDecimal getMaterialId() {
        return getId();
    }

    /**
     * An alias for {@link #setId(java.math.BigDecimal)}.
     */
    public void setMaterialId(BigDecimal materialId) {
        setId(materialId);
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getNote() {
        return note;
    }

    public void setNote(String note) {
        this.note = note;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public String getTypeAsString() {
        return type.name();
    }

    public void setTypeAsString(String type) throws IllegalArgumentException {
        this.type = Type.valueOf(type);
    }

}
