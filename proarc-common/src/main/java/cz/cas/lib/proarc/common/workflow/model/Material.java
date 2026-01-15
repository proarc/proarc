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

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import java.math.BigDecimal;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class Material {

    @XmlElement(name = WorkflowModelConsts.MATERIAL_ID)
    private BigDecimal id;
    /**
     * The description of a material's value
     */
    @XmlElement(name = WorkflowModelConsts.MATERIAL_LABEL)
    private String label;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_NAME)
    private String name;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_NOTE)
    private String note;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_STATE)
    private String state;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_TYPE)
    private MaterialType type;

    public BigDecimal getId() {
        return id;
    }

    public void setId(BigDecimal id) {
        this.id = id;
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

    public MaterialType getType() {
        return type;
    }

    public void setType(MaterialType type) {
        this.type = type;
    }

    public String getTypeAsString() {
        return type.name();
    }

    public void setTypeAsString(String type) throws IllegalArgumentException {
        this.type = MaterialType.valueOf(type);
    }

}
