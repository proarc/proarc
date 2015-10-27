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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlIDREF;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class SetMaterialDefinition {

    @XmlAttribute(name = WorkflowProfileConsts.SETMATERIAL_MATREF_ATT, required = true)
    @XmlIDREF
    private MaterialTypeDefinition material;

    @XmlAttribute(name = WorkflowProfileConsts.SETMATERIAL_WAY_ATT, required = true)
    private String way;

    public MaterialTypeDefinition getMaterial() {
        return material;
    }

    public SetMaterialDefinition setMaterial(MaterialTypeDefinition material) {
        this.material = material;
        return this;
    }

    public String getWay() {
        return way;
    }

    public SetMaterialDefinition setWay(String way) {
        this.way = way;
        return this;
    }

}
