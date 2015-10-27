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

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class TaskDefinition extends DisplayableType<TaskDefinition> {

    @XmlAttribute(name = WorkflowProfileConsts.TASK_NAME_ATT, required = true)
    @XmlID
    private String name;

    @XmlElement(name = WorkflowProfileConsts.TASK_MATERIAL_EL)
    private List<MaterialDefinition> materials;

    @XmlElement(name = WorkflowProfileConsts.TASK_PARAMTYPE_EL)
    private List<ParamTypeDefinition> params;

    public String getName() {
        return name;
    }

    public TaskDefinition setName(String name) {
        this.name = name;
        return this;
    }

    public List<MaterialDefinition> getMaterials() {
        if (materials == null) {
            materials = new ArrayList<MaterialDefinition>();
        }
        return materials;
    }

    public List<ParamTypeDefinition> getParams() {
        if (params == null) {
            params = new ArrayList<ParamTypeDefinition>();
        }
        return params;
    }

}
