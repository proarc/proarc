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
import javax.xml.bind.annotation.XmlElement;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class TaskDefinition extends DisplayableType<TaskDefinition> {

    @XmlElement(name = WorkflowProfileConsts.TASK_MATERIAL_EL)
    private List<SetMaterialDefinition> materialSetters;

    @XmlElement(name = WorkflowProfileConsts.TASK_PARAM_EL)
    private List<ParamDefinition> params;

    @XmlElement(name = WorkflowProfileConsts.TASK_ACTION_EL)
    private List<ActionDefinition> actions = new ArrayList<>();

    public List<SetMaterialDefinition> getMaterialSetters() {
        if (materialSetters == null) {
            materialSetters = new ArrayList<>();
        }
        return materialSetters;
    }

    public List<ParamDefinition> getParams() {
        if (params == null) {
            params = new ArrayList<>();
        }
        return params;
    }

    public List<ActionDefinition> getActions() {
        return actions;
    }
}
