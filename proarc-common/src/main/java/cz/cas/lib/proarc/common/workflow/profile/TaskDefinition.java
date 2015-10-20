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
import javax.xml.bind.annotation.XmlIDREF;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class TaskDefinition {

    @XmlAttribute(name = WorkflowProfileConsts.TASK_TYPE_ATT, required = true)
    @XmlIDREF
    private TaskTypeDefinition type;

    @XmlElement(name = WorkflowProfileConsts.TASK_WORKER_EL)
    private String worker;

    @XmlElement(name = WorkflowProfileConsts.TASK_PARAM_EL)
    private List<ParamDefinition> params;

    public TaskTypeDefinition getType() {
        return type;
    }

    public TaskDefinition setType(TaskTypeDefinition type) {
        this.type = type;
        return this;
    }

    public String getWorker() {
        return worker;
    }

    public TaskDefinition setWorker(String worker) {
        this.worker = worker;
        return this;
    }

    public List<ParamDefinition> getParams() {
        if (params == null) {
            params = new ArrayList<ParamDefinition>();
        }
        return params;
    }

}
