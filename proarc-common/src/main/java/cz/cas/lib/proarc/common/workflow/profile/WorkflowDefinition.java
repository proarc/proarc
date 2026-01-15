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
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
@XmlType
@XmlRootElement(name = WorkflowProfileConsts.WORKFLOW_EL)
@XmlAccessorType(XmlAccessType.FIELD)
public class WorkflowDefinition {

    @XmlElement(name = WorkflowProfileConsts.WORKFLOW_VERSION)
    private String version;

    @XmlElement(name = WorkflowProfileConsts.WORKFLOW_JOB_EL)
    private List<JobDefinition> jobs;

    @XmlElement(name = WorkflowProfileConsts.WORKFLOW_MATERIAL_EL)
    private List<MaterialDefinition> materials;

    @XmlElement(name = WorkflowProfileConsts.WORKFLOW_TASK_EL)
    private List<TaskDefinition> tasks;

    @XmlElement(name = WorkflowProfileConsts.WORKFLOW_VALUEMAP_EL)
    private List<ValueMapDefinition> valueMaps;

    public String getVersion() {
        return version;
    }

    public List<JobDefinition> getJobs() {
        if (jobs == null) {
            jobs = new ArrayList<JobDefinition>();
        }
        return jobs;
    }

    public List<MaterialDefinition> getMaterials() {
        if (materials == null) {
            materials = new ArrayList<MaterialDefinition>();
        }
        return materials;
    }

    public List<TaskDefinition> getTasks() {
        if (tasks == null) {
            tasks = new ArrayList<TaskDefinition>();
        }
        return tasks;
    }

    public List<ValueMapDefinition> getValueMaps() {
        if (valueMaps == null) {
            valueMaps = new ArrayList<ValueMapDefinition>();
        }
        return valueMaps;
    }

}
