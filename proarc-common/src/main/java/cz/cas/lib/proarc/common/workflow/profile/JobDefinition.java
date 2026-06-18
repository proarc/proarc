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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class JobDefinition extends DisplayableType<JobDefinition> {

    @XmlElement(name = WorkflowProfileConsts.JOB_WORKER_EL)
    private WorkerDefinition worker;

    @XmlElement(name = WorkflowProfileConsts.WORKFLOW_MODEL_EL)
    private List<ModelDefinition> model;

    @XmlAttribute(name = WorkflowProfileConsts.JOB_PRIORITY_AT)
    private Integer priority;

    @XmlElement(name = WorkflowProfileConsts.JOB_STEP_EL)
    private List<StepDefinition> steps;

    @XmlElement(name = WorkflowProfileConsts.JOB_SUBJOB_EL)
    private List<SubjobDefinition> subjobs;

    private transient List<String> taskNamesSortedByBlockers;

    public int getPriority() {
        return priority == null ? 2 : priority;
    }

    public JobDefinition setPriority(int priority) {
        this.priority = priority;
        return this;
    }

    public WorkerDefinition getWorker() {
        return worker;
    }

    public JobDefinition setWorker(WorkerDefinition worker) {
        this.worker = worker;
        return this;
    }

    public List<StepDefinition> getSteps() {
        if (steps == null) {
            steps = new ArrayList<>();
        }
        return steps;
    }

    public List<SubjobDefinition> getSubjobs() {
        if (subjobs == null) {
            subjobs = new ArrayList<>();
        }
        return subjobs;
    }

    public List<ModelDefinition> getModel() {
        if (model == null) {
            model = new ArrayList<>();
        }
        return model;
    }

    public List<String> getTaskNamesSortedByBlockers() {
        return taskNamesSortedByBlockers != null
                ? taskNamesSortedByBlockers
                : Collections.<String>emptyList();
    }

    void setTaskNamesSortedByBlockers(List<String> taskNamesSortedByBlockers) {
        this.taskNamesSortedByBlockers = taskNamesSortedByBlockers;
    }

}
