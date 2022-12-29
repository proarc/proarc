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
public class StepDefinition {

    @XmlAttribute(name = WorkflowProfileConsts.STEP_TASKREF_ATT, required = true)
    @XmlIDREF
    private TaskDefinition task;

    @XmlAttribute(name = WorkflowProfileConsts.STEP_OPTIONAL)
    private boolean optional;

    @XmlElement(name = WorkflowProfileConsts.STEP_WORKER_EL)
    private WorkerDefinition worker;

    @XmlElement(name = WorkflowProfileConsts.STEP_PARAM_EL)
    private List<SetParamDefinition> paramSetters;

    @XmlElement(name = WorkflowProfileConsts.STEP_BLOCKER_EL)
    private List<BlockerDefinition> blockers;

    @XmlElement(name = WorkflowProfileConsts.STEP_MAIN_BLOCKER_EL)
    private List<MainBlockerDefinition> mainBlockers;

    public TaskDefinition getTask() {
        return task;
    }

    public StepDefinition setTask(TaskDefinition type) {
        this.task = type;
        return this;
    }

    public boolean isOptional() {
        return optional;
    }

    public void setOptional(boolean optional) {
        this.optional = optional;
    }

    public WorkerDefinition getWorker() {
        return worker;
    }

    public StepDefinition setWorker(WorkerDefinition worker) {
        this.worker = worker;
        return this;
    }

    public List<SetParamDefinition> getParamSetters() {
        if (paramSetters == null) {
            paramSetters = new ArrayList<SetParamDefinition>();
        }
        return paramSetters;
    }

    public List<BlockerDefinition> getBlockers() {
        if (blockers == null) {
            blockers = new ArrayList<BlockerDefinition>();
        }
        return blockers;
    }

    public List<MainBlockerDefinition> getMainBlockers() {
        if (mainBlockers == null) {
            mainBlockers = new ArrayList<MainBlockerDefinition>();
        }
        return mainBlockers;
    }

}
