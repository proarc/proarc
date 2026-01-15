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
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public class JobDefinitionView extends WorkflowItemView {

    private JobDefinition item;

    public JobDefinitionView(JobDefinition item, String lang) {
        super(item, lang);
        this.item = item;
    }

    @XmlElement(name = WorkflowProfileConsts.JOBVIEW_TASK)
    public List<WorkflowItemView> getTasks() {
        ArrayList<WorkflowItemView> tasks = new ArrayList<>();
        for (StepDefinition step : item.getSteps()) {
            TaskDefinition td = step.getTask();
            if (!td.isDisabled()) {
                tasks.add(new WorkflowItemView(td, lang));
            }
        }
        return tasks;
    }

    @XmlElement(name = WorkflowProfileConsts.JOBVIEW_SUBJOB)
    public List<WorkflowItemView> getSubjobs() {
        return item.getSubjobs().stream()
                .filter(sj -> !sj.getJob().isDisabled() && !item.getName().equals(sj.getJob().getName()))
                .map(sj -> new WorkflowItemView(sj.getJob(), lang))
                .collect(Collectors.toList());
    }

    @XmlElement(name = WorkflowProfileConsts.WORKFLOW_MODEL_EL)
    public List<WorkflowItemView> getModels() {
        return item.getModel().stream()
                .map(modelDefinition ->
                        new WorkflowItemView(modelDefinition, lang))
                .collect(Collectors.toList());
    }

}
