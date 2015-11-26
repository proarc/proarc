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
@XmlAccessorType(XmlAccessType.NONE)
public class JobDefinitionView extends WorkflowItemView {

    private JobDefinition item;

    public JobDefinitionView(JobDefinition item, String lang) {
        super(item, lang);
        this.item = item;
    }

    @XmlElement(name = WorkflowProfileConsts.JOBVIEW_TASK)
    public List<WorkflowItemView> getTasks() {
        ArrayList<WorkflowItemView> tasks = new ArrayList<WorkflowItemView>();
        for (StepDefinition step : item.getSteps()) {
            TaskDefinition td = step.getTask();
            if (!td.isDisabled()) {
                tasks.add(new WorkflowItemView(td, lang));
            }
        }
        return tasks;
    }

}
