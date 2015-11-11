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
package cz.cas.lib.proarc.common.workflow;

import cz.cas.lib.proarc.common.workflow.model.TaskParameterFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterView;
import cz.cas.lib.proarc.common.workflow.profile.ParamDefinition;
import cz.cas.lib.proarc.common.workflow.profile.TaskDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Joins parameters stored in DB with parameters declared in workflow XML.
 *
 * @author Jan Pokorsky
 */
public class FilterFindParameterQuery {

    private final WorkflowProfiles wp;

    public FilterFindParameterQuery(WorkflowProfiles wp) {
        this.wp = wp;
    }

    public List<TaskParameterView> filter(
            List<TaskParameterView> params,
            TaskParameterFilter filter,
            WorkflowDefinition wd) {

        // outer join of db params with task params declared in workflow
        Map<String, Map<String, ParamEntry>> join
                = new LinkedHashMap<String, Map<String, ParamEntry>>();
        for (TaskParameterView param : params) {
            Map<String, ParamEntry> taskParamCache = selectProfileTaskParams(join, param, wd);

            // left outer join of db param with param profile
            outerJoinDbParamsWithProfileParams(taskParamCache, param, filter);
        }

        // right outer join of db param with param profiles
        return outerJoinProfileParamsWithDbParams(join, filter);
    }

    /**
     * Returns a mapping of param names to param entries with param profiles of given task.
     */
    private Map<String, ParamEntry> selectProfileTaskParams(
            Map<String, Map<String, ParamEntry>> join,
            TaskParameterView param,
            WorkflowDefinition wd
    ) {
        String taskProfileName = param.getTaskProfileName();
        Map<String, ParamEntry> taskParams = join.get(taskProfileName);
        if (taskParams == null) {
            taskParams = new LinkedHashMap<String, ParamEntry>();
            join.put(taskProfileName, taskParams);
            TaskDefinition taskProfile = wp.getTaskProfile(wd, taskProfileName);
            if (taskProfile != null) {
                for (ParamDefinition pd : taskProfile.getParams()) {
                    taskParams.put(pd.getName(), new ParamEntry(null, pd, param));
                }
            }
        }
        return taskParams;
    }

    private Map<String, ParamEntry> outerJoinDbParamsWithProfileParams(
            Map<String, ParamEntry> taskParams,
            TaskParameterView param,
            TaskParameterFilter filter
    ) {
        // left join of db param with param profile
        ParamEntry paramEntry = taskParams.get(param.getParamRef());
        if (paramEntry != null) {
            ParamDefinition profile = paramEntry.getDefinition();
            String label = profile.getTitle(filter.getLocale().getLanguage(), param.getParamRef());
            param.setProfileLabel(label);
            param.setProfile(profile);
            paramEntry.setParam(param);
        } else {
            // no param definition
            String label = "Unknow parameter profile: " + param.getParamRef()
                    + " in task: " + param.getTaskProfileName();
            param.setProfileLabel(label);
            paramEntry = new ParamEntry(param, null, param);
            taskParams.put(param.getParamRef(), paramEntry);
        }
        return taskParams;
    }

    /**
     * Creates TaskParameterView for not db stored params.
     * Adds params that are declared but have no value yet
     * or newly declared params.
     */
    private ArrayList<TaskParameterView> outerJoinProfileParamsWithDbParams(
            Map<String, Map<String, ParamEntry>> join,
            TaskParameterFilter filter
    ) {
        // right outer join of db param with param profiles
        ArrayList<TaskParameterView> result = new ArrayList<TaskParameterView>();
        for (Map<String, ParamEntry> taskParams : join.values()) {
            for (ParamEntry entry : taskParams.values()) {
                TaskParameterView param = entry.getParam();
                ParamDefinition profile = entry.getDefinition();
                if (param == null && !profile.isDisabled()) {
                    param = new TaskParameterView();
                    param.setJobId(entry.getPrototype().getJobId());
                    param.setParamRef(profile.getName());
                    param.setProfile(profile);
                    param.setProfileLabel(profile.getTitle(filter.getLocale().getLanguage(), profile.getName()));
                    param.setTaskId(entry.getPrototype().getTaskId());
                    param.setTaskProfileName(entry.getPrototype().getTaskProfileName());
                    param.setValueType(profile.getValueType());
                }
                result.add(param);
            }
        }
        return result;
    }

    private static class ParamEntry {

        private TaskParameterView param;
        private ParamDefinition definition;
        private final TaskParameterView prototype;

        public ParamEntry(TaskParameterView param, ParamDefinition definition, TaskParameterView protoype) {
            this.param = param;
            this.definition = definition;
            this.prototype = protoype;
        }

        public TaskParameterView getParam() {
            return param;
        }

        public void setParam(TaskParameterView param) {
            this.param = param;
        }

        public ParamDefinition getDefinition() {
            return definition;
        }

        public void setDefinition(ParamDefinition definition) {
            this.definition = definition;
        }

        public TaskParameterView getPrototype() {
            return prototype;
        }

    }

}
