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

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowProfileConsts {

    public static final String NS_WORKFLOW_V1 = "http://proarc.lib.cas.cz/xml/common/workflow/v1";

    /**
     * The value map ID. It provides all declared tasks of the workflow.
     */
    public static final String WORKFLOWITEMVIEW_TASKS_VALUEMAP = "proarc.workflow.tasks";

    public static final String DISABLED = "disabled";
    public static final String HINT_EL = "hint";
    public static final String NAME = "name";
    public static final String TITLE_EL = "title";

    public static final String WORKFLOW_EL = "workflow";
    public static final String WORKFLOW_JOB_EL = "job";
    public static final String WORKFLOW_MATERIAL_EL = "material";
    public static final String WORKFLOW_TASK_EL = "task";
    public static final String WORKFLOW_MODEL_EL = "model";
    public static final String WORKFLOW_VALUEMAP_EL = "valuemap";

    public static final String JOB_STEP_EL = "step";
    public static final String JOB_SUBJOB_EL = "subjob";
    public static final String JOB_WORKER_EL = "worker";
    public static final String JOB_PRIORITY_AT = "priority";

    public static final String SUBJOB_JOBREF_ATT = "jobRef";

    public static final String JOBVIEW_SUBJOB = "subjob";
    public static final String JOBVIEW_TASK = "task";

    public static final String STEP_BLOCKER_EL = "blocker";
    public static final String STEP_OPTIONAL = "optional";
    public static final String STEP_PARAM_EL = "setParam";
    public static final String STEP_TASKREF_ATT = "taskRef";
    public static final String STEP_WORKER_EL = "worker";

    public static final String BLOCKER_TASKREF_ATT = "taskRef";

    public static final String TASK_MATERIAL_EL = "setMaterial";
    public static final String TASK_PARAM_EL = "param";
    public static final String TASK_ACTION_EL = "action";

    public static final String SETPARAM_PARAMREF_ATT = "paramRef";

    public static final String PARAM_REQUIRED_ATT = "required";
    public static final String PARAM_DATASOURCE_ATT = "datasource";
    public static final String PARAM_VALUETYPE = "type";
    public static final String PARAM_DISPLAYTYPE = "displayType";
    public static final String PARAM_OPTIONVALUEFIELD = "optionValueField";
    public static final String PARAM_OPTIONDISPLAYFIELD = "optionDisplayField";

    public static final String ACTION_COMMAND = "command";
    public static final String ACTION_COMMAND_ARG = "arg";

    public static final String MATERIAL_TYPE = "type";

    public static final String MODEL_PID = "pid";
    public static final String MODEL_NAME = "name";
    public static final String MODEL_TITLE = "title";


    public static final String SETMATERIAL_MATREF_ATT = "materialRef";
    public static final String SETMATERIAL_WAY_ATT = "way";

    public static final String VALUEMAP_NAME_ATT = "name";
    public static final String VALUEMAP_SOURCE_ATT = "source";
    public static final String VALUEMAP_VALUE_EL = "value";

    public static final String VALUEMAPITEM_KEY_ATT = "key";
    public static final String VALUEMAPITEM_VALUE = "value";
    
    public static final String I18N_LANG_ATT = "lang";

    public static final String WORKER_ACTUAL_ATT = "actual";

}
