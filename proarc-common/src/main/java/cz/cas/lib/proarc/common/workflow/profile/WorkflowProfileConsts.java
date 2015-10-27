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

    public static final String WORKFLOW_EL = "workflow";
    public static final String WORKFLOW_JOB_EL = "job";
    public static final String WORKFLOW_MATERIALTYPE_EL = "materialType";
    public static final String WORKFLOW_TASKTYPE_EL = "taskType";
    public static final String WORKFLOW_VALUEMAP_EL = "valuemap";

    public static final String JOB_NAME_ATT = "name";
    public static final String JOB_STEP_EL = "step";
    public static final String JOB_WORKER_EL = "worker";
    public static final String JOB_PRIORITY_EL = "priority";

    public static final String STEP_PARAM_EL = "param";
    public static final String STEP_TYPE_ATT = "type";
    public static final String STEP_WORKER_EL = "worker";

    public static final String TASKTYPE_MATERIAL_EL = "material";
    public static final String TASKTYPE_PARAMTYPE_EL = "paramType";
    public static final String TASKTYPE_NAME_ATT = "name";

    public static final String PARAM_TYPE_ATT = "type";

    public static final String PARAMTYPE_NAME_ATT = "name";
    public static final String PARAMTYPE_REQUIRED_ATT = "required";
    public static final String PARAMTYPE_DATASOURCE_ATT = "datasource";

    public static final String MATERIALTYPE_NAME_ATT = "name";

    public static final String MATERIAL_TYPE_ATT = "type";
    public static final String MATERIAL_WAY_ATT = "way";

    public static final String VALUEMAP_NAME_ATT = "name";
    public static final String VALUEMAP_SOURCE_ATT = "source";
    public static final String VALUEMAP_VALUE_EL = "value";

    public static final String VALUEMAPITEM_KEY_ATT = "value";

    public static final String HINT_EL = "hint";
    public static final String TITLE_EL = "title";
    
    public static final String I18N_LANG_ATT = "lang";

}
