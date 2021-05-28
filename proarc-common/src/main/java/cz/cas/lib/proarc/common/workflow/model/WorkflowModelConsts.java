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
package cz.cas.lib.proarc.common.workflow.model;

/**
 *
 * @author Jan Pokorsky
 */
public final class WorkflowModelConsts {

    public static final String JOB_CREATED = "created";
    public static final String JOB_FINANCED = "financed";
    public static final String JOB_ID = "id";
    public static final String JOB_LABEL = "label";
    public static final String JOB_NOTE = "note";
    public static final String JOB_OWNERID = "ownerId";
    public static final String JOB_OWNERNAME = "ownerName";
    public static final String JOB_PARENTID = "parentId";
    public static final String JOB_PRIORITY = "priority";
    public static final String JOB_PROFILEHINT = "profileHint";
    public static final String JOB_PROFILELABEL = "profileLabel";
    public static final String JOB_PROFILENAME = "profileName";
    public static final String JOB_STATE = "state";
    public static final String JOB_MODIFIED = "modified";
    public static final String JOB_TASK_NAME = "taskName";
    public static final String JOB_TASK_LABEL = "taskLabel";
    public static final String JOB_TASK_HINT = "taskHint";
    public static final String JOB_TASK_CHANGE_DATE = "taskDate";
    public static final String JOB_TASK_CHANGE_USER = "taskUser";
    /**
     * It is same like {@link #JOB_MODIFIED} but in a long number format to keep precision.
     */
    public static final String JOB_TIMESTAMP = "timestamp";

    public static final String JOB_FILTER_ID = "id";
    public static final String JOB_FILTER_CREATED = "created";
    public static final String JOB_FILTER_LABEL = "label";
    public static final String JOB_FILTER_FINANCED = "financed";
    public static final String JOB_FILTER_MODIFIED = "modified";
    public static final String JOB_FILTER_MATERIAL_BARCODE = "barcode";
    public static final String JOB_FILTER_MATERIAL_DETAIL = "detail";
    public static final String JOB_FILTER_MATERIAL_FIELD001 = "field001";
    public static final String JOB_FILTER_MATERIAL_ISSUE = "issue";
    public static final String JOB_FILTER_MATERIAL_SIGLA = "sigla";
    public static final String JOB_FILTER_MATERIAL_SIGNATURE = "signature";
    public static final String JOB_FILTER_MATERIAL_VOLUME = "volume";
    public static final String JOB_FILTER_MATERIAL_YEAR = "year";
    public static final String JOB_FILTER_MATERIAL_EDITION = "edition";
    public static final String JOB_FILTER_OFFSET = "_startRow";
    public static final String JOB_FILTER_OWNERID = "ownerId";
    public static final String JOB_FILTER_PARENTID = "parentId";
    public static final String JOB_FILTER_PRIORITY = "priority";
    public static final String JOB_FILTER_PROFILENAME = "profileName";
    public static final String JOB_FILTER_SORTBY = "_sortBy";
    public static final String JOB_FILTER_STATE = "state";
    public static final String JOB_FILTER_DIGOBJ_PID = "pid";
    public static final String JOB_FILTER_MODEL = "model";
    public static final String JOB_FILTER_RAW_PATH = "rawPath";
//    public static final String JOB_FILTER_MASTER_PATH = "masterPath";

    public static final String TASK_CREATED = "created";
    public static final String TASK_ID = "id";
    public static final String TASK_JOBID = "jobId";
    public static final String TASK_JOBLABEL = "jobLabel";
    public static final String TASK_NOTE = "note";
    public static final String TASK_OWNERID = "ownerId";
    public static final String TASK_OWNERNAME = "ownerName";
    public static final String TASK_PARAMETERS = "params";
    public static final String TASK_PRIORITY = "priority";
    public static final String TASK_PROFILEHINT = "profileHint";
    public static final String TASK_PROFILELABEL = "profileLabel";
    public static final String TASK_PROFILENAME = "profileName";
    public static final String TASK_STATE = "state";
    public static final String TASK_MODIFIED = "modified";
    public static final String TASK_ORDER = "order";
    /**
     * It is same like {@link #TASK_MODIFIED} but in a long number format to keep precision.
     */
    public static final String TASK_TIMESTAMP = "timestamp";

    public static final String TASK_FILTER_ID = "id";
    public static final String TASK_FILTER_CREATED = "created";
    public static final String TASK_FILTER_JOBID = "jobId";
    public static final String TASK_FILTER_JOBLABEL = "jobLabel";
    public static final String TASK_FILTER_MODIFIED = "modified";
    public static final String TASK_FILTER_OFFSET = "_startRow";
    public static final String TASK_FILTER_OWNERID = "ownerId";
    public static final String TASK_FILTER_PRIORITY = "priority";
    public static final String TASK_FILTER_PROFILENAME = "profileName";
    public static final String TASK_FILTER_SORTBY = "_sortBy";
    public static final String TASK_FILTER_STATE = "state";
    public static final String TASK_FILTER_BARCODE = "barcode";

    public static final String PARAMETER_DISPLAYTYPE = "displayType";
    public static final String PARAMETER_JOBID = "jobId";
    public static final String PARAMETER_OPTION_DISPLAY_FIELD = "optionDisplayField";
    public static final String PARAMETER_OPTION_VALUE_FIELD = "optionValueField";
    public static final String PARAMETER_PROFILEHINT = "profileHint";
    public static final String PARAMETER_PROFILELABEL = "profileLabel";
    public static final String PARAMETER_PROFILENAME = "profileName";
    /** JSON. */
    public static final String PARAMETER_REQUIRED = "required";
    public static final String PARAMETER_VALUE = "value";
    public static final String PARAMETER_VALUEMAPID = "valueMapId";
    public static final String PARAMETER_VALUEMAPTYPE = "valueMapType";
    public static final String PARAMETER_VALUETYPE = "valueType";
    public static final String PARAMETER_TASKID = "taskId";
//    public static final String PARAMETER_TASKPROFILENAME = "taskProfileName";

    public static final String PARAMETERPROFILE_TASKID = "taskId";

    // common material attributes
    public static final String MATERIAL_ID = "id";
    public static final String MATERIAL_JOB_ID = "jobId";
    public static final String MATERIAL_LABEL = "label";
    public static final String MATERIAL_NAME = "name";
    public static final String MATERIAL_NOTE = "note";
    public static final String MATERIAL_PROFILELABEL = "profileLabel";
    public static final String MATERIAL_STATE = "state";
    public static final String MATERIAL_TASKID = "taskId";
    public static final String MATERIAL_TYPE = "type";
    public static final String MATERIAL_WAY = "way";
    // folder material attributes
    public static final String MATERIAL_PATH = "path";
    // digital material attributes
    public static final String MATERIAL_PID = "pid";
    // physical material attributes
    public static final String MATERIAL_BARCODE = "barcode";
    public static final String MATERIAL_CATALOG = "source";
    public static final String MATERIAL_DETAIL = "detail";
    public static final String MATERIAL_FIELD001 = "field001";
    public static final String MATERIAL_ISSUE = "issue";
    public static final String MATERIAL_METADATA = "metadata";
    public static final String MATERIAL_SIGLA = "sigla";
    public static final String MATERIAL_SIGNATURE = "signature";
    public static final String MATERIAL_RDCZID = "rdczId";
    public static final String MATERIAL_VOLUME = "volume";
    public static final String MATERIAL_YEAR = "year";
    public static final String MATERIAL_YEARNUMBER = "yearNumber";
    public static final String MATERIAL_EDITION = "edition";

    public static final String MATERIALFILTER_ID = "id";
    public static final String MATERIALFILTER_JOBID = "jobId";
    public static final String MATERIALFILTER_OFFSET = "_startRow";
    public static final String MATERIALFILTER_SORTBY = "_sortBy";
    public static final String MATERIALFILTER_TASKID = "taskId";
    public static final String MATERIALFILTER_TYPE = "materialType";
}
