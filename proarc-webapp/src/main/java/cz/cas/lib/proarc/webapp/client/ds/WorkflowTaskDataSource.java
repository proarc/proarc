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
package cz.cas.lib.proarc.webapp.client.ds;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.common.workflow.model.Task.State;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.HashMap;
import java.util.LinkedHashMap;

/**
 * The data source of workflow's tasks.
 *
 * @author Jan Pokorsky
 */
public class WorkflowTaskDataSource extends RestDataSource {

    public static final String ID = "WorkflowTaskDataSource";

    public static final String FIELD_CREATED = WorkflowModelConsts.TASK_CREATED;
    public static final String FIELD_JOB_ID = WorkflowModelConsts.TASK_JOBID;
    public static final String FIELD_JOB_LABEL = WorkflowModelConsts.TASK_JOBLABEL;
    public static final String FIELD_ID = WorkflowModelConsts.TASK_ID;
    // LABEL stands for i18n of TYPE
    public static final String FIELD_LABEL = WorkflowModelConsts.TASK_PROFILELABEL;
    public static final String FIELD_MATERIALS = "materials";
    public static final String FIELD_MODIFIED = WorkflowModelConsts.TASK_TIMESTAMP;
    public static final String FIELD_NOTE = WorkflowModelConsts.TASK_NOTE;
    public static final String FIELD_OWNER = WorkflowModelConsts.TASK_OWNERID;
    public static final String FIELD_PARAMETERS = "params";
    public static final String FIELD_PRIORITY = WorkflowModelConsts.TASK_PRIORITY;
    public static final String FIELD_STATE = WorkflowModelConsts.TASK_STATE;
    public static final String FIELD_TYPE = WorkflowModelConsts.TASK_FILTER_PROFILENAME;

    private static WorkflowTaskDataSource INSTANCE;

    public static WorkflowTaskDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowTaskDataSource();
        }
        return INSTANCE;
    }

    public WorkflowTaskDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_WORKFLOW_TASK);

        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setPrimaryKey(Boolean.TRUE);
        fieldId.setDetail(true);
        fieldId.setTitle("ID");
        fieldId.setCanEdit(false);

        DataSourceTextField label = new DataSourceTextField(FIELD_LABEL);
        label.setTitle("Typ úkolu");
        label.setLength(255);

        DataSourceTextField jobId = new DataSourceTextField(FIELD_JOB_ID);
        jobId.setTitle("ID záměru");
        jobId.setDetail(true);
        jobId.setCanEdit(false);

        DataSourceTextField jobLabel = new DataSourceTextField(FIELD_JOB_LABEL);
        jobLabel.setTitle("Záměr");
        jobLabel.setDetail(true);
        jobLabel.setCanEdit(false);

        DataSourceEnumField state = new DataSourceEnumField(FIELD_STATE);
        state.setTitle("Stav");
        state.setValueMap(new LinkedHashMap<String, String>() {{
            put(State.WAITING.name(), "Čeká");
            put(State.READY.name(), "Připraven");
            put(State.STARTED.name(), "Probíhá");
            put(State.FINISHED.name(), "Dokončen");
            put(State.CANCELED.name(), "Zrušen");
        }});
        state.setRequired(true);

        DataSourceTextField owner = new DataSourceTextField(FIELD_OWNER);
        owner.setTitle("Pracovník");
        owner.setDisplayField(WorkflowModelConsts.TASK_OWNERNAME);

        DataSourceTextField note = new DataSourceTextField(FIELD_NOTE);
        note.setTitle("Poznámka");
        note.setDetail(true);

        DataSourceTextField type = new DataSourceTextField(FIELD_TYPE);
        type.setTitle("Typ ID");
        type.setDetail(true);
        type.setCanEdit(false);

        DataSourceEnumField priority = new DataSourceEnumField(FIELD_PRIORITY);
        priority.setTitle("Priorita");
        priority.setCanEdit(false);
        priority.setDetail(true);
        String[] priorities = new String[10];
        for (int i = 0; i < priorities.length; i++) {
            priorities[i] = String.valueOf(i + 1);
        }
        priority.setValueMap(priorities);

        DataSourceDateTimeField created = new DataSourceDateTimeField(FIELD_CREATED);
        created.setTitle("Vytvořeno");
        created.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        created.setCanEdit(false);
        created.setDetail(true);

        DataSourceDateTimeField modified = new DataSourceDateTimeField(FIELD_MODIFIED);
        modified.setTitle("Změněno");
        modified.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        modified.setCanEdit(false);
        modified.setDetail(true);

        DataSourceField params = new DataSourceField(FIELD_PARAMETERS, FieldType.ANY);
        params.setHidden(true);

        DataSourceField materials = new DataSourceField(FIELD_MATERIALS, FieldType.ANY);
        materials.setHidden(true);

        setFields(label, type, state, priority, owner, created, modified,
                fieldId, note, jobId, jobLabel, params, materials);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        setOperationBindings(
                RestConfig.createAddOperation(),
//                RestConfig.createDeleteOperation(),
                RestConfig.createUpdateOperation());
    }

    @Override
    protected Object transformRequest(DSRequest dsRequest) {
        if (dsRequest.getOperationType() == DSOperationType.FETCH) {
            Criteria criteria = dsRequest.getCriteria();
            if (criteria.isAdvanced()) {
                HashMap<String, Object> record = new HashMap<String, Object>();
                ClientUtils.advanceCriteriaAsParams(
                        criteria.asAdvancedCriteria(), record, new HashMap<String, String>(){{
                            put(FIELD_CREATED, WorkflowModelConsts.TASK_FILTER_CREATED);
                            put(FIELD_MODIFIED, WorkflowModelConsts.TASK_FILTER_MODIFIED);
                        }});
                dsRequest.setData(record);
            }
        }
        return super.transformRequest(dsRequest);
    }

}
