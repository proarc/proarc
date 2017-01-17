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

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.common.workflow.model.Task.State;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The data source of workflow's tasks.
 *
 * @author Jan Pokorsky
 */
public class WorkflowTaskDataSource extends ProarcDataSource {

    public static final String ID = "WorkflowTaskDataSource";

    public static final String FIELD_CREATED = WorkflowModelConsts.TASK_CREATED;
    public static final String FIELD_JOB_ID = WorkflowModelConsts.TASK_JOBID;
    public static final String FIELD_JOB_LABEL = WorkflowModelConsts.TASK_JOBLABEL;
    public static final String FIELD_ID = WorkflowModelConsts.TASK_ID;
    // LABEL stands for i18n of TYPE
    public static final String FIELD_HINT = WorkflowModelConsts.TASK_PROFILEHINT;
    public static final String FIELD_LABEL = WorkflowModelConsts.TASK_PROFILELABEL;
    public static final String FIELD_MATERIALS = "materials";
    public static final String FIELD_MODIFIED = WorkflowModelConsts.TASK_MODIFIED;
    public static final String FIELD_NOTE = WorkflowModelConsts.TASK_NOTE;
    public static final String FIELD_OWNER = WorkflowModelConsts.TASK_OWNERID;
    public static final String FIELD_PARAMETERS = WorkflowModelConsts.TASK_PARAMETERS;
    public static final String FIELD_PRIORITY = WorkflowModelConsts.TASK_PRIORITY;
    public static final String FIELD_STATE = WorkflowModelConsts.TASK_STATE;
    public static final String FIELD_TYPE = WorkflowModelConsts.TASK_FILTER_PROFILENAME;
    private static WorkflowTaskDataSource INSTANCE;
    private final LinkedHashMap<String, String> allTaskStates;

    public static WorkflowTaskDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowTaskDataSource();
        }
        return INSTANCE;
    }

    public WorkflowTaskDataSource() {
        setID(ID);
        setDataURL(RestConfig.URL_WORKFLOW_TASK);
        final ClientMessages i18n = GWT.create(ClientMessages.class);

        allTaskStates = new LinkedHashMap<String, String>() {{
            put(State.WAITING.name(), i18n.WorkflowTask_State_Waiting_Title());
            put(State.READY.name(), i18n.WorkflowTask_State_Ready_Title());
            put(State.STARTED.name(), i18n.WorkflowTask_State_Started_Title());
            put(State.FINISHED.name(), i18n.WorkflowTask_State_Finished_Title());
            put(State.CANCELED.name(), i18n.WorkflowTask_State_Canceled_Title());
        }};
        DataSourceIntegerField fieldId = new DataSourceIntegerField(FIELD_ID);
        fieldId.setPrimaryKey(Boolean.TRUE);
        fieldId.setDetail(true);
        fieldId.setTitle(i18n.WorkflowTask_Field_Id_Title());
        fieldId.setCanEdit(false);

        DataSourceTextField label = new DataSourceTextField(FIELD_LABEL);
        label.setTitle(i18n.WorkflowTask_Field_Label_Title());
        label.setLength(255);
        label.setCanEdit(false);

        DataSourceTextField jobId = new DataSourceTextField(FIELD_JOB_ID);
        jobId.setTitle(i18n.WorkflowTask_Field_JobId_Title());
        jobId.setDetail(true);
        jobId.setCanEdit(false);

        DataSourceTextField jobLabel = new DataSourceTextField(FIELD_JOB_LABEL);
        jobLabel.setTitle(i18n.WorkflowTask_Field_JobLabel_Title());
        jobLabel.setDetail(true);
        jobLabel.setCanEdit(false);

        DataSourceEnumField state = new DataSourceEnumField(FIELD_STATE);
        state.setTitle(i18n.WorkflowTask_Field_State_Title());
        state.setValueMap(allTaskStates);
        state.setRequired(true);

        DataSourceTextField owner = new DataSourceTextField(FIELD_OWNER);
        owner.setTitle(i18n.WorkflowTask_Field_Owner_Title());
        owner.setDisplayField(WorkflowModelConsts.TASK_OWNERNAME);

        DataSourceTextField note = new DataSourceTextField(FIELD_NOTE);
        note.setTitle(i18n.WorkflowTask_Field_Note_Title());
        note.setDetail(true);

        DataSourceTextField type = new DataSourceTextField(FIELD_TYPE);
        type.setTitle(i18n.WorkflowTask_Field_Profile_Title());
        type.setDetail(true);
        type.setCanEdit(false);

        DataSourceEnumField priority = new DataSourceEnumField(FIELD_PRIORITY);
        priority.setTitle(i18n.WorkflowTask_Field_Priority_Title());
        priority.setDetail(true);
        priority.setValueMap(new LinkedHashMap() {{
            put("1", i18n.Workflow_Priority_1_Title());
            put("2", i18n.Workflow_Priority_2_Title());
            put("3", i18n.Workflow_Priority_3_Title());
            put("4", i18n.Workflow_Priority_4_Title());
        }});

        DataSourceDateTimeField created = new DataSourceDateTimeField(FIELD_CREATED);
        created.setTitle(i18n.WorkflowTask_Field_Created_Title());
        created.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        created.setCanEdit(false);
        created.setDetail(true);

        DataSourceDateTimeField modified = new DataSourceDateTimeField(FIELD_MODIFIED);
        modified.setTitle(i18n.WorkflowTask_Field_Modified_Title());
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
                RestConfig.createUpdatePostOperation());
    }

    public LinkedHashMap<String, String> getAllTaskStates() {
        return allTaskStates;
    }

    @Override
    protected Object transformRequest(DSRequest dsRequest) {
        if (dsRequest.getOperationType() == DSOperationType.FETCH) {
            Criteria criteria = dsRequest.getCriteria();
            Map<String, Object> criteriaMap;
            if (criteria.isAdvanced()) {
                HashMap<String, Object> record = new HashMap<String, Object>();
                ClientUtils.advanceCriteriaAsParams(
                        criteria.asAdvancedCriteria(), record, new HashMap<String, String>(){{
                            put(FIELD_CREATED, WorkflowModelConsts.TASK_FILTER_CREATED);
                            put(FIELD_MODIFIED, WorkflowModelConsts.TASK_FILTER_MODIFIED);
                        }});
                criteriaMap = record;
            } else {
                criteriaMap = criteria.getValues();
            }
            if (criteriaMap.containsKey(FIELD_LABEL)) {
                criteriaMap.put(WorkflowModelConsts.TASK_FILTER_PROFILENAME, criteriaMap.remove(FIELD_LABEL));
            } else if (criteriaMap.containsKey(FIELD_TYPE)) {
                criteriaMap.put(WorkflowModelConsts.TASK_FILTER_PROFILENAME, criteriaMap.remove(FIELD_TYPE));
            }
            dsRequest.setData(criteriaMap);
        } else if (dsRequest.getOperationType() == DSOperationType.UPDATE) {
            return ClientUtils.dump(dsRequest.getData());
        }
        return super.transformRequest(dsRequest);
    }

    @Override
    protected void transformResponse(DSResponse dsResponse, DSRequest dsRequest, Object data) {
        if (dsRequest.getOperationType() == DSOperationType.FETCH) {
            // #482: ignore HTTP 404/Not Found, e.g. when ID filter is not a number
            if (dsResponse.getHttpResponseCode() == 404) {
                dsResponse.setData(new Record[0]);
                dsResponse.setTotalRows(0);
                data = dsResponse.getData();
            }
        }
        super.transformResponse(dsResponse, dsRequest, data);
    }

}
