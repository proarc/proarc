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

import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.HashMap;
import java.util.LinkedHashMap;
import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DateDisplayFormat;

/**
 * The data source of workflow jobs.
 *
 * @author Jan Pokorsky
 */
public class WorkflowJobDataSource extends ProarcDataSource {

    public static final String ID = "WorkflowJobDataSource";

    public static final String FIELD_CREATED = WorkflowModelConsts.JOB_CREATED;
    public static final String FIELD_ID = WorkflowModelConsts.JOB_ID;
    public static final String FIELD_FINANCED = WorkflowModelConsts.JOB_FILTER_FINANCED;
    public static final String FIELD_LABEL = WorkflowModelConsts.JOB_LABEL;
    public static final String FIELD_MATERIALS = "materials";
    public static final String FIELD_MODIFIED = WorkflowModelConsts.JOB_MODIFIED;
    public static final String FIELD_NOTE = WorkflowModelConsts.JOB_NOTE;
    public static final String FIELD_OWNER = WorkflowModelConsts.JOB_OWNERID;
    public static final String FIELD_PARENTID = WorkflowModelConsts.JOB_PARENTID;
    public static final String FIELD_PRIORITY = WorkflowModelConsts.JOB_PRIORITY;
    public static final String FIELD_PROFILE_ID = WorkflowModelConsts.JOB_PROFILENAME;
    public static final String FIELD_PROFILE_HINT = WorkflowModelConsts.JOB_PROFILEHINT;
    public static final String FIELD_TASK_HINT = WorkflowModelConsts.JOB_TASK_HINT;
    public static final String FIELD_TASK_NAME = WorkflowModelConsts.JOB_TASK_NAME;
    public static final String FIELD_TASK_LABEL = WorkflowModelConsts.JOB_TASK_LABEL;
    public static final String FIELD_TASK_CHANGE_DATE = WorkflowModelConsts.JOB_TASK_CHANGE_DATE;
    public static final String FIELD_TASK_CHANGE_USER = WorkflowModelConsts.JOB_TASK_CHANGE_USER;
    public static final String FIELD_TASK_CHANGE_USERNAME = WorkflowModelConsts.JOB_TASK_CHANGE_USERNAME;
    public static final String FIELD_STATE = WorkflowModelConsts.JOB_STATE;
    public static final String FIELD_MBARCODE = WorkflowModelConsts.JOB_FILTER_MATERIAL_BARCODE;
    public static final String FIELD_MDETAIL = WorkflowModelConsts.JOB_FILTER_MATERIAL_DETAIL;
    public static final String FIELD_MFIELD001 = WorkflowModelConsts.JOB_FILTER_MATERIAL_FIELD001;
    public static final String FIELD_MISSUE = WorkflowModelConsts.JOB_FILTER_MATERIAL_ISSUE;
    public static final String FIELD_MSIGLA = WorkflowModelConsts.JOB_FILTER_MATERIAL_SIGLA;
    public static final String FIELD_MSIGNATURE = WorkflowModelConsts.JOB_FILTER_MATERIAL_SIGNATURE;
    public static final String FIELD_MVOLUME = WorkflowModelConsts.JOB_FILTER_MATERIAL_VOLUME;
    public static final String FIELD_MYEAR = WorkflowModelConsts.JOB_FILTER_MATERIAL_YEAR;
    public static final String FIELD_MEDITION = WorkflowModelConsts.JOB_FILTER_MATERIAL_EDITION;
    public static final String FIELD_MPID = WorkflowModelConsts.JOB_FILTER_DIGOBJ_PID;
    public static final String FIELD_MODEL = WorkflowModelConsts.JOB_FILTER_MODEL;
    public static final String FIELD_RAW_PATH = WorkflowModelConsts.JOB_FILTER_RAW_PATH;
    //public static final String FIELD_MASTER_PATH = WorkflowModelConsts.JOB_FILTER_MASTER_PATH;

    private static WorkflowJobDataSource INSTANCE;

    public static WorkflowJobDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowJobDataSource();
        }
        return INSTANCE;
    }

    public WorkflowJobDataSource() {
        setID(ID);
        setDataURL(RestConfig.URL_WORKFLOW);
        final ClientMessages i18n = GWT.create(ClientMessages.class);

        DataSourceIntegerField fieldId = new DataSourceIntegerField(FIELD_ID);
        fieldId.setPrimaryKey(Boolean.TRUE);
        fieldId.setTitle(i18n.WorkflowJob_Field_Id_Title());
        fieldId.setDetail(true);
        fieldId.setCanEdit(false);

        DataSourceTextField label = new DataSourceTextField(FIELD_LABEL);
        label.setTitle(i18n.WorkflowJob_Field_Label_Title());
        label.setLength(2000);
        label.setRequired(true);

        DataSourceTextField profileId = new DataSourceTextField(FIELD_PROFILE_ID);
        profileId.setTitle(i18n.WorkflowJob_Field_Profile_Title());
        profileId.setCanEdit(false);
        profileId.setDisplayField(WorkflowModelConsts.JOB_PROFILELABEL);

        DataSourceEnumField state = new DataSourceEnumField(FIELD_STATE);
        state.setTitle(i18n.WorkflowJob_Field_State_Title());
        state.setValueMap(new LinkedHashMap<String,String>() {{
            put(Job.State.OPEN.name(), i18n.WorkflowJob_State_Open_Title());
            put(Job.State.FINISHED.name(), i18n.WorkflowJob_State_Finished_Title());
            put(Job.State.CANCELED.name(), i18n.WorkflowJob_State_Canceled_Title());
        }});
        state.setRequired(true);

        DataSourceTextField owner = new DataSourceTextField(FIELD_OWNER);
        owner.setTitle(i18n.WorkflowJob_Field_Owner_Title());
        owner.setDisplayField(WorkflowModelConsts.JOB_OWNERNAME);

        DataSourceEnumField priority = new DataSourceEnumField(FIELD_PRIORITY);
        priority.setTitle(i18n.WorkflowJob_Field_Priority_Title());
        priority.setRequired(true);
        priority.setValueMap(new LinkedHashMap() {{
            put("1", i18n.Workflow_Priority_1_Title());
            put("2", i18n.Workflow_Priority_2_Title());
            put("3", i18n.Workflow_Priority_3_Title());
            put("4", i18n.Workflow_Priority_4_Title());
        }});

        DataSourceTextField note = new DataSourceTextField(FIELD_NOTE);
        note.setTitle(i18n.WorkflowJob_Field_Note_Title());
        note.setDetail(true);

        DataSourceTextField financed = new DataSourceTextField(FIELD_FINANCED);
        financed.setTitle(i18n.WorkflowJob_Field_Financed_Title());
        financed.setDetail(true);

        DataSourceDateTimeField created = new DataSourceDateTimeField(FIELD_CREATED);
        created.setTitle(i18n.WorkflowJob_Field_Created_Title());
        created.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        created.setCanEdit(false);
        created.setDetail(true);

        DataSourceDateTimeField modified = new DataSourceDateTimeField(FIELD_MODIFIED);
        modified.setTitle(i18n.WorkflowJob_Field_Modified_Title());
        modified.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        modified.setCanEdit(false);
        modified.setDetail(true);

        DataSourceTextField barcode = new DataSourceTextField(FIELD_MBARCODE);
        barcode.setTitle(i18n.WorkflowMaterial_Field_Barcode_Title());
        barcode.setDetail(true);

        DataSourceTextField detail = new DataSourceTextField(FIELD_MDETAIL);
        detail.setTitle(i18n.WorkflowMaterial_Field_Detail_Title());
        detail.setDetail(true);

        DataSourceTextField field001 = new DataSourceTextField(FIELD_MFIELD001);
        field001.setTitle(i18n.WorkflowMaterial_Field_Field001_Title());
        field001.setDetail(true);

        DataSourceTextField issue = new DataSourceTextField(FIELD_MISSUE);
        issue.setTitle(i18n.WorkflowMaterial_Field_Issue_Title());
        issue.setDetail(true);

        DataSourceTextField sigla = new DataSourceTextField(FIELD_MSIGLA);
        sigla.setTitle(i18n.WorkflowMaterial_Field_Sigla_Title());
        sigla.setDetail(true);

        DataSourceTextField signature = new DataSourceTextField(FIELD_MSIGNATURE);
        signature.setTitle(i18n.WorkflowMaterial_Field_Signature_Title());
        signature.setDetail(true);

        DataSourceTextField volume = new DataSourceTextField(FIELD_MVOLUME);
        volume.setTitle(i18n.WorkflowMaterial_Field_Volume_Title());
        volume.setDetail(true);

        DataSourceTextField year = new DataSourceTextField(FIELD_MYEAR);
        year.setTitle(i18n.WorkflowMaterial_Field_Year_Title());
        year.setDetail(true);

        DataSourceTextField edition = new DataSourceTextField(FIELD_MEDITION);
        edition.setTitle(i18n.WorkflowMaterial_Field_Edition_Title());
        edition.setDetail(true);

        DataSourceTextField pid = new DataSourceTextField(FIELD_MPID);
        pid.setTitle(i18n.WorkflowMaterial_Field_Pid_Title());
        pid.setDetail(true);

        DataSourceTextField rawPath = new DataSourceTextField(FIELD_RAW_PATH);
        rawPath.setTitle(i18n.WorkflowMaterial_Field_Raw_Path_Title());
        rawPath.setDetail(true);

        DataSourceTextField taskName = new DataSourceTextField(FIELD_TASK_NAME);
        taskName.setTitle(i18n.WorkflowJob_Task_Name());
        taskName.setCanEdit(false);
        taskName.setDetail(true);
        taskName.setDisplayField(FIELD_TASK_LABEL);

        DataSourceDateTimeField taskUpdated = new DataSourceDateTimeField(FIELD_TASK_CHANGE_DATE);
        taskUpdated.setTitle(i18n.WorkflowJob_Task_Date());
        taskUpdated.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        taskUpdated.setCanEdit(false);
        taskUpdated.setDetail(true);

        DataSourceTextField taskOwner = new DataSourceTextField(FIELD_TASK_CHANGE_USER);
        taskOwner.setTitle(i18n.WorkflowJob_Task_User());
        taskOwner.setDisplayField(FIELD_TASK_CHANGE_USERNAME);


        setFields(fieldId, label, state, profileId, priority, created, modified,
                note, financed, barcode,
                detail, field001, issue, sigla, signature, volume, year, pid, edition,
                rawPath, taskName, taskUpdated, taskOwner
        );
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
                            put(FIELD_CREATED, WorkflowModelConsts.JOB_FILTER_CREATED);
                            put(FIELD_MODIFIED, WorkflowModelConsts.JOB_FILTER_MODIFIED);
                        }});
                dsRequest.setData(record);
            }
        } else if (dsRequest.getOperationType() == DSOperationType.UPDATE) {
            Record d = ClientUtils.normalizeData(new Record(dsRequest.getData()));
            dsRequest.setData(d);
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
