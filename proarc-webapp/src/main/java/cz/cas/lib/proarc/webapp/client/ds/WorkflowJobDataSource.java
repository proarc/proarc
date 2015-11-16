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
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DateDisplayFormat;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.HashMap;
import java.util.LinkedHashMap;

/**
 * The data source of workflow jobs.
 *
 * @author Jan Pokorsky
 */
public class WorkflowJobDataSource extends RestDataSource {

    public static final String ID = "WorkflowJobDataSource";

    public static final String FIELD_CREATED = WorkflowModelConsts.JOB_CREATED;
    public static final String FIELD_ID = WorkflowModelConsts.JOB_ID;
    public static final String FIELD_FINANCED = WorkflowModelConsts.JOB_FINANCED;
    public static final String FIELD_LABEL = WorkflowModelConsts.JOB_LABEL;
    public static final String FIELD_MATERIALS = "materials";
    public static final String FIELD_MODIFIED = WorkflowModelConsts.JOB_MODIFIED;
    public static final String FIELD_NOTE = WorkflowModelConsts.JOB_NOTE;
    public static final String FIELD_OWNER = WorkflowModelConsts.JOB_OWNERID;
    public static final String FIELD_PRIORITY = WorkflowModelConsts.JOB_PRIORITY;
    public static final String FIELD_PROFILE_ID = WorkflowModelConsts.JOB_PROFILENAME;
    public static final String FIELD_STATE = WorkflowModelConsts.JOB_STATE;

    private static WorkflowJobDataSource INSTANCE;

    public static WorkflowJobDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowJobDataSource();
        }
        return INSTANCE;
    }

    public WorkflowJobDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_WORKFLOW);

        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setPrimaryKey(Boolean.TRUE);
        fieldId.setTitle("ID");
        fieldId.setDetail(true);
        fieldId.setCanEdit(false);

        DataSourceTextField label = new DataSourceTextField(FIELD_LABEL);
        label.setTitle("Název");
        label.setLength(2000);
        label.setRequired(true);

        DataSourceTextField profileId = new DataSourceTextField(FIELD_PROFILE_ID);
        profileId.setTitle("Profil");
        profileId.setCanEdit(false);
        profileId.setDisplayField(WorkflowModelConsts.JOB_PROFILELABEL);

        DataSourceEnumField state = new DataSourceEnumField(FIELD_STATE);
        state.setTitle("Stav");
        state.setValueMap(new LinkedHashMap<String,String>() {{
            put(Job.State.OPEN.name(), "Otevřený");
            put(Job.State.FINISHED.name(), "Hotový");
            put(Job.State.CANCELED.name(), "Zrušený");
        }});
        state.setRequired(true);

        DataSourceTextField owner = new DataSourceTextField(FIELD_OWNER);
        owner.setTitle("Vlastník");
        owner.setDisplayField(WorkflowModelConsts.JOB_OWNERNAME);

        DataSourceEnumField priority = new DataSourceEnumField(FIELD_PRIORITY);
        priority.setTitle("Priorita");
        priority.setRequired(true);
        priority.setValueMap(new LinkedHashMap() {{
            put("1", "Spěchá");
            put("2", "Normální");
            put("3", "Nízká");
            put("4", "Odloženo");
        }});

        DataSourceTextField note = new DataSourceTextField(FIELD_NOTE);
        note.setTitle("Poznámka");
        note.setDetail(true);

        DataSourceTextField financed = new DataSourceTextField(FIELD_FINANCED);
        financed.setTitle("Financováno");
        financed.setDetail(true);

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

        setFields(fieldId, label, state, profileId, priority, owner, created, modified, note, financed);
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
        }
        return super.transformRequest(dsRequest);
    }

}
