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

import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfileConsts;

/**
 * The data source of workflow profiles.
 *
 * @author Jan Pokorsky
 */
public class WorkflowProfileDataSource extends RestDataSource {

    public static final String ID = "WorkflowProfileDataSource";

    public static final String FIELD_ID = WorkflowProfileConsts.JOB_NAME_ATT;
    public static final String FIELD_LABEL = WorkflowProfileConsts.TITLE_EL;
    public static final String FIELD_HINT = WorkflowProfileConsts.HINT_EL;
    public static final String FIELD_DISABLED = WorkflowProfileConsts.DISABLED;

    private static WorkflowProfileDataSource INSTANCE = null;

    public static WorkflowProfileDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowProfileDataSource();
        }
        return INSTANCE;
    }

    public WorkflowProfileDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_WORKFLOW_PROFILE);

        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setPrimaryKey(Boolean.TRUE);

        DataSourceTextField label = new DataSourceTextField(FIELD_LABEL);
        DataSourceTextField hint = new DataSourceTextField(FIELD_HINT);
        DataSourceBooleanField disabled = new DataSourceBooleanField(FIELD_DISABLED);

        setFields(fieldId, label, hint, disabled);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }
}
