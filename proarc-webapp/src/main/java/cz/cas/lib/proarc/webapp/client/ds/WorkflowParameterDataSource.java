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
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowParameterDataSource extends RestDataSource {

    public static final String ID = "WorkflowParameterDataSource";
    public static final String FIELD_TASK_ID = WorkflowModelConsts.PARAMETER_TASKID;
    public static final String FIELD_NAME = WorkflowModelConsts.PARAMETER_PROFILENAME;
    public static final String FIELD_VALUE = WorkflowModelConsts.PARAMETER_VALUE;

    public static WorkflowParameterDataSource INSTANCE;

    public static WorkflowParameterDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowParameterDataSource();
        }
        return INSTANCE;
    }

    public WorkflowParameterDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_WORKFLOW_PARAMETER);

        DataSourceTextField name = new DataSourceTextField(FIELD_NAME);
        name.setTitle("Název parametru");
        name.setDisplayField(WorkflowModelConsts.PARAMETER_PROFILELABEL);
        name.setCanEdit(false);

        DataSourceTextField value = new DataSourceTextField(FIELD_VALUE);
        value.setTitle("Hodnota");
        value.setCanEdit(true);

        setFields(name, value);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
//        setOperationBindings(
//                RestConfig.createAddOperation(),
//                RestConfig.createDeleteOperation(),
//                RestConfig.createUpdateOperation());
    }


}
