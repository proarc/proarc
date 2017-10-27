/*
 * Copyright (C) 2017 Martin Rumanek
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

import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;

/**
 * Datasource for MODS/JSON custom mapping in workflow.
 * It's similar to ModsCustomDataSouce, but this uses different identifier (workflow job id).
 *
 * @see ModsCustomDataSource
 * @author Martin Rumanek
 */
public class WorkflowModsCustomDataSource extends ProarcDataSource implements ModsConstants {

    public static final String ID = "WorkflowModsCustomDataSource";

    public static final String FIELD_WF_JOB_ID = DigitalObjectResourceApi.WORKFLOW_JOB_ID;

    public static WorkflowModsCustomDataSource INSTANCE;

    public static WorkflowModsCustomDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowModsCustomDataSource();
        }
        return INSTANCE;
    }


    public WorkflowModsCustomDataSource() {
        setID(ID);
        setDataURL(RestConfig.URL_WORKFLOW_MODS);
        DataSourceField fieldPid = new DataSourceField(FIELD_WF_JOB_ID, FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setRequired(true);
        setFields(fieldPid);
        setOperationBindings(RestConfig.createUpdateOperation());
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public void saveDescription(ModsCustomDataSource.DescriptionMetadata update, String modelId, Long workflowJobId, ModsCustomDataSource.DescriptionSaveHandler callback, Boolean showPrompt) {
        Record customRecord = update.getWrapper();
        callback.getUpdateRequest().setShowPrompt(showPrompt);
        customRecord.setAttribute(MetaModelDataSource.FIELD_MODELOBJECT, modelId);
        customRecord.setAttribute(WorkflowModelConsts.PARAMETER_JOBID, workflowJobId);
        callback.setUpdateRecord(customRecord);
        updateData(customRecord, callback, callback.getUpdateRequest());
    }

}
