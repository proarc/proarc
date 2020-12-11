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
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSOperationType;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.Way;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.LinkedHashMap;

/**
 * The data source of workflow materials.
 *
 * @author Jan Pokorsky
 */
public class WorkflowMaterialDataSource extends ProarcDataSource {

    public static final String ID = "WorkflowMaterialDataSource";
    public static final String PRIMARY_KEY = "__syntheticPrimaryKey";
    public static final String FIELD_ID = WorkflowModelConsts.MATERIAL_ID;
    public static final String FIELD_JOB_ID = WorkflowModelConsts.MATERIAL_JOB_ID;
    public static final String FIELD_TASK_ID = WorkflowModelConsts.MATERIAL_TASKID;
    public static final String FIELD_NOTE = WorkflowModelConsts.MATERIAL_NOTE;
    public static final String FIELD_TYPE = WorkflowModelConsts.MATERIAL_TYPE;
    public static final String FIELD_VALUE = WorkflowModelConsts.MATERIAL_LABEL;
    public static final String FIELD_PROFILENAME = WorkflowModelConsts.MATERIAL_NAME;
    public static final String FIELD_WAY = WorkflowModelConsts.MATERIAL_WAY;

    // custom fields
    public static final String FIELD_FOLDER_PATH = WorkflowModelConsts.MATERIAL_PATH;
    public static final String FIELD_PHYSICAL_BARCODE = WorkflowModelConsts.MATERIAL_BARCODE;
    public static final String FIELD_PHYSICAL_CATALOG = WorkflowModelConsts.MATERIAL_CATALOG;
    public static final String FIELD_PHYSICAL_FIELD001 = WorkflowModelConsts.MATERIAL_FIELD001;
    public static final String FIELD_PHYSICAL_RDCZID = WorkflowModelConsts.MATERIAL_RDCZID;
    public static final String FIELD_PHYSICAL_SIGNATURE = WorkflowModelConsts.MATERIAL_SIGNATURE;
    public static final String FIELD_PHYSICAL_METADATA = WorkflowModelConsts.MATERIAL_METADATA;
    public static final String FIELD_PHYSICAL_DETAIL = WorkflowModelConsts.MATERIAL_DETAIL;
    public static final String FIELD_PHYSICAL_ISSUE = WorkflowModelConsts.MATERIAL_ISSUE;
    public static final String FIELD_PHYSICAL_SIGLA = WorkflowModelConsts.MATERIAL_SIGLA;
    public static final String FIELD_PHYSICAL_VOLUME = WorkflowModelConsts.MATERIAL_VOLUME;
    public static final String FIELD_PHYSICAL_YEAR = WorkflowModelConsts.MATERIAL_YEAR;
    public static final String FIELD_PHYSICAL_EDITION = WorkflowModelConsts.MATERIAL_EDITION;
    public static final String FIELD_DIGITAL_PID = WorkflowModelConsts.MATERIAL_PID;

    public static WorkflowMaterialDataSource INSTANCE;

    public static WorkflowMaterialDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowMaterialDataSource();
        }
        return INSTANCE;
    }

    public WorkflowMaterialDataSource() {
        setID(ID);
        setDataURL(RestConfig.URL_WORKFLOW_MATERIAL);
        final ClientMessages i18n = GWT.create(ClientMessages.class);

        DataSourceTextField primaryKey = new DataSourceTextField(PRIMARY_KEY);
        primaryKey.setCanEdit(false);
        primaryKey.setCanSave(false);
        primaryKey.setHidden(true);
        primaryKey.setPrimaryKey(true);

        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setCanEdit(false);
        fieldId.setDetail(true);
        fieldId.setTitle(i18n.WorkflowMaterial_Field_Id_Title());

        DataSourceTextField note = new DataSourceTextField(FIELD_NOTE);
        note.setTitle(i18n.WorkflowMaterial_Field_Note_Title());

        DataSourceTextField value = new DataSourceTextField(FIELD_VALUE);
        value.setTitle(i18n.WorkflowMaterial_Field_Label_Title());

        DataSourceTextField profile = new DataSourceTextField(FIELD_PROFILENAME);
        profile.setTitle(i18n.WorkflowMaterial_Field_Profile_Title());
        profile.setDisplayField(WorkflowModelConsts.MATERIAL_PROFILELABEL);

        DataSourceEnumField type = new DataSourceEnumField(FIELD_TYPE);
        type.setTitle(i18n.WorkflowMaterial_Field_Type_Title());
        type.setValueMap(new LinkedHashMap<String, String>() {{
            put(MaterialType.FOLDER.name(), i18n.WorkflowMaterial_Type_Folder_Title());
            put(MaterialType.PHYSICAL_DOCUMENT.name(), i18n.WorkflowMaterial_Type_Physical_Title());
            put(MaterialType.DIGITAL_OBJECT.name(), i18n.WorkflowMaterial_Type_Digital_Title());
        }});

        DataSourceEnumField way = new DataSourceEnumField(FIELD_WAY);
        way.setTitle(i18n.WorkflowMaterial_Field_Way_Title());
        way.setPrompt(i18n.WorkflowMaterial_Field_Way_Hint());
        way.setValueMap(new LinkedHashMap<String, String>() {{
            put(Way.INPUT.name(), i18n.WorkflowMaterial_Way_Input_Title());
            put(Way.OUTPUT.name(), i18n.WorkflowMaterial_Way_Output_Title());
        }});

        DataSourceTextField path = new DataSourceTextField(FIELD_FOLDER_PATH);
        path.setTitle(i18n.WorkflowMaterial_Field_Path_Title());
        path.setDetail(true);

        DataSourceTextField catalog = new DataSourceTextField(FIELD_PHYSICAL_CATALOG);
        catalog.setTitle(i18n.WorkflowMaterial_Field_Catalog_Title());
        catalog.setDetail(true);

        DataSourceTextField barcode = new DataSourceTextField(FIELD_PHYSICAL_BARCODE);
        barcode.setTitle(i18n.WorkflowMaterial_Field_Barcode_Title());
        barcode.setDetail(true);

        DataSourceTextField field001 = new DataSourceTextField(FIELD_PHYSICAL_FIELD001);
        field001.setTitle(i18n.WorkflowMaterial_Field_Field001_Title());
        field001.setDetail(true);

        DataSourceTextField rdCzId = new DataSourceTextField(FIELD_PHYSICAL_RDCZID);
        rdCzId.setTitle(i18n.WorkflowMaterial_Field_RdCzId_Title());
        rdCzId.setDetail(true);

        DataSourceTextField signature = new DataSourceTextField(FIELD_PHYSICAL_SIGNATURE);
        signature.setTitle(i18n.WorkflowMaterial_Field_Signature_Title());
        signature.setDetail(true);

        DataSourceTextField detail = new DataSourceTextField(FIELD_PHYSICAL_DETAIL,
                i18n.WorkflowMaterial_Field_Detail_Title(), 200);
        detail.setDetail(true);

        DataSourceTextField issue = new DataSourceTextField(FIELD_PHYSICAL_ISSUE,
                i18n.WorkflowMaterial_Field_Issue_Title(), 100);
        issue.setDetail(true);

        DataSourceTextField sigla = new DataSourceTextField(FIELD_PHYSICAL_SIGLA,
                i18n.WorkflowMaterial_Field_Sigla_Title(), 6);
        sigla.setDetail(true);

        DataSourceTextField volume = new DataSourceTextField(FIELD_PHYSICAL_VOLUME,
                i18n.WorkflowMaterial_Field_Volume_Title(), 100);
        volume.setDetail(true);

        DataSourceTextField year = new DataSourceTextField(FIELD_PHYSICAL_YEAR,
                i18n.WorkflowMaterial_Field_Year_Title(), 100);
        year.setDetail(true);

        DataSourceTextField edition = new DataSourceTextField(FIELD_PHYSICAL_EDITION,
                i18n.WorkflowMaterial_Field_Edition_Title(), 100);
        edition.setDetail(true);

        DataSourceTextField pid = new DataSourceTextField(FIELD_DIGITAL_PID);
        pid.setTitle(i18n.WorkflowMaterial_Field_Pid_Title());
        pid.setDetail(true);

        DataSourceTextField metadata = new DataSourceTextField(FIELD_PHYSICAL_METADATA);
        metadata.setTitle(i18n.WorkflowMaterial_Field_Metadata_Title());
        metadata.setDetail(true);

        setFields(profile, type, value, way, note, fieldId, primaryKey,
                path,
                barcode, field001, signature, rdCzId, catalog, metadata, detail, issue, sigla, volume, year,
                pid, edition
        );
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        setOperationBindings(
                RestConfig.createUpdatePostOperation());
    }

    @Override
    protected Object transformRequest(DSRequest dsRequest) {
        if (dsRequest.getOperationType() == DSOperationType.UPDATE) {
            return ClientUtils.dump(dsRequest.getData());
        }
        return super.transformRequest(dsRequest);
    }

    @Override
    protected void transformResponse(DSResponse dsResponse, DSRequest dsRequest, Object data) {
        if (RestConfig.isStatusOk(dsResponse)) {
            Record[] records = dsResponse.getData();
            for (Record record : records) {
                String mid = record.getAttribute(FIELD_ID);
                String way = record.getAttribute(FIELD_WAY);
                String pk = mid;
                if (way != null) {
                    pk = mid + way;
                }
                record.setAttribute(PRIMARY_KEY, pk);
            }
            // #509: do not use the data object as it breaks super.transformResponse
            data = null;
        }
        super.transformResponse(dsResponse, dsRequest, data);
    }

}
