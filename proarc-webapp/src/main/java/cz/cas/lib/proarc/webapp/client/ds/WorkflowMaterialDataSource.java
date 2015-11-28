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

import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.Way;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.LinkedHashMap;

/**
 * The data source of workflow materials.
 *
 * @author Jan Pokorsky
 */
public class WorkflowMaterialDataSource extends RestDataSource {

    public static final String ID = "WorkflowMaterialDataSource";
    public static final String PRIMARY_KEY = "__syntheticPrimaryKey";
    public static final String FIELD_ID = WorkflowModelConsts.MATERIAL_ID;
    public static final String FIELD_JOB_ID = "jobId";
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
    public static final String FIELD_PHYSICAL_METADATA = WorkflowModelConsts.MATERIAL_METADATA;
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
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_WORKFLOW_MATERIAL);

        DataSourceTextField primaryKey = new DataSourceTextField(PRIMARY_KEY);
        primaryKey.setCanEdit(false);
        primaryKey.setCanSave(false);
        primaryKey.setHidden(true);
        primaryKey.setPrimaryKey(true);

        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setCanEdit(false);
        fieldId.setDetail(true);
        fieldId.setTitle("ID");

        DataSourceTextField note = new DataSourceTextField(FIELD_NOTE);
        note.setTitle("Poznámka");

        DataSourceTextField value = new DataSourceTextField(FIELD_VALUE);
        value.setTitle("Obsah");

        DataSourceTextField profile = new DataSourceTextField(FIELD_PROFILENAME);
        profile.setTitle("Popis");
        profile.setDisplayField(WorkflowModelConsts.MATERIAL_PROFILELABEL);

        DataSourceEnumField type = new DataSourceEnumField(FIELD_TYPE);
        type.setTitle("Typ materiálu");
        type.setValueMap(new LinkedHashMap<String, String>() {{
            put(MaterialType.FOLDER.name(), "Adresář");
            put(MaterialType.PHYSICAL_DOCUMENT.name(), "Předloha");
            put(MaterialType.DIGITAL_OBJECT.name(), "Dig. objekt");
        }});

        DataSourceEnumField way = new DataSourceEnumField(FIELD_WAY);
        way.setTitle("V/V");
        way.setPrompt("Vstupní/Výstupní");
        way.setValueMap(new LinkedHashMap<String, String>() {{
            put(Way.INPUT.name(), "Vstupní");
            put(Way.OUTPUT.name(), "Výstupní");
        }});

        DataSourceTextField path = new DataSourceTextField(FIELD_FOLDER_PATH);
        path.setTitle("Cesta");
        path.setDetail(true);

        DataSourceTextField catalog = new DataSourceTextField(FIELD_PHYSICAL_CATALOG);
        catalog.setTitle("Zdroj");
        catalog.setDetail(true);

        DataSourceTextField barcode = new DataSourceTextField(FIELD_PHYSICAL_BARCODE);
        barcode.setTitle("Čárový kód");
        barcode.setDetail(true);

        DataSourceTextField field001 = new DataSourceTextField(FIELD_PHYSICAL_FIELD001);
        field001.setTitle("Pole 001");
        field001.setDetail(true);

        DataSourceTextField rdCzId = new DataSourceTextField(FIELD_PHYSICAL_RDCZID);
        rdCzId.setTitle("RD CZ ID");
        rdCzId.setDetail(true);

        DataSourceTextField pid = new DataSourceTextField(FIELD_DIGITAL_PID);
        pid.setTitle("PID");
        pid.setDetail(true);

        DataSourceTextField metadata = new DataSourceTextField(FIELD_PHYSICAL_METADATA);
        metadata.setTitle("Metadata");
        metadata.setDetail(true);

        setFields(profile, type, value, way, note, fieldId, primaryKey,
                path,
                barcode, field001, rdCzId, catalog, metadata,
                pid
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
        }
        super.transformResponse(dsResponse, dsRequest, data);
    }

}
