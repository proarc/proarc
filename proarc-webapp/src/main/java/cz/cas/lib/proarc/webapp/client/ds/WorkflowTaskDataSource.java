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

import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import java.util.Date;
import java.util.LinkedHashMap;

/**
 * The data source of workflow's tasks.
 *
 * @author Jan Pokorsky
 */
public class WorkflowTaskDataSource extends RestDataSource {

    public static final String ID = "WorkflowTaskDataSource";

    public static final String FIELD_CREATED = "created";
    public static final String FIELD_JOB_ID = "jobId";
    public static final String FIELD_JOB_LABEL = "jobLabel";
    public static final String FIELD_ID = "id";
    // LABEL stands for i18n of TYPE
    public static final String FIELD_LABEL = "label";
    public static final String FIELD_MATERIALS = "materials";
    public static final String FIELD_MODIFIED = "modified";
    public static final String FIELD_NOTE = "note";
    public static final String FIELD_OWNER = "owner";
    public static final String FIELD_PARAMETERS = "params";
    public static final String FIELD_PRIORITY = "priority";
    public static final String FIELD_STATE = "state";
    public static final String FIELD_TYPE = "type";

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
        setClientOnly(true);
        setTestData(createDemoData());
//        setDataURL(RestConfig.URL_DEVICE);

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
            put("waiting", "Čeká");
            put("ready", "Připraven");
            put("started", "Probíhá");
            put("finished", "Dokončen");
            put("canceled", "Zrušen");
        }});
        state.setRequired(true);

        DataSourceTextField owner = new DataSourceTextField(FIELD_OWNER);
        owner.setTitle("Pracovník");

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

    private static Record[] createDemoData() {
        return new Record[] {
            createDemoTask()
        };
    }

    private static Record createDemoTask() {
        Record task = new ListGridRecord();
        task.setAttribute(WorkflowTaskDataSource.FIELD_ID, "1");
        task.setAttribute(WorkflowTaskDataSource.FIELD_LABEL, "OCR");
        task.setAttribute(WorkflowTaskDataSource.FIELD_PRIORITY, "1");
        task.setAttribute(WorkflowTaskDataSource.FIELD_OWNER, "operator");
        task.setAttribute(WorkflowTaskDataSource.FIELD_TYPE, "ocrTask");
        task.setAttribute(WorkflowTaskDataSource.FIELD_STATE, "ready");
        task.setAttribute(WorkflowTaskDataSource.FIELD_PRIORITY, "5");
        task.setAttribute(WorkflowTaskDataSource.FIELD_JOB_ID, "1");
        task.setAttribute(WorkflowTaskDataSource.FIELD_JOB_LABEL, "Babička");
        task.setAttribute(WorkflowTaskDataSource.FIELD_CREATED, new Date());
        task.setAttribute(WorkflowTaskDataSource.FIELD_MODIFIED, new Date());

        Record[] params = new Record[]{new Record(), new Record()};
        params[0].setAttribute("label", "dpi");
        params[0].setAttribute("value", "300");
        params[1].setAttribute("label", "resolution");
        params[1].setAttribute("value", "2048x1024");
        task.setAttribute(FIELD_PARAMETERS, params);

        task.setAttribute(FIELD_MATERIALS, createDemoMaterials());
        return task;
    }

    static Record[] createDemoMaterials() {
        Record[] materials = new Record[]{new Record(), new Record(), new Record()};
        materials[0].setAttribute("ID", "1");
        materials[0].setAttribute("type", "folder");
        materials[0].setAttribute("way", "input");
        materials[0].setAttribute("value", "tmp");
        materials[0].setAttribute("note", "vzkaz");
        materials[0].setAttribute("folderPath", "C:\\tmp");

        materials[1].setAttribute("ID", "2");
        materials[1].setAttribute("type", "physicalDocument");
        materials[1].setAttribute("way", "input");
        materials[1].setAttribute("value", "Babička");
        materials[1].setAttribute("physicalBarCode", "123456");
        materials[1].setAttribute("physicalField001", "Cz123456");
        materials[1].setAttribute("physicalRdCzId", "321");
        materials[1].setAttribute("physicalMetadata", "<mods></mods>");

        materials[2].setAttribute("ID", "3");
        materials[2].setAttribute("type", "digitalDocument");
        materials[2].setAttribute("way", "output");
        materials[2].setAttribute("value", "Babička");
        materials[2].setAttribute("digitalPid", "uuid:d0667b41-ef19-445b-afae-873ebe5756d8");
        return materials;
    }

}
