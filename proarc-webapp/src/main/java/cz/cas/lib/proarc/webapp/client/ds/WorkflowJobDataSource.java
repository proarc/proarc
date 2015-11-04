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

import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import java.util.Date;
import java.util.LinkedHashMap;

/**
 * The data source of workflow jobs.
 *
 * @author Jan Pokorsky
 */
public class WorkflowJobDataSource extends RestDataSource {

    public static final String ID = "WorkflowJobDataSource";

    public static final String FIELD_CREATED = "created";
    public static final String FIELD_ID = "id";
    public static final String FIELD_FINANCED = "financed";
    public static final String FIELD_LABEL = "label";
    public static final String FIELD_MATERIALS = "materials";
    public static final String FIELD_MODIFIED = "modified";
    public static final String FIELD_NOTE = "note";
    public static final String FIELD_OWNER = "owner";
    public static final String FIELD_PRIORITY = "priority";
    public static final String FIELD_PROFILE_ID = "profileId";
    public static final String FIELD_STATE = "state";

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
//        setClientOnly(true);
//        setTestData(createDemoJob());
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

        DataSourceEnumField state = new DataSourceEnumField(FIELD_STATE);
        state.setTitle("Stav");
        state.setValueMap(new LinkedHashMap<String,String>() {{
            put("open", "Otevřený");
            put("finished", "Hotový");
            put("canceled", "Zrušený");
        }});
        state.setRequired(true);

        DataSourceTextField owner = new DataSourceTextField(FIELD_OWNER);
        owner.setTitle("Vlastník");

        DataSourceEnumField priority = new DataSourceEnumField(FIELD_PRIORITY);
        priority.setTitle("Priorita");
        priority.setRequired(true);
        String[] priorities = new String[10];
        for (int i = 0; i < priorities.length; i++) {
            priorities[i] = String.valueOf(i + 1);
        }
        priority.setValueMap(priorities);

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

    private static Record createDemoJob() {
        Record job = new ListGridRecord();
        job.setAttribute(FIELD_CREATED, new Date());
        job.setAttribute(FIELD_FINANCED, "VISK");
        job.setAttribute(FIELD_ID, "1");
        job.setAttribute(FIELD_LABEL, "Záměr - Babička");
        job.setAttribute(FIELD_MODIFIED, new Date());
        job.setAttribute(FIELD_NOTE, "spěchá");
        job.setAttribute(FIELD_OWNER, "proarc");
        job.setAttribute(FIELD_PRIORITY, "1");
        job.setAttribute(FIELD_PROFILE_ID, "ndk");
        job.setAttribute(FIELD_STATE, "open");
        job.setAttribute(FIELD_MATERIALS, WorkflowTaskDataSource.createDemoMaterials());
        return job;
    }

}
