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

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import java.util.LinkedHashMap;

/**
 * The data source of workflow materials.
 *
 * @author Jan Pokorsky
 */
public class WorkflowMaterialDataSource extends DataSource {

    public static final String ID = "WorkflowMaterialDataSource";
    public static final String FIELD_ID = "id";
    public static final String FIELD_JOB_ID = "jobId";
    public static final String FIELD_TASK_ID = "taskId";
    public static final String FIELD_NOTE = "note";
    public static final String FIELD_TYPE = "type";
    public static final String FIELD_VALUE = "value";
    public static final String FIELD_WAY = "way";

    // custom fields
    public static final String FIELD_FOLDER_PATH = "folderPath";
    public static final String FIELD_PHYSICAL_BARCODE = "physicalBarCode";
    public static final String FIELD_PHYSICAL_FIELD001 = "physicalField001";
    public static final String FIELD_PHYSICAL_RDCZID = "physicalRdCzId";
    public static final String FIELD_PHYSICAL_METADATA = "physicalMetadata";
    public static final String FIELD_DIGITAL_PID = "digitalPid";

    public static WorkflowMaterialDataSource INSTANCE;

    public static WorkflowMaterialDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new WorkflowMaterialDataSource();
        }
        return INSTANCE;
    }

    public WorkflowMaterialDataSource() {
        setID(ID);
        setClientOnly(true);
        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setCanEdit(false);
        fieldId.setDetail(true);
        fieldId.setTitle("ID");

        DataSourceTextField note = new DataSourceTextField(FIELD_NOTE);
        note.setTitle("Poznámka");

        DataSourceTextField value = new DataSourceTextField(FIELD_VALUE);
        value.setTitle("Obsah");

        DataSourceEnumField type = new DataSourceEnumField(FIELD_TYPE);
        type.setTitle("Typ materiálu");
        type.setValueMap(new LinkedHashMap<String, String>() {{
            put("folder", "Adresář");
            put("physicalDocument", "Předloha");
            put("digitalDocument", "Dig. objekt");
        }});

        DataSourceEnumField way = new DataSourceEnumField(FIELD_WAY);
        way.setTitle("IO");
        way.setValueMap(new LinkedHashMap<String, String>() {{
            put("input", "Vstupní");
            put("output", "Výstupní");
        }});

        DataSourceTextField path = new DataSourceTextField(FIELD_FOLDER_PATH);
        path.setTitle("Cesta");
        path.setDetail(true);

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

        setFields(type, value, way, note, fieldId,
                path,
                barcode, field001, rdCzId, metadata,
                pid
        );
    }

}
