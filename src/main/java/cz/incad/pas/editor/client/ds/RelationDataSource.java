/*
 * Copyright (C) 2011 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.client.ds;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;

/**
 *
 * @author Jan Pokorsky
 */
public class RelationDataSource extends DataSource {

    public static final String ID = "RelationDataSource";

    public static final String FIELD_PID = "pid";
    public static final String FIELD_MODEL = "model";
    public static final String FIELD_DISPLAY_NAME = "displayName";
    public static final String FIELD_PARENT = "parent";
    public static final String FIELD_ROOT = "root";

    public RelationDataSource() {
        setID(ID);

//        setDataFormat(DSDataFormat.XML);
        setDataFormat(DSDataFormat.JSON);
//        setRecordXPath("/items/item");
        setRecordXPath("item");

//        setDataURL(RestConfig.URL_IMPORT_BATCH_ITEM);
        setDataURL("ds/RelationDataSource.json");
        setClientOnly(true);

        DataSourceField fieldPid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setHidden(true);

        DataSourceField fieldParent = new DataSourceField(FIELD_PARENT, FieldType.TEXT);
        fieldParent.setForeignKey(FIELD_PID);
        fieldParent.setHidden(true);

        DataSourceField fieldRoot = new DataSourceField(FIELD_ROOT, FieldType.TEXT);
        fieldRoot.setHidden(true);

        DataSourceTextField fieldModel = new DataSourceTextField(FIELD_MODEL);
        fieldModel.setForeignKey(MetaModelDataSource.ID + '.' + MetaModelDataSource.FIELD_PID);

        DataSourceField fieldDisplayName = new DataSourceField(FIELD_DISPLAY_NAME, FieldType.TEXT);

        setFields(fieldPid, fieldParent, fieldDisplayName, fieldModel, fieldRoot);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        
    }

    public static RelationDataSource getInstance() {
        RelationDataSource ds = (RelationDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new RelationDataSource();
        return ds;
    }

}
