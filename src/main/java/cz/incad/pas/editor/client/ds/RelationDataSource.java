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

import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.FieldType;

/**
 *
 * @author Jan Pokorsky
 */
public class RelationDataSource extends RestDataSource {

    public static final String ID = "RelationDataSource";

    public static final String FIELD_PID = "pid";
    public static final String FIELD_PARENT = "parent";
    public static final String FIELD_ROOT = "root";
    public static final String FIELD_MODEL = "model";
    public static final String FIELD_OWNER = "owner";
    public static final String FIELD_LABEL = "label";
    public static final String FIELD_STATE = "state";
    public static final String FIELD_CREATED = "created";
    public static final String FIELD_MODIFIED = "modified";

    public RelationDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_DIGOBJECT_CHILDREN);

        DataSourceField pid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        pid.setPrimaryKey(true);
        pid.setRequired(true);

        DataSourceField parent = new DataSourceField(FIELD_PARENT, FieldType.TEXT);
        parent.setForeignKey(ID + '.' + FIELD_PID);
        parent.setRequired(true);

        DataSourceField root = new DataSourceField(FIELD_ROOT, FieldType.TEXT);
        root.setHidden(true);

        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        DataSourceField owner = new DataSourceField(FIELD_OWNER, FieldType.TEXT);
        DataSourceField label = new DataSourceField(FIELD_LABEL, FieldType.TEXT);
        DataSourceDateTimeField created = new DataSourceDateTimeField(FIELD_CREATED);
        DataSourceDateTimeField modified = new DataSourceDateTimeField(FIELD_MODIFIED);

        setFields(pid, parent, label, model, created, modified, owner);
        setTitleField(FIELD_LABEL);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        
    }

    @Override
    protected void transformResponse(DSResponse response, DSRequest request, Object data) {
        if (response.getStatus() == RPCResponse.STATUS_SUCCESS
                && request.getOperationType() == DSOperationType.FETCH) {
            // fill parent fields
            String parent = request.getCriteria().getAttribute(FIELD_PARENT);
            for (Record record : response.getData()) {
                record.setAttribute(FIELD_PARENT, parent);
            }
        }
        super.transformResponse(response, request, data);
    }

    public static RelationDataSource getInstance() {
        RelationDataSource ds = (RelationDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new RelationDataSource();
        return ds;
    }

}
