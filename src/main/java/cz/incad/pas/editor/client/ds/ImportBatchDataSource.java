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
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportBatchDataSource extends DataSource {

    public static final String ID = "ImportBatchDataSource";
    public static final String FIELD_ID = "id";
    public static final String FIELD_PATH = "folderPath";
    public static final String FIELD_TIMESTAMP = "timeStamp";
    public static final String FIELD_STATE = "state";
    // XXX Jak udelat foreing key ??? <= v gridu nebo ve formu udelej text field s options data sourcem a zadej display value
    public static final String FIELD_USER_ID = "userId";
    public static final String FIELD_USER_DISPLAYNAME = "user";

    public ImportBatchDataSource() {
        setID(ID);

//        setDataFormat(DSDataFormat.XML);
        setDataFormat(DSDataFormat.JSON);
//        setRecordXPath("/batches/batch");
        setRecordXPath("batch");

        setDataURL(RestConfig.URL_IMPORT_BATCH);
        setDataURL("ds/ImportBatchDataSource.json");
        setClientOnly(true);

        DataSourceIntegerField id = new DataSourceIntegerField(FIELD_ID);
        id.setPrimaryKey(true);

        DataSourceTextField path = new DataSourceTextField(FIELD_PATH);

        DataSourceTextField user = new DataSourceTextField(FIELD_USER_DISPLAYNAME);

        DataSourceIntegerField userId = new DataSourceIntegerField(FIELD_USER_ID);
        userId.setForeignKey(UserDataSource.ID + '.' + UserDataSource.FIELD_ID);

        DataSourceDateTimeField timestamp = new DataSourceDateTimeField(FIELD_TIMESTAMP);

        DataSourceBooleanField state = new DataSourceBooleanField(FIELD_STATE);

        setFields(id, path, userId, user, timestamp, state);
        
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public Record newBatch(String folderPath, String model) {
        Record r = new Record();
        r.setAttribute(FIELD_PATH, folderPath);
        r.setAttribute("model", model);
        return r;
    }

    public static ImportBatchDataSource getInstance() {
        ImportBatchDataSource ds = (ImportBatchDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new ImportBatchDataSource();
        return ds;
    }

    public static class BatchRecord {

        private final Record delegate;

        public BatchRecord(Record delegate) {
            this.delegate = delegate;
        }

        public String getId() {
            return delegate.getAttribute(FIELD_ID);
        }
    }

}
