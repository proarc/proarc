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
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;

/**
 *
 * @author Jan Pokorsky
 */
public final class OcrDataSource extends DataSource {

    public static final String ID = "OcrDataSource";

    public static final String FIELD_PID = "pid";
    public static final String FIELD_BATCHID = "batchId";
    public static final String FIELD_TIMESTAMP = "timestamp";
    public static final String FIELD_OCR = "content";

    public OcrDataSource() {
        setID(ID);

        setRecordXPath("/record");
        setDataFormat(DSDataFormat.JSON);
        
        setDataURL(RestConfig.URL_DIGOBJECT_OCR);

        DataSourceField fieldPid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setRequired(true);

        DataSourceField fieldBatchId = new DataSourceField(FIELD_BATCHID, FieldType.TEXT);
        fieldBatchId.setHidden(true);
        fieldBatchId.setCanView(true);

        DataSourceField fieldTimestamp = new DataSourceField(FIELD_TIMESTAMP, FieldType.TEXT);
        fieldTimestamp.setRequired(true);
        fieldTimestamp.setHidden(true);

        DataSourceField fieldOcr = new DataSourceField(FIELD_OCR, FieldType.TEXT);

        setFields(fieldPid, fieldBatchId, fieldTimestamp, fieldOcr);

        setOperationBindings(RestConfig.createUpdateOperation());

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        
    }

    public static OcrDataSource getInstance() {
        OcrDataSource ds = (OcrDataSource) DataSource.get(ID);
        ds = (ds != null) ? ds : new OcrDataSource();
        return ds;
    }

}
