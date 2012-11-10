/*
 * Copyright (C) 2012 Jan Pokorsky
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
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;

/**
 * Data source for digital object's data streams as raw text.
 *
 * @author Jan Pokorsky
 */
public final class TextDataSource extends DataSource {

    public static final String ID_MODS = "ModsTextDataSource";
    public static final String ID_PRIVATE_NOTE = "PrivateNoteTextDataSource";
    public static final String ID_OCR = "OcrDataSource";

    public static final String FIELD_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    // Do not include batchId as DS field as DynamicForm always sets null for missing fetched values.
    // It results in HTTP param batchId=null on update and Jersey reads is as "null" string.
    public static final String FIELD_BATCHID = DigitalObjectResourceApi.BATCHID_PARAM;
    public static final String FIELD_TIMESTAMP = DigitalObjectResourceApi.TIMESTAMP_PARAM;
    public static final String FIELD_CONTENT = DigitalObjectResourceApi.STRINGRECORD_CONTENT;

    public TextDataSource(String dsId, String dsUrl) {
        setID(id);

        setRecordXPath('/' + DigitalObjectResourceApi.STRINGRECORD_ELEMENT);
        setDataFormat(DSDataFormat.JSON);

        setDataURL(dsUrl);

        DataSourceField fieldPid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setRequired(true);

//        DataSourceField fieldBatchId = new DataSourceField(FIELD_BATCHID, FieldType.TEXT);
//        fieldBatchId.setHidden(true);

        DataSourceField fieldTimestamp = new DataSourceField(FIELD_TIMESTAMP, FieldType.TEXT);
        fieldTimestamp.setRequired(true);
        fieldTimestamp.setHidden(true);

        DataSourceField fieldContent = new DataSourceField(FIELD_CONTENT, FieldType.TEXT);

        setFields(fieldPid, fieldTimestamp, fieldContent);

        setOperationBindings(RestConfig.createUpdateOperation());

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static TextDataSource getOcr() {
        return getDS(ID_OCR, RestConfig.URL_DIGOBJECT_OCR);
    }

    public static TextDataSource getNote() {
        return getDS(ID_PRIVATE_NOTE, RestConfig.URL_DIGOBJECT_PRIVATE_NOTE);
    }

    public static TextDataSource getMods() {
        return getDS(ID_MODS, RestConfig.URL_DIGOBJECT_MODS_PLAIN);
    }

    private static TextDataSource getDS(String dsId, String dsUrl) {
        TextDataSource ds = (TextDataSource) DataSource.get(dsId);
        ds = (ds != null) ? ds : new TextDataSource(dsId, dsUrl);
        return ds;
    }

}
