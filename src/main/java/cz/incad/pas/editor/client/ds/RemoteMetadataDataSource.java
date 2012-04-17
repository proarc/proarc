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

import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import cz.incad.pas.editor.client.ClientUtils;
import java.util.logging.Logger;

/**
 * metadata dowl
 *
 * @author Jan Pokorsky
 */
public final class RemoteMetadataDataSource extends DataSource {
    
    private static final Logger LOG = Logger.getLogger(RemoteMetadataDataSource.class.getName());

    public static final String ID = "RemoteMetadataDataSource";

    public static final String FIELD_ID = "id";
    public static final String FIELD_MODS = "mods";
    public static final String FIELD_PREVIEW = "preview";
    public static final String FIELD_TITLE = "title";

    public RemoteMetadataDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.JSON);
        setRecordXPath("/metadataCatalogEntries/entry");

        setDataURL(RestConfig.URL_METADATACATALOG);

        DataSourceField fieldId = new DataSourceField(FIELD_ID, FieldType.TEXT);
        fieldId.setPrimaryKey(true);
        fieldId.setRequired(true);
        fieldId.setHidden(true);

        DataSourceField fieldMods = new DataSourceField(FIELD_MODS, FieldType.TEXT);
        fieldMods.setHidden(true);

        DataSourceField fieldPreview = new DataSourceField(FIELD_PREVIEW, FieldType.TEXT);
        fieldPreview.setDetail(true);

        DataSourceField fieldTitle = new DataSourceField(FIELD_TITLE, FieldType.TEXT);

        setFields(fieldId, fieldMods, fieldPreview, fieldTitle);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));

    }

    @Override
    protected void transformResponse(DSResponse response, DSRequest request, Object data) {
        int code = response.getStatus();
        int httpResponseCode = response.getHttpResponseCode();
        ClientUtils.fine(LOG, "transformResponse: %s, status: %s, http: %s", data, code, httpResponseCode);
        if (code == DSResponse.STATUS_FAILURE && httpResponseCode == 200 && data == null) {
            // jersye serialize empty JSON as null
            response.setData(new Record[0]);
            response.setStatus(DSResponse.STATUS_SUCCESS);
        }
        super.transformResponse(response, request, data);
    }

    public static RemoteMetadataDataSource getInstance() {
        RemoteMetadataDataSource ds = (RemoteMetadataDataSource) DataSource.get(ID);
        ds = (ds != null) ? ds : new RemoteMetadataDataSource();
        return ds;
    }

}
