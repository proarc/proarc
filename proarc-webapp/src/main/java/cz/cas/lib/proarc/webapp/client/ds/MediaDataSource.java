/*
 * Copyright (C) 2017 Jakub Kremlacek
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
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;

/**
 * Serves as access point to specific datastream for its removal.
 *
 * @author Jakub Kremlacek
 */
public class MediaDataSource extends RestDataSource {

    public static final String FIELD_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    public static final String ID_RAW = "RawDataSource";
    public static final String ID_PREVIEW = "PreviewDataSource";

    public MediaDataSource(String dsId, String dsUrl) {
        setID(dsId);

        setRecordXPath('/' + DigitalObjectResourceApi.STRINGRECORD_ELEMENT);
        setDataFormat(DSDataFormat.JSON);

        setDataURL(dsUrl);

        DataSourceField fieldPid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setRequired(true);

        setFields(fieldPid);

        setOperationBindings(RestConfig.createDeleteOperation());

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static MediaDataSource getRaw() {
        return getDS(ID_RAW, RestConfig.URL_DIGOBJECT_RAW);
    }

    public static MediaDataSource getPreview() {
        return getDS(ID_PREVIEW, RestConfig.URL_DIGOBJECT_PREVIEW);
    }

    private static MediaDataSource getDS(String dsId, String dsUrl) {
        MediaDataSource ds = (MediaDataSource) DataSource.get(dsId);

        ds = (ds != null) ? ds : new MediaDataSource(dsId, dsUrl);
        return ds;
    }
}
