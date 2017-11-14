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
import com.smartgwt.client.types.CriteriaPolicy;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;

/**
 * Serves as access point to specific datastream for its removal.
 *
 * @author Jakub Kremlacek
 */
public class MediaDataSource extends RestDataSource {

    public static final String OBJECT_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    public static final String DATASTREAM_ID = DigitalObjectResourceApi.DISSEMINATION_DATASTREAM;
    public static final String BATCH_ID = DigitalObjectResourceApi.BATCHID_PARAM;

    public static final String ID = "MediaDataSource";

    private static MediaDataSource INSTANCE;

    public MediaDataSource() {
        setID(ID);

        setRecordXPath('/' + DigitalObjectResourceApi.STRINGRECORD_ELEMENT);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_DIGOBJECT_DISSEMINATION);

        DataSourceField objectPidDSF = new DataSourceField(OBJECT_PID, FieldType.TEXT);
        objectPidDSF.setPrimaryKey(true);
        objectPidDSF.setRequired(true);

        DataSourceField datastreamIdDSF = new DataSourceField(DATASTREAM_ID, FieldType.TEXT);
        DataSourceField batchIdDSF = new DataSourceField(BATCH_ID, FieldType.TEXT);

        setFields(objectPidDSF, batchIdDSF, datastreamIdDSF);
        setTitleField(OBJECT_PID);

        setOperationBindings(RestConfig.createDeleteOperation());
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));

        setCriteriaPolicy(CriteriaPolicy.DROPONCHANGE);
    }

    public static MediaDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = (MediaDataSource) DataSource.get(ID);
            // DataSource.get does not work reliably
            INSTANCE = INSTANCE != null ? INSTANCE : new MediaDataSource();
        }
        return INSTANCE;
    }
}
