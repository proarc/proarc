/*
 * Copyright (C) 2018 Martin Rumanek
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
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;

/**
 *
 */
public class AuthorityModsDataSource extends ProarcDataSource {
    public static final String ID = "AuthorityModsDataSource";

    public static final String FIELD_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    public static final String FIELD_DATA = DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA;
    public static final String FIELD_TIMESTAMP = DigitalObjectResourceApi.TIMESTAMP_PARAM;

    public AuthorityModsDataSource() {
        setID(ID);
        setDataURL(RestConfig.URL_DIGOBJECT_MODS_ADD_AUTHORITY);

        DataSourceField fieldPid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setRequired(true);

        DataSourceField fieldTimestamp = new DataSourceField(FIELD_TIMESTAMP, FieldType.TEXT);
        fieldTimestamp.setRequired(true);
        fieldTimestamp.setHidden(true);

        DataSourceField fieldData = new DataSourceField(FIELD_DATA, FieldType.ANY);

        setFields(fieldTimestamp, fieldData, fieldPid);
        setOperationBindings(RestConfig.createUpdateOperation());
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static AuthorityModsDataSource getInstance() {
        AuthorityModsDataSource ds = (AuthorityModsDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new AuthorityModsDataSource();
        return ds;
    }

    public void addAuthorityXML(DigitalObjectDataSource.DigitalObject dobj, String mods, long timestamp, ModsCustomDataSource.DescriptionSaveHandler callback) {
        Record update = new Record();
        update.setAttribute(FIELD_PID, dobj.getPid());
        update.setAttribute(FIELD_DATA, mods);
        update.setAttribute(FIELD_TIMESTAMP, timestamp);

        updateData(update, callback, callback.getUpdateRequest());
    }
}
