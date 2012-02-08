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
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;

/**
 *
 * @author Jan Pokorsky
 */
public class UserDataSource extends DataSource {

    public static final String ID = "UserDataSource";
    public static final String FIELD_ID = "id";
    public static final String FIELD_USERNAME = "username";
    public static final String FIELD_USER_DISPLAYNAME = "displayname";
    public static final String FIELD_IMPORT_FOLDER = "importFolder";
    public static final String FIELD_TIMESTAMP = "created";
    public static final String FIELD_LOGIN_TIMESTAMP = "lastLogin";

    public UserDataSource() {
        setID(ID);

//        setDataFormat(DSDataFormat.XML);
        setDataFormat(DSDataFormat.JSON);
//        setRecordXPath("/users/user");
        setRecordXPath("user");

// XXX        setDataURL(RestConfig.URL_IMPORT_BATCH);
        setDataURL("ds/UserDataSource.json");

        DataSourceIntegerField id = new DataSourceIntegerField(FIELD_ID);
        id.setPrimaryKey(true);

        DataSourceTextField userName = new DataSourceTextField(FIELD_USERNAME);

        DataSourceTextField displayname = new DataSourceTextField(FIELD_USER_DISPLAYNAME);

        DataSourceTextField importFolder = new DataSourceTextField(FIELD_IMPORT_FOLDER);

        DataSourceDateTimeField timestamp = new DataSourceDateTimeField(FIELD_TIMESTAMP);

        setFields(id, userName, displayname, timestamp, importFolder);

// XXX       setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        setClientOnly(true);
    }

    public static ImportBatchDataSource getInstance() {
        ImportBatchDataSource ds = (ImportBatchDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new ImportBatchDataSource();
        return ds;
    }

}
