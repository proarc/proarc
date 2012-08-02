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
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import java.util.HashSet;

/**
 *
 * @author Jan Pokorsky
 */
public final class UserPermissionDataSource extends RestDataSource {

    public static final String ID = "UserPermissionsDataSource";
    public static final String FIELD_USERID = "userId";
    public static final String FIELD_PERMISSIONID = "permissionId";
    public static final String FIELD_DISPLAY = "display";

    public UserPermissionDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_USER_PERMISSIONS);

        DataSourceTextField permId = new DataSourceTextField(FIELD_PERMISSIONID);
        permId.setPrimaryKey(true);
        permId.setCanEdit(false);
        permId.setHidden(true);

        DataSourceTextField display = new DataSourceTextField(FIELD_DISPLAY);

        setFields(permId, display);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static UserPermissionDataSource getInstance() {
        UserPermissionDataSource ds = (UserPermissionDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new UserPermissionDataSource();
        return ds;
    }

    public static HashSet<String> asPermissions(Record[] records) {
        HashSet<String> result = new HashSet<String>(records.length);
        for (Record record : records) {
            result.add(record.getAttribute(FIELD_PERMISSIONID));
        }
        return result;
    }

}
