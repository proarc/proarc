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
package cz.cas.lib.proarc.webapp.client.ds;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourcePasswordField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.widget.Organization;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;

/**
 * Manages application users.
 *
 * @author Jan Pokorsky
 */
public final class UserDataSource extends ProarcDataSource {

    public static final String ID = "UserDataSource";
    public static final String FIELD_ID = UserResourceApi.USER_ID;
    public static final String FIELD_USERNAME = UserResourceApi.USER_NAME;
    public static final String FIELD_PASSWORD = UserResourceApi.USER_PASSWORD;
    public static final String FIELD_ROLE = UserResourceApi.USER_ROLE;
    public static final String FIELD_WHOAMI = UserResourceApi.USER_WHOAMI_PARAM;
    public static final String FIELD_CHANGE_MODEL_FUNCTION = UserResourceApi.USER_RUN_CHANGE_MODEL_FUNCTION;
    public static final String FIELD_UPDATE_MODEL_FUNCTION = UserResourceApi.USER_RUN_UPDATE_MODEL_FUNCTION;
    public static final String FIELD_LOCK_OBJECT_FUNCTION = UserResourceApi.USER_RUN_LOCK_OBJECT_FUNCTION;
    public static final String FIELD_UNLOCK_OBJECT_FUNCTION = UserResourceApi.USER_RUN_UNLOCK_OBJECT_FUNCTION;

    private static UserDataSource INSTANCE;

    public UserDataSource() {
        setID(ID);

        ClientMessages i18n = GWT.create(ClientMessages.class);

        setDataURL(RestConfig.URL_USER);

        DataSourceIntegerField userId = new DataSourceIntegerField(FIELD_ID);
        userId.setPrimaryKey(true);
        userId.setCanEdit(false);
        userId.setHidden(true);
        userId.setTitle(i18n.UsersView_ListHeader_Id_Title());

        DataSourceTextField userName = new DataSourceTextField(FIELD_USERNAME);
        userName.setCanEdit(false);
        userName.setRequired(true);
        userName.setReadOnlyEditorProperties(new StaticTextItem());
        userName.setTitle(i18n.UsersView_ListHeader_Username_Title());
        userName.setPrompt(i18n.UsersView_ListHeader_Username_Hint());

        DataSourcePasswordField passwd = new DataSourcePasswordField(FIELD_PASSWORD);
        passwd.setHidden(true);
        passwd.setTitle(i18n.UsersView_ListHeader_Password_Title());

        DataSourceTextField surname = new DataSourceTextField(UserResourceApi.USER_SURNAME);
        surname.setRequired(true);
        surname.setTitle(i18n.UsersView_ListHeader_Surname_Title());

        DataSourceTextField forename = new DataSourceTextField(UserResourceApi.USER_FORENAME);
        forename.setTitle(i18n.UsersView_ListHeader_Forename_Title());

        DataSourceTextField organization = new DataSourceTextField(UserResourceApi.USER_ORGANIZATION);
        organization.setTitle(i18n.UsersView_ListHeader_Organization_Title());
        organization.setValueMap(Organization.getMap());
        organization.setReadOnlyEditorProperties(new StaticTextItem());

        DataSourceTextField role = new DataSourceTextField(FIELD_ROLE);
        role.setTitle(i18n.UsersView_ListHeader_Role_Title());
        role.setValueMap(UserRole.getMap());
        role.setReadOnlyEditorProperties(new StaticTextItem());

        DataSourceBooleanField changeModelFunction = new DataSourceBooleanField(FIELD_CHANGE_MODEL_FUNCTION);
        changeModelFunction.setTitle(i18n.UsersView_ListHeader_ChangeModelFunction_Title());

        DataSourceBooleanField updateModelFunction = new DataSourceBooleanField(FIELD_UPDATE_MODEL_FUNCTION);
        updateModelFunction.setTitle(i18n.UsersView_ListHeader_UpdateModelFunction_Title());

        DataSourceBooleanField lockObjectFunction = new DataSourceBooleanField(FIELD_LOCK_OBJECT_FUNCTION);
        lockObjectFunction.setTitle(i18n.UsersView_ListHeader_LockObjectFunction_Title());

        DataSourceBooleanField unlockObjectFunction = new DataSourceBooleanField(FIELD_UNLOCK_OBJECT_FUNCTION);
        unlockObjectFunction.setTitle(i18n.UsersView_ListHeader_UnlockObjectFunction_Title());

        DataSourceTextField email = new DataSourceTextField(UserResourceApi.USER_EMAIL);
        email.setTitle(i18n.UsersView_ListHeader_Email_Title());

        DataSourceTextField home = new DataSourceTextField(UserResourceApi.USER_HOME);
        home.setCanEdit(false);
        home.setReadOnlyEditorProperties(new StaticTextItem());
        home.setHidden(true);
        home.setTitle(i18n.UsersView_ListHeader_Home_Title());

        DataSourceDateTimeField created = new DataSourceDateTimeField(UserResourceApi.USER_CREATED);
        created.setCanEdit(false);
        created.setTitle(i18n.UsersView_ListHeader_Created_Title());
        created.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);

        DataSourceTextField remoteName = new DataSourceTextField(UserResourceApi.USER_REMOTENAME);
        remoteName.setTitle(i18n.UsersView_ListHeader_RemoteName_Title());
        remoteName.setCanEdit(false);
        remoteName.setHidden(true);

        DataSourceTextField remoteType = new DataSourceTextField(UserResourceApi.USER_REMOTETYPE);
        remoteType.setTitle(i18n.UsersView_ListHeader_RemoteType_Title());
        remoteType.setCanEdit(false);
        remoteType.setHidden(true);

        setFields(userId, userName, passwd, surname, forename, organization, role, email, created, remoteName,
                remoteType, home, changeModelFunction, updateModelFunction, lockObjectFunction, unlockObjectFunction);

        setOperationBindings(RestConfig.createAddOperation(), RestConfig.createUpdateOperation());
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    @Override
    protected Object transformRequest(DSRequest dsRequest) {
        DSOperationType op = dsRequest.getOperationType();
        if (op == DSOperationType.ADD || op == DSOperationType.UPDATE) {
            JavaScriptObject data = dsRequest.getData();
            Record nonNulls = ClientUtils.removeNulls(new Record(data));
            dsRequest.setData(nonNulls);
        }
        return super.transformRequest(dsRequest);
    }

    public static UserDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new UserDataSource();
        }
        return INSTANCE;
    }

}
