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

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourcePasswordField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import cz.incad.pas.editor.client.PasEditorMessages;

/**
 *
 * @author Jan Pokorsky
 */
public final class UserDataSource extends RestDataSource {

    public static final String ID = "UserDataSource";
    public static final String FIELD_ID = "userId";
    public static final String FIELD_USERNAME = "userName";
    public static final String FIELD_PASSWORD = "userPassword";
    public static final String FIELD_SURNAME = "surname";
    public static final String FIELD_FORENAME = "forename";
    public static final String FIELD_EMAIL = "email";
    public static final String FIELD_HOME = "userHome";
    public static final String FIELD_CREATED = "created";
    public static final String FIELD_LASTLOGIN = "lastLogin";
    public static final String FIELD_WHOAMI = "whoAmI";

    public UserDataSource() {
        setID(ID);

        PasEditorMessages i18nPas = GWT.create(PasEditorMessages.class);

        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_USER);

        DataSourceIntegerField userId = new DataSourceIntegerField(FIELD_ID);
        userId.setPrimaryKey(true);
        userId.setCanEdit(false);
        userId.setHidden(true);
        userId.setTitle(i18nPas.UsersView_ListHeader_Id_Title());

        DataSourceTextField userName = new DataSourceTextField(FIELD_USERNAME);
        userName.setCanEdit(false);
        userName.setRequired(true);
        userName.setReadOnlyEditorType(new StaticTextItem());
        userName.setTitle(i18nPas.UsersView_ListHeader_Username_Title());
        userName.setPrompt(i18nPas.UsersView_ListHeader_Username_Hint());

        DataSourcePasswordField passwd = new DataSourcePasswordField(FIELD_PASSWORD);
        passwd.setHidden(true);
        passwd.setTitle(i18nPas.UsersView_ListHeader_Password_Title());

        DataSourceTextField surname = new DataSourceTextField(FIELD_SURNAME);
        surname.setRequired(true);
        surname.setTitle(i18nPas.UsersView_ListHeader_Surname_Title());

        DataSourceTextField forename = new DataSourceTextField(FIELD_FORENAME);
        forename.setTitle(i18nPas.UsersView_ListHeader_Forename_Title());

        DataSourceTextField email = new DataSourceTextField(FIELD_EMAIL);
        email.setTitle(i18nPas.UsersView_ListHeader_Email_Title());

        DataSourceTextField home = new DataSourceTextField(FIELD_HOME);
        home.setCanEdit(false);
        home.setReadOnlyEditorType(new StaticTextItem());
        home.setHidden(true);
        home.setTitle(i18nPas.UsersView_ListHeader_Home_Title());

        DataSourceDateTimeField created = new DataSourceDateTimeField(FIELD_CREATED);
        created.setCanEdit(false);
        created.setTitle(i18nPas.UsersView_ListHeader_Created_Title());
        created.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);

        setFields(userId, userName, passwd, surname, forename, email, created, home);

        setOperationBindings(RestConfig.createAddOperation(), RestConfig.createUpdateOperation());
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static UserDataSource getInstance() {
        UserDataSource ds = (UserDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new UserDataSource();
        return ds;
    }

}
