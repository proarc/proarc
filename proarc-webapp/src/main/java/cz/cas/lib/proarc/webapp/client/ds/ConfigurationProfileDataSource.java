/*
 * Copyright (C) 2015 Jan Pokorsky
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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.widgets.form.fields.FormItem;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi.ProfileGroup;

/**
 * Configuration profiles.
 *
 * @author Jan Pokorsky
 */
public class ConfigurationProfileDataSource extends RestDataSource {

    public static final String ID = "ConfigurationProfileDataSource";

    private static ConfigurationProfileDataSource INSTANCE;

    public static ConfigurationProfileDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new ConfigurationProfileDataSource();
        }
        return INSTANCE;
    }

    public ConfigurationProfileDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_CONFIGPROFILE);

        DataSourceField fId = new DataSourceField(ConfigurationProfileResourceApi.PROFILE_ID, FieldType.TEXT);
        fId.setPrimaryKey(true);
        DataSourceField fLabel = new DataSourceField(ConfigurationProfileResourceApi.PROFILE_LABEL, FieldType.TEXT);
        DataSourceField fDesc = new DataSourceField(ConfigurationProfileResourceApi.PROFILE_DESCRIPTION, FieldType.TEXT);
        DataSourceField fError = new DataSourceField(ConfigurationProfileResourceApi.PROFILE_ERROR, FieldType.TEXT);

        setFields(fId, fLabel, fDesc, fError);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static void setOptionDataSource(FormItem field, ProfileGroup profileGroup) {
        field.setOptionDataSource(getInstance());
        field.setOptionCriteria(new Criteria(
                ConfigurationProfileResourceApi.PROFILE_GROUP_PARAM, profileGroup.getId()));
        field.setValueField(ConfigurationProfileResourceApi.PROFILE_ID);
        field.setDisplayField(ConfigurationProfileResourceApi.PROFILE_LABEL);
//        if (field instanceof SelectItem) {
//            ((SelectItem) field).setSortField(FIELD_LABEL);
//        }
    }

}
