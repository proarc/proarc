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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.widgets.form.fields.FormItem;
import cz.incad.pas.editor.shared.rest.LocalizationResourceApi;
import cz.incad.pas.editor.shared.rest.LocalizationResourceApi.BundleName;

/**
 * Loads localized messages configurable by server configuration files.
 * Values are cached.
 *
 * @author Jan Pokorsky
 */
public final class LocalizationDataSource extends RestDataSource {

    public static final String ID = "LocalizationDataSource";

    public LocalizationDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_LOCALIZATION);

        DataSourceTextField bundle = new DataSourceTextField(LocalizationResourceApi.ITEM_BUNDLENAME);
        bundle.setHidden(true);
        DataSourceTextField key = new DataSourceTextField(LocalizationResourceApi.ITEM_KEY);
        key.setHidden(true);
        // primary key is composite of bundleName and key; unsupported by SmartGWT 3.0
//        key.setPrimaryKey(true);

        DataSourceTextField value = new DataSourceTextField(LocalizationResourceApi.ITEM_VALUE);

        setFields(bundle, key, value);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        // cache data source for session
        setCacheAllData(Boolean.TRUE);
    }

    public static LocalizationDataSource getInstance() {
        LocalizationDataSource ds = (LocalizationDataSource) DataSource.get(ID);
        return  ds != null ? ds : new LocalizationDataSource();
    }

    public static Criteria asCriteria(BundleName bundleName) {
        Criteria criteria = new Criteria(LocalizationResourceApi.ITEM_BUNDLENAME,
                bundleName.toString());
        return criteria;
    }

    public static void setOptionDataSource(FormItem field, BundleName bundleName) {
        field.setOptionDataSource(getInstance());
        field.setOptionCriteria(asCriteria(bundleName));
        field.setValueField(LocalizationResourceApi.ITEM_KEY);
        field.setDisplayField(LocalizationResourceApi.ITEM_VALUE);
    }

}
