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
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import cz.incad.pas.editor.shared.rest.LocalizationResourceApi;

/**
 *
 * @author Jan Pokorsky
 */
public final class LocalizationDataSource extends RestDataSource {

    public static final String ID = "LocalizationDataSource";

    public LocalizationDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_LOCALIZATION);

        DataSourceTextField key = new DataSourceTextField(LocalizationResourceApi.ITEM_KEY);
        key.setHidden(true);
        key.setPrimaryKey(true);

        DataSourceTextField value = new DataSourceTextField(LocalizationResourceApi.ITEM_VALUE);

        setFields(key, value);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static LocalizationDataSource getInstance() {
        LocalizationDataSource ds = (LocalizationDataSource) DataSource.get(ID);
        return  ds != null ? ds : new LocalizationDataSource();
    }

}
