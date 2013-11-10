/*
 * Copyright (C) 2013 Jan Pokorsky
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

import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.widgets.form.fields.FormItem;
import cz.cas.lib.proarc.webapp.shared.rest.DeviceResourceApi;
import java.util.logging.Logger;

/**
 * Data source to manage digitizing devices.
 *
 * @author Jan Pokorsky
 */
public final class DeviceDataSource extends RestDataSource {

    private static final Logger LOG = Logger.getLogger(DeviceDataSource.class.getName());
    public static final String ID = "DeviceDataSource";

    public static final String FIELD_ID = DeviceResourceApi.DEVICE_ITEM_ID;
    public static final String FIELD_LABEL = DeviceResourceApi.DEVICE_ITEM_LABEL;

    public static DeviceDataSource getInstance() {
        DeviceDataSource ds = (DeviceDataSource) DataSource.get(ID);
        return  ds != null ? ds : new DeviceDataSource();
    }

    public DeviceDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_DEVICE);

        DataSourceTextField fieldId = new DataSourceTextField(id);
        fieldId.setPrimaryKey(Boolean.TRUE);

        DataSourceTextField label = new DataSourceTextField(FIELD_LABEL);

        setFields(fieldId, label);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public DeviceDataSource(JavaScriptObject jsObj) {
        super(jsObj);
    }

    public static void setOptionDataSource(FormItem field) {
        field.setOptionDataSource(getInstance());
        field.setValueField(FIELD_ID);
        field.setDisplayField(FIELD_LABEL);
    }

}
