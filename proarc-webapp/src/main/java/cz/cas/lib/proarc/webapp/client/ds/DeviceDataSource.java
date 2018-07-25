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

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.shared.rest.DeviceResourceApi;
import java.util.logging.Logger;

/**
 * Data source to manage digitizing devices.
 *
 * @author Jan Pokorsky
 */
public final class DeviceDataSource extends ProarcDataSource {

    private static final Logger LOG = Logger.getLogger(DeviceDataSource.class.getName());
    public static final String ID = "DeviceDataSource";

    public static final String FIELD_ID = DeviceResourceApi.DEVICE_ITEM_ID;
    public static final String FIELD_LABEL = DeviceResourceApi.DEVICE_ITEM_LABEL;
    public static final String FIELD_MODEL = DeviceResourceApi.DEVICE_ITEM_MODEL;

    private static DeviceDataSource INSTANCE;

    public static DeviceDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new DeviceDataSource();
        }
        return INSTANCE;
    }

    public DeviceDataSource() {
        ClientMessages i18n = GWT.create(ClientMessages.class);
        setID(ID);
        setDataURL(RestConfig.URL_DEVICE);

        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setPrimaryKey(Boolean.TRUE);
        fieldId.setTitle(i18n.DeviceManager_Id_Title());

        DataSourceTextField label = new DataSourceTextField(FIELD_LABEL);
        label.setTitle(i18n.DeviceManager_Label_Title());
        label.setLength(255);

        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        model.setTitle(i18n.DeviceManager_Model_Title());

        setFields(fieldId, label, model);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        setOperationBindings(
                RestConfig.createAddOperation(),
                RestConfig.createDeleteOperation(),
                RestConfig.createUpdateOperation());
    }

    public DeviceDataSource(JavaScriptObject jsObj) {
        super(jsObj);
    }

    public static void setOptionDataSource(FormItem field) {
        field.setOptionDataSource(getInstance());
        field.setValueField(FIELD_ID);
        field.setDisplayField(FIELD_LABEL);
        if (field instanceof SelectItem) {
            ((SelectItem) field).setSortField(FIELD_LABEL);
        }
    }

}
