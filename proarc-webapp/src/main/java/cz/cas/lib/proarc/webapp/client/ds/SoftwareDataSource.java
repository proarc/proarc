/*
 * Copyright (C) 2025 Lukas Sykora
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
import cz.cas.lib.proarc.webapp.shared.rest.SoftwareResourceApi;
import java.util.logging.Logger;

/**
 * Data source to manage digitizing software.
 *
 * @author Lukas Sykora
 */
public final class SoftwareDataSource extends ProarcDataSource {

    private static final Logger LOG = Logger.getLogger(SoftwareDataSource.class.getName());
    public static final String ID = "SoftwareDataSource";

    public static final String FIELD_ID = SoftwareResourceApi.SOFTWARE_ITEM_ID;
    public static final String FIELD_LABEL = SoftwareResourceApi.SOFTWARE_ITEM_LABEL;
    public static final String FIELD_MODEL = SoftwareResourceApi.SOFTWARE_ITEM_MODEL;

    private static SoftwareDataSource INSTANCE;

    public static SoftwareDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new SoftwareDataSource();
        }
        return INSTANCE;
    }

    public SoftwareDataSource() {
        ClientMessages i18n = GWT.create(ClientMessages.class);
        setID(ID);
        setDataURL(RestConfig.URL_SOFTWARE_SET);

        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setPrimaryKey(Boolean.TRUE);
        fieldId.setTitle(i18n.SoftwareManager_Id_Title());

        DataSourceTextField label = new DataSourceTextField(FIELD_LABEL);
        label.setTitle(i18n.SoftwareManager_Label_Title());
        label.setLength(255);

        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        model.setTitle(i18n.SoftwareManager_Model_Title());

        setFields(fieldId, label, model);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        setOperationBindings(
                RestConfig.createAddOperation(),
                RestConfig.createDeleteOperation(),
                RestConfig.createUpdateOperation());
    }

    public SoftwareDataSource(JavaScriptObject jsObj) {
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
