/*
 * Copyright (C) 2014 Jan Pokorsky
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

import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.PromptStyle;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;

/**
 *
 * @author Jan Pokorsky
 */
public class UrnNbnDataSource extends ProarcDataSource {

    private static UrnNbnDataSource INSTANCE;

    public static UrnNbnDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new UrnNbnDataSource();
        }
        return INSTANCE;
    }

    public UrnNbnDataSource() {
        setDataURL(RestConfig.URL_DIGOBJECT_URNNBN);

        DataSourceTextField target = new DataSourceTextField(ExportResourceApi.RESULT_TARGET);
        setFields(target);

        DSRequest dsRequest = RestConfig.createRestRequest(getDataFormat());
        dsRequest.setPromptStyle(PromptStyle.DIALOG);
        setRequestProperties(dsRequest);

        setOperationBindings(RestConfig.createAddOperation());
    }

}
