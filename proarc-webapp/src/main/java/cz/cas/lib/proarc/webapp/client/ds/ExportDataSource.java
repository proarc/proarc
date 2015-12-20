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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.ds;

import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.PromptStyle;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;

/**
 * The generic data source for export services.
 *
 * @author Jan Pokorsky
 */
public final class ExportDataSource extends RestDataSource {

    private ExportDataSource(String dataUrl) {
        setDataURL(dataUrl);
        setDataFormat(DSDataFormat.JSON);

        DataSourceTextField target = new DataSourceTextField(ExportResourceApi.RESULT_TARGET);
        setFields(target);

        DSRequest dsRequest = RestConfig.createRestRequest(getDataFormat());
        dsRequest.setPromptStyle(PromptStyle.DIALOG);
        setRequestProperties(dsRequest);

        setOperationBindings(RestConfig.createAddOperation());
    }

    public static ExportDataSource getArchive() {
        return new ExportDataSource(RestConfig.URL_EXPORT_ARCHIVE);
    }

    public static ExportDataSource getCejsh() {
        return new ExportDataSource(RestConfig.URL_EXPORT_CEJSH);
    }

    public static ExportDataSource getDataStream() {
        return new ExportDataSource(RestConfig.URL_EXPORT_DATASTREAM);
    }

    public static ExportDataSource getDesa() {
        return new ExportDataSource(RestConfig.URL_EXPORT_DESA);
    }

    public static ExportDataSource getKramerius4() {
        return new ExportDataSource(RestConfig.URL_EXPORT_KRAMERIUS4);
    }

    public static ExportDataSource getNdk() {
        return new ExportDataSource(RestConfig.URL_EXPORT_NDK);
    }

}
