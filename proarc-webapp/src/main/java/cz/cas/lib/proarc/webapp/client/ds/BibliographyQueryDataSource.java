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
package cz.cas.lib.proarc.webapp.client.ds;

import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.shared.rest.BibliographicCatalogResourceApi;
import java.util.logging.Logger;

/**
 * Bibliographic metadata queries.
 *
 * @author Jan Pokorsky
 */
public final class BibliographyQueryDataSource extends DataSource {
    
    private static final Logger LOG = Logger.getLogger(BibliographyQueryDataSource.class.getName());

    public static final String ID = "BibliographyQueryDataSource";

    public static final String FIELD_ID = "id";
    public static final String FIELD_MODS = "mods";
    public static final String FIELD_PREVIEW = "preview";
    public static final String FIELD_TITLE = "title";
    public static final String FIELD_RDCZ_ID = "rdczId";
    public static final String FIELD_SELECTED_CATALOG = "catalogId";

    public BibliographyQueryDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.JSON);
        setRecordXPath("/metadataCatalogEntries/entry");

        setDataURL(RestConfig.URL_BIBLIOCATALOG_QUERY);

        DataSourceField fieldId = new DataSourceField(FIELD_ID, FieldType.TEXT);
        fieldId.setPrimaryKey(true);
        fieldId.setRequired(true);
        fieldId.setHidden(true);

        DataSourceField fieldMods = new DataSourceField(FIELD_MODS, FieldType.TEXT);
        fieldMods.setHidden(true);

        DataSourceField fieldPreview = new DataSourceField(FIELD_PREVIEW, FieldType.TEXT);
        fieldPreview.setDetail(true);

        DataSourceField fieldTitle = new DataSourceField(FIELD_TITLE, FieldType.TEXT);

        DataSourceField fieldRdczId = new DataSourceField(FIELD_RDCZ_ID, FieldType.TEXT);
        fieldRdczId.setHidden(true);

        DataSourceField fieldSelectedCatalogId = new DataSourceField(FIELD_SELECTED_CATALOG, FieldType.TEXT);
        fieldSelectedCatalogId.setHidden(true);

        setFields(fieldId, fieldMods, fieldPreview, fieldTitle, fieldRdczId, fieldSelectedCatalogId);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));

    }

    @Override
    protected void transformResponse(DSResponse response, DSRequest request, Object data) {
        int code = response.getStatus();
        int httpResponseCode = response.getHttpResponseCode();
        ClientUtils.fine(LOG, "transformResponse: %s, status: %s, http: %s", data, code, httpResponseCode);
        if (code == DSResponse.STATUS_FAILURE && httpResponseCode == 200 && data == null) {
            // jersye serialize empty JSON as null
            response.setData(new Record[0]);
            response.setStatus(DSResponse.STATUS_SUCCESS);
        }
        if (RestConfig.isStatusOk(response)) {
            Object catalogId = request.getCriteria().getValues().get(BibliographicCatalogResourceApi.FIND_CATALOG_PARAM);
            for (Record r : response.getData()) {
                r.setAttribute(BibliographicCatalogResourceApi.CATALOG_ID, catalogId);
            }
        }
        super.transformResponse(response, request, data);
    }

    public static BibliographyQueryDataSource getInstance() {
        BibliographyQueryDataSource ds = (BibliographyQueryDataSource) DataSource.get(ID);
        ds = (ds != null) ? ds : new BibliographyQueryDataSource();
        return ds;
    }

}
