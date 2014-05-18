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

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.webapp.shared.rest.BibliographicCatalogResourceApi;
import java.util.logging.Logger;

/**
 * Lists bibliographic catalogs.
 *
 * @author Jan Pokorsky
 */
public final class BibliographyDataSource extends RestDataSource {
    
    private static final Logger LOG = Logger.getLogger(BibliographyDataSource.class.getName());

    public static final String ID = "BibliographyDataSource";

    public BibliographyDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.JSON);

        setDataURL(RestConfig.URL_BIBLIOCATALOG);

        DataSourceField fieldId = new DataSourceField(BibliographicCatalogResourceApi.CATALOG_ID, FieldType.TEXT);
        fieldId.setPrimaryKey(true);

        DataSourceField fieldTitle = new DataSourceField(BibliographicCatalogResourceApi.CATALOG_NAME, FieldType.TEXT);
        DataSourceField fieldQueryFields = new DataSourceField(BibliographicCatalogResourceApi.CATALOG_FIELDS, FieldType.ANY);

        setFields(fieldId, fieldTitle, fieldQueryFields);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));

    }

    public static BibliographyDataSource getInstance() {
        BibliographyDataSource ds = (BibliographyDataSource) DataSource.get(ID);
        ds = (ds != null) ? ds : new BibliographyDataSource();
        return ds;
    }

}
