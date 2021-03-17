/*
 * Copyright (C) 2018 Martin Rumanek
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

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import java.util.logging.Logger;

public class AuthorityQueryDataSource extends DataSource {
    private static final Logger LOG = Logger.getLogger(AuthorityQueryDataSource.class.getName());

    public static final String ID = "BibliographyQueryDataSource";

    public static final String FIELD_ID = "id";
    public static final String FIELD_MODS = "mods";
    public static final String FIELD_PREVIEW = "preview";
    public static final String FIELD_TITLE = "title";
    public static final String FIELD_RDCZ_ID = "rdczId";

    public AuthorityQueryDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.JSON);
        setRecordXPath("/metadataCatalogEntries/entry");

        setDataURL(RestConfig.URL_AUTHORITYCATALOG_QUERY);

        DataSourceField fieldId = new DataSourceField(FIELD_ID, FieldType.TEXT);
        fieldId.setPrimaryKey(true);
        fieldId.setRequired(true);
        fieldId.setHidden(true);

        DataSourceField fieldMods = new DataSourceField(FIELD_MODS, FieldType.TEXT);
        fieldMods.setHidden(true);

        DataSourceField fieldPreview = new DataSourceField(FIELD_PREVIEW, FieldType.TEXT);
        fieldPreview.setDetail(true);

        DataSourceField fieldTitle = new DataSourceField(FIELD_TITLE, FieldType.TEXT);
//
//        DataSourceField fieldRdczId = new DataSourceField(FIELD_RDCZ_ID, FieldType.TEXT);
//        fieldRdczId.setHidden(true);

        setFields(fieldId, fieldMods, fieldPreview, fieldTitle); //fieldRdczId);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));

    }

    public static AuthorityQueryDataSource getInstance() {
        AuthorityQueryDataSource ds = (AuthorityQueryDataSource) DataSource.get(ID);
        ds = (ds != null) ? ds : new AuthorityQueryDataSource();
        return ds;
    }
}
