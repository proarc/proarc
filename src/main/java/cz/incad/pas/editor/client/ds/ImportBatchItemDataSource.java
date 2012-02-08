/*
 * Copyright (C) 2011 Jan Pokorsky
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
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportBatchItemDataSource extends DataSource {

    public static final String ID = "ImportBatchItemDataSource";

    public static final String FIELD_ID = "id";
    public static final String FIELD_BATCHID = "batchId";
    public static final String FIELD_FILENAME = "filename";
    public static final String FIELD_PID = "pid";
    public static final String FIELD_MODEL = "model";
    public static final String FIELD_PAGE_TYPE = "pageType";
    public static final String FIELD_PAGE_INDEX = "pageIndex";
    public static final String FIELD_PAGE_NUMBER = "pageNumber";
//    public static final String FIELD_TIMESTAMP = "timeStamp";
    // XXX Jak udelat foreing key ??? <= v gridu nebo ve formu udelej text field s options data sourcem a zadej display value
    public static final String FIELD_USER = "userId";
    public static final String FIELD_PREVIEW = "preview";
    public static final String FIELD_THUMBNAIL = "thumbnail";

    public ImportBatchItemDataSource() {
        setID(ID);

//        setDataFormat(DSDataFormat.XML);
        setDataFormat(DSDataFormat.JSON);
//        setRecordXPath("/items/item");
        setRecordXPath("item");

//        setDataURL(RestConfig.URL_IMPORT_BATCH_ITEM);
        setDataURL("ds/ImportBatchItemDataSource.json");
        setClientOnly(true);

        DataSourceIntegerField id = new DataSourceIntegerField(FIELD_ID);
        id.setPrimaryKey(true);

        DataSourceIntegerField batchId = new DataSourceIntegerField(FIELD_BATCHID);
        batchId.setForeignKey(ImportBatchDataSource.ID + '.' + ImportBatchDataSource.FIELD_ID);

        DataSourceTextField filename = new DataSourceTextField(FIELD_FILENAME);

        DataSourceIntegerField user = new DataSourceIntegerField(FIELD_USER);
        user.setForeignKey(UserDataSource.ID + '.' + UserDataSource.FIELD_ID);

        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        model.setForeignKey(MetaModelDataSource.ID + '.' + MetaModelDataSource.FIELD_PID);

        DataSourceImageField preview = new DataSourceImageField(FIELD_PREVIEW);
        preview.setImageURLPrefix(RestConfig.URL_DIGOBJECT_PREVIEW + "?pid=");

        DataSourceImageField thumbnail = new DataSourceImageField(FIELD_THUMBNAIL);
        thumbnail.setImageURLPrefix(RestConfig.URL_DIGOBJECT_THUMBNAIL + "?pid=");

        DataSourceField pageType = new DataSourceField(FIELD_PAGE_TYPE, FieldType.TEXT, "Page Type");
        DataSourceField pageIndex = new DataSourceField(FIELD_PAGE_INDEX, FieldType.INTEGER, "Page Index");
        DataSourceField pageNumber = new DataSourceField(FIELD_PAGE_NUMBER, FieldType.TEXT, "Page Number");

        setFields(id, batchId, filename, user, model, preview, thumbnail, pageIndex, pageNumber, pageType);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        
    }

    public static ImportBatchItemDataSource getInstance() {
        ImportBatchItemDataSource ds = (ImportBatchItemDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new ImportBatchItemDataSource();
        return ds;
    }

}
