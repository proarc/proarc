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

import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import cz.incad.pas.editor.client.ClientUtils;

/**
 *
 * @author Jan Pokorsky
 */
public final class ImportBatchItemDataSource extends RestDataSource {

    public static final String ID = "ImportBatchItemDataSource";

    public static final String FIELD_BATCHID = "batchId";
    public static final String FIELD_FILENAME = "filename";
    public static final String FIELD_PID = "pid";
    public static final String FIELD_MODEL = "model";
    public static final String FIELD_PAGE_TYPE = "pageType";
    public static final String FIELD_PAGE_INDEX = "pageIndex";
    public static final String FIELD_PAGE_NUMBER = "pageNumber";
    public static final String FIELD_TIMESTAMP = "timestamp";
    public static final String FIELD_USER = "user";
    public static final String FIELD_PREVIEW = "preview";
    public static final String FIELD_THUMBNAIL = "thumbnail";

    public ImportBatchItemDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.JSON);

        setDataURL(RestConfig.URL_IMPORT_BATCH_ITEM);

        DataSourceField pid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        pid.setPrimaryKey(true);

        DataSourceIntegerField batchId = new DataSourceIntegerField(FIELD_BATCHID);
        batchId.setForeignKey(ImportBatchDataSource.ID + '.' + ImportBatchDataSource.FIELD_ID);

        DataSourceField timestamp = new DataSourceField(FIELD_TIMESTAMP, FieldType.TEXT);
        timestamp.setRequired(true);
        timestamp.setHidden(true);

        DataSourceTextField filename = new DataSourceTextField(FIELD_FILENAME);

        DataSourceTextField user = new DataSourceTextField(FIELD_USER);

        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        model.setForeignKey(MetaModelDataSource.ID + '.' + MetaModelDataSource.FIELD_PID);

        DataSourceImageField preview = new DataSourceImageField(FIELD_PREVIEW);
        preview.setImageURLPrefix(RestConfig.URL_DIGOBJECT_PREVIEW + "?");

        DataSourceImageField thumbnail = new DataSourceImageField(FIELD_THUMBNAIL);
        thumbnail.setImageURLPrefix(RestConfig.URL_DIGOBJECT_THUMBNAIL + "?");

        DataSourceField pageType = new DataSourceField(FIELD_PAGE_TYPE, FieldType.ENUM);
        pageType.setValueMap(ModsCustomDataSource.getPageTypes());
        
        DataSourceField pageIndex = new DataSourceField(FIELD_PAGE_INDEX, FieldType.INTEGER, "Page Index");
        DataSourceField pageNumber = new DataSourceField(FIELD_PAGE_NUMBER, FieldType.TEXT, "Page Number");

        setFields(pid, batchId, timestamp, filename, user, model, preview, thumbnail, pageIndex, pageNumber, pageType);

        setOperationBindings(RestConfig.createUpdateOperation(), RestConfig.createDeleteOperation());

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        
    }

    @Override
    protected void transformResponse(DSResponse response, DSRequest request, Object data) {
        int status = response.getStatus();
        if (status == RPCResponse.STATUS_SUCCESS) {
            for (Record record : response.getData()) {
                String pid = record.getAttribute(FIELD_PID);
                String batchId = record.getAttribute(FIELD_BATCHID);

                String imgParams = ClientUtils.format("%s=%s&%s=%s",
                        FIELD_PID, pid, FIELD_BATCHID, batchId);
                record.setAttribute(FIELD_PREVIEW, imgParams);
                record.setAttribute(FIELD_THUMBNAIL, imgParams);
            }
        }
        super.transformResponse(response, request, data);
    }

    public static ImportBatchItemDataSource getInstance() {
        ImportBatchItemDataSource ds = (ImportBatchItemDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new ImportBatchItemDataSource();
        return ds;
    }

}
