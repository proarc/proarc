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
package cz.cas.lib.proarc.webapp.client.ds;

import com.google.gwt.core.client.Callback;
import com.google.gwt.core.shared.GWT;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.types.PromptStyle;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import java.util.ArrayList;
import java.util.Arrays;

/**
 *
 * @author Jan Pokorsky
 */
public final class ImportBatchItemDataSource extends RestDataSource {

    public static final String ID = "ImportBatchItemDataSource";

    public static final String FIELD_BATCHID = ImportResourceApi.BATCHITEM_BATCHID;
    public static final String FIELD_FILENAME = ImportResourceApi.BATCHITEM_FILENAME;
    public static final String FIELD_PID = ImportResourceApi.BATCHITEM_PID;
    public static final String FIELD_MODEL = ImportResourceApi.BATCHITEM_MODEL;
    public static final String FIELD_PAGE_TYPE = ImportResourceApi.BATCHITEM_PAGETYPE;
    public static final String FIELD_PAGE_INDEX = ImportResourceApi.BATCHITEM_PAGEINDEX;
    public static final String FIELD_PAGE_NUMBER = ImportResourceApi.BATCHITEM_PAGENUMBER;
    public static final String FIELD_TIMESTAMP = ImportResourceApi.BATCHITEM_TIMESTAMP;
    public static final String FIELD_USER = ImportResourceApi.BATCHITEM_USER;
    /** synthetic field holding batchid and pid URL parameters */
    public static final String FIELD_PREVIEW = "preview";
    /** synthetic field holding batchid and pid URL parameters */
    public static final String FIELD_THUMBNAIL = "thumbnail";

    public ImportBatchItemDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.JSON);

        setDataURL(RestConfig.URL_IMPORT_BATCH_ITEM);

        DataSourceField pid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        pid.setPrimaryKey(true);

        DataSourceIntegerField batchId = new DataSourceIntegerField(FIELD_BATCHID);
        batchId.setPrimaryKey(true);
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
        super.transformResponse(response, request, data);
        if (RestConfig.isStatusOk(response)) {
            for (Record record : response.getData()) {
                String pid = record.getAttribute(FIELD_PID);
                String batchId = record.getAttribute(FIELD_BATCHID);

                String imgParams = ClientUtils.format("%s=%s&%s=%s",
                        FIELD_PID, pid, FIELD_BATCHID, batchId);
                record.setAttribute(FIELD_PREVIEW, imgParams);
                record.setAttribute(FIELD_THUMBNAIL, imgParams);
            }
        } else {
            // In case of any error DataSource invokes further fetches in never ending loop
            // Following seems to help.
            response.setEndRow(0);
            response.setTotalRows(0);
        }
    }

    /**
     * Removes list of digital objects from the given batch import.
     * @param callback callback to get the result
     * @param batchId import ID
     * @param pids digital object IDs
     */
    public void delete(final Callback<Record[], String> callback, String batchId, String... pids) {
        DeleteTask task = new DeleteTask(this, batchId, pids, callback);
        task.delete();
    }

    public static ImportBatchItemDataSource getInstance() {
        ImportBatchItemDataSource ds = (ImportBatchItemDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new ImportBatchItemDataSource();
        return ds;
    }

    /**
     * Deletes objects from a given batch import.
     * It splits PIDs array to chunks not to exceed URL length limit (8k).
     * <p>The alternative is to increase {@code Connector@maxHttpHeaderSize}
     * at {@code server.xml}.
     * <p>SmartGWT does not allow to use HTTP DELETE method with body.
     */
    private static class DeleteTask {

        private static int CHUNK_LIMIT = 100;
        private int itemIndex = 0;
        private final DataSource ds;
        private final String batchId;
        private final String[] pids;
        private final Callback<Record[], String> callback;
        private final ClientMessages i18n;
        private final ArrayList<Record> deleted;

        public DeleteTask(DataSource ds, String batchId, String[] pids,
                Callback<Record[], String> callback) {

            this.ds = ds;
            this.pids = pids;
            this.callback = callback;
            this.i18n = GWT.create(ClientMessages.class);
            this.batchId = batchId;
            this.deleted = new ArrayList<Record>(pids.length);
        }

        public void delete() {
            deleteItem();
        }

        private void deleteItem() {
            String[] chunk = nextChunk();
            if (chunk == null) {
                StatusView.getInstance().show(i18n.DeleteAction_Done_Msg());
                callback.onSuccess(deleted.toArray(new Record[0]));
                return ;
            }
            Record query = new Record();
            query.setAttribute(FIELD_BATCHID, batchId);
            query.setAttribute(FIELD_PID, chunk);
            DSRequest dsRequest = new DSRequest();
            dsRequest.setPromptStyle(PromptStyle.DIALOG);
            dsRequest.setPrompt(i18n.DeleteAction_Deleting_Msg());
            // TileGrid.removeSelectedData uses queuing support in case of multi-selection.
            // It will require extra support on server. For now remove data in separate requests.
            //thumbGrid.removeSelectedData();
            ds.removeData(query, new DSCallback() {
                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    if (RestConfig.isStatusOk(response)) {
                        ds.updateCaches(response, request);
                        deleted.addAll(Arrays.asList(response.getData()));
                        deleteItem();
                    } else {
                        if (deleted.isEmpty()) {
                            callback.onFailure(null);
                        } else {
                            callback.onSuccess(deleted.toArray(new Record[0]));
                            callback.onFailure(null);
                        }
                    }
                }
            }, dsRequest);
        }

        private String[] nextChunk() {
            String[] chunk = null;
            int remainingLength = pids.length - itemIndex;
            if (remainingLength > 0) {
                if (pids.length <= CHUNK_LIMIT) {
                    chunk = pids;
                } else {
                    int sliceLength = remainingLength > CHUNK_LIMIT ? CHUNK_LIMIT : remainingLength;
                    chunk = new String[sliceLength];
                    System.arraycopy(pids, itemIndex, chunk, 0, sliceLength);
                }
                itemIndex += chunk.length;
            }
            return chunk;
        }
    }

}
