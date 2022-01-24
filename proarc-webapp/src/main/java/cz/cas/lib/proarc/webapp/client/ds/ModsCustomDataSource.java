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

import com.google.gwt.core.client.Callback;
import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ErrorHandler;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.ds.mods.IdentifierDataSource;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;

/**
 * Data source for MODS/JSON custom mapping. Later it should be fully dynamic
 * and pluggable.
 *
 * <pre>
 * {@code
 * {pid:"uuid:1",
 *  timestamp:"0",
 *  data:{
 *      identifier:[{type:"uuid", value:"1"}],
 *      pageType:"Blank"
 *  }
 * }
 * </pre>
 *
 * <p>See reasons for NOT using GWT-RPC http://forums.smartclient.com/showthread.php?t=8159#aGWTRPC,
 * http://code.google.com/p/smartgwt/issues/detail?id=303
 *
 * @author Jan Pokorsky
 */
public final class ModsCustomDataSource extends ProarcDataSource implements ModsConstants {

    private static final Logger LOG = Logger.getLogger(ModsCustomDataSource.class.getName());

    public static final String ID = "ModsCustomDataSource";
    public static final String FIELD_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    public static final String FIELD_BATCHID = DigitalObjectResourceApi.BATCHID_PARAM;
    public static final String FIELD_EDITOR = DigitalObjectResourceApi.MODS_CUSTOM_EDITORID;
    public static final String FIELD_TIMESTAMP = DigitalObjectResourceApi.TIMESTAMP_PARAM;
    public static final String FIELD_DATA = DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA;
    public static final String FIELD_IS_LOCKED = DigitalObjectResourceApi.MEMBERS_ITEM_LOCKED;
    
    // follows custom field names
    // custom field names are defined by ModsConstants for now

    public ModsCustomDataSource() {
        setID(ID);
        setDataURL(RestConfig.URL_DIGOBJECT_MODS_CUSTOM);

        DataSourceField fieldPid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setRequired(true);

        DataSourceField fieldTimestamp = new DataSourceField(FIELD_TIMESTAMP, FieldType.TEXT);
        fieldTimestamp.setRequired(true);
        fieldTimestamp.setHidden(true);

        DataSourceField fieldEditor = new DataSourceField(MetaModelDataSource.FIELD_EDITOR, FieldType.TEXT);
        fieldEditor.setRequired(true);
        fieldEditor.setHidden(true);

        DataSourceField fieldData = new DataSourceField(FIELD_DATA, FieldType.ANY);
        fieldData.setTypeAsDataSource(new DataSource() {
            {
                DataSourceField identifiers = new DataSourceField(FIELD_IDENTIFIERS, FieldType.ANY);
                identifiers.setTypeAsDataSource(IdentifierDataSource.getInstance());
                setFields(identifiers);
            }
        });

        setFields(fieldPid, fieldTimestamp, fieldEditor, fieldData);

        setOperationBindings(RestConfig.createUpdateOperation());

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static ModsCustomDataSource getInstance() {
        ModsCustomDataSource ds = (ModsCustomDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new ModsCustomDataSource();
        return ds;
    }

    public static String getDefaultPageType() {
        return "normalPage";
    }

    public void fetchDescription(final DigitalObject dobj, final Callback<DescriptionMetadata, String> cb) {
        fetchDescription(dobj, cb, true);
    }

    public void fetchDescription(final DigitalObject dobj, final Callback<DescriptionMetadata, String> cb, boolean showPrompt) {
        MetaModelRecord model = dobj.getModel();
        Criteria criteria = new Criteria(MetaModelDataSource.FIELD_EDITOR, model.getEditorId());
        criteria.addCriteria(FIELD_PID, dobj.getPid());
        String batchId = dobj.getBatchId();
        if (batchId != null) {
            criteria.addCriteria(FIELD_BATCHID, batchId);
        }
        DSRequest request = new DSRequest();
        request.setShowPrompt(showPrompt);
        fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                String errorMsg;
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    if (data != null && data.length == 1) {
                        Record customRecord = data[0];
                        cb.onSuccess(new DescriptionMetadata(customRecord));
                        return ;
                    } else {
                        errorMsg = "No record found! " + dobj;
                    }
                } else {
                    errorMsg = "Fetch failed! " + dobj;
                }
                cb.onFailure(errorMsg);
            }
        }, request);
    }

    public void saveXmlDescription(DigitalObject dobj, String xml, DescriptionSaveHandler callback) {
        saveXmlDescription(dobj, xml, -1, callback);
    }

    public void saveXmlDescription(DigitalObject dobj, String xml, long timestamp, DescriptionSaveHandler callback) {
        saveXmlDescription(dobj, xml, timestamp, callback, false);
    }

    public void saveXmlDescription(DigitalObject dobj, String xml, long timestamp, DescriptionSaveHandler callback, Boolean ignoreValidation) {
        Record update = new Record();
        dobj.toCriteria();
        update.setAttribute(FIELD_PID, dobj.getPid());

        if (ignoreValidation != null && ignoreValidation) {
            update.setAttribute(DigitalObjectResourceApi.MODS_CUSTOM_IGNOREVALIDATION, true);
        }

        if (dobj.getBatchId() != null) {
            update.setAttribute(FIELD_BATCHID, dobj.getBatchId());
        }
        if (xml == null || xml.isEmpty()) {
            return ;
        }
        update.setAttribute(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMXMLDATA, xml);
        // timestamp -1 stands for rewrite without concurrency check
        update.setAttribute(FIELD_TIMESTAMP, timestamp);
        update.setAttribute(FIELD_EDITOR, dobj.getModel().getEditorId());
        callback.setUpdateRecord(update);
        updateData(update, callback, callback.getUpdateRequest());
    }

    public void saveDescription(DescriptionMetadata update, DescriptionSaveHandler callback, Boolean showPrompt) {
        Record customRecord = update.getWrapper();
        callback.getUpdateRequest().setShowPrompt(showPrompt);
        callback.setUpdateRecord(customRecord);
        updateData(customRecord, callback, callback.getUpdateRequest());
    }

    public static class DescriptionMetadata {

        private Record wrapper;

        /**
         * Constructs the object from a record fetched by the {@link ModsCustomDataSource}.
         */
        public DescriptionMetadata(Record wrapper) {
            this.wrapper = wrapper;
        }

        public Record getWrapper() {
            return wrapper;
        }

        public DigitalObject getDigitalObject() {
            return DigitalObject.create(wrapper);
        }

        public Record getDescription() {
            Record customModsRecord = wrapper.getAttributeAsRecord(ModsCustomDataSource.FIELD_DATA);
            return customModsRecord;
        }

        public void setDescription(Record r) {
            wrapper.setAttribute(ModsCustomDataSource.FIELD_DATA, r);
        }
    }

    /**
     * A helper class to handle save responses.
     */
    public static class DescriptionSaveHandler implements DSCallback {

        private DSResponse response;
        /** The template of the request. */
        private DSRequest updateRequest;
        /** The used request by query. */
        private DSRequest sentRequest;
        /** Data to save. */
        private Record updateRecord;
        private final ClientMessages i18n;

        public DescriptionSaveHandler() {
            this(GWT.<ClientMessages>create(ClientMessages.class));
        }

        public DescriptionSaveHandler(ClientMessages i18n) {
            this.i18n = i18n;
        }

        public DSResponse getResponse() {
            return response;
        }

        public Record getUpdateRecord() {
            return updateRecord;
        }

        public DSRequest getUpdateRequest() {
            if (updateRequest == null) {
                updateRequest = new DSRequest();
                updateRequest.setWillHandleError(true);
            }
            return updateRequest;
        }

        public void setUpdateRecord(Record updateRecord) {
            this.updateRecord = updateRecord;
        }

        @Override
        public void execute(DSResponse response, Object rawData, DSRequest request) {
            this.response = response;
            this.sentRequest = request;
            if (RestConfig.isStatusOk(response)) {
                Record[] data = response.getData();
                if (data != null && data.length == 1) {
                    Record customRecord = data[0];
                    onSave(new DescriptionMetadata(customRecord));
                }
            } else if (response.getStatus() == RPCResponse.STATUS_VALIDATION_ERROR) {
                onValidationError();
            } else if (response.getStatus() == -41) {
                onLockedError();
            } else if (RestConfig.isConcurrentModification(response)) { // concurrency conflict
                onConcurrencyError();
            } else {
                onError();
            }
        }

        protected void onLockedError() {
            String msg = i18n.SaveAction_Validation_Msg(i18n.SaveAction_Locked_Msg());
            SC.warn(msg);
        }

        protected void onSave(DescriptionMetadata dm) {
        }

        protected void onValidationError() {
            String msg = i18n.SaveAction_IgnoreRemoteInvalid_Msg(getValidationMessage());
            SC.ask(i18n.SaveAction_Title(), msg, new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    // save again
                    if (value != null && value) {
                        updateRecord.setAttribute(DigitalObjectResourceApi.MODS_CUSTOM_IGNOREVALIDATION, true);
                        getInstance().updateData(updateRecord, DescriptionSaveHandler.this, updateRequest);
                    }
                }
            });
        }

        protected void onConcurrencyError() {
        }

        protected void onError() {
            ErrorHandler.warn(response, sentRequest);
        }

        /**
         * Gets a {@code <li>} list of validation messages.
         */
        public String getValidationMessage() {
            Map<?,?> errors = response.getErrors();
            StringBuilder sb = new StringBuilder(1024);
            for (Entry<?,?> entry : errors.entrySet()) {
                Object errMsgs = entry.getValue();
                if (errMsgs instanceof List) {
                    for (Object errMsg : (List) errMsgs) {
                    sb.append("<li>").append(errMsg).append("</li>");
                    }
                } else {
                    sb.append("<li>").append(errMsgs).append("</li>");
                }
            }
            if (sb.length() == 0) {
                sb.append(response.getDataAsString());
            } else {
                sb.insert(0, "<ul>");
                sb.append("</ul>");
            }
            if (sb.length() == 0) {
                sb.append(response.getHttpResponseText());
            }
            return sb.toString();
        }

    }

}
