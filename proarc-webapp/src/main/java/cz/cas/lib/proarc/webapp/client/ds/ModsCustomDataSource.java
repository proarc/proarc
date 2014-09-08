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
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.ds.mods.IdentifierDataSource;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.LinkedHashMap;
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
public final class ModsCustomDataSource extends DataSource implements ModsConstants {

    private static final Logger LOG = Logger.getLogger(ModsCustomDataSource.class.getName());

    public static final String ID = "ModsCustomDataSource";
    public static final String FIELD_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    public static final String FIELD_BATCHID = DigitalObjectResourceApi.BATCHID_PARAM;
    public static final String FIELD_EDITOR = DigitalObjectResourceApi.MODS_CUSTOM_EDITORID;
    public static final String FIELD_TIMESTAMP = DigitalObjectResourceApi.TIMESTAMP_PARAM;
    public static final String FIELD_DATA = DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA;
    
    // follows custom field names
    // custom field names are defined by ModsConstants for now

    public ModsCustomDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_DIGOBJECT_MODS_CUSTOM);
        setRecordXPath('/' + DigitalObjectResourceApi.CUSTOMMODS_ELEMENT);

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

    public static LinkedHashMap<String, String> getPageTypes() {
        return LocalizationDataSource.getInstance().asValueMap(BundleName.MODS_PAGE_TYPES);
    }

    public static String getDefaultPageType() {
        return "NormalPage";
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

    public void saveDescription(DescriptionMetadata update, final Callback<DescriptionMetadata, String> cb, boolean showPrompt) {
        DSRequest request = new DSRequest();
        request.setShowPrompt(showPrompt);
        Record customRecord = update.getWrapper();
        ModsCustomDataSource.getInstance().updateData(customRecord, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                String errorMsg;
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    if (data != null && data.length == 1) {
                        Record customRecord = data[0];
                        cb.onSuccess(new DescriptionMetadata(customRecord));
                    } else {
//                        errorMsg = "No record found! " + dobj;
                        cb.onSuccess(null);
                    }
                    return ;
                } else {
                    errorMsg = "Update failed!";
                }
                cb.onFailure(errorMsg);
            }
        }, request);
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

    }

}
