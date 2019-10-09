/*
 * Copyright (C) 2019 Lukas Sykora
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

import com.google.gwt.core.client.Callback;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.PromptStyle;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;

/**
 *
 * @author Lukas Sykora
 */
public class CopyObjectDataSource extends ProarcDataSource {
    
    public static final String FIELD_PIDOLD = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    public static final String FIELD_PIDNEW = DigitalObjectResourceApi.DIGITALOBJECT_PIDNEW;
    public static final String FIELD_MODEL = DigitalObjectResourceApi.DIGITALOBJECT_MODEL;

    private static CopyObjectDataSource INSTANCE;

    public static CopyObjectDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new CopyObjectDataSource();
        }
        return INSTANCE;
    }

    public CopyObjectDataSource() {
        setDataURL(RestConfig.URL_DIGOBJECT_COPYOBJECT);
        DataSourceTextField pidold = new DataSourceTextField(FIELD_PIDOLD);
        DataSourceTextField pidnew = new DataSourceTextField(FIELD_PIDNEW);
        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        setFields(pidold, pidnew, model);
        setOperationBindings(RestConfig.createAddOperation());
    }

    public void copyObject(String modelId, String pidOld, String pidNew, Callback<String, DigitalObjectDataSource.ErrorSavingDigitalObject> callback) {
        Record r = new Record();
        DigitalObjectDataSource ds = DigitalObjectDataSource.getInstance();
        if (modelId != null && !modelId.isEmpty()) {
            r.setAttribute(DigitalObjectDataSource.FIELD_MODEL, modelId);
        }
        if (pidOld != null && !pidOld.isEmpty()) {
            r.setAttribute(CopyObjectDataSource.FIELD_PIDOLD, pidOld);
        }
        if (pidNew != null && !pidNew.isEmpty()) {
            r.setAttribute(CopyObjectDataSource.FIELD_PIDNEW, pidNew);
        }
            DSRequest dsRequest = new DSRequest();
            dsRequest.setWillHandleError(true);
            ds.addData(r, new DSCallback() {
                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    /*if (response.getStatus() == RPCResponse.STATUS_VALIDATION_ERROR) {
                        DigitalObjectDataSource.ErrorSavingDigitalObject validationError = DigitalObjectDataSource.ErrorSavingDigitalObject.VALIDATION_ERROR;
                        validationError.setValidationErrors(response.getErrors());
                        callback.onFailure(validationError);
                        request.setWillHandleError(true);
                    } else */{
                        Record[] data = response.getData();
                        if (data != null && data.length > 0) {
                            String pid = data[0].getAttribute(DigitalObjectDataSource.FIELD_PID);
                            callback.onSuccess(pid);
                            CopyObjectDataSource.this.updateCaches(response, request);
                            SearchDataSource.getInstance().updateCaches(response, request);
                            RelationDataSource.getInstance().updateCaches(response, request);
                        } else {
                            callback.onFailure(DigitalObjectDataSource.ErrorSavingDigitalObject.ERROR_SAVING_DIGITAL_OBJECT);
                        }
                    }
                }
            }, dsRequest);

    }

    public void copyObject(String modelId, String pidOld, String pidNew, DSCallback dsCallback, DSRequest dsRequest) {
    }
}
