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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.util.BooleanCallback;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;

/**
 * Data provider for member relations of digital objects.
 *
 * @author Jan Pokorsky
 */
public class RelationDataSource extends RestDataSource {

    public static final String ID = "RelationDataSource";

    /**
     * Synthetic record ID to support TreeGrid. Format: {@code PID['|'PID]*}.
     * @see #transformResponse
     */
    public static final String FIELD_ID = "id";
    /**
     * Synthetic parent record ID to support TreeGrid.
     * @see #transformResponse
     */
    public static final String FIELD_PARENTID = "parentId";
    public static final String FIELD_PID = DigitalObjectResourceApi.MEMBERS_ITEM_PID;
    public static final String FIELD_PARENT = DigitalObjectResourceApi.MEMBERS_ITEM_PARENT;
    public static final String FIELD_ROOT = DigitalObjectResourceApi.MEMBERS_ROOT_PARAM;
    public static final String FIELD_MODEL = DigitalObjectResourceApi.MEMBERS_ITEM_MODEL;
    public static final String FIELD_OWNER = DigitalObjectResourceApi.MEMBERS_ITEM_OWNER;
    public static final String FIELD_LABEL = DigitalObjectResourceApi.MEMBERS_ITEM_LABEL;
    public static final String FIELD_STATE = DigitalObjectResourceApi.MEMBERS_ITEM_STATE;
    public static final String FIELD_CREATED = DigitalObjectResourceApi.MEMBERS_ITEM_CREATED;
    public static final String FIELD_MODIFIED = DigitalObjectResourceApi.MEMBERS_ITEM_MODIFIED;

    public RelationDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_DIGOBJECT_CHILDREN);

        DataSourceField recorId = new DataSourceField(FIELD_ID, FieldType.TEXT);
        recorId.setPrimaryKey(true);
        recorId.setHidden(true);
        DataSourceField parentId = new DataSourceField(FIELD_PARENTID, FieldType.TEXT);
        parentId.setForeignKey(ID + '.' + FIELD_ID);
        parentId.setRequired(true);
        parentId.setHidden(true);

        DataSourceField pid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        pid.setRequired(true);

        DataSourceField parent = new DataSourceField(FIELD_PARENT, FieldType.TEXT);

        DataSourceField root = new DataSourceField(FIELD_ROOT, FieldType.TEXT);
        root.setHidden(true);

        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        DataSourceField owner = new DataSourceField(FIELD_OWNER, FieldType.TEXT);
        DataSourceField label = new DataSourceField(FIELD_LABEL, FieldType.TEXT);
        DataSourceDateTimeField created = new DataSourceDateTimeField(FIELD_CREATED);
        created.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        DataSourceDateTimeField modified = new DataSourceDateTimeField(FIELD_MODIFIED);
        modified.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);

        setFields(recorId, parentId, pid, parent, label, model, created, modified, owner);
        setTitleField(FIELD_LABEL);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        setOperationBindings(
                RestConfig.createAddOperation(),
                RestConfig.createDeleteOperation());
        
    }

    /**
     * Sets {@code pid} and {@code parent} parameters to the fetch request
     * as TreeGrid passes only primary key and parent ID.
     */
    @Override
    protected Object transformRequest(DSRequest dsRequest) {
        if (dsRequest.getOperationType() == DSOperationType.FETCH) {
            // criteria can be queried just in case of FETCH!
            Criteria c = dsRequest.getCriteria();
            String parentId = c.getAttribute(FIELD_PARENTID);
            if (parentId != null) {
                Record selection = dsRequest.getAttributeAsRecord("parentNode");
                String parentPid = selection.getAttribute(FIELD_PID);
                c.addCriteria(FIELD_PARENT, parentPid);
            }
        }
        return super.transformRequest(dsRequest);
    }

    /**
     * Sets synthetic ID and parent ID to each response record
     * in order to comply with DataSource policy of single primary key.
     *
     * <p>For fetch it reads request criteria to get parent ID.
     * <br>For other operations it uses request attribute to get parent ID.
     */
    @Override
    protected void transformResponse(DSResponse response, DSRequest request, Object data) {
        super.transformResponse(response, request, data);
        if (RestConfig.isStatusOk(response)) {
            String parentId = null;
            if (request.getOperationType() == DSOperationType.FETCH) {
                // criteria can be queried just in case of FETCH!
                parentId = request.getCriteria().getAttribute(FIELD_PARENTID);
            }
            if (parentId == null) {
                parentId = request.getAttribute(FIELD_PARENTID);
            }
            for (Record record : response.getData()) {
                String pid = record.getAttribute(FIELD_PID);
                String recordId = parentId == null ? pid : parentId + '|' + pid;
                record.setAttribute(FIELD_ID, recordId);
                record.setAttribute(FIELD_PARENTID, parentId);
            }
        }
    }

    public static RelationDataSource getInstance() {
        RelationDataSource ds = (RelationDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new RelationDataSource();
        return ds;
    }

    public void addChild(String parentId, String parentPid, String pid, final BooleanCallback call) {
        if (pid == null || pid.isEmpty()) {
            throw new IllegalArgumentException("Missing PID!");
        }
        if (parentPid == null || parentPid.isEmpty()) {
            throw new IllegalArgumentException("Missing parent PID!");
        }

        DSRequest dsRequest = new DSRequest();
        // set parent ID to request to build synthetic ID later in transformResponse
        dsRequest.setAttribute(FIELD_PARENTID, parentId);

        Record update = new Record();
        update.setAttribute(RelationDataSource.FIELD_PARENT, parentPid);
        update.setAttribute(RelationDataSource.FIELD_PID, pid);
        addData(update, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (!RestConfig.isStatusOk(response)) {
                    call.execute(false);
                    return;
                }
                call.execute(true);
            }
        }, dsRequest);
    }

    public void removeChild(String parentId, String parentPid, String pid, final BooleanCallback call) {
        if (pid == null || pid.isEmpty()) {
            throw new IllegalArgumentException("Missing PID!");
        }
        if (parentPid == null || parentPid.isEmpty()) {
            throw new IllegalArgumentException("Missing parent PID!");
        }

        Record update = new Record();
        update.setAttribute(RelationDataSource.FIELD_PARENT, parentPid);
        update.setAttribute(RelationDataSource.FIELD_PID, pid);
        DSRequest dsRequest = new DSRequest();
        dsRequest.setData(update);
        // set parent ID to request to build synthetic ID later in transformResponse
        dsRequest.setAttribute(FIELD_PARENTID, parentId);
        removeData(update, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (!RestConfig.isStatusOk(response)) {
                    call.execute(false);
                    return;
                }
                call.execute(true);
            }
        }, dsRequest);
    }

    public void moveChild(final String pid,
            final String oldParentId, final String oldParentPid,
            final String parentId, final String parentPid,
            final BooleanCallback call) {

        final RelationDataSource ds = RelationDataSource.getInstance();
        BooleanCallback toAdd = new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value != null && value) {
                    if (parentPid != null) {
                        ds.addChild(parentId, parentPid, pid, call);
                    } else {
                        call.execute(value);
                    }
                } else {
                    call.execute(value);
                }
            }
        };
        if (oldParentPid != null) {
            ds.removeChild(oldParentId, oldParentPid, pid, toAdd);
        } else {
            toAdd.execute(Boolean.TRUE);
        }
    }

}
