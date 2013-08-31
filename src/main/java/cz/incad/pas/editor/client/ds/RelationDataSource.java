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

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.docs.TreeDataBinding;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.util.BooleanCallback;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;

/**
 * Data provider for member relations of digital objects.
 * <p>
 * Fetch with {@link #FIELD_ROOT} to get the tree hierarchy of members
 * including initial PID as root. See {@link TreeDataBinding} for loading tree nodes on demand.
 * TreeGrid should treat {@link #FIELD_PID} as {@code id} and {@link #FIELD_PARENT} as {@code parentId}.
 * <p>
 * Fetch with {@link #FIELD_PARENT} to get direct members.
 *
 * @author Jan Pokorsky
 */
public class RelationDataSource extends RestDataSource {

    public static final String ID = "RelationDataSource";

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

        DataSourceField pid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        pid.setPrimaryKey(true);
        pid.setRequired(true);

        DataSourceField parent = new DataSourceField(FIELD_PARENT, FieldType.TEXT);
        parent.setForeignKey(ID + '.' + FIELD_PID);
        // canView:false excludes column from grid picker menu
        parent.setCanView(false);
        parent.setHidden(true);

        DataSourceField root = new DataSourceField(FIELD_ROOT, FieldType.TEXT);
        root.setHidden(true);

        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        DataSourceField owner = new DataSourceField(FIELD_OWNER, FieldType.TEXT);
        DataSourceField label = new DataSourceField(FIELD_LABEL, FieldType.TEXT);
        DataSourceDateTimeField created = new DataSourceDateTimeField(FIELD_CREATED);
        created.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        DataSourceDateTimeField modified = new DataSourceDateTimeField(FIELD_MODIFIED);
        modified.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);

        setFields(pid, parent, label, model, created, modified, owner);
        setTitleField(FIELD_LABEL);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        setOperationBindings(
                RestConfig.createAddOperation(),
                RestConfig.createDeleteOperation());
        
    }

    public static RelationDataSource getInstance() {
        RelationDataSource ds = (RelationDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new RelationDataSource();
        return ds;
    }

    public void addChild(String parentPid, String pid, final BooleanCallback call) {
        if (pid == null || pid.isEmpty()) {
            throw new IllegalArgumentException("Missing PID!");
        }
        if (parentPid == null || parentPid.isEmpty()) {
            throw new IllegalArgumentException("Missing parent PID!");
        }

        DSRequest dsRequest = new DSRequest();

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

    public void removeChild(String parentPid, String pid, final BooleanCallback call) {
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
        dsRequest.setData(update); // prevents removeData to drop other than primary key attributes
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
            final String oldParentPid,
            final String parentPid,
            final BooleanCallback call) {

        final RelationDataSource ds = RelationDataSource.getInstance();
        BooleanCallback toAdd = new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value != null && value) {
                    if (parentPid != null) {
                        ds.addChild(parentPid, pid, call);
                    } else {
                        call.execute(value);
                    }
                } else {
                    call.execute(value);
                }
            }
        };
        if (oldParentPid != null) {
            ds.removeChild(oldParentPid, pid, toAdd);
        } else {
            toAdd.execute(Boolean.TRUE);
        }
    }

}
