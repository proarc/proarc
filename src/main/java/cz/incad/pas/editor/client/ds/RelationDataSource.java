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

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HandlerRegistration;
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
import com.smartgwt.client.docs.TreeDataBinding;
import com.smartgwt.client.types.CriteriaPolicy;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.util.BooleanCallback;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import java.util.Arrays;
import java.util.logging.Logger;

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

    private static final Logger LOG = Logger.getLogger(RelationDataSource.class.getName());
    public static final String ID = "RelationDataSource";
    private static RelationDataSource INSTANCE;

    public static final String FIELD_PID = DigitalObjectResourceApi.MEMBERS_ITEM_PID;
    public static final String FIELD_PARENT = DigitalObjectResourceApi.MEMBERS_ITEM_PARENT;
    public static final String FIELD_ROOT = DigitalObjectResourceApi.MEMBERS_ROOT_PARAM;
    public static final String FIELD_MODEL = DigitalObjectResourceApi.MEMBERS_ITEM_MODEL;
    public static final String FIELD_OWNER = DigitalObjectResourceApi.MEMBERS_ITEM_OWNER;
    public static final String FIELD_LABEL = DigitalObjectResourceApi.MEMBERS_ITEM_LABEL;
    public static final String FIELD_STATE = DigitalObjectResourceApi.MEMBERS_ITEM_STATE;
    public static final String FIELD_CREATED = DigitalObjectResourceApi.MEMBERS_ITEM_CREATED;
    public static final String FIELD_MODIFIED = DigitalObjectResourceApi.MEMBERS_ITEM_MODIFIED;

    /**
     * Attribute holding PIDs of the reorder update {@link #reorderChildren operation}.
     * See also {@link #transformResponse }.
     */
    private static final String ATTR_REORDER = "reorder";

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
                RestConfig.createDeleteOperation(),
                RestConfig.createUpdateOperation()
                );
        setCriteriaPolicy(CriteriaPolicy.DROPONCHANGE);
    }

    /**
     * Checks sent and returned PID lists after reorder request. If they differ
     * then invalidate cache to refresh widget records.
     */
    @Override
    protected void transformResponse(DSResponse response, DSRequest request, Object data) {
        super.transformResponse(response, request, data);
//        LOG.info("RelationDataSource.transformResponse");
        if (RestConfig.isStatusOk(response)) {
            if (request.getOperationType() == DSOperationType.UPDATE) {
                String[] oldPids = request.getAttributeAsStringArray(ATTR_REORDER);
                if (oldPids == null) {
                    return ;
                }
                String[] newPids = ClientUtils.toFieldValues(response.getData(), FIELD_PID);
                if (!Arrays.equals(oldPids, newPids)) {
                    response.setInvalidateCache(Boolean.TRUE);
                }
            }
        }
    }

    public static RelationDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = (RelationDataSource) DataSource.get(ID);
            // DataSource.get does not work reliably
            INSTANCE = INSTANCE != null ? INSTANCE : new RelationDataSource();
        }
        return INSTANCE;
    }

    public void addChild(String parentPid, String pid, final BooleanCallback call) {
        if (pid == null || pid.isEmpty()) {
            throw new IllegalArgumentException("Missing PID!");
        }
        addChild(parentPid, new String[] {pid}, call);
    }

    public void addChild(String parentPid, String[] pid, final BooleanCallback call) {
        if (pid == null || pid.length < 1) {
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
        removeChild(parentPid, new String[] {pid}, call);
    }

    public void removeChild(String parentPid, String[] pid, final BooleanCallback call) {
        if (pid == null || pid.length < 1) {
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

        moveChild(new String[] {pid}, oldParentPid, parentPid, call);
    }
    public void moveChild(final String[] pid,
            final String parentPidFrom,
            final String parentPidTo,
            final BooleanCallback call) {

        final RelationDataSource ds = RelationDataSource.getInstance();
        BooleanCallback toAdd = new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value != null && value) {
                    if (parentPidTo != null) {
                        ds.addChild(parentPidTo, pid, call);
                    } else {
                        call.execute(value);
                    }
                } else {
                    call.execute(value);
                }
            }
        };
        if (parentPidFrom != null) {
            ds.removeChild(parentPidFrom, pid, toAdd);
        } else {
            toAdd.execute(Boolean.TRUE);
        }
    }

    /**
     * Saves new children sequence for a given parent object.
     * 
     * @param parent parent object
     * @param childPids children sequence
     * @param call callback
     */
    public void reorderChildren(DigitalObject parent, String[] childPids, final BooleanCallback call) {
        if (childPids == null || childPids.length < 2) {
            throw new IllegalArgumentException("Unexpected children: " + Arrays.toString(childPids));
        }
        if (parent == null) {
            throw new NullPointerException("parent");
        }

        DSRequest dsRequest = new DSRequest();
        dsRequest.setAttribute(ATTR_REORDER, childPids);

        Record update = new Record();
        update.setAttribute(RelationDataSource.FIELD_PID, childPids);
        String parentPid = parent.getPid();
        String batchId = parent.getBatchId();
        if (batchId != null) {
            update.setAttribute(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID, batchId);
        } else {
            update.setAttribute(RelationDataSource.FIELD_PARENT, parentPid);
        }
        updateData(update, new DSCallback() {

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

    /**
     * Compares arrays of PIDS as array of records.
     */
    public static boolean equals(Record[] rs1, Record[] rs2) {
        if (rs1 == null && rs1 == rs2) {
            return true;
        }
        if (rs1 == null || rs2 == null) {
            return false;
        }
        if (rs1.length != rs2.length) {
            return false;
        }
        for (int i = 0; i < rs1.length; i++) {
            String pid1 = rs1[i].getAttribute(RelationDataSource.FIELD_PID);
            String pid2 = rs2[i].getAttribute(RelationDataSource.FIELD_PID);
            if (!pid1.equals(pid2)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Invoked by other digital object editors that can change object label.
     * @param pid PID of modified object
     */
    public void fireRelationChange(String pid) {
        fireEvent(new RelationChangeEvent(pid));
    }

    /**
     * Fetches relations of parent object and update caches to notify widgets.
     * @param parentPid PID of parent object
     */
    public final void updateCaches(String parentPid) {
        Criteria criteria = new Criteria(RelationDataSource.FIELD_ROOT, parentPid);
        criteria.addCriteria(RelationDataSource.FIELD_PARENT, parentPid);
        fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                request.setOperationType(DSOperationType.UPDATE);
                updateCaches(response, request);
            }
        });
    }

    public final HandlerRegistration addRelationChangeHandler(RelationChangeHandler handler) {
        return doAddHandler(handler, RelationChangeEvent.TYPE);
    }

    /**
     * Notifies changes of relation labels.
     */
    public static interface RelationChangeHandler extends EventHandler {

        void onRelationChange(RelationChangeEvent event);

    }

    public static final class RelationChangeEvent extends GwtEvent<RelationChangeHandler> {

        public static final Type<RelationChangeHandler> TYPE = new Type<RelationChangeHandler>();
        private final String pid;

        public RelationChangeEvent() {
            this(null);
        }

        public RelationChangeEvent(String pid) {
            this.pid = pid;
        }

        public String getPid() {
            return pid;
        }

        @Override
        public Type<RelationChangeHandler> getAssociatedType() {
            return TYPE;
        }

        @Override
        protected void dispatch(RelationChangeHandler handler) {
            handler.onRelationChange(this);
        }
    }

}
