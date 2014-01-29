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
import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataArrivedEvent;
import com.smartgwt.client.data.events.DataArrivedHandler;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.FetchMode;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi.SearchType;
import java.util.HashMap;

/**
 *
 * @author Jan Pokorsky
 */
public final class SearchDataSource extends RestDataSource {

    public static final String ID = "SearchDataSource";

    public static final String FIELD_PID = DigitalObjectResourceApi.MEMBERS_ITEM_PID;
    public static final String FIELD_MODEL = DigitalObjectResourceApi.MEMBERS_ITEM_MODEL;
    public static final String FIELD_OWNER = DigitalObjectResourceApi.MEMBERS_ITEM_OWNER;
    public static final String FIELD_LABEL = DigitalObjectResourceApi.MEMBERS_ITEM_LABEL;
    public static final String FIELD_STATE = DigitalObjectResourceApi.MEMBERS_ITEM_STATE;
    public static final String FIELD_CREATED = DigitalObjectResourceApi.MEMBERS_ITEM_CREATED;
    public static final String FIELD_MODIFIED = DigitalObjectResourceApi.MEMBERS_ITEM_MODIFIED;
    public static final String FIELD_EXPORT = DigitalObjectResourceApi.MEMBERS_ITEM_EXPORT;

    public SearchDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_DIGOBJECT_SEARCH);

        DataSourceField pid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        pid.setPrimaryKey(true);

        DataSourceField owner = new DataSourceField(FIELD_OWNER, FieldType.TEXT);
        DataSourceField label = new DataSourceField(FIELD_LABEL, FieldType.TEXT);
        DataSourceField state = new DataSourceField(FIELD_STATE, FieldType.ENUM);
        HashMap<String, String> states = new HashMap<String, String>();
        states.put("fedora-system:def/model#Active", "Active");
        states.put("fedora-system:def/model#Inactive", "Inactive");
        states.put("fedora-system:def/model#Deleted", "Deleted");
        state.setValueMap(states);
        DataSourceDateTimeField created = new DataSourceDateTimeField(FIELD_CREATED);
        created.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        DataSourceDateTimeField modified = new DataSourceDateTimeField(FIELD_MODIFIED);
        modified.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);
        DataSourceField export = new DataSourceField(FIELD_EXPORT, FieldType.TEXT);

        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        model.setForeignKey(MetaModelDataSource.ID + '.' + MetaModelDataSource.FIELD_PID);

        setFields(label, model, pid, created, modified, owner, state, export);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static SearchDataSource getInstance() {
        SearchDataSource ds = (SearchDataSource) DataSource.get(ID);
        ds = (ds != null) ? ds : new SearchDataSource();
        return ds;
    }

    /**
     * Finds digital object records.
     * @param pids PIDs to find
     * @param callback result set of found records; no paging
     */
    public void find(final String[] pids, final Callback<ResultSet, Void> callback) {
        Criteria criteria = new Criteria(
                DigitalObjectResourceApi.SEARCH_TYPE_PARAM, SearchType.PIDS.toString());
        if (pids != null && pids.length > 0) {
            criteria.addCriteria(DigitalObjectResourceApi.SEARCH_PID_PARAM, pids);
        } else {
            throw new IllegalArgumentException("pids");
        }
        basicFetch(criteria, callback);
    }

    /**
     * Finds parent object for a PID or a Batch Import.
     *
     * @param pid PID of digital object; {@code null} if looking for batch import
     * @param batchId ID of batch import; {@code null} if looking for PID
     * @param callback result set of found records
     */
    public void findParent(String pid, String batchId, final Callback<ResultSet, Void> callback) {
        Criteria criteria = new Criteria(
                DigitalObjectResourceApi.SEARCH_TYPE_PARAM, SearchType.PARENT.toString());
        if (pid != null && !pid.isEmpty()) {
            criteria.addCriteria(DigitalObjectResourceApi.SEARCH_PID_PARAM, pid);
        }
        if (batchId != null && !batchId.isEmpty()) {
            criteria.addCriteria(DigitalObjectResourceApi.SEARCH_BATCHID_PARAM, batchId);
        }
        basicFetch(criteria, callback);
    }

    private void basicFetch(Criteria criteria, final Callback<ResultSet, Void> callback) {
        final ResultSet resultSet = new ResultSet(this);
        resultSet.setCriteria(criteria);
        resultSet.setFetchMode(FetchMode.BASIC);
//        resultSet.setCriteriaPolicy(CriteriaPolicy.DROPONCHANGE);
        // server resource returns full result in case of SearchType.PIDS query
        if (resultSet.lengthIsKnown()) {
            callback.onSuccess(resultSet);
        } else {
            final HandlerRegistration[] handler = new HandlerRegistration[1];
            handler[0] = resultSet.addDataArrivedHandler(new DataArrivedHandler() {

                @Override
                public void onDataArrived(DataArrivedEvent event) {
                    handler[0].removeHandler();
                    callback.onSuccess(resultSet);
                }
            });
            resultSet.get(0);
        }
    }

    public static boolean isDeleted(Record r) {
        return "fedora-system:def/model#Deleted".equals(r.getAttribute(FIELD_STATE));
    }

}
