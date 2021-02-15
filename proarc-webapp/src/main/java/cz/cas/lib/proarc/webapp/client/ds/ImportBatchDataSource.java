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

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Criterion;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.OperatorId;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import java.util.HashMap;
import java.util.LinkedHashMap;

/**
 *
 * @author Jan Pokorsky
 */
public final class ImportBatchDataSource extends ProarcDataSource {

    public static final String ID = "ImportBatchDataSource";
    public static final String FIELD_ID = ImportResourceApi.IMPORT_BATCH_ID;
    public static final String FIELD_PATH = ImportResourceApi.IMPORT_BATCH_FOLDER;
    public static final String FIELD_DESCRIPTION = ImportResourceApi.IMPORT_BATCH_DESCRIPTION;
    public static final String FIELD_TIMESTAMP = ImportResourceApi.IMPORT_BATCH_TIMESTAMP;
    public static final String FIELD_CREATE = ImportResourceApi.IMPORT_BATCH_CREATE;
    public static final String FIELD_STATE = ImportResourceApi.IMPORT_BATCH_STATE;
    public static final String FIELD_USER_ID = ImportResourceApi.IMPORT_BATCH_USERID;
    public static final String FIELD_USER_DISPLAYNAME = ImportResourceApi.IMPORT_BATCH_USER;
    public static final String FIELD_PARENT = ImportResourceApi.IMPORT_BATCH_PARENTPID;
    public static final String FIELD_LOG = ImportResourceApi.IMPORT_BATCH_FAILURE;
    public static final String FIELD_PROFILE_ID = ImportResourceApi.IMPORT_BATCH_PROFILE;

    public static final String FIELD_DEVICE = ImportResourceApi.NEWBATCH_DEVICE_PARAM;
    public static final String FIELD_INDICES = ImportResourceApi.NEWBATCH_INDICES_PARAM;

    public ImportBatchDataSource() {
        setID(ID);

        setDataURL(RestConfig.URL_IMPORT_BATCH);

        ClientMessages i18n = GWT.create(ClientMessages.class);

        DataSourceIntegerField id = new DataSourceIntegerField(FIELD_ID);
        id.setPrimaryKey(true);

        DataSourceTextField description = new DataSourceTextField(FIELD_DESCRIPTION);

        DataSourceTextField user = new DataSourceTextField(FIELD_USER_DISPLAYNAME);

        DataSourceIntegerField userId = new DataSourceIntegerField(FIELD_USER_ID);
        userId.setForeignKey(UserDataSource.ID + '.' + UserDataSource.FIELD_ID);

        DataSourceDateTimeField create = new DataSourceDateTimeField(FIELD_CREATE);
        create.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);

        DataSourceDateTimeField timestamp = new DataSourceDateTimeField(FIELD_TIMESTAMP);
        timestamp.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATETIME);

        DataSourceEnumField state = new DataSourceEnumField(FIELD_STATE);
        LinkedHashMap<String, String> states = new LinkedHashMap<String, String>();
        states.put(State.LOADING.name(), i18n.ImportBatchDataSource_State_LOADING());
        states.put(State.LOADING_FAILED.name(), i18n.ImportBatchDataSource_State_LOADING_FAILED());
        states.put(State.LOADED.name(), i18n.ImportBatchDataSource_State_LOADED());
        states.put(State.INGESTING.name(), i18n.ImportBatchDataSource_State_INGESTING());
        states.put(State.INGESTING_FAILED.name(), i18n.ImportBatchDataSource_State_INGESTING_FAILED());
        states.put(State.INGESTED.name(), i18n.ImportBatchDataSource_State_INGESTED());
        state.setValueMap(states);

        DataSourceTextField parent = new DataSourceTextField(FIELD_PARENT);
        parent.setHidden(true);

        DataSourceTextField profileId = new DataSourceTextField(FIELD_PROFILE_ID);
        LinkedHashMap<String, String> profiles = new LinkedHashMap<>();
        profiles.put(ConfigurationProfile.DEFAULT, i18n.ImportProfile_DEFAULT());
        profiles.put(ConfigurationProfile.DEFAULT_ARCHIVE_IMPORT, i18n.ImportProfile_DEFAULT_ARCHIVE_IMPORT());
        profiles.put(ConfigurationProfile.DEFAULT_KRAMERIUS_IMPORT, i18n.ImportProfile_DEFAULT_KRAMERIUS_IMPORT());
        profiles.put(ConfigurationProfile.STT_KRAMERIUS_IMPORT, i18n.ImportProfile_DEFAULT_KRAMERIUS_STT_IMPORT());
        profiles.put(ConfigurationProfile.NDK_MONOGRAPH_KRAMERIUS_IMPORT, i18n.ImportProfile_DEFAULT_KRAMERIUS_MONOGRAPH_IMPORT());
        profiles.put(ConfigurationProfile.NDK_PERIODICAL_KRAMERIUS_IMPORT, i18n.ImportProfile_DEFAULT_KRAMERIUS_PERIODIKA_IMPORT());
        profiles.put("profile.chronicle", i18n.ImportProfile_DEFAULT_CHRONICLE_IMPORT());
        profiles.put("profile.oldprint", i18n.ImportProfile_DEFAULT_OLDPRINT_IMPORT());
        profiles.put(ConfigurationProfile.DEFAULT_SOUNDRECORDING_IMPORT, i18n.ImportProfile_DEFAULT_SOUNDRECORDING_IMPORT());
        profileId.setValueMap(profiles);
        profileId.setHidden(true);

        DataSourceTextField log = new DataSourceTextField(FIELD_LOG);

        setFields(id, description, userId, user, create, timestamp, state, parent, log, profileId);
        
        setOperationBindings(RestConfig.createAddOperation(), RestConfig.createUpdateOperation());
        
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    @Override
    protected Object transformRequest(DSRequest dsRequest) {
        if (dsRequest.getOperationType() == DSOperationType.FETCH) {
            Criteria criteria = dsRequest.getCriteria();
            if (criteria.isAdvanced()) {
                HashMap<String, Object> record = new HashMap<String, Object>();
                advanceCriteriaAsParams(criteria.asAdvancedCriteria(), record);
                dsRequest.setData(record);
            }
        }
        return super.transformRequest(dsRequest);
    }

    /**
     * Gets advanced criteria as HTTP GET params.
     * @param ac advanced criteria
     * @param map map of params
     */
    private void advanceCriteriaAsParams(AdvancedCriteria ac, HashMap<String, Object> map) {
        Criterion[] criteria = ac.getCriteria();
        if (criteria == null) {
            return ;
        }
        for (Criterion criterion : criteria) {
            String fieldName = criterion.getFieldName();
            if (criterion.isAdvanced() && fieldName == null) {
                advanceCriteriaAsParams(criterion.asAdvancedCriteria(), map);
            } else {
                if (FIELD_CREATE.equals(fieldName)) {
                    if (criterion.getOperator() == OperatorId.LESS_OR_EQUAL) {
                        map.put(ImportResourceApi.IMPORT_BATCH_CREATE_TO, criterion.getValueAsDate());
                    } else {
                        map.put(ImportResourceApi.IMPORT_BATCH_CREATE_FROM, criterion.getValueAsDate());
                    }
                } else if (FIELD_TIMESTAMP.equals(fieldName)) {
                    if (criterion.getOperator() == OperatorId.LESS_OR_EQUAL) {
                        map.put(ImportResourceApi.IMPORT_BATCH_MODIFIED_TO, criterion.getValueAsDate());
                    } else {
                        map.put(ImportResourceApi.IMPORT_BATCH_MODIFIED_FROM, criterion.getValueAsDate());
                    }
                } else {
                    map.put(fieldName, criterion.getValueAsString());
                }
            }
        }
    }

    public Record newBatch(String folderPath, String profile, String device, Boolean indices) {
        Record r = new Record();
        r.setAttribute(FIELD_PATH, folderPath);
        if (profile != null) {
            r.setAttribute(FIELD_PROFILE_ID, profile);
        }
        if (indices != null) {
            r.setAttribute(FIELD_INDICES, indices);
        }
        if (device != null) {
            r.setAttribute(FIELD_DEVICE, device);
        }
        return r;
    }

    public static ImportBatchDataSource getInstance() {
        ImportBatchDataSource ds = (ImportBatchDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new ImportBatchDataSource();
        return ds;
    }

    public static class BatchRecord {

        private final Record delegate;

        public BatchRecord(Record delegate) {
            this.delegate = delegate;
        }

        public String getId() {
            return delegate.getAttribute(FIELD_ID);
        }

        public void setId(String id) {
            delegate.setAttribute(FIELD_ID, id);
        }

        public String getParentPid() {
            return delegate.getAttribute(FIELD_PARENT);
        }

        public void setParentPid(String pid) {
            delegate.setAttribute(FIELD_PARENT, pid);
        }

        public String getProfileId() {
            return delegate.getAttribute(FIELD_PROFILE_ID);
        }

        public static boolean isArchive(String profileId) {
            return ConfigurationProfileResourceApi.ARCHIVE_ID.equals(profileId);
        }

        public static boolean isKramerius(String profileID) {
            return ConfigurationProfileResourceApi.KRAMERIUS_DEFAULT_ID.equals(profileID)
                    || ConfigurationProfileResourceApi.KRAMERIUS_NDK_MONOGRAPH_ID.equals(profileID)
                    || ConfigurationProfileResourceApi.KRAMERIUS_NDK_PERIODICAL_ID.equals(profileID)
                    || ConfigurationProfileResourceApi.KRAMERIUS_STT_ID.equals(profileID);
        }

        public boolean isArchive() {
            return isArchive(getProfileId());
        }

        public boolean isKrameirus() {
            return isKramerius(getProfileId());
        }

        public State getState() {
            String attr = delegate.getAttribute(FIELD_STATE);
            return State.fromString(attr);
        }

        public String getLog() {
            return delegate.getAttribute(FIELD_LOG);
        }

        public Record getDelegate() {
            return delegate;
        }

        public boolean isArchiveOrKramerius() {
            return isKrameirus() || isArchive();
        }
    }

    /**
     * Copy of {@link cz.cas.lib.proarc.common.imports.ImportBatchManager.ImportBatch.State State}.
     * XXX make it GWT accessible and remove this.
     */
    public enum State {
        EMPTY, LOADING, LOADING_FAILED, LOADED, INGESTING, INGESTING_FAILED, INGESTED;

        public static State fromString(String value) {
            try {
                return value == null ? null : valueOf(value);
            } catch (IllegalArgumentException e) {
                return null;
            }
        }
    }

}
