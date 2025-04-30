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
import cz.cas.lib.proarc.common.dao.Batch;
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
    public static final String FIELD_PRIORITY = ImportResourceApi.IMPORT_BATCH_PRIORITY;

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

        DataSourceTextField priority = new DataSourceTextField(FIELD_PRIORITY);
        LinkedHashMap<String, String> priorities = new LinkedHashMap<>();
        priorities.put(Batch.PRIORITY_LOWEST, i18n.ImportSourceChooser_OptionPriority_Lowest());
        priorities.put(Batch.PRIORITY_LOW, i18n.ImportSourceChooser_OptionPriority_Low());
        priorities.put(Batch.PRIORITY_MEDIUM, i18n.ImportSourceChooser_OptionPriority_Medium());
        priorities.put(Batch.PRIORITY_HIGH, i18n.ImportSourceChooser_OptionPriority_High());
        priorities.put(Batch.PRIORITY_HIGHEST, i18n.ImportSourceChooser_OptionPriority_Highest());
        priority.setValueMap(priorities);


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
        states.put(State.STOPPED.name(), i18n.ImportBatchDataSource_State_STOPPED());
        states.put(State.LOADING_CONFLICT.name(), i18n.ImportBatchDataSource_State_LOADING_CONFLICT());
        states.put(State.EXPORTING.name(), i18n.ImportBatchDataSource_State_EXPORTING());
        states.put(State.EXPORT_PLANNED.name(), i18n.ImportBatchDataSource_State_EXPORT_PLANNED());
        states.put(State.EXPORT_FAILED.name(), i18n.ImportBatchDataSource_State_EXPORT_FAILED());
        states.put(State.EXPORT_VALID_WARNING.name(), i18n.ImportBatchDataSource_State_EXPORT_FINISHED_WITH_WARNING());
        states.put(State.EXPORT_DONE.name(), i18n.ImportBatchDataSource_State_EXPORT_DONE());

//        states.put(State.REINDEXING.name(), i18n.ImportBatchDataSource_State_REINDEXING());
//        states.put(State.REINDEX_FAILED.name(), i18n.ImportBatchDataSource_State_REINDEX_FAILED());
//        states.put(State.REINDEX_DONE.name(), i18n.ImportBatchDataSource_State_REINDEX_DONE());
//        states.put(State.CHANGING_OWNERS.name(), i18n.ImportBatchDataSource_State_CHANGING_OWNERS());
//        states.put(State.CHANGE_OWNERS_DONE.name(), i18n.ImportBatchDataSource_State_CHANGE_OWNERS_DONE());
//        states.put(State.CHANGE_OWNERS_FAILED.name(), i18n.ImportBatchDataSource_State_CHANGE_OWNERS_FAILED());
        states.put(State.UPLOADING.name(), i18n.ImportBatchDataSource_State_UPLOADING());
        states.put(State.UPLOAD_FAILED.name(), i18n.ImportBatchDataSource_State_UPLOAD_FAILED());
        states.put(State.UPLOAD_DONE.name(), i18n.ImportBatchDataSource_State_UPLOAD_DONE());

        states.put(State.INTERNAL_PLANNED.name(), i18n.ImportBatchDataSource_State_INTERNAL_PLANNED());
        states.put(State.INTERNAL_RUNNING.name(), i18n.ImportBatchDataSource_State_INTERNAL_RUNNING());
        states.put(State.INTERNAL_DONE.name(), i18n.ImportBatchDataSource_State_INTERNAL_DONE());
        states.put(State.INTERNAL_FAILED.name(), i18n.ImportBatchDataSource_State_INTERNAL_FAILED());

        state.setValueMap(states);

        DataSourceTextField parent = new DataSourceTextField(FIELD_PARENT);
        parent.setHidden(true);

        DataSourceTextField profileId = new DataSourceTextField(FIELD_PROFILE_ID);
        LinkedHashMap<String, String> profiles = new LinkedHashMap<>();
        profiles.put(ConfigurationProfile.DEFAULT, i18n.ImportProfile_DEFAULT());
        profiles.put("profile.defaultocr", i18n.ImportProfile_DEFAULT_OCR());
        profiles.put(ConfigurationProfile.DEFAULT_ARCHIVE_IMPORT, i18n.ImportProfile_ARCHIVE_IMPORT());
        profiles.put(ConfigurationProfile.DEFAULT_NDK_IMPORT, i18n.ImportProfile_NDK_IMPORT());
        profiles.put(ConfigurationProfile.DEFAULT_KRAMERIUS_IMPORT, i18n.ImportProfile_KRAMERIUS_IMPORT());
        profiles.put(ConfigurationProfile.STT_KRAMERIUS_IMPORT, i18n.ImportProfile_KRAMERIUS_STT_IMPORT());
        profiles.put(ConfigurationProfile.NDK_MONOGRAPH_KRAMERIUS_IMPORT, i18n.ImportProfile_KRAMERIUS_MONOGRAPH_IMPORT());
        profiles.put(ConfigurationProfile.NDK_MONOGRAPH_TITLE_KRAMERIUS_IMPORT, i18n.ImportProfile_KRAMERIUS_MONOGRAPH_TITLE_IMPORT());
        profiles.put(ConfigurationProfile.NDK_PERIODICAL_KRAMERIUS_IMPORT, i18n.ImportProfile_KRAMERIUS_PERIODIKA_IMPORT());
        profiles.put(ConfigurationProfile.NDK_EMONOGRAPH_KRAMERIUS_IMPORT, i18n.ImportProfile_KRAMERIUS_EMONOGRAPH_IMPORT());
        profiles.put(ConfigurationProfile.NDK_EPERIODICAL_KRAMERIUS_IMPORT, i18n.ImportProfile_KRAMERIUS_EPERIODIKA_IMPORT());
        profiles.put(ConfigurationProfile.REPLACE_STREAM_IMPORT, i18n.ImportProfile_REPLACE_STREAM_IMPORT());
        profiles.put("profile.chronicle", i18n.ImportProfile_CHRONICLE_IMPORT());
        profiles.put("profile.oldprint", i18n.ImportProfile_OLDPRINT_IMPORT());
        profiles.put("profile.oldprintocr", i18n.ImportProfile_OLDPRINT_OCR_IMPORT());
        profiles.put("profile.ndk_full_import", i18n.ImportProfile_NDK_FULL_IMPORT());
        profiles.put("profile.ndk_without_ocr", i18n.ImportProfile_NDK_FULL_WITHOUR_OCR_IMPORT());
        profiles.put("profile.oldprint_full_import", i18n.ImportProfile_OLDPRINT_FULL_IMPORT());
        profiles.put("profile.oldprint_without_ocr", i18n.ImportProfile_OLDPRINT_FULL_WITHOUR_OCR_IMPORT());
        profiles.put("profile.createObjectWithMetadata_import", i18n.ImportProfile_CREATE_OBJECT_WITH_METADATA());
        profiles.put(ConfigurationProfile.DEFAULT_SOUNDRECORDING_IMPORT, i18n.ImportProfile_SOUNDRECORDING_IMPORT());

        profiles.put(Batch.UPLOAD_PROARC, i18n.UploadProfile_PROARC());
        profiles.put(Batch.UPLOAD_KRAMERIUS, i18n.UploadProfile_KRAMERIUS());

        profiles.put("exportProfile.datastream", i18n.ExportProfile_Datastream());
        profiles.put("exportProfile.kramerius", i18n.ExportProfile_Kramerius());
        profiles.put("exportProfile.ndk", i18n.ExportProfile_Ndk());
        profiles.put("exportProfile.archive", i18n.ExportProfile_Archive());
        profiles.put("exportProfile.desa", i18n.ExportProfile_Desa());
        profiles.put("exportProfile.cejsh", i18n.ExportProfile_Cejsh());
        profiles.put("exportProfile.crossref", i18n.ExportProfile_Crossref());
        profiles.put("exportProfile.kwis", i18n.ExportProfile_Kwis());
//        profiles.put("exportProfile.aleph", i18n.ExportProfile_Aleph());

        profiles.put("internalProfile.reindex", i18n.InternalProfile_Reindex());
        profiles.put("internalProfile.pero", i18n.InternalProfile_Pero());
        profiles.put("internalProfile.changeOwners", i18n.InternalProfile_Change_Owners());
        profiles.put("internalProfile.validation", i18n.InternalProfile_Validation());
        profiles.put("internalProfile.updateCatalogRecords", i18n.InternalProfile_UpdateCatalogRecords());
        profileId.setValueMap(profiles);
        profileId.setHidden(true);

        DataSourceTextField log = new DataSourceTextField(FIELD_LOG);

        setFields(id, description, userId, user, create, timestamp, state, parent, log, profileId, priority);
        
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

    public Record newBatch(String folderPath, String profile, String device, Boolean indices, String priority) {
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
        if (priority != null) {
            r.setAttribute(FIELD_PRIORITY, priority);
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

        public static boolean isNdk(String profileId) {
            return ConfigurationProfileResourceApi.NDK_ID.equals(profileId);
        }

        public static boolean isReplaceStream(String profileId) {
            return ConfigurationProfileResourceApi.REPLACE_STREAM_ID.equals(profileId);
        }

        public static boolean isKramerius(String profileID) {
            return ConfigurationProfileResourceApi.KRAMERIUS_DEFAULT_ID.equals(profileID)
                    || ConfigurationProfileResourceApi.KRAMERIUS_NDK_MONOGRAPH_ID.equals(profileID)
                    || ConfigurationProfileResourceApi.KRAMERIUS_NDK_MONOGRAPH_TITLE_ID.equals(profileID)
                    || ConfigurationProfileResourceApi.KRAMERIUS_NDK_PERIODICAL_ID.equals(profileID)
                    || ConfigurationProfileResourceApi.KRAMERIUS_NDK_EMONOGRAPH_ID.equals(profileID)
                    || ConfigurationProfileResourceApi.KRAMERIUS_NDK_EPERIODICAL_ID.equals(profileID)
                    || ConfigurationProfileResourceApi.KRAMERIUS_STT_ID.equals(profileID);
        }

        public boolean isArchive() {
            return isArchive(getProfileId());
        }

        public boolean isNdk() {
            return isNdk(getProfileId());
        }

        public boolean isReplaceStream() {
            return isReplaceStream(getProfileId());
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

        public boolean isArchiveOrKrameriusOrReplaceOrNdkStream() {
            return isKrameirus() || isArchive() || isReplaceStream() || isNdk();
        }
    }

    /**
     * Copy of {@link cz.cas.lib.proarc.common.dao.Batch.State State}.
     * XXX make it GWT accessible and remove this.
     */
    public enum State {
        EMPTY, LOADING, LOADING_FAILED, LOADED, INGESTING, INGESTING_FAILED, INGESTED, STOPPED,LOADING_CONFLICT,
        EXPORTING, EXPORT_PLANNED, EXPORT_FAILED, EXPORT_VALID_WARNING, EXPORT_DONE,
//        REINDEXING, REINDEX_FAILED, REINDEX_DONE,
//        CHANGING_OWNERS, CHANGE_OWNERS_FAILED, CHANGE_OWNERS_DONE,
        UPLOADING, UPLOAD_FAILED, UPLOAD_DONE,
        INTERNAL_RUNNING, INTERNAL_PLANNED, INTERNAL_FAILED, INTERNAL_DONE;

        public static State fromString(String value) {
            try {
                return value == null ? null : valueOf(value);
            } catch (IllegalArgumentException e) {
                return null;
            }
        }
    }

}
