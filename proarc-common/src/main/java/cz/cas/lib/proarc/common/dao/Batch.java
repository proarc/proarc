/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.dao;

import java.io.StringReader;
import java.io.StringWriter;
import java.sql.Timestamp;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

/**
 * The batch describes group of {@link BatchItem items} related to some task
 * e.g. import.
 *
 * @author Jan Pokorsky
 */
public class Batch {

    public static final String PRIORITY_LOWEST = "lowest";
    public static final String PRIORITY_LOW = "low";
    public static final String PRIORITY_MEDIUM = "medium";
    public static final String PRIORITY_HIGH = "high";
    public static final String PRIORITY_HIGHEST = "highest";

    public static final String EXPORT_DATASTREAM = "exportProfile.datastream";
    public static final String EXPORT_KRAMERIUS = "exportProfile.kramerius";
    public static final String EXPORT_NDK = "exportProfile.ndk";
    public static final String EXPORT_ARCHIVE = "exportProfile.archive";
    public static final String EXPORT_DESA = "exportProfile.desa";
    public static final String EXPORT_CEJSH = "exportProfile.cejsh";
    public static final String EXPORT_CROSSREF = "exportProfile.crossref";
    public static final String EXPORT_KWIS = "exportProfile.kwis";
    public static final String EXPORT_ALEPH = "exportProfile.aleph";

    public static final String UPLOAD_PROARC = "uploadProfile.proarc";
    public static final String UPLOAD_KRAMERIUS = "uploadProfile.kramerius";

    public static final String INTERNAL_REINDEX = "internalProfile.reindex";
    public static final String INTERNAL_CHANGE_OBJECTS_OWNERS = "internalProfile.changeOwners";
    public static final String INTERNAL_UPDATE_CATALOG_RECORDS = "internalProfile.updateCatalogRecords";
    public static final String INTERNAL_VALIDATION = "internalProfile.validation";
    public static final String INTERNAL_URNNBN = "internalProfile.urnnbn";
    public static final String INTERNAL_DELETION = "internalProfile.deletion";

    public static final String EXTERNAL_PERO = "externalProfile.pero";
    public static final String EXTERNAL_PDFA = "externalProfile.pdfa";

    public enum State {

        EMPTY, LOADING, LOADING_FAILED, LOADED, INGESTING, INGESTING_FAILED, INGESTED, LOADING_CONFLICT,
        EXPORTING, EXPORT_PLANNED, EXPORT_FAILED, EXPORT_VALID_WARNING, EXPORT_DONE,
//        REINDEXING, REINDEX_FAILED, REINDEX_DONE,
//        CHANGING_OWNERS, CHANGE_OWNERS_FAILED, CHANGE_OWNERS_DONE,
        UPLOADING, UPLOAD_FAILED, UPLOAD_DONE,
        INTERNAL_RUNNING, INTERNAL_PLANNED, INTERNAL_FAILED, INTERNAL_DONE,
        EXTERNAL_RUNNING, EXTERNAL_PLANNED, EXTERNAL_FAILED, EXTERNAL_DONE,
        STOPPED
    }
    
    private Integer id;
    private String folder;
    private String title;
    private String parentPid;
    private Timestamp create;
    private Timestamp timestamp;
    private State state;
    private Integer userId;
    private Integer estimateItemNumber;
    // user input fields
    private String device;
    private boolean generateIndices;
    private boolean generatePageNumber;
    private String log;
    private String profileId;
    private String priority;
    private String params;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Timestamp getCreate() {
        return create;
    }

    public void setCreate(Timestamp create) {
        this.create = create;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public String getFolder() {
        return folder;
    }

    public void setFolder(String folderPath) {
        this.folder = folderPath;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public State getState() {
        return state;
    }

    public void setState(State state) {
        this.state = state;
    }

    public String getStateAsString() {
        return state == null ? null : state.name();
    }

    public void setStateAsString(String state) {
        this.state = state == null ? null : State.valueOf(state);
    }

    public Integer getUserId() {
        return userId;
    }

    public void setUserId(Integer userId) {
        this.userId = userId;
    }

    public Integer getEstimateItemNumber() {
        return estimateItemNumber;
    }

    public void setEstimateItemNumber(Integer estimateItemNumber) {
        this.estimateItemNumber = estimateItemNumber;
    }

    public String getParentPid() {
        return parentPid;
    }

    public void setParentPid(String parentPid) {
        this.parentPid = parentPid;
    }

    public String getDevice() {
        return device;
    }

    public void setDevice(String device) {
        this.device = device;
    }

    public boolean isGenerateIndices() {
        return generateIndices;
    }

    public void setGenerateIndices(boolean generateIndices) {
        this.generateIndices = generateIndices;
    }

    public boolean isGeneratePageNumber() {
        return generatePageNumber;
    }

    public void setGeneratePageNumber(boolean generatePageNumber) {
        this.generatePageNumber = generatePageNumber;
    }

    public String getLog() {
        return log;
    }

    public void setLog(String log) {
        this.log = log;
    }

    public String getProfileId() {
        return profileId;
    }

    public void setProfileId(String profileId) {
        this.profileId = profileId;
    }

    public String getParams() {
        return this.params;
    }

    public BatchParams getParamsAsObject() {
        if (this.params == null) {
            return null;
        }
        try {
            JAXBContext context = JAXBContext.newInstance(BatchParams.class);
            Unmarshaller unmarshaller = context.createUnmarshaller();
            StringReader reader = new StringReader(this.params);
            BatchParams batchParams = (BatchParams) unmarshaller.unmarshal(reader);
            return batchParams;
        } catch (JAXBException e) {
            return null;
        }
    }

    public void setParams(String params) {
        this.params = params;
    }

    public void setParamsFromObject(BatchParams params) {
        try {
            JAXBContext context = JAXBContext.newInstance(BatchParams.class);
            Marshaller marshaller = context.createMarshaller();
            StringWriter paramsWriter = new StringWriter();
            marshaller.marshal(params, paramsWriter);
            this.params = paramsWriter.toString();
        } catch (JAXBException ex) {
            this.params = null;
        }
    }

    public String getPriority() {
        return priority;
    }

    public void setPriority(String priority) {
        this.priority = priority;
    }

    @Override
    public String toString() {
        return "Batch{" + "id=" + id + ", folder=" + folder + ", title=" + title
                + ", parentPid=" + parentPid + ", create=" + create
                + ", timestamp=" + timestamp + ", state=" + state
                + ", userId=" + userId + ", estimateItemNumber=" + estimateItemNumber
                + ", device=" + device + ", generateIndices=" + generateIndices
                + ", profileId=" + profileId + ", log=" + log
                + ", priority=" + priority + ", params = " + params + "}";
    }

}
