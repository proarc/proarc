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
package cz.cas.lib.proarc.webapp.shared.rest;

/**
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectResourceApi {

    // resource /object
    public static final String PATH = "object";

    public static final String DIGITALOBJECT_PID = "pid";
    public static final String DIGITALOBJECT_PIDS = "pids";
    public static final String DIGITALOBJECT_PIDNEW = "pidNew";
    public static final String DIGITALOBJECT_MODEL = "model";
    public static final String BATCHID_PARAM = "batchId";
    public static final String TIMESTAMP_PARAM = "timestamp";
    public static final String DIGITALOBJECT_SERIES_DATE_FROM_PARAM = "seriesDateFrom";
    public static final String DIGITALOBJECT_SERIES_DATE_TO_PARAM = "seriesDateTo";
    public static final String DIGITALOBJECT_SERIES_DAYS_INCLUDED_PARAM = "seriesDaysIncluded";
    public static final String DIGITALOBJECT_SERIES_PARTNUMBER_FROM_PARAM = "seriesPartNumberFrom";

    public static final String NEWOBJECT_XML_PARAM = "xml";

    public static final String DELETE_HIERARCHY_PARAM = "hierarchy";
    public static final String DELETE_PID_PARAM = DIGITALOBJECT_PID;
    public static final String DELETE_PURGE_PARAM = "purge";
    public static final String DELETE_RESTORE_PARAM = "restore";


    public static final String SEARCH_PATH = "search";
    public static final String SEARCH_OWNER_PARAM = "owner";
    public static final String SEARCH_START_ROW_PARAM = "_startRow";
    public static final String SEARCH_SORT_PARAM = "_sort";
    public static final String SEARCH_SORT_FIELD_PARAM = "sortField";
    public static final String SEARCH_PHRASE_PARAM = "phrase";
    public static final String SEARCH_PID_PARAM = "pid";
    public static final String SEARCH_BATCHID_PARAM = "batchId";
    public static final String SEARCH_MODEL_PARAM_REMEMBER = "modelRemember";
    public static final String SEARCH_QUERY_CREATOR_PARAM = "queryCreator";
    public static final String SEARCH_QUERY_IDENTIFIER_PARAM = "queryIdentifier";
    public static final String SEARCH_QUERY_LABEL_PARAM = "queryLabel";
    public static final String SEARCH_QUERY_MODEL_PARAM = "queryModel";
    public static final String SEARCH_QUERY_TITLE_PARAM = "queryTitle";
    public static final String SEARCH_TYPE_PARAM = "type";
    public static final String SEARCH_STATUS_PARAM = "status";
    public static final String SEACH_ORGANIZATION_PARAM = "organization";
    public static final String SEARCH_PROCESSOR_PARAM = "processor";
    public static final String MODS_ADD_AUTHORITY = "addAuthority";
    /** XXX workaround to fix GWT 2.5 compiler bug related to the use of enum's static
     * field in enum's constant declaration.
     */
    private static final String SEARCH_TYPE_PARAM_DEFAULT = SearchType.DEFAULT;

    private static final String SEARCH_SORT_PARAM_DEFAULT = SearchSort.DEFAULT_DESC;

    // resource /object/dc
    public static final String DC_PATH = "dc";

    // DublinCoreRecord
    public static final String DUBLINCORERECORD_ELEMENT = "dcRecord";
    public static final String DUBLINCORERECORD_NS = "http://proarc.lib.cas.cz/xml/dor/v1/";
    public static final String DUBLINCORERECORD_NS_OAIDC = "http://www.openarchives.org/OAI/2.0/oai_dc/";
    public static final String DUBLINCORERECORD_NS_DC = "http://purl.org/dc/elements/1.1/";
    public static final String DUBLINCORERECORD_PID = "pid";
    public static final String DUBLINCORERECORD_BATCHID = "batchId";
    public static final String DUBLINCORERECORD_TIMESTAMP = "timestamp";
    public static final String DUBLINCORERECORD_DC = "dc";

    // resource /object/member
    public static final String MEMBERS_PATH = "member";
    public static final String MEMBERS_ITEM_PID = "pid";
    public static final String MEMBERS_ITEM_BATCHID = ImportResourceApi.BATCHITEM_BATCHID;
    public static final String MEMBERS_ITEM_MODEL = "model";
    public static final String MEMBERS_ITEM_OWNER = "owner";
    public static final String MEMBERS_ITEM_LABEL = "label";
    public static final String MEMBERS_ITEM_STATE = "state";
    public static final String MEMBERS_ITEM_CREATED = "created";
    public static final String MEMBERS_ITEM_MODIFIED = "modified";
    public static final String MEMBERS_ITEM_PARENT = "parent";
    public static final String MEMBERS_ITEM_EXPORT = "export";
    public static final String MEMBERS_ITEM_NDK_EXPORT = "ndkExport";
    public static final String MEMBERS_ITEM_ARCHIVE_EXPORT = "archiveExport";
    public static final String MEMBERS_ITEM_KRAMERIUS_EXPORT = "krameriusExport";
    public static final String MEMBERS_ITEM_CROSSREF_EXPORT = "crossrefExport";
    public static final String MEMBERS_ITEM_ORGANIZATION = "organization";
    public static final String MEMBERS_ITEM_USER = "processor";
    public static final String MEMBERS_ITEM_STATUS = "status";

    public static final String MEMBERS_ROOT_PARAM = "root";

    // resource /object/member/move
    public static final String MEMBERS_MOVE_PATH = "move";
    public static final String MEMBERS_MOVE_SRCPID = "srcPid";
    public static final String MEMBERS_MOVE_DSTPID = "dstPid";

    // resource /object/metamodel
    public static final String METAMODEL_PATH = "metamodel";
    public static final String METAMODEL_PID_PARAM = "pid";
    public static final String METAMODEL_ROOT_PARAM = "root";
    public static final String METAMODEL_LEAF_PARAM = "leaf";
    public static final String METAMODEL_DISPLAYNAME_PARAM = "displayName";
    public static final String METAMODEL_MODSCUSTOMEDITORID_PARAM = "editorId";
    public static final String METAMODEL_METADATAFORMAT_PARAM = "metadataFormat";
    /**
     * @see DatastreamEditorType
     */
    public static final String METAMODEL_DATASTREAMEDITOR_PARAM = "dataStreamEditor";

    // resource /object/mods
    public static final String MODS_PATH = "mods";
    
    // resource /object/mods/custom
    public static final String MODS_CUSTOM_PATH = "custom";
    public static final String MODS_CUSTOM_EDITORID = "editorId";
    public static final String MODS_CUSTOM_EDITOR_PAGES = "editorPages";
    public static final String MODS_CUSTOM_CUSTOMJSONDATA = "jsonData";
    public static final String MODS_CUSTOM_CUSTOMXMLDATA = "xmlData";
    public static final String MODS_CUSTOM_IGNOREVALIDATION = "ignoreValidation";
    public static final String MODS_CUSTOM_CATALOGID = "catalogId";

    public static final String TECHNICAL_CUSTOM_XMLDATA = "xmlData";
    public static final String TECHNICAL_CUSTOM_JSONDATA = "jsonData";
    public static final String TECHNICAL_CUSTOM_TYPE = "type";

    public static final String MODS_PAGE_RULES_APPLY_TO = "applyTo";
    public static final String MODS_PAGE_RULES_APPLY_TO_FIRST_PAGE = "applyToFirstPage";
    public static final String MODS_PAGE_RULES_NUMBER_PREFIX = "prefix";
    public static final String MODS_PAGE_RULES_NUMBER_SUFFIX = "suffix";
    public static final String MODS_PAGE_RULES_NUMBER_SEQUENCE_TYPE = "sequence";
    public static final String MODS_PAGE_RULES_NUMBER_START_NUMBER = "startNumber";
    public static final String MODS_PAGE_RULES_NUMBER_INCREMENT_NUMBER = "incrementNumber";
    public static final String MODS_PAGE_RULES_INDEX_START_NUMBER = "startIndex";
    public static final String MODS_PAGE_RULES_TYPE_PAGE = "pageType";

    // CustomMods
    public static final String CUSTOMMODS_ELEMENT = "mods";

    // resource /object/mods/plain
    public static final String MODS_PLAIN_PATH = "plain";

    // StringRecord
    public static final String STRINGRECORD_ELEMENT = "record";
    public static final String STRINGRECORD_CONTENT = "content";

    /** Resource /object/streamprofile. */
    public static final String STREAMPROFILE_PATH = "streamprofile";
    public static final String STREAMPROFILE_ID = "dsid";
    public static final String STREAMPROFILE_MIME = "mime";

    /** Resource /object/dissemination */
    public static final String DISSEMINATION_PATH = "dissemination";
    public static final String DISSEMINATION_DATASTREAM = "datastream";
    public static final String DISSEMINATION_FILE = "file";
    public static final String DISSEMINATION_MIME = "mime";
    public static final String DISSEMINATION_ERROR = "jsonErrors";

    /** Resource /object/full */
    public static final String FULL_PATH = "full";

    /** Resource /object/ocr */
    public static final String OCR_PATH = "ocr";

    /** Resource /object/technicalMetadata */
    public static final String TECHNICALMETADATA_PATH = "technicalMetadata";
    public static final String TECHNICALMETADATA_CODING_HISTORY_PATH = "technicalMetadataCodingHistory";
    public static final String TECHNICALMETADATA_XML_PATH = "technicalMetadataXml";
    public static final String TECHNICALMETADATA_XML_CODING_HISTORY_PATH = "technicalMetadataXmlCodingHistory";

    /** Resource /object/preview */
    public static final String PREVIEW_PATH = "preview";

    /** Resource /object/privatenote */
    public static final String PRIVATENOTE_PATH = "privatenote";

    /** Resource /object/raw */
    public static final String RAW_PATH = "raw";

    /** Resource /object/thumb */
    public static final String THUMB_PATH = "thumb";

    /** Resource /object/atm */
    public static final String ATM_PATH = "atm";
    public static final String ATM_ITEM_BATCHID = ImportResourceApi.BATCHITEM_BATCHID;
    public static final String ATM_ITEM_CREATED = MEMBERS_ITEM_CREATED;
    public static final String ATM_ITEM_DEVICE = "device";
    public static final String ATM_ITEM_EXPORTRESULT = "exportResult";
    public static final String ATM_ITEM_NDK_EXPORT = "ndkExportResult";
    public static final String ATM_ITEM_ARCHIVE_EXPORT = "archiveExportResult";
    public static final String ATM_ITEM_KRAMERIUS_EXPORT = "krameriusExportResult";
    public static final String ATM_ITEM_CROSSREF_EXPORT = "crossrefExportResult";
    public static final String ATM_ITEM_STATUS = "status";
    public static final String ATM_ITEM_ORGANIZATION = "organization";
    public static final String ATM_ITEM_USER = "userProcessor";
    public static final String ATM_ITEM_FILENAME = "filename";
    public static final String ATM_ITEM_MODEL = DIGITALOBJECT_MODEL;
    public static final String ATM_ITEM_MODIFIED = MEMBERS_ITEM_MODIFIED;
    public static final String ATM_ITEM_OWNER = MEMBERS_ITEM_OWNER;
    public static final String ATM_ITEM_PID = DIGITALOBJECT_PID;
    public static final String ATM_ITEM_STATE = MEMBERS_ITEM_STATE;

    /** Resource /object/thumb */
    public static final String URNNBN_PATH = "urnnbn";
    public static final String COPYOBJECT_PATH = "copyObject";
    public static final String URNNBN_HIERARCHY = "hierarchy";
    public static final String URNNBN_RESOLVER = "resolverId";
    public static final String REINDEX_PATH = "reindexObjects";
    public static final String UPDATE_ALL_OBJECTS_PATH = "updateAllObjectsObjects";

    public static final String URNNBN_ITEM_URNNBN = "urnnbn";
    public static final String URNNBN_ITEM_MESSAGE = "message";
    public static final String URNNBN_ITEM_STATUSTYPE = "statusType";
    public static final String URNNBN_ITEM_WARNING = "warning";
    public static final String URNNBN_ITEM_LOG = "log";
    public static final String WORKFLOW_JOB_ID = "wfJobId";

    public static final String GENERATE_JP2_PATH = "generateJp2";
    public static final String GENERATE_TYPE = "generateType";

    public static final String CHANGE_PAGE_TO_NDK_PAGE = "changePageToNdkPage";
    public static final String CHANGE_NDK_PAGE_TO_PAGE = "changeNdkPageToPage";
    public static final String CHANGE_CLIPPINGS_VOLUME_TO_NDK_MONOGRAPH_VOLUME = "changeClippingsVolumeToNdkMonographVolume";
    public static final String CHANGE_CLIPPINGS_TITLE_TO_NDK_MONOGRAPH_TITLE = "changeClippingsTitleToNdkMonographTitle";

    public static final String UPDATE_NDK_ARTICLE = "updateNdkArticle";

    public enum SearchType {
        
        LAST_CREATED(SEARCH_TYPE_PARAM_DEFAULT),
        LAST_MODIFIED("lastModified"),
        /** Search object's parents. Accepted parameters: pid | batchId */
        PARENT("parent"),
        PIDS("pids"),
        PHRASE("phrase"),
        QUERY("query"),
        DELETED("deleted"),
        ALPHABETICAL("alphabetical"),
        ADVANCED("advanced"),
        STATUS("status"),
        ALL("all");
        
        public static final String DEFAULT = "lastCreated";
        private String external;

        SearchType(String external) {
            this.external = external;
        }

        @Override
        public String toString() {
            return external;
        }

        public static SearchType fromString(String external) {
            for (SearchType type : values()) {
                if (type.external.equals(external)) {
                    return type;
                }
            }
            return null;
        }

    }

    public enum SearchSort {

        DESC(SEARCH_SORT_PARAM_DEFAULT),
        ASC("asc");

        public static final String DEFAULT_DESC = "desc";
        private String external;

        SearchSort(String external) {
            this.external = external;
        }

        @Override
        public String toString() {
            return external;
        }

        public static SearchSort fromString(String external) {
            for (SearchSort type : values()) {
                if (type.external.equals(external)) {
                    return type;
                }
            }
            return null;
        }

    }

}
