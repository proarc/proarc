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
    public static final String DIGITALOBJECT_PARENT_PID = "parent";
    public static final String DIGITALOBJECT_PIDS = "pids";
    public static final String DIGITALOBJECT_PIDNEW = "pidNew";
    public static final String DIGITALOBJECT_MODEL = "model";
    public static final String BATCHID_PARAM = "batchId";
    public static final String TIMESTAMP_PARAM = "timestamp";
    public static final String DIGITALOBJECT_SERIES_DATE_FROM_PARAM = "seriesDateFrom";
    public static final String DIGITALOBJECT_SERIES_DATE_TO_PARAM = "seriesDateTo";
    public static final String DIGITALOBJECT_SERIES_DAYS_INCLUDED_PARAM = "seriesDaysIncluded";
    public static final String DIGITALOBJECT_SERIES_MISSING_DAYS_INCLUDED_PARAM = "seriesMissingDaysIncluded";
    public static final String DIGITALOBJECT_SERIES_DAYS_IN_RANGE_PARAM = "seriesDaysInRange";
    public static final String DIGITALOBJECT_SERIES_PARTNUMBER_FROM_PARAM = "seriesPartNumberFrom";
    public static final String DIGITALOBJECT_SERIES_SIGNATURA = "seriesSignatura";
    public static final String DIGITALOBJECT_SERIES_FREQUENCY = "seriesFrequency";
    public static final String DIGITALOBJECT_SERIES_DATE_FORMAT = "seriesDateFormat";
    public static final String DIGITALOBJECT_SOURCE_PIDS = "sourcePids";
    public static final String DIGITALOBJECT_DESTINATION_PIDS = "destinationPids";
    public static final String DIGITALOBJECT_COPY_PAGE_NUMBER = "copyPageNumber";
    public static final String DIGITALOBJECT_COPY_PAGE_INDEX = "copyPageIndex";
    public static final String DIGITALOBJECT_COPY_PAGE_TYPE = "copyPageType";
    public static final String DIGITALOBJECT_COPY_PAGE_POSITION = "copyPagePosition";
    public static final String DIGITALOBJECT_COPY_PAGE_REPRE = "copyReprePage";

    public static final String CHANGE_OWNER_PATH = "changeOwner";
    public static final String DIGITALOBJECT_CHANGE_OWNER_OLD = "oldOwner";
    public static final String DIGITALOBJECT_CHANGE_OWNER_NEW = "newOwner";

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

    // resource /object/createPid
    public static final String CREATE_PID_PATH = "createPid";

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

    public static final String MEMBERS_ITEM_LOCKED = "isLocked";
    public static final String MEMBERS_ITEM_PAGE_NUMBER = "pageNumber";
    public static final String MEMBERS_ITEM_PAGE_TYPE = "pageType";
    public static final String MEMBERS_ITEM_PAGE_INDEX = "pageIndex";
    public static final String MEMBERS_ITEM_PAGE_POSITION = "pagePosition";
    public static final String MEMBERS_ITEM_PAGE_REPRE = "pageRepre";

    public static final String MEMBERS_ITEM_URNNBN = "urnNbn";
    public static final String MEMBERS_ITEM_DESCRIPTION_STANDARD = "descriptionStandard";

    public static final String ITEM_VALIDATION = "validation";
    public static final String ITEM_VALIDATION_STATUS = "validationStatus";
    public static final String ITEM_VALIDATION_PROCESS = "validationProcess";

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
    public static final String METAMODEL_DATASTREAMEDITOR_PARAM = "dataStreamEditor";

    // resource /object/mods
    public static final String MODS_PATH = "mods";
    
    // resource /object/mods/custom
    public static final String MODS_CUSTOM_PATH = "custom";
    public static final String MODS_CUSTOM_EDITORID = "editorId";
    public static final String MODS_CUSTOM_EDITOR_PAGES = "editorPages";
    public static final String MODS_CUSTOM_EDITOR_PAGES_COPY_METADATA = "editorPagesCopyMetadata";
    public static final String MODS_CUSTOM_CUSTOMJSONDATA = "jsonData";
    public static final String MODS_CUSTOM_CUSTOMXMLDATA = "xmlData";
    public static final String MODS_CUSTOM_IGNOREVALIDATION = "ignoreValidation";
    public static final String MODS_CUSTOM_STANDARD = "standard";
    public static final String MODS_CUSTOM_CATALOGID = "catalogId";
    public static final String MODS_CUSTOM_CREATE_OBJECT = "createObject";
    public static final String MODS_CUSTOM_VALIDATE_OBJECT = "validate";

    public static final String MODS_VALIDATE_OBJECT_PATH = "validate";

    public static final String TECHNICAL_CUSTOM_XMLDATA = "xmlData";
    public static final String TECHNICAL_CUSTOM_JSONDATA = "jsonData";
    public static final String TECHNICAL_CUSTOM_TYPE = "type";

    public static final String MODS_PAGE_RULES_APPLY_TO = "applyTo";
    public static final String MODS_PAGE_RULES_APPLY_TO_FIRST_PAGE = "applyToFirstPage";
    public static final String MODS_PAGE_RULES_NUMBER_PREFIX = "prefix";
    public static final String MODS_PAGE_RULES_NUMBER_SUFFIX = "suffix";
    public static final String MODS_PAGE_RULES_USE_BRACKETS = "useBrackets";
    public static final String MODS_PAGE_RULES_NUMBER_SEQUENCE_TYPE = "sequence";
    public static final String MODS_PAGE_RULES_NUMBER_START_NUMBER = "startNumber";
    public static final String MODS_PAGE_RULES_NUMBER_INCREMENT_NUMBER = "incrementNumber";
    public static final String MODS_PAGE_RULES_INDEX_START_NUMBER = "startIndex";
    public static final String MODS_PAGE_RULES_TYPE_PAGE = "pageType";
    public static final String MODS_PAGE_RULES_DOUBLE_COLUMNS = "doubleColumns";
    public static final String MODS_PAGE_RULES_PAGE_POSITION = "pagePosition";

    public static final String MODS_CUSTOM_FUNCTION_ADD_BRACKETS = "addBrackets";
    public static final String MODS_CUSTOM_FUNCTION_REMOVE_BRACKETS = "removeBrackets";

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
    public static final String STREAMPROFILE_SIZE = "size";
    public static final String STREAMPROFILE_HEIGHT = "height";
    public static final String STREAMPROFILE_WIDTH = "width";

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

    public static final String GENERATE_PDFA = "generatePdfA";
    public static final String GENERATE_ALTO_PATH = "generateAlto";
    public static final String VALIDATE_OBJECT_PATH = "validate";

    /** Resource /object/technicalMetadata */
    public static final String TECHNICALMETADATA_AES_PATH = "technicalMetadataAes";
    public static final String TECHNICALMETADATA_CODING_HISTORY_PATH = "technicalMetadataCodingHistory";
    public static final String TECHNICALMETADATA_PREMIS_PATH = "technicalPremis";
    public static final String TECHNICALMETADATA_XML_AES_PATH = "technicalMetadataAesXml";
    public static final String TECHNICALMETADATA_XML_CODING_HISTORY_PATH = "technicalMetadataXmlCodingHistory";
    public static final String TECHNICALMETADATA_XML_PREMIS_PATH = "technicalMetadataXmlPremis";

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
    public static final String ATM_ITEM_DONATOR = "donator";
    public static final String ATM_ITEM_ARCHIVAL_COPIES = "archivalCopies";
    public static final String ATM_ITEM_FILENAME = "filename";
    public static final String ATM_ITEM_MODEL = DIGITALOBJECT_MODEL;
    public static final String ATM_ITEM_MODIFIED = MEMBERS_ITEM_MODIFIED;
    public static final String ATM_ITEM_OWNER = MEMBERS_ITEM_OWNER;
    public static final String ATM_ITEM_PID = DIGITALOBJECT_PID;
    public static final String ATM_ITEM_STATE = MEMBERS_ITEM_STATE;
    public static final String ATM_ITEM_LOCKED = "locked";
    public static final String ATM_ITEM_LOCKED_BY = "lockedBy";
    public static final String ATM_ITEM_LOCKED_DATE = "lockedDate";


    /** Resource /object/thumb */
    public static final String URNNBN_PATH = "urnnbn";
    public static final String URNNBN_VALUE_TO_DEACTIVATE = "valueToDeactivate";
    public static final String URNNBN_INVALIDATE_LOCAL_PATH = "invalidateLocal";
    public static final String URNNBN_INVALIDATE_REMOTE_PATH = "invalidateRemote";
    public static final String URNNBN_CREATE_SUCCESSOR_PATH = "createSuccessor";
    public static final String URNNBN_REGISTER_AGAIN_PATH = "registerAgain";
    public static final String URNNBN_UPDATE_IDENTIFIER_PATH = "updateIdentifier";
    public static final String URNNBN_HIERARCHY = "hierarchy";
    public static final String URNNBN_RESOLVER = "resolverId";
    public static final String URNNBN_IDENTIFIER = "identifier";
    public static final String URNNBN_OPERATION = "operation";

    public static final String REINDEX_PATH = "reindexObjects";
    public static final String UPDATE_ALL_OBJECTS_PATH = "updateAllObjectsObjects";

    public static final String URNNBN_ITEM_URNNBN = "urnnbn";
    public static final String URNNBN_ITEM_MESSAGE = "message";
    public static final String URNNBN_ITEM_STATUSTYPE = "statusType";
    public static final String URNNBN_ITEM_WARNING = "warning";
    public static final String URNNBN_ITEM_LOG = "log";
    public static final String WORKFLOW_JOB_ID = "wfJobId";

    public static final String COPYOBJECT_PATH = "copyObject";
    public static final String LOCK_OBJECT_PATH = "lockObject";
    public static final String UNLOCK_OBJECT_PATH = "unlockObject";

    public static final String GENERATE_JP2_PATH = "generateJp2";
    public static final String GENERATE_TYPE = "generateType";

    public static final String CHANGE_PAGE_TO_NDK_PAGE = "changePageToNdkPage";
    public static final String CHANGE_STT_PAGE_TO_NDK_PAGE = "changeSttPageToNdkPage";
    public static final String CHANGE_NDK_PAGE_TO_PAGE = "changeNdkPageToPage";
    public static final String CHANGE_NDK_PAGE_TO_STT_PAGE = "changeNdkPageToSttPage";
    public static final String CHANGE_PAGE_TO_STT_PAGE = "changePageToSttPage";
    public static final String CHANGE_STT_PAGE_TO_PAGE = "changeSttPageToPage";
    public static final String CHANGE_CLIPPINGS_VOLUME_TO_NDK_MONOGRAPH_VOLUME = "changeClippingsVolumeToNdkMonographVolume";
    public static final String CHANGE_NDK_MONOGRAPH_VOLUME_TO_CLIPPINGS_VOLUME = "changeNdkMonographVolumeToClippingsVolume";
    public static final String CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_TITLE = "changeNdkMonographVolumeToNdkMonographTitle";
    public static final String CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_UNIT = "changeNdkMonographVolumeToNdkMonographUnit";
    public static final String CHANGE_CLIPPINGS_TITLE_TO_NDK_MONOGRAPH_TITLE = "changeClippingsTitleToNdkMonographTitle";
    public static final String CHANGE_NDK_MONOGRAPH_TITLE_TO_CLIPPINGS_TITLE = "changeNdkMonographTitleToClippingsTitle";
    public static final String CHANGE_NDK_MONOGRAPH_TITLE_TO_NDK_MONOGRAPH_VOLUME = "changeNdkMonographTitleToNdkMonographVolume";
    public static final String CHANGE_NDK_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME = "changeNdkMonographUnitToNdkMonographVolume";
    public static final String CHANGE_K4_PERIODICAL_TO_NDK_PERIODICAL = "changeK4PeriodicalToNdkPeriodical";
    public static final String CHANGE_K4_PERIODICAL_VOLUME_TO_NDK_PERIODICAL_VOLUME = "changeK4PeriodicalVolumeToNdkPeriodicalVolume";
    public static final String CHANGE_K4_PERIODICAL_ISSUE_TO_NDK_PERIODICAL_ISSUE = "changeK4PeriodicalIssueToNdkPeriodicalIssue";
    public static final String CHANGE_K4_MONOGRAPH_TO_NDK_MONOGRAPH_VOLUME = "changeK4MonographToNdkMonographVolume";
    public static final String CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME = "changeK4MonographUnitToNdkMonographVolume";
    public static final String CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_UNIT = "changeK4MonographUnitToNdkMonographUnit";
    public static final String CHANGE_NDK_CHAPTER_TO_STT_CHAPTER = "changeNdkChapterToOldPrintChapter";
    public static final String CHANGE_STT_CHAPTER_TO_NDK_CHAPTER = "changeOldPrintChapterToNdkChapter";
    public static final String CHANGE_NDK_PICTURE_TO_STT_GRAPHIC = "changeNdkPictureToOldPrintGraphic";
    public static final String CHANGE_STT_GRAPHIC_TO_NDK_PICTURE = "changeOldPrintGraphicToNdkPicture";
    public static final String CHANGE_NDK_CARTOGRAPHIC_TO_STT_CARTOGRAPHIC = "changeNdkCartographicToOldPrintCartographic";
    public static final String CHANGE_STT_CARTOGRAPHIC_TO_NDK_CARTOGRAPHIC = "changeOldPrintCartographicToNdkCartographic";
    public static final String CHANGE_NDK_MONOGRAPH_TO_STT_MONOGRAPH = "changeNdkMonographVolumeToOldPrintMonographVolume";
    public static final String CHANGE_STT_MONOGRAPH_TO_NDK_MONOGRAPH = "changeOldPrintMonographVolumeToNdkMonographVolume";
    public static final String CHANGE_NDK_MUSICSHEET_TO_STT_MUSICSHEET = "changeNdkMusicSheetToOldPrintMusicSheet";
    public static final String CHANGE_STT_MUSICSHEET_TO_NDK_MUSICSHEET = "changeOldPrintMusicSheetToNdkMusicSheet";
    public static final String CHANGE_NDK_SUPPLEMENT_TO_STT_SUPPLEMENT = "changeNdkMonographSupplementToOldPrintMonographSupplement";
    public static final String CHANGE_STT_SUPPLEMENT_TO_NDK_SUPPLEMENT = "changeOldPrintMonographSupplementToNdkMonographSupplement";
    public static final String CHANGE_STT_GRAPHIC_TO_STT_MONOGRAPH_VOLUME = "changeOldPrintGraphicToOldprintMonographVolume";
    public static final String CHANGE_STT_MONOGRAPH_TO_STT_GRAPHIC = "changeOldPrintMonographVolumeToOldPrintGraphic";
    public static final String CHANGE_STT_MONOGRAPH_TO_STT_MUSICSHEET = "changeOldPrintMonographVolumeToOldPrintMusicSheet";
    public static final String CHANGE_NDK_PERIODICAL_TO_NDK_EPERIODICAL = "changeNdkPeriodicalToNdkEPeriodical";
    public static final String CHANGE_NDK_PERIODICAL_VOLUME_TO_NDK_EPERIODICAL_VOLUME = "changeNdkPeriodicalVolumeToNdkEPeriodicalVolume";
    public static final String CHANGE_NDK_PERIODICAL_ISSUE_TO_NDK_EPERIODICAL_ISSUE = "changeNdkPeriodicalIssueToNdkEPeriodicalIssue";
    public static final String CHANGE_NDK_PERIODICAL_SUPPLEMENT_TO_NDK_EPERIODICAL_SUPPLEMENT = "changeNdkPeriodicalSupplementToNdkEPeriodicalSupplement";
    public static final String CHANGE_NDK_ARTICLE_TO_NDK_EARTICLE = "changeNdkArticleToNdkEArticle";
    public static final String CHANGE_BDM_ARTICLE_TO_NDK_EARTICLE = "changeBdmArticleToNdkEArticle";
    public static final String CHANGE_NDK_EPERIODICAL_TO_NDK_PERIODICAL = "changeNdkEPeriodicalToNdkPeriodical";
    public static final String CHANGE_NDK_EPERIODICAL_VOLUME_TO_NDK_PERIODICAL_VOLUME = "changeNdkEPeriodicalVolumeToNdkPeriodicalVolume";
    public static final String CHANGE_NDK_EPERIODICAL_ISSUE_TO_NDK_PERIODICAL_ISSUE = "changeNdkEPeriodicalIssueToNdkPeriodicalIssue";
    public static final String CHANGE_NDK_EPERIODICAL_SUPPLEMENT_TO_NDK_PERIODICAL_SUPPLEMENT = "changeNdkEPeriodicalSupplementToNdkPeriodicalSupplement";
    public static final String CHANGE_NDK_EARTICLE_TO_NDK_ARTICLE = "changeNdkEArticleToNdkArticle";
    public static final String CHANGE_NDK_EARTICLE_TO_BDM_ARTICLE = "changeNdkEArticleToBdmArticle";

    public static final String UPDATE_NDK_ARTICLE = "updateNdkArticle";
    public static final String UPDATE_NDK_PAGE = "updateNdkPage";

    public static final String UPDATE_CATALOG_RECORD = "updateCatalogRecord";

    public static final String RESULT_ID = "processId";
    public static final String RESULT_MSG = "msg";

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
