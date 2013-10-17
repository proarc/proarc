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
package cz.incad.pas.editor.shared.rest;

/**
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectResourceApi {

    // resource /object
    public static final String PATH = "object";

    public static final String DIGITALOBJECT_PID = "pid";
    public static final String DIGITALOBJECT_MODEL = "model";
    public static final String BATCHID_PARAM = "batchId";
    public static final String TIMESTAMP_PARAM = "timestamp";

    public static final String NEWOBJECT_MODS_PARAM = "mods";

    public static final String DELETE_HIERARCHY_PARAM = "hierarchy";
    public static final String DELETE_PID_PARAM = DIGITALOBJECT_PID;
    public static final String DELETE_PURGE_PARAM = "purge";

    public static final String SEARCH_PATH = "search";
    public static final String SEARCH_OWNER_PARAM = "owner";
    public static final String SEARCH_START_ROW_PARAM = "_startRow";
    public static final String SEARCH_PHRASE_PARAM = "phrase";
    public static final String SEARCH_PID_PARAM = "pid";
    public static final String SEARCH_BATCHID_PARAM = "batchId";
    public static final String SEARCH_QUERY_IDENTIFIER_PARAM = "queryIdentifier";
    public static final String SEARCH_QUERY_LABEL_PARAM = "queryLabel";
    public static final String SEARCH_QUERY_MODEL_PARAM = "queryModel";
    public static final String SEARCH_QUERY_TITLE_PARAM = "queryTitle";
    public static final String SEARCH_TYPE_PARAM = "type";
    /** XXX workaround to fix GWT 2.5 compiler bug related to the use of enum's static
     * field in enum's constant declaration.
     */
    private static final String SEARCH_TYPE_PARAM_DEFAULT = SearchType.DEFAULT;

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
    public static final String MEMBERS_ITEM_MODEL = "model";
    public static final String MEMBERS_ITEM_OWNER = "owner";
    public static final String MEMBERS_ITEM_LABEL = "label";
    public static final String MEMBERS_ITEM_STATE = "state";
    public static final String MEMBERS_ITEM_CREATED = "created";
    public static final String MEMBERS_ITEM_MODIFIED = "modified";
    public static final String MEMBERS_ITEM_PARENT = "parent";

    public static final String MEMBERS_ROOT_PARAM = "root";

    // resource /object/metamodel
    public static final String METAMODEL_PATH = "metamodel";
    public static final String METAMODEL_PID_PARAM = "pid";
    public static final String METAMODEL_ROOT_PARAM = "root";
    public static final String METAMODEL_LEAF_PARAM = "leaf";
    public static final String METAMODEL_DISPLAYNAME_PARAM = "displayName";
    public static final String METAMODEL_MODSCUSTOMEDITORID_PARAM = "editorId";
    /**
     * @see DatastreamEditorType
     */
    public static final String METAMODEL_DATASTREAMEDITOR_PARAM = "dataStreamEditor";

    // resource /object/mods
    public static final String MODS_PATH = "mods";
    
    // resource /object/mods/custom
    public static final String MODS_CUSTOM_PATH = "custom";
    public static final String MODS_CUSTOM_EDITORID = "editorId";
    public static final String MODS_CUSTOM_CUSTOMJSONDATA = "customJsonData";

    // CustomMods
    public static final String CUSTOMMODS_ELEMENT = "mods";

    // resource /object/mods/plain
    public static final String MODS_PLAIN_PATH = "plain";

    // StringRecord
    public static final String STRINGRECORD_ELEMENT = "record";
    public static final String STRINGRECORD_CONTENT = "content";

    /** Resource /object/dissemination */
    public static final String DISSEMINATION_PATH = "dissemination";
    public static final String DISSEMINATION_DATASTREAM = "datastream";

    /** Resource /object/full */
    public static final String FULL_PATH = "full";

    /** Resource /object/ocr */
    public static final String OCR_PATH = "ocr";

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
    public static final String ATM_ITEM_FILENAME = "filename";
    public static final String ATM_ITEM_MODEL = DIGITALOBJECT_MODEL;
    public static final String ATM_ITEM_MODIFIED = MEMBERS_ITEM_MODIFIED;
    public static final String ATM_ITEM_OWNER = MEMBERS_ITEM_OWNER;
    public static final String ATM_ITEM_PID = DIGITALOBJECT_PID;
    public static final String ATM_ITEM_STATE = MEMBERS_ITEM_STATE;

    public enum SearchType {
        
        LAST_CREATED(SEARCH_TYPE_PARAM_DEFAULT),
        LAST_MODIFIED("lastModified"),
        /** Search object's parents. Accepted parameters: pid | batchId */
        PARENT("parent"),
        PIDS("pids"),
        PHRASE("phrase"),
        QUERY("query");
        
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

    /**
     * Supported datastream UI editors.
     */
    public enum DatastreamEditorType {
        NOTE, OCR, MEDIA, MODS, PARENT, CHILDREN, ATM
    }

}
