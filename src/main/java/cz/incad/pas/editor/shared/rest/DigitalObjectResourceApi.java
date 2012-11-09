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

    public enum SearchType {
        
        LAST_CREATED(SearchType.DEFAULT),
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
        NOTE, OCR, MEDIA, MODS, PARENT
    }

}
