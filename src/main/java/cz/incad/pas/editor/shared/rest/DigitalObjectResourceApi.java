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

    public static final String DELETE_HIERARCHY_PARAM = "hierarchy";
    public static final String DELETE_PID_PARAM = "pid";
    public static final String DELETE_PURGE_PARAM = "purge";

    public static final String METAMODEL_PATH = "metamodel";
    public static final String METAMODEL_PID_PARAM = "pid";
    public static final String METAMODEL_ROOT_PARAM = "root";
    public static final String METAMODEL_LEAF_PARAM = "leaf";
    public static final String METAMODEL_DISPLAYNAME_PARAM = "displayName";
    public static final String METAMODEL_EDITORID_PARAM = "editorId";

    public static final String SEARCH_PATH = "search";
    public static final String SEARCH_OWNER_PARAM = "owner";
    public static final String SEARCH_START_ROW_PARAM = "_startRow";
    public static final String SEARCH_PHRASE_PARAM = "phrase";
    public static final String SEARCH_PID_PARAM = "pid";
    public static final String SEARCH_QUERY_IDENTIFIER_PARAM = "queryIdentifier";
    public static final String SEARCH_QUERY_LABEL_PARAM = "queryLabel";
    public static final String SEARCH_QUERY_MODEL_PARAM = "queryModel";
    public static final String SEARCH_QUERY_TITLE_PARAM = "queryTitle";
    public static final String SEARCH_TYPE_PARAM = "type";

    public enum SearchType {
        
        LAST_CREATED(SearchType.DEFAULT),
        LAST_MODIFIED("lastModified"),
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

}
