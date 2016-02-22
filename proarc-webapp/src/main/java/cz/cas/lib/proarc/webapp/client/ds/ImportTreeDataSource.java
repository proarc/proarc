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
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import java.util.HashMap;
import java.util.Map;

public class ImportTreeDataSource extends RestDataSource {

    public static final String FIELD_NAME = "name";
    public static final String FIELD_PARENT = ImportResourceApi.IMPORT_FOLDER_PARENT_PARAM;
    public static final String FIELD_PATH = ImportResourceApi.IMPORT_FOLDER_PATH;
    public static final String FIELD_PROFILE = ImportResourceApi.IMPORT_BATCH_PROFILE;
    public static final String FIELD_STATE = ImportResourceApi.IMPORT_FOLDER_STATE;
    private static final String ID = "ImportTreeDataSource";
    private static final Map<String, String> states = new HashMap<String, String>();

    public static ImportTreeDataSource getInstance() {
        ImportTreeDataSource ds = (ImportTreeDataSource) DataSource.get(ID);
        return ds != null ? ds : new ImportTreeDataSource();
    }

    private ImportTreeDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);

        ClientMessages i18n = GWT.create(ClientMessages.class);

        DataSourceTextField path = new DataSourceTextField(FIELD_PATH);
        path.setPrimaryKey(true);
        path.setHidden(true);

        DataSourceTextField parent = new DataSourceTextField(FIELD_PARENT);
        parent.setForeignKey(FIELD_PATH);
        parent.setHidden(true);

        DataSourceTextField name = new DataSourceTextField(FIELD_NAME);

        DataSourceEnumField state = new DataSourceEnumField(FIELD_STATE);
        states.put(FolderState.IMPORTED, i18n.ImportBatchDataSource_State_LOADED());
        states.put(FolderState.NEW, "");
        states.put(FolderState.EMPTY, "");
        state.setValueMap(states);

        setFields(path, parent, name, state);
        setDataURL(RestConfig.URL_IMPORT_FOLDER);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));

    }

    @Override
    protected void transformResponse(DSResponse response, DSRequest request, Object data) {
        if (RestConfig.isStatusOk(response)) {
            for (Record record : response.getData()) {
                String path = record.getAttribute(FIELD_PATH);
                RegExp pathRegExp = RegExp.compile("(.*/)?(.*)/$");
                MatchResult mr = pathRegExp.exec(path);
                String parent = mr.getGroup(1);
                String name = mr.getGroup(2);
//                System.out.println("## ITRDS.path: " + path);
//                System.out.println("## ITRDS.parent: " + parent);
//                System.out.println("## ITRDS.name: " + name);

                record.setAttribute(FIELD_NAME, name);
                record.setAttribute(FIELD_PARENT, parent);
            }
        }
        super.transformResponse(response, request, data);
    }

    /**
     * Helper to minimize params send to update the record. Actually it uses just 'path'.
     */
    public static Record createUpdateRecord(Record rec, String model) {
        Record filtered = new Record();
        filtered.setAttribute(FIELD_PATH, rec.getAttribute(FIELD_PATH));
        filtered.setAttribute("model", model);
        return filtered;
    }

    public static final class ImportRecord {

        private final Record delegate;

        public ImportRecord(Record delegate) {
            this.delegate = delegate;
        }

        public String getPath() {
            return delegate.getAttribute(FIELD_PATH);
        }

        public boolean isImported() {
            return FolderState.IMPORTED.equals(delegate.getAttribute(FIELD_STATE));
        }

        public boolean isNew() {
            String state = delegate.getAttribute(FIELD_STATE);
            return state == null || FolderState.NEW.equals(state);
        }
    }

    public static final class FolderState {

        public static final String EMPTY = "EMPTY";
        public static final String IMPORTED = "IMPORTED";
        public static final String NEW = "NEW";

    }
}
