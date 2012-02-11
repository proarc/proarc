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
package cz.incad.pas.editor.client.ds;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FetchMode;

/**
 *
 * @author Jan Pokorsky
 */
public class MetaModelDataSource extends DataSource {

    public static final String ID = "MetaModelDataSource";
    public static final String FIELD_PID = "pid";
    public static final String FIELD_DISPLAY_NAME = "displayName";
    public static final String FIELD_IS_ROOT = "root";
    public static final String FIELD_IS_LEAF = "leaf";
    public static final String FIELD_EDITOR = "editorId";

    public static final String EDITOR_PAGE = "cz.incad.pas.editor.client.widget.mods.PageForm";
    public static final String EDITOR_PERIODICAL = "cz.incad.pas.editor.client.widget.mods.PeriodicalForm";
    private static ResultSet resultSet;

    public MetaModelDataSource() {
        setID(ID);

//        setDataFormat(DSDataFormat.XML);
        setDataFormat(DSDataFormat.JSON);
//        setRecordXPath("/models/model");
        setRecordXPath("model");
        
        setDataURL(RestConfig.URL_DIGOBJECT_METAMODEL);

        DataSourceTextField pid = new DataSourceTextField(FIELD_PID);
        pid.setPrimaryKey(true);

        DataSourceTextField displayName = new DataSourceTextField(FIELD_DISPLAY_NAME);

        DataSourceBooleanField isRoot = new DataSourceBooleanField(FIELD_IS_ROOT);

        DataSourceBooleanField isLeaf = new DataSourceBooleanField(FIELD_IS_LEAF);

        DataSourceTextField editor = new DataSourceTextField(FIELD_EDITOR);

        setFields(pid, displayName, isRoot, isLeaf, editor);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static MetaModelDataSource getInstance() {
        MetaModelDataSource ds = (MetaModelDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new MetaModelDataSource();
//        ds.fetchData(null, new DSCallback() {
//
//            @Override
//            public void execute(DSResponse response, Object rawData, DSRequest request) {
//                String selectString = XMLTools.selectString(rawData, "//model/pid");
//                System.out.println("## result: " + selectString);
//            }
//        });
        return ds;
    }

    public static ResultSet getModels() {
        if (resultSet == null) {
            resultSet = new ResultSet(getInstance());
            resultSet.setFetchMode(FetchMode.LOCAL);
        }
        return resultSet;
    }

    public static final class MetaModelRecord {
        
        private final Record record;

        public MetaModelRecord(Record r) {
            this.record = r;
        }

        public String getId() {
            return record.getAttribute(FIELD_PID);
        }

        public boolean isRoot() {
            return record.getAttributeAsBoolean(FIELD_IS_ROOT);
        }

        public String getEditorId() {
            return record.getAttribute(FIELD_EDITOR);
        }
    }

}
