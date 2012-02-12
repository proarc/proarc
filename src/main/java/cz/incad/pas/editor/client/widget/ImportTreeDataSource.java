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
package cz.incad.pas.editor.client.widget;

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import cz.incad.pas.editor.client.PasEditorMessages;
import java.util.HashMap;
import java.util.Map;

public class ImportTreeDataSource extends DataSource {
    
    private static final String ID = "ImportTreeDataSource";
    private static final Map<String, String> states = new HashMap<String, String>();
    
    public static ImportTreeDataSource getInstance() {
        ImportTreeDataSource ds = (ImportTreeDataSource) DataSource.get(ID);
        return ds != null ? ds : new ImportTreeDataSource();
    }

    private ImportTreeDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);

        PasEditorMessages i18nPas = GWT.create(PasEditorMessages.class);
        
        DataSourceTextField path = new DataSourceTextField("path");
        path.setPrimaryKey(true);
        path.setHidden(true);
        
        DataSourceTextField parent = new DataSourceTextField("parent");
        parent.setForeignKey("path");
        parent.setHidden(true);
        
        DataSourceTextField name = new DataSourceTextField("name");
        
        DataSourceEnumField state = new DataSourceEnumField("state");
        states.put("IMPORTED", i18nPas.ImportSourceChooser_StateImported_Title());
        states.put("NEW", "");
        states.put("RUNNING", i18nPas.ImportSourceChooser_StateRunning_Title());
        state.setValueMap(states);
        
        setFields(path, parent, name, state);
        setDataURL("ds/ImportSourceTree.json");
        setClientOnly(true);
    }
}
