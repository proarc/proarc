package cz.incad.pas.editor.client.widget;

import java.util.HashMap;
import java.util.Map;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceEnumField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;

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
        
        DataSourceTextField path = new DataSourceTextField("path");
        path.setPrimaryKey(true);
        path.setHidden(true);
        
        DataSourceTextField parent = new DataSourceTextField("parent");
        parent.setForeignKey("path");
        parent.setHidden(true);
        
        DataSourceTextField name = new DataSourceTextField("name");
        
        DataSourceEnumField state = new DataSourceEnumField("state", "State");
        states.put("IMPORTED", "Imported");
        states.put("NEW", "");
        states.put("RUNNING", "Running");
        state.setValueMap(states);
        
        setFields(path, parent, name, state);
        setDataURL("ds/ImportSourceTree.json");
        setClientOnly(true);
    }
}
