package cz.incad.pas.editor.client.widget;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeGridField;
import com.smartgwt.client.widgets.tree.events.FolderClickEvent;
import com.smartgwt.client.widgets.tree.events.FolderClickHandler;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import java.util.LinkedHashMap;

public class ImportSourceChooser extends VLayout {

    private static final String currentSelection_NothingSelected = "Nothing selected.";
    
    private final DataSource dataSource = ImportTreeRestDataSource.getInstance();
    private final DataSource metaModelSource = MetaModelDataSource.getInstance();
    private final TreeGrid treeGrid;
    private final DynamicForm optionsForm;
    private final Label lblCurrSelection;
    private ImportSourceChooserHandler viewHandler;
    
    public ImportSourceChooser() {
        VLayout layout = this;
        setWidth100();
        setHeight100();
        
        lblCurrSelection = new Label(currentSelection_NothingSelected);
        lblCurrSelection.setWidth100();
        lblCurrSelection.setAutoFit(true);
        layout.addMember(lblCurrSelection);
        
        treeGrid = new TreeGrid();
        treeGrid.setHeight100();
        treeGrid.setDataSource(dataSource);
//        treeGrid.setAutoFetchData(true);
        treeGrid.setFields(new TreeGridField("name"), new TreeGridField("state"));
        treeGrid.setShowConnectors(true);
        treeGrid.setEmptyMessage("Please provide import data on server.");
        treeGrid.setAlternateRecordStyles(true);
        treeGrid.setSelectionType(SelectionStyle.SINGLE);

        treeGrid.addFolderClickHandler(new FolderClickHandler() {

            @Override
            public void onFolderClick(FolderClickEvent event) {
                updateOnSelection();
                viewHandler.sourceSelected();
            }
        });

        layout.addMember(treeGrid);

//        ListGrid testList = new ListGrid();
//        testList.setWidth100();
//        testList.setHeight100();
//        testList.setAutoFetchData(true);
//        testList.setUseAllDataSourceFields(true);
//        testList.setDataSource(metaModelSource);
//
//        layout.addMember(testList);

        optionsForm = new DynamicForm();
        SelectItem selectModel = new SelectItem("model", "Import as");
        selectModel.setOptionDataSource(metaModelSource);
//        selectModel.setShowOptionsFromDataSource(true);
        selectModel.setValueField(MetaModelDataSource.FIELD_PID);
        selectModel.setDisplayField(MetaModelDataSource.FIELD_DISPLAY_NAME);
        selectModel.setAutoFetchData(true);

//        LinkedHashMap<String, String> modelMap = new LinkedHashMap<String, String>();
//        modelMap.put("model:issue", "Issue");
//        modelMap.put("mode:monograph", "Monograph");
//        modelMap.put("model:page", "Page");
//        modelMap.put("model:periodical", "Periodical");
//        modelMap.put("model:unit", "Unit");
//        modelMap.put("model:volume", "Volume");
//        selectModel.setValueMap(modelMap);
//        selectModel.setDefaultValue("model:page");
        
        CheckboxItem cbiPageIndexes = new CheckboxItem("genIndex", "Generate Page Indexes");

        SelectItem selectScanner = new SelectItem("scanner", "Used Scanner");
        LinkedHashMap<String, String> scannerMap = new LinkedHashMap<String, String>();
        scannerMap.put("scanner:scanner1", "Zeutschel OS 7000");
        scannerMap.put("scanner:scanner2", "Zeutschel OS 8000");
        selectScanner.setValueMap(scannerMap);
        selectScanner.setDefaultValue("scanner:scanner1");
        
        optionsForm.setFields(selectModel, cbiPageIndexes, selectScanner);
        layout.addMember(optionsForm);
    }

    public void setViewHandler(ImportSourceChooserHandler handler) {
//        form.clearValues();
        this.viewHandler = handler;
    }

    public void setFolderDataSource(DataSource ds) {
        this.treeGrid.fetchData(null, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                updateOnSelection();
            }
        });
    }

    public void setDigitalObjectModelDataSource(DataSource ds) {
        optionsForm.resetValues();
        optionsForm.setValue("model", "model:page");
    }

//    public void update() {
//        ListGridRecord record = treeGrid.getSelectedRecord();
//        Map changedValues = optionsForm.getChangedValues();
//        Map oldValues = optionsForm.getOldValues();
//        String valueAsString = optionsForm.getValueAsString("model");
//        System.out.println("## changedValues: " + changedValues);
//        System.out.println("## oldValues: " + oldValues);
//        System.out.println("## valueAsString: " + valueAsString);
//        dataSource.updateData(ImportTreeRestDataSource.createUpdateRecord(record, valueAsString));
//    }

    public Record getImportSource() {
        return treeGrid.getSelectedRecord();
    }

    public String getImportAsType() {
        String valueAsString = optionsForm.getValueAsString("model");
        return valueAsString;
    }

    private void updateOnSelection() {
        ListGridRecord selectedRecord = treeGrid.getSelectedRecord();
        String label = (selectedRecord == null)
                ? currentSelection_NothingSelected
                : selectedRecord.getAttribute(ImportTreeRestDataSource.FIELD_PATH);
        lblCurrSelection.setContents(label);
    }

    public interface ImportSourceChooserHandler {

        void sourceSelected();
        
    }
}
