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
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import java.util.LinkedHashMap;

public class ImportSourceChooser extends VLayout {

    private final DataSource dataSource = ImportTreeRestDataSource.getInstance();
    private final DataSource metaModelSource = MetaModelDataSource.getInstance();
    private final TreeGrid treeGrid;
    private final DynamicForm optionsForm;
    private final Label lblCurrSelection;
    private ImportSourceChooserHandler viewHandler;
    private final PasEditorMessages i18nPas;
    
    public ImportSourceChooser(PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
        VLayout layout = this;
        setWidth100();
        setHeight100();
        
        lblCurrSelection = new Label(i18nPas.ImportSourceChooser_NothingSelected_Title());
        lblCurrSelection.setWidth100();
        lblCurrSelection.setAutoFit(true);
        layout.addMember(lblCurrSelection);
        
        treeGrid = new TreeGrid();
        treeGrid.setHeight100();
        treeGrid.setDataSource(dataSource);
//        treeGrid.setAutoFetchData(true);
        treeGrid.setFields(
                new TreeGridField("name", i18nPas.ImportSourceChooser_TreeHeaderFolderName_Title()),
                new TreeGridField("state", i18nPas.ImportSourceChooser_TreeHeaderImportState_Title()));
        treeGrid.setShowConnectors(true);
        treeGrid.setEmptyMessage(i18nPas.ImportSourceChooser_NoDataOnServer_Title());
        treeGrid.setAlternateRecordStyles(true);
        treeGrid.setSelectionType(SelectionStyle.SINGLE);

        treeGrid.addFolderClickHandler(new FolderClickHandler() {

            @Override
            public void onFolderClick(FolderClickEvent event) {
                updateOnSelection();
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
//        SelectItem selectModel = new SelectItem("model", i18nPas.ImportSourceChooser_OptionImportModel_Title());
        CheckboxItem cbiPageIndexes = new CheckboxItem("genIndex",
                i18nPas.ImportSourceChooser_OptionPageIndices_Title());

        SelectItem selectScanner = new SelectItem("scanner", i18nPas.ImportSourceChooser_OptionScanner_Title());
        LinkedHashMap<String, String> scannerMap = new LinkedHashMap<String, String>();
        scannerMap.put("scanner:scanner1", "Zeutschel OS 7000");
        scannerMap.put("scanner:scanner2", "Zeutschel OS 8000");
        selectScanner.setValueMap(scannerMap);
        selectScanner.setDefaultValue("scanner:scanner1");
        
        optionsForm.setFields(cbiPageIndexes, selectScanner);
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
                treeGrid.selectRecord(0);
                treeGrid.focus();
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
                ? i18nPas.ImportSourceChooser_NothingSelected_Title()
                : selectedRecord.getAttribute(ImportTreeRestDataSource.FIELD_PATH);
        lblCurrSelection.setContents(label);
        viewHandler.sourceSelected();
    }

    public interface ImportSourceChooserHandler {

        void sourceSelected();
        
    }
}
