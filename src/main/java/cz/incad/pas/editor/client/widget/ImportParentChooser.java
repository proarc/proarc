package cz.incad.pas.editor.client.widget;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.SelectionAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.tree.Tree;
import com.smartgwt.client.widgets.tree.TreeGrid;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.RelationDataSource;

public class ImportParentChooser extends VLayout {

    private final PasEditorMessages i18nPas;
    private TreeGrid treeSelector;
    private ImportParentHandler handler;
    private final ListGrid foundGrid;
    
    public ImportParentChooser(PasEditorMessages i18nPas) {
        super(4);
        this.i18nPas = i18nPas;
        setLayoutMargin(4);
        setWidth100();
        setHeight100();
        TabSet tabSet = new TabSet();
        Tab tabLastUsed = new Tab(i18nPas.ImportParentChooser_TabLastUsed_Title()); // selected as parents in previous processings
        Tab tabLastCreated = new Tab(i18nPas.ImportParentChooser_TabLastCreated_Title());
        Tab tabSearch = new Tab(i18nPas.ImportParentChooser_TabSearch_Title());
        initTabSearch(tabSearch);
        tabSet.setTabs(tabLastUsed, tabLastCreated, tabSearch);
        // XXX implement tabs
        tabLastCreated.setDisabled(true);
        tabSearch.setDisabled(true);
        
        foundGrid = new ListGrid();
//        foundGrid.setCanDragRecordsOut(true);
//        foundGrid.setDragDataAction(DragDataAction.COPY);
        foundGrid.setSelectionType(SelectionStyle.SINGLE);
//        foundGrid.setSelectionAppearance(SelectionAppearance.CHECKBOX);
        
        DataSource ds = new DataSource("ds/FedoraObjectsLastUsed.js");
        ds.setFields(
                new DataSourceTextField("pid", "PID"),
                new DataSourceDateField("created", "Created"),
                new DataSourceTextField("user", "User"),
                new DataSourceTextField("type", "Model")
                );
        ds.setClientOnly(true);
        foundGrid.setDataSource(ds);
//        foundGrid.setAutoFetchData(true);
        foundGrid.setUseAllDataSourceFields(true);
        foundGrid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                ListGridRecord selectedRecord = foundGrid.getSelectedRecord();
                if (selectedRecord != null) {
                    String pid = selectedRecord.getAttribute(RelationDataSource.FIELD_PID);
                    treeSelector.fetchData(new Criteria(RelationDataSource.FIELD_ROOT, pid), new DSCallback() {

                        @Override
                        public void execute(DSResponse response, Object rawData, DSRequest request) {
                            treeSelector.selectRecord(0);
                        }
                    });
                } else {
                    treeSelector.fetchData(new Criteria(RelationDataSource.FIELD_ROOT, "dummy root to clear current data"));
//                    treeSelector.setData((Record[]) null);
                }
            }
        });
        
        final ListGrid selectedGrid = new ListGrid();
        selectedGrid.setCanAcceptDroppedRecords(true);
        selectedGrid.setFields(new ListGridField("pid"));
//        DataSource ds2 = new DataSource();
//        ds2.setInheritsFrom(ds2);
//        selectedGrid.setDataSource(ds2);
        selectedGrid.setUseAllDataSourceFields(true);

        treeSelector = createTreeSelector();
        treeSelector.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                handler.onParentSelectionUpdated();
            }
        });

        IButton btnAdd = new IButton("Add selected", new ClickHandler() {
            
            @Override
            public void onClick(ClickEvent event) {
                // TODO Auto-generated method stub
                ListGridRecord[] selectedRecords = foundGrid.getSelectedRecords();
                selectedGrid.transferSelectedData(foundGrid);
                foundGrid.deselectAllRecords();
                for (ListGridRecord rec : selectedRecords) {
                    rec.setEnabled(false);
                }
                foundGrid.refreshFields();
                
//                selectedGrid.setData(duplicates);
            }
        });
        IButton btnRemove = new IButton("Remove selected");
        
        tabLastUsed.setPane(foundGrid);
        addMember(tabSet);
//        addMember(btnAdd);
        addMember(treeSelector);
//        addMember(selectedGrid);
//        addMember(btnRemove);
    }

    public void setHandler(ImportParentHandler handler) {
        this.handler = handler;
    }

    public void setDataSource() {
        foundGrid.fetchData(null, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                foundGrid.selectRecord(0);
            }
        });
    }

    public Record getSelectedParent() {
        return treeSelector.getSelectedRecord();
    }

    private void initTabSearch(Tab tabSearch) {
        DynamicForm df = new DynamicForm();
        ComboBoxItem cbi = new ComboBoxItem("search", "Search");
        ListGrid lg = new ListGrid();
        cbi.setPickListProperties(lg);
        
        df.setFields(cbi);
        tabSearch.setPane(df);
    }

    private TreeGrid createTreeSelector() {
        TreeGrid treeGrid = new TreeGrid();
        // pid
        // display name
        treeGrid.setDataSource(RelationDataSource.getInstance());
        treeGrid.setUseAllDataSourceFields(true);
        treeGrid.setShowConnectors(true);
        treeGrid.setEmptyMessage(i18nPas.ImportParentChooser_EmptySelection_Title());
        treeGrid.setAlternateRecordStyles(true);
        treeGrid.setSelectionType(SelectionStyle.SINGLE);
//        treeGrid.setAutoFetchData(true);
        return treeGrid;
    }

    public interface ImportParentHandler {
        void onParentSelectionUpdated();
    }

}
