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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.IconButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.DataArrivedEvent;
import com.smartgwt.client.widgets.form.fields.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.tree.Tree;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeGridField;
import com.smartgwt.client.widgets.tree.TreeNode;
import com.smartgwt.client.widgets.tree.events.FolderClickEvent;
import com.smartgwt.client.widgets.tree.events.FolderClickHandler;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.DeviceDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchDataSource.BatchRecord;
import cz.cas.lib.proarc.webapp.client.ds.ImportTreeDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ImportTreeDataSource.ImportRecord;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi.ProfileGroup;
import java.util.logging.Logger;

/**
 * UI allowing to select file system folder with digitalized content.
 *
 * @author Jan Pokorsky
 */
public final class ImportSourceChooser extends VLayout implements Refreshable {

    private static final Logger LOG = Logger.getLogger(ImportSourceChooser.class.getName());

    private final DataSource dataSource = ImportTreeDataSource.getInstance();
//    private final DataSource metaModelSource = MetaModelDataSource.getInstance();
    private final TreeGrid treeGrid;
    private final DynamicForm optionsForm;
    private final Label lblCurrSelection;
    private ImportSourceChooserHandler viewHandler;
    private final ClientMessages i18n;
    private IconButton loadButton;

    public ImportSourceChooser(ClientMessages i18n) {
        this.i18n = i18n;
        VLayout layout = this;
        setWidth100();
        setHeight100();
        VLayout innerLayout = new VLayout();
        innerLayout.setLayoutMargin(4);

        lblCurrSelection = new Label(i18n.ImportSourceChooser_NothingSelected_Title());
        lblCurrSelection.setWidth100();
        lblCurrSelection.setAutoFit(true);
        lblCurrSelection.setMargin(4);
        lblCurrSelection.setCanSelectText(true);

        treeGrid = new TreeGrid();
        treeGrid.setHeight100();
        treeGrid.setDataSource(dataSource);
        TreeGridField stateField = new TreeGridField(
                ImportTreeDataSource.FIELD_STATE,
                i18n.ImportSourceChooser_TreeHeaderImportState_Title());
        stateField.setWidth(100);
        treeGrid.setFields(
                new TreeGridField(ImportTreeDataSource.FIELD_NAME, i18n.ImportSourceChooser_TreeHeaderFolderName_Title()),
                stateField);
        treeGrid.setShowConnectors(true);
        treeGrid.setEmptyMessage(i18n.ImportSourceChooser_NoDataOnServer_Title());
        treeGrid.setAlternateRecordStyles(true);
        treeGrid.setSelectionType(SelectionStyle.SINGLE);
        ListGridPersistance treeGridPersistence = new ListGridPersistance("ImportSourceChooser.directoryList", treeGrid);
        treeGrid.setViewState(treeGridPersistence.getViewState());

        treeGrid.addFolderClickHandler(new FolderClickHandler() {

            @Override
            public void onFolderClick(FolderClickEvent event) {
                updateOnSelection();
                // issue 41: open node on single click
                TreeNode folder = event.getFolder();
                event.getViewer().getTree().openFolder(folder);
            }
        });

        ToolStrip toolbar = createToolbar();

        optionsForm = createOptionsForm();

        innerLayout.setMembers(optionsForm, lblCurrSelection, treeGrid);
        layout.setMembers(toolbar, innerLayout);
    }

    public void setViewHandler(ImportSourceChooserHandler handler) {
        this.viewHandler = handler;
    }

    public void edit() {
        optionsForm.resetValues();
        this.treeGrid.fetchData(null, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                treeGrid.selectRecord(0);
                treeGrid.focus();
                updateOnSelection();
            }
        });
    }

    public Record getImportSource() {
        return treeGrid.getSelectedRecord();
    }

    public String getImportProfile() {
        return optionsForm.getValueAsString(ImportBatchDataSource.FIELD_PROFILE_ID);
    }

    public Boolean getGenerateIndices() {
        return (Boolean) optionsForm.getValue(ImportBatchDataSource.FIELD_INDICES);
    }

    public String getDevice() {
        return optionsForm.getValueAsString(ImportBatchDataSource.FIELD_DEVICE);
    }

    public boolean validateOptions() {
        return optionsForm.validate();
    }

    @Override
    public void refresh() {
        treeGrid.invalidateCache();
    }

    /**
     * Refreshes selected node or the whole tree.
     */
    public void refreshSelectedNode() {
        Tree tree = treeGrid.getTree();
        TreeNode node = treeGrid.getSelectedRecord();
        if (node != null) {
            TreeNode parent = tree.getParent(node);
            if (parent != null) {
                tree.reloadChildren(parent);
                return ;
            }
        }
        treeGrid.invalidateCache();
    }

    /**
     * Updates folder status of the selected node in the data source cache.
     */
    public void updateCache(String status) {
        TreeNode node = treeGrid.getSelectedRecord();
        // issue 205
        node.setAttribute(ImportTreeDataSource.FIELD_STATE, status);
        dataSource.updateCaches(new DSResponse(null, DSOperationType.UPDATE, node));
    }

    private void updateOnSelection() {
        ListGridRecord selectedRecord = treeGrid.getSelectedRecord();
        String label = (selectedRecord == null)
                ? i18n.ImportSourceChooser_NothingSelected_Title()
                : selectedRecord.getAttribute(ImportTreeDataSource.FIELD_PATH);
        lblCurrSelection.setContents(label);
        ImportRecord importRecord = selectedRecord == null ? null : new ImportRecord(selectedRecord);
        loadButton.setDisabled(importRecord == null || !importRecord.isNew());
    }

    private ToolStrip createToolbar() {
        ToolStrip t = Actions.createToolStrip();
        RefreshAction refreshAction = new RefreshAction(i18n);
        t.addMember(Actions.asIconButton(refreshAction, this));

        Action loadAction = new AbstractAction(i18n.ImportWizard_ButtonLoadFolder_Title(),
                "[SKIN]/actions/add.png", null) {

            @Override
            public void performAction(ActionEvent event) {
                viewHandler.sourceSelected();
            }
        };
        loadButton = Actions.asIconButton(loadAction, this);
        t.addMember(loadButton);
        return t;
    }

    private DynamicForm createOptionsForm() {
        DynamicForm form = new DynamicForm();
        form.setNumCols(10);
        form.setGroupTitle(i18n.ImportSourceChooser_Options_Title());
        form.setIsGroup(true);
        form.setWrapItemTitles(false);

        final CheckboxItem cbiPageIndexes = new CheckboxItem(ImportBatchDataSource.FIELD_INDICES,
                i18n.ImportSourceChooser_OptionPageIndices_Title());
        cbiPageIndexes.setValue(true);

        final SelectItem selectScanner = createScannerSelection();
        final SelectItem selectProfile = ProfileChooser.createProfileSelection(ProfileGroup.IMPORTS, i18n);
        selectProfile.setName(ImportBatchDataSource.FIELD_PROFILE_ID);
        selectProfile.addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                String profile = getImportProfile();
                Criteria criteria = new Criteria();
                if (profile != null) {
                    criteria.addCriteria(ImportTreeDataSource.FIELD_PROFILE, profile);
                }
                treeGrid.setCriteria(criteria);
                boolean notArchive = !BatchRecord.isArchive(profile);
                selectScanner.setRequired(notArchive);
                if (notArchive) {
                    selectScanner.show();
                    cbiPageIndexes.show();
                } else {
                    selectScanner.hide();
                    cbiPageIndexes.hide();
                }
            }
        });

        form.setFields(selectProfile, selectScanner, cbiPageIndexes);
        return form;
    }

    private SelectItem createScannerSelection() {
        final SelectItem selectScanner = new SelectItem(ImportBatchDataSource.FIELD_DEVICE,
                i18n.ImportSourceChooser_OptionScanner_Title());
        DeviceDataSource.setOptionDataSource(selectScanner);
        selectScanner.setAllowEmptyValue(true);
        selectScanner.setEmptyDisplayValue(
                ClientUtils.format("<i>&lt;%s&gt;</i>", i18n.NewDigObject_OptionModel_EmptyValue_Title()));
        selectScanner.setRequired(true);
        selectScanner.setWidth(300);
        selectScanner.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                if (event.getStartRow() == 0) {
                    ResultSet data = event.getData();
                    int length = data.getLength();
                    if (length == 1) {
                        // issue 190: select in case of single device
                        Record device = data.get(0);
                        String deviceId = device.getAttribute(DeviceDataSource.FIELD_ID);
                        selectScanner.setValue(deviceId);
                        selectScanner.setDefaultValue(deviceId);
                    }
                }
            }
        });
        return selectScanner;
    }

    public interface ImportSourceChooserHandler {

        void sourceSelected();

    }
}
