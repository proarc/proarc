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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.TextAreaWrap;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.MiniDateRangeItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchDataSource.BatchRecord;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditing.DigitalObjectEditorPlace;
import cz.cas.lib.proarc.webapp.client.widget.Dialog.DialogCloseHandler;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi.ProfileGroup;

/**
 * The widget to select a batch from import history. There should be 2 kinds of
 * batch items. The imported and not yet imported batches. The second one can be
 * resumed.
 *
 * @author Jan Pokorsky
 */
public final class ImportBatchChooser extends VLayout implements Refreshable {

    private ImportBatchChooserHandler handler;
    private final ListGrid lGridBatches;
    private final DynamicForm logForm;
    private final ClientMessages i18n;
    private final ActionSource actionSource;
    private Action resumeAction;
    private BatchRecord lastSelection;
    private String lastProfileSelection;

    public ImportBatchChooser(ClientMessages i18n) {
        this.i18n = i18n;
        this.actionSource = new ActionSource(this);

        setWidth100();
        setHeight100();

        lGridBatches = initBatchesListGrid();
        lGridBatches.setDataSource(ImportBatchDataSource.getInstance());
        lGridBatches.setShowResizeBar(true);
        lGridBatches.setResizeBarTarget("next");

        ToolStrip toolbar = createToolbar();

        logForm = createLogForm();

        VLayout innerLayout = new VLayout();
        innerLayout.setMargin(4);
        innerLayout.addMember(lGridBatches);
        innerLayout.addMember(logForm);

        setMembers(toolbar, innerLayout);
    }

    private ListGrid initBatchesListGrid() {
        ListGrid lg = new ListGrid();
        ListGridPersistance lgPersistance = new ListGridPersistance("ImportBatchChooserWidget.importList", lg);
        lg.setHeight100();
        lg.setSelectionType(SelectionStyle.SINGLE);
        lg.setCanReorderFields(false);
        lg.setShowFilterEditor(true);
        lg.setAllowFilterOperators(false);
        lg.setFilterOnKeypress(true);
        lg.setGenerateDoubleClickOnEnter(true);
        ListGridField lgfFolder = new ListGridField(ImportBatchDataSource.FIELD_DESCRIPTION,
                i18n.ImportBatchDataSource_FolderFieldTitle());
//        lgfFolder.setAutoFitWidth(false);
        lgfFolder.setCanFilter(true);
        lgfFolder.setFilterOnKeypress(false);
        TextItem lgfFolderFilter = new TextItem();
        lgfFolderFilter.setPrompt(i18n.ImportBatchChooser_FilterFolder_Hint());
        lgfFolderFilter.setHoverWidth(200);
        lgfFolder.setFilterEditorProperties(lgfFolderFilter);
        lgfFolder.setCanSort(false);
        ListGridField lgfDate = new ListGridField(ImportBatchDataSource.FIELD_CREATE,
                i18n.ImportBatchDataSource_ImportDateFieldTitle());
        lgfDate.setWidth(120);
        lgfDate.setAlign(Alignment.CENTER);
        lgfDate.setCanSort(true);
        MiniDateRangeItem dateRangeItem = new MiniDateRangeItem();
        dateRangeItem.setAttribute("allowRelativeDates", false);
        lgfDate.setFilterEditorProperties(dateRangeItem);
        lgfDate.setCanFilter(true);

        ListGridField lgfModified = new ListGridField(ImportBatchDataSource.FIELD_TIMESTAMP,
                i18n.ImportBatchDataSource_ImportModifiedFieldTitle());
        lgfModified.setWidth(120);
        lgfModified.setAlign(Alignment.CENTER);
        lgfModified.setCanSort(true);
        MiniDateRangeItem modifiedRangeItem = new MiniDateRangeItem();
        modifiedRangeItem.setAttribute("allowRelativeDates", false);
        lgfModified.setFilterEditorProperties(modifiedRangeItem);
        lgfModified.setCanFilter(true);

        ListGridField lgfImported = new ListGridField(ImportBatchDataSource.FIELD_STATE,
                i18n.ImportBatchDataSource_StateFieldTitle());
        lgfImported.setWidth(150);
        lgfImported.setPrompt(ClientUtils.format("<div style='width:250px;'>%s</div>",
                i18n.ImportBatchDataSource_StateFieldHint()));
        lgfImported.setCanFilter(true);
        lgfImported.setCanSort(true);
        ListGridField lgfUser = new ListGridField(ImportBatchDataSource.FIELD_USER_DISPLAYNAME,
                i18n.ImportBatchDataSource_UserFieldTitle());
        lgfUser.setWidth(150);
        lgfUser.setCanFilter(false);
        lgfUser.setCanSort(false);
        lg.setFields(lgfFolder, lgfDate, lgfModified, lgfImported, lgfUser);

        lg.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                updateOnSelection();
            }
        });
        Criteria filter = new Criteria();
        filter.addCriteria(lgfImported.getName(), ImportBatchDataSource.State.LOADED.toString());
        lg.setInitialCriteria(filter);
        lg.setSortField(lgfModified.getName());
        lg.setSortDirection(SortDirection.DESCENDING);
        lg.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                int startRow = event.getStartRow();
                if (startRow == 0) {
                    int select = 0;
                    if (lastSelection != null) {
                        RecordList rl = lGridBatches.getResultSet();
                        int findIndex = rl.findIndex(ImportBatchDataSource.FIELD_ID, lastSelection.getId());
                        select = findIndex < 0 ? select : findIndex;
                    }
                    lGridBatches.selectSingleRecord(select);
                    lGridBatches.scrollToRow(select);
                    lGridBatches.focus();
                }
            }
        });
        lg.addCellDoubleClickHandler(new CellDoubleClickHandler() {

            @Override
            public void onCellDoubleClick(CellDoubleClickEvent event) {
                ActionEvent evt = new ActionEvent(actionSource.getSource());
                if (resumeAction.accept(evt)) {
                    resumeAction.performAction(evt);
                }
            }
        });
        return lg;
    }

    @Override
    public void refresh() {
        bind();
    }

    public void bind() {
        lastProfileSelection = null;
        lGridBatches.invalidateCache();
        lGridBatches.fetchData(lGridBatches.getCriteria());
    }

    public void setHandler(ImportBatchChooserHandler handler) {
        this.handler = handler;
    }

    public BatchRecord getSelectedBatch() {
        Record r = getSelectedRecord();
        return r == null ? null : new BatchRecord(r);
    }

    /**
     * A configuration profile as a parameter to reset the batch.
     * @return the profile
     * @see #getSelectedBatch() 
     */
    public String getSelectedProfile() {
        return lastProfileSelection;
    }

    public Record getSelectedRecord() {
        return lGridBatches.getSelectedRecord();
    }

    private void showLog(BatchRecord batch) {
        if (batch != null) {
            logForm.editRecord(batch.getDelegate());
            String log = batch.getLog();
            logForm.setVisible(log != null && !log.isEmpty());
        } else {
            logForm.clearValues();
            logForm.hide();
        }
    }

    private void updateOnSelection() {
        actionSource.fireEvent();
        BatchRecord r = getSelectedBatch();
        lastSelection = r;
        showLog(r);
    }

    private ToolStrip createToolbar() {
        RefreshAction refreshAction = new RefreshAction(i18n);
        resumeAction = new ResumeAction(i18n.ImportBatchChooser_ActionResume_Title(),
                "[SKIN]/actions/next.png", i18n.ImportBatchChooser_ActionResume_Hint());
        Action resetAction = new ResetImportAction(i18n.ImportBatchChooser_ActionResetLoad_Title(),
                "[SKIN]/actions/undo.png",
                i18n.ImportBatchChooser_ActionResetLoad_Hint());
        Action parentAction = new ParentAction(i18n.ImportBatchChooser_ActionGotoParent_Title(),
                "[SKIN]/actions/edit.png",
                i18n.ImportBatchChooser_ActionGotoParent_Hint());

        ToolStrip t = Actions.createToolStrip();
        t.addMember(Actions.asIconButton(refreshAction, this));
        t.addMember(Actions.asIconButton(resetAction, actionSource));
        t.addMember(Actions.asIconButton(parentAction, actionSource));
        t.addMember(Actions.asIconButton(resumeAction, actionSource));

        return t;
    }

    private void resetImportFolder(final BatchRecord batch) {
        ImportBatchDataSource.State state = batch.getState();
        final BooleanCallback callback = new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value != null && value) {
                    handler.itemReset();
                }
            }
        };
        if (state == ImportBatchDataSource.State.INGESTING_FAILED) {
            callback.execute(true);
        } else {
            askForBatchReload(callback, batch);
        }
    }

    private void askForBatchReload(final BooleanCallback callback, BatchRecord batch) {
        final Dialog dialog = new Dialog(i18n.ImportBatchChooser_ActionResetLoad_Title());
        dialog.getDialogLabelContainer().setContents(i18n.ImportBatchChooser_ActionResetLoad_Ask_MSg());

        final DynamicForm dialogForm = new DynamicForm();
        final SelectItem profileSelect = ProfileChooser.createProfileSelection(ProfileGroup.IMPORTS, i18n);
        profileSelect.setValue(batch.getProfileId());
        dialogForm.setFields(profileSelect);
        dialog.getDialogContentContainer().setMembers(dialogForm);

        dialog.addYesButton(new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                if (dialogForm.validate()) {
                    lastProfileSelection = profileSelect.getValueAsString();
                    dialog.destroy();
                    callback.execute(true);
                }
            }
        });
        dialog.addNoButton(new DialogCloseHandler() {

            @Override
            public void onClose() {
                dialog.destroy();
                lastProfileSelection = null;
                callback.execute(false);
            }
        });
        dialog.setWidth(400);
        dialog.show();
    }

    private DynamicForm createLogForm() {
        DynamicForm form = new DynamicForm();
        form.setBrowserSpellCheck(false);
        form.setCanEdit(false);
        form.setWidth100();
        form.setHeight("40%");
        TextAreaItem textAreaItem = new TextAreaItem(ImportBatchDataSource.FIELD_LOG);
        textAreaItem.setColSpan("*");
        textAreaItem.setHeight("*");
        textAreaItem.setWrap(TextAreaWrap.OFF);
        textAreaItem.setShowTitle(false);
        textAreaItem.setWidth("*");
        textAreaItem.setCanEdit(false);
        form.setItems(textAreaItem);
        return form;
    }

    public interface ImportBatchChooserHandler {
        void itemSelected();
        void itemReset();
    }

    /**
     * Opens a loaded import.
     */
    private final class ResumeAction extends AbstractAction {

        public ResumeAction(String title, String icon, String tooltip) {
            super(title, icon, tooltip);
        }

        @Override
        public void performAction(ActionEvent event) {
            handler.itemSelected();
        }

        @Override
        public boolean accept(ActionEvent event) {
            BatchRecord record = getSelectedBatch();
            if (record != null && !record.isArchive()) {
                return record.getState() == ImportBatchDataSource.State.LOADED;
            }
            return false;
        }
    }

    /**
     * Resets a failed import.
     */
    private final class ResetImportAction extends AbstractAction {

        public ResetImportAction(String title, String icon, String tooltip) {
            super(title, icon, tooltip);
        }

        @Override
        public void performAction(ActionEvent event) {
            BatchRecord record = getSelectedBatch();
            if (record != null) {
                resetImportFolder(record);
            }
        }

        @Override
        public boolean accept(ActionEvent event) {
            BatchRecord record = getSelectedBatch();
            boolean accept = false;
            if (record != null) {
                switch(record.getState()) {
                    case INGESTING_FAILED:
                        setTitle(i18n.ImportBatchChooser_ActionResetIngest_Title());
                        setTooltip(i18n.ImportBatchChooser_ActionResetIngest_Hint());
                        accept = true;
                        break;
                    case LOADING_FAILED:
                    case LOADED:
                        setTitle(i18n.ImportBatchChooser_ActionResetLoad_Title());
                        setTooltip(i18n.ImportBatchChooser_ActionResetLoad_Hint());
                        accept = true;
                        break;
                }
            }
            return accept;
        }
    }

    /** Opens a selected parent object. */
    private final  class ParentAction extends AbstractAction {

        public ParentAction(String title, String icon, String tooltip) {
            super(title, icon, tooltip);
        }

        @Override
        public boolean accept(ActionEvent event) {
            BatchRecord batch = getSelectedBatch();
            return batch != null && batch.getParentPid() != null && !batch.isArchive();
        }

        @Override
        public void performAction(ActionEvent event) {
            BatchRecord batch = getSelectedBatch();
            if (batch != null && batch.getParentPid() != null) {
                Editor.getInstance().getEditorWorkFlow().getPlaceController()
                        .goTo(new DigitalObjectEditorPlace(DatastreamEditorType.CHILDREN, batch.getParentPid()));
            }
        }
    }

}
