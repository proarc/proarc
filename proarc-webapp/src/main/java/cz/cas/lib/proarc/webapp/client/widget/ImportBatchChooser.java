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
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.MiniDateRangeItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
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
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchDataSource.BatchRecord;

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
        lg.setHeight100();
        lg.setSelectionType(SelectionStyle.SINGLE);
        lg.setCanReorderFields(false);
        lg.setShowFilterEditor(true);
        lg.setFilterOnKeypress(true);
        lg.setGenerateDoubleClickOnEnter(true);
        ListGridField lgfFolder = new ListGridField(ImportBatchDataSource.FIELD_DESCRIPTION,
                i18n.ImportBatchDataSource_FolderFieldTitle());
//        lgfFolder.setAutoFitWidth(false);
        lgfFolder.setCanFilter(false);
        lgfFolder.setCanSort(false);
        ListGridField lgfDate = new ListGridField(ImportBatchDataSource.FIELD_CREATE,
                i18n.ImportBatchDataSource_ImportDateFieldTitle());
        lgfDate.setWidth(120);
        lgfDate.setAlign(Alignment.CENTER);
        lgfDate.setCanSort(true);
        MiniDateRangeItem dateRangeItem = new MiniDateRangeItem();
        dateRangeItem.setAttribute("allowRelativeDates", false);
        lgfDate.setFilterEditorType(dateRangeItem);
        lgfDate.setCanFilter(true);
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
        lg.setFields(lgfFolder, lgfDate, lgfImported, lgfUser);

        lg.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                updateOnSelection();
            }
        });
        Criteria filter = new Criteria();
        filter.addCriteria(lgfImported.getName(), ImportBatchDataSource.State.LOADED.toString());
        lg.setInitialCriteria(filter);
        lg.setSortField(lgfDate.getName());
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
        ToolStrip t = Actions.createToolStrip();
        RefreshAction refreshAction = new RefreshAction(i18n);
        t.addMember(Actions.asIconButton(refreshAction, this));

        resumeAction = new AbstractAction(i18n.ImportBatchChooser_ActionResume_Title(),
                "[SKIN]/actions/next.png", i18n.ImportBatchChooser_ActionResume_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                handler.itemSelected();
            }

            @Override
            public boolean accept(ActionEvent event) {
                BatchRecord record = getSelectedBatch();
                if (record != null) {
                    return record.getState() == ImportBatchDataSource.State.LOADED;
                }
                return false;
            }
        };

        Action resetAction = new AbstractAction(i18n.ImportBatchChooser_ActionReset_Title(),
                "[SKIN]/actions/undo.png",
                i18n.ImportBatchChooser_ActionResetLoad_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                BatchRecord record = getSelectedBatch();
                if (record != null) {
                    resetImportFolder(record.getState());
                }
            }

            @Override
            public boolean accept(ActionEvent event) {
                BatchRecord record = getSelectedBatch();
                boolean accept = false;
                if (record != null) {
                    switch(record.getState()) {
                        case INGESTING_FAILED:
                            setTooltip(i18n.ImportBatchChooser_ActionResetIngest_Hint());
                            accept = true;
                            break;
                        case LOADING_FAILED:
                        case LOADED:
                            setTooltip(i18n.ImportBatchChooser_ActionResetLoad_Hint());
                            accept = true;
                            break;
                    }
                }
                return accept;
            }

        };

        t.addMember(Actions.asIconButton(resetAction, actionSource));
        t.addMember(Actions.asIconButton(resumeAction, actionSource));

        return t;
    }

    private void resetImportFolder(ImportBatchDataSource.State state) {
        BooleanCallback callback = new BooleanCallback() {

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
            SC.ask(i18n.ImportBatchChooser_ActionReset_Title(),
                    i18n.ImportBatchChooser_ActionReset_Ask_MSg(), callback);
        }
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

}
