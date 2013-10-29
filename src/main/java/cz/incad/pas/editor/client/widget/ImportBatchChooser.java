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
package cz.incad.pas.editor.client.widget;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.form.fields.MiniDateRangeItem;
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
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.action.AbstractAction;
import cz.incad.pas.editor.client.action.Action;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.Actions.ActionSource;
import cz.incad.pas.editor.client.action.RefreshAction;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.ds.ImportBatchDataSource;
import cz.incad.pas.editor.client.ds.ImportBatchDataSource.BatchRecord;

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
    private final ClientMessages i18n;
    private final ActionSource actionSource;
    private Action resumeAction;

    public ImportBatchChooser(ClientMessages i18n) {
        this.i18n = i18n;
        this.actionSource = new ActionSource(this);

        setWidth100();
        setHeight100();

        lGridBatches = initBatchesListGrid();
        lGridBatches.setMargin(4);
        lGridBatches.setDataSource(ImportBatchDataSource.getInstance());

        ToolStrip toolbar = createToolbar();

        addMember(toolbar);
        addMember(lGridBatches);
    }

    private ListGrid initBatchesListGrid() {
        ListGrid lg = new ListGrid();
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
        lgfDate.setAutoFitWidth(true);
        lgfDate.setCanSort(true);
        MiniDateRangeItem dateRangeItem = new MiniDateRangeItem();
        dateRangeItem.setAttribute("allowRelativeDates", false);
        lgfDate.setFilterEditorType(dateRangeItem);
        lgfDate.setCanFilter(true);
        ListGridField lgfImported = new ListGridField(ImportBatchDataSource.FIELD_STATE,
                i18n.ImportBatchDataSource_StateFieldTitle());
        lgfImported.setAutoFitWidth(true);
        lgfImported.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        lgfImported.setPrompt(ClientUtils.format("<div style='width:250px;'>%s</div>",
                i18n.ImportBatchDataSource_StateFieldHint()));
        lgfImported.setCanFilter(true);
        lgfImported.setCanSort(true);
        ListGridField lgfUser = new ListGridField(ImportBatchDataSource.FIELD_USER_DISPLAYNAME,
                i18n.ImportBatchDataSource_UserFieldTitle());
        lgfUser.setAutoFitWidth(true);
        lgfUser.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
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
                    lGridBatches.selectSingleRecord(0);
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

    public Record getSelectedBatch() {
        return lGridBatches.getSelectedRecord();
    }

    private void updateOnSelection() {
        actionSource.fireEvent();
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
                Record record = getSelectedBatch();
                if (record != null) {
                    return new BatchRecord(record).getState() == ImportBatchDataSource.State.LOADED;
                }
                return false;
            }
        };
        t.addMember(Actions.asIconButton(resumeAction, actionSource));

        return t;
    }

    public interface ImportBatchChooserHandler {
        void itemSelected();
    }

}
