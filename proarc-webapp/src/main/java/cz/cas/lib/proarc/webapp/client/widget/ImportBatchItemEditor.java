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

import com.google.gwt.activity.shared.ActivityManager;
import com.google.gwt.core.client.Callback;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceController;
import com.google.web.bindery.event.shared.SimpleEventBus;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.EventHandler;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.BrowserEvent;
import com.smartgwt.client.widgets.events.DropEvent;
import com.smartgwt.client.widgets.events.DropHandler;
import com.smartgwt.client.widgets.events.KeyPressEvent;
import com.smartgwt.client.widgets.events.KeyPressHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.BodyKeyPressEvent;
import com.smartgwt.client.widgets.grid.events.BodyKeyPressHandler;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;
import com.smartgwt.client.widgets.grid.events.RecordDropEvent;
import com.smartgwt.client.widgets.grid.events.RecordDropHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.tile.TileGrid;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.viewer.DetailFormatter;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectCopyMetadataAction;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectCopyMetadataAction.CopySelector;
import cz.cas.lib.proarc.webapp.client.action.DeleteAction;
import cz.cas.lib.proarc.webapp.client.action.DeleteAction.RecordDeletable;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectFormValidateAction;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectFormValidateAction.ValidatableList;
import cz.cas.lib.proarc.webapp.client.action.FoxmlViewAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchDataSource.BatchRecord;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchItemDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource.RelationChangeEvent;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource.RelationChangeHandler;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadEvent;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadHandler;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditing.DigitalObjectEditorPlace;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditor;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectChildrenEditor.ChildActivities;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectChildrenEditor.ChildEditorDisplay;
import java.util.LinkedHashMap;
import java.util.logging.Logger;

/**
 * Prepares items of a batch import. It involves meta data, item order, ...
 *
 * @author Jan Pokorsky
 */
public final class ImportBatchItemEditor extends HLayout implements Selectable<Record>,
        RefreshAction.Refreshable, CopySelector {

    private static final Logger LOG = Logger.getLogger(ImportBatchItemEditor.class.getName());

    private final ClientMessages i18n;

    private final ListGrid batchItemGrid;
    private ListGridField fieldItemModel;
    private final TileGrid thumbViewer;
    private final MediaEditor digitalObjectPreview;
    private boolean selectThumbInProgress = false;
    private boolean selectListInProgress = false;
    private BatchRecord batchRecord;
    private FoxmlViewAction foxmlViewAction;
    private DeleteAction deleteAction;
    private SelectAction selectAllAction;
    private DigitalObjectCopyMetadataAction copyMetadataAction;
    private final PlaceController childPlaces;
    private final DigitalObjectEditor childEditor;
    private final ActionSource actionSource;
    private ReorderTask reorderTask = new ReorderTask();
    private Action resumeAction;
    private Handler handler;

    public ImportBatchItemEditor(ClientMessages i18n) {
        this.i18n = i18n;
        this.setHeight100();
        this.setWidth100();
        this.actionSource = new ActionSource(this);
        
        VLayout layout = new VLayout();
        layout.setShowResizeBar(true);
        layout.setResizeBarTarget("next");

        batchItemGrid = createItemList();
        layout.addMember(batchItemGrid);

        // child editors
        SimpleEventBus eventBus = new SimpleEventBus();
        childPlaces = new PlaceController(eventBus);
        childEditor = new DigitalObjectEditor(i18n, childPlaces, true);
        layout.addMember(initDigitalObjectEditor(childEditor, eventBus));

        HLayout editorThumbLayout = new HLayout();
        editorThumbLayout.setHeight100();
        editorThumbLayout.addMember(layout);

        thumbViewer = createThumbViewer();
        editorThumbLayout.addMember(thumbViewer);

        VLayout editorThumbToolbarLayout = new VLayout();
        editorThumbToolbarLayout.setShowResizeBar(true);
        editorThumbToolbarLayout.setResizeBarTarget("next");

        createActions();
        ToolStrip editorToolStrip = createEditorToolBar(actionSource);
        editorThumbToolbarLayout.addMember(editorToolStrip);
        editorThumbToolbarLayout.addMember(editorThumbLayout);

        addMember(editorThumbToolbarLayout);

        digitalObjectPreview = new MediaEditor(i18n);
        digitalObjectPreview.addBackgroundColorListeners(thumbViewer);
        ToolStrip previewToolbar = Actions.createToolStrip();
        previewToolbar.setMembers(digitalObjectPreview.getToolbarItems());
        VLayout previewLayout = new VLayout();
        previewLayout.setMembers(previewToolbar, digitalObjectPreview.getUI());
        previewLayout.setWidth("40%");
        previewLayout.setHeight100();
//        previewLayout.setShowResizeBar(true);
//        previewLayout.setResizeFrom("L");
        addMember(previewLayout);
        createEditorContextMenu(batchItemGrid.getContextMenu(), this);
        createEditorContextMenu(thumbViewer.getContextMenu(), this);
    }

    public void setHandler(Handler handler) {
        this.handler = handler;
    }

    public void onShow(BatchRecord batch) {
        this.batchRecord = batch;
        refresh();
    }

    public void onHide(BooleanCallback callback) {
        DigitalObjectCopyMetadataAction.resetSelection();
        callback.execute(true);
    }

    private Canvas initDigitalObjectEditor(DigitalObjectEditor childEditor, SimpleEventBus eventBus) {
        childEditor.setImportView(true);
        RelationDataSource relationDataSource = RelationDataSource.getInstance();
        relationDataSource.addRelationChangeHandler(new RelationChangeHandler() {

            @Override
            public void onRelationChange(RelationChangeEvent event) {
                if (batchItemGrid.isVisible()) {
                    updateCache();
                }
            }
        });
        ActivityManager activityManager = new ActivityManager(
                new ChildActivities(childEditor), eventBus);

        VLayout editorsLayout = new VLayout();
        editorsLayout.addStyleName("defaultBorder");
        activityManager.setDisplay(new ChildEditorDisplay(editorsLayout));
        return editorsLayout;
    }

    private ListGrid createItemList() {
        final ListGrid grid = new ListGrid() {

            @Override
            protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
                // do not replace with hilites as they do not support UI refresh
                if (DigitalObjectCopyMetadataAction.isSelectedCopyRecord(record)) {
                    return "color: #FF0000;";
                } else {
                    return super.getCellCSSText(record, rowNum, colNum);
                }
            }

        };
        grid.setShowResizeBar(true);
        grid.setSelectionType(SelectionStyle.MULTIPLE);
        grid.setCanSort(false);
        grid.setCanReorderRecords(true);
        // disable autofit as it has rendering problems
//        batchItemGrid.setAutoFitFieldWidths(true);
//        batchItemGrid.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        grid.setLeaveScrollbarGap(false);
        grid.setDataSource(ImportBatchItemDataSource.getInstance());

        fieldItemModel = new ListGridField(ImportBatchItemDataSource.FIELD_MODEL,
                i18n.ImportBatchItemEditor_ListHeaderModel_Title());
        fieldItemModel.setPrompt(i18n.ImportBatchItemEditor_ListHeaderModel_Hint());
        fieldItemModel.setOptionDataSource(MetaModelDataSource.getInstance());
        fieldItemModel.setValueField(MetaModelDataSource.FIELD_PID);
        fieldItemModel.setDisplayField(MetaModelDataSource.FIELD_DISPLAY_NAME);
        fieldItemModel.setAutoFetchDisplayMap(true);
        fieldItemModel.setHidden(true);

        ListGridField fieldPid = new ListGridField(ImportBatchItemDataSource.FIELD_PID,
                i18n.ImportBatchItemEditor_ListHeaderPID_Title());
        fieldPid.setPrompt(i18n.ImportBatchItemEditor_ListHeaderPID_Hint());
        fieldPid.setHidden(true);

        ListGridField fieldUser = new ListGridField(ImportBatchItemDataSource.FIELD_USER,
                i18n.ImportBatchItemEditor_ListHeaderUser_Title());
        fieldUser.setPrompt(i18n.ImportBatchItemEditor_ListHeaderUser_Hint());
        fieldUser.setHidden(true);

        ListGridField fieldFilename = new ListGridField(ImportBatchItemDataSource.FIELD_FILENAME,
                i18n.ImportBatchItemEditor_ListHeaderFilename_Title());
        fieldFilename.setPrompt(i18n.ImportBatchItemEditor_ListHeaderFilename_Hint());

        ListGridField fieldPageIndex = new ListGridField(ImportBatchItemDataSource.FIELD_PAGE_INDEX,
                i18n.ImportBatchItemEditor_ListHeaderPageIndex_Title());
        fieldPageIndex.setPrompt(i18n.ImportBatchItemEditor_ListHeaderPageIndex_Hint());
        fieldPageIndex.setHidden(true);

        ListGridField fieldPageNumber = new ListGridField(ImportBatchItemDataSource.FIELD_PAGE_NUMBER,
                i18n.ImportBatchItemEditor_ListHeaderPageNumber_Title());
        fieldPageNumber.setPrompt(i18n.ImportBatchItemEditor_ListHeaderPageNumber_Hint());

        ListGridField fieldPageType = new ListGridField(ImportBatchItemDataSource.FIELD_PAGE_TYPE,
                i18n.ImportBatchItemEditor_ListHeaderPageType_Title());
        fieldPageType.setPrompt(i18n.ImportBatchItemEditor_ListHeaderPageType_Hint());
        fieldPageType.setEmptyCellValue(ModsCustomDataSource.getPageTypes().get(ModsCustomDataSource.getDefaultPageType()));

        grid.setFields(fieldFilename, fieldPageNumber, fieldPageIndex, fieldPageType, fieldPid, fieldItemModel, fieldUser);
        grid.setContextMenu(Actions.createMenu());
        Actions.fixListGridContextMenu(grid);

        // issue 7: default BodyKeyPressHandler does not change row focus properly
        // in case of mixing mouse and keyboard navigation.
        // ListGrid.getSelectedRecord and ListGrid.getFocusRow() do not return same result!
        grid.addBodyKeyPressHandler(new BodyKeyPressHandler() {

            @Override
            public void onBodyKeyPress(BodyKeyPressEvent event) {
                selectAllAction.processEvent(event);
                if (event.isCancelled() || event.isCtrlKeyDown() || EventHandler.shiftKeyDown()) {
                    return ;
                }
                if ("Arrow_Down".equals(EventHandler.getKey())) {
                    int nextSelection = getNextSelection();
                    grid.selectSingleRecord(nextSelection);
                } else if ("Arrow_Up".equals(EventHandler.getKey())) {
                    Integer focusRow = grid.getFocusRow();
                    if (focusRow != null && focusRow > 0) {
                        grid.selectSingleRecord(focusRow - 1);
                    }
                }
            }
        });

        grid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                if (selectListInProgress) {
                    selectListInProgress = false;
                    return ;
                }
                ListGridRecord[] selectedRecords = grid.getSelectedRecords();
                thumbViewer.deselectAllRecords();
                if (selectedRecords != null && selectedRecords.length == 1) {
                    // select thumbnail just in case of the single selection
                    int tileIndex = getThumbIndex(selectedRecords[0]);
//                    selectThumbInProgress = true;
                    // use record index instead of ListGridRecord to work around a smartgwt bug
                    thumbViewer.selectRecord(tileIndex);
                    ClientUtils.scrollToTile(thumbViewer, tileIndex);
                } else if (selectedRecords != null && selectedRecords.length > 1) {
                    int[] indexes = new int[selectedRecords.length];
                    for (int i = 0; i < indexes.length; i++) {
                        indexes[i] = getThumbIndex(selectedRecords[i]);
                    }
                    thumbViewer.selectRecords(indexes);
                }
                selectBatchItem(true, selectedRecords);
            }

        });

        grid.addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                Record record = grid.anySelected() ? event.getRecord() : null;
                // always preview last clicked record
                previewItem(record);
            }
        });

        grid.addRecordDropHandler(new RecordDropHandler() {

            @Override
            public void onRecordDrop(RecordDropEvent event) {
                reorderTask.reorder(grid);
            }
        });
        return grid;
    }

    @Override
    public Record[] getSelection() {
        return batchItemGrid.anySelected()
                ? batchItemGrid.getSelectedRecords()
                : thumbViewer.getSelection();
    }

    @Override
    public void refresh() {
        // fetch models before digital objects
        MetaModelDataSource.getModels(true, new Callback<ResultSet, Void>() {

            @Override
            public void onFailure(Void reason) {
            }

            @Override
            public void onSuccess(ResultSet result) {
                refreshData();
            }
        });
    }

    @Override
    public void showCopySelection(Record[] records) {
        if (records == null) {
            return ;
        }
        RecordList copySelection = new RecordList(records);
        for (int i = batchItemGrid.getRecords().length - 1; i >= 0; i--) {
            Record item = batchItemGrid.getRecord(i);
            DigitalObject listItem = DigitalObject.create(item);
            Record select = copySelection.find(ImportBatchItemDataSource.FIELD_PID, listItem.getPid());
            boolean refresh = false;
            if (select != null) {
                if (!DigitalObjectCopyMetadataAction.isSelectedCopyRecord(item)) {
                    DigitalObjectCopyMetadataAction.selectCopyRecord(item);
                    refresh = true;
                }
            } else {
                if (DigitalObjectCopyMetadataAction.isSelectedCopyRecord(item)) {
                    DigitalObjectCopyMetadataAction.deselectCopyRecord(item);
                    refresh = true;
                }
            }
            if (refresh) {
                batchItemGrid.refreshRow(i);
            }
        }
    }

    private void refreshData() {
        DigitalObjectCopyMetadataAction.resetSelection();
        Criteria criteria = new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, batchRecord.getId());
        batchItemGrid.invalidateCache();
        thumbViewer.setData(new Record[0]);
        previewItem(null);

        batchItemGrid.fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    Record[] copyRecords = batchItemGrid.getDataSource().copyRecords(data);
                    thumbViewer.setData(copyRecords);

                    batchItemGrid.selectSingleRecord(0);
                    batchItemGrid.focus();
                    ValidatableList.clearRowErrors(batchItemGrid);
                }
            }
        });
    }

    /**
     * As there is no API to reorder ListGrid records, it reloads data and
     * tries to preserve selection of records.
     */
    private void syncListWithTilesOnReorder() {
        Record[] selection = thumbViewer.getSelection();
        final int[] selectionIndex = new int[selection.length];
        for (int i = 0; i < selection.length; i++) {
            selectionIndex[i] = thumbViewer.getRecordIndex(selection[i]);
        }
        Criteria criteria = new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, batchRecord.getId());
        DigitalObjectCopyMetadataAction.resetSelection();
        batchItemGrid.invalidateCache();
        batchItemGrid.fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    selectListInProgress = true;
                    batchItemGrid.selectRecords(selectionIndex);
                }
            }
        });
    }

    /**
     * Updates data source cache and attached data bounded components (ListGrid, TileGrid).
     * It should be called when child editor changes affects import item descriptions
     * or validation status.
     */
    private void updateCache() {
        final Record[] selections = getSelection();
        Criteria criteria = new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, batchRecord.getId());
        if (selections.length == 0) {
            return ;
        } else if (selections.length == 1) {
            // on single selection update only the involved row
            DigitalObject dobj = DigitalObject.create(selections[0]);
            criteria.addCriteria(ImportBatchItemDataSource.FIELD_PID, dobj.getPid());
        }
        final DataSource ds = batchItemGrid.getDataSource();
        ds.fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    if (selections.length == 1) {
                        request.setOperationType(DSOperationType.UPDATE);
                        ds.updateCaches(response, request);

                        Record record = response.getData()[0];
                        Record copyRecord = ds.copyRecord(record);
                        int thumbIndex = getThumbIndex(copyRecord);
                        thumbViewer.getRecordList().set(thumbIndex, copyRecord);

                        selectNextChildAndFocus(record);
                    } else {
                        request.setOperationType(DSOperationType.UPDATE);
                        ds.updateCaches(response, request);
                        
                        Record[] copyRecords = ds.copyRecords(response.getData());
                        thumbViewer.setData(copyRecords);
                    }
                    ValidatableList.clearRowErrors(batchItemGrid);
                    // refresh the copy selection as updated records are missing the copy attribute
                    showCopySelection(DigitalObjectCopyMetadataAction.getSelection());
                }
            }
        });
    }

    private void selectNextChildAndFocus(Record record) {
        int nextSelection = getNextSelection(record);
        if (nextSelection < 0) {
            return ;
        }
        final HandlerRegistration[] editorLoadHandler = new HandlerRegistration[1];
        editorLoadHandler[0] = childEditor.addEditorLoadHandler(new EditorLoadHandler() {

            @Override
            public void onEditorLoad(EditorLoadEvent evt) {
                editorLoadHandler[0].removeHandler();
                Scheduler.get().scheduleDeferred(new ScheduledCommand() {

                    @Override
                    public void execute() {
                        childEditor.focus();
                    }
                });
            }
        });
        batchItemGrid.selectSingleRecord(nextSelection);
        batchItemGrid.scrollToRow(nextSelection);
    }

    private TileGrid createThumbViewer() {
        final TileGridEnhanced thumbGrid = new TileGridEnhanced();
        thumbGrid.setBackgroundColor(DigitalObjectPreview.BACKGROUND_COLOR);
        thumbGrid.setWidth(150);
        thumbGrid.setHeight100();
        thumbGrid.setMinWidth(150);
        thumbGrid.setShowEdges(false);
        thumbGrid.setCanReorderTiles(true);
        thumbGrid.setWrapValues(true);
        thumbGrid.setSelectionType(SelectionStyle.MULTIPLE);
        // setTileProperties does not work; it replaces default renderer (smartgwt 2.5)
        //thumbGrid.setTileProperties(tileCanvas);
        // setDetailViewerProperties replaces default renderer and it is impossible to customize it not to show field titles (smartgwt 2.5)
        //thumbGrid.setDetailViewerProperties(thumbViewer);

        DetailViewerField dvfPageIndex = new DetailViewerField(ImportBatchItemDataSource.FIELD_PAGE_INDEX);
        final LinkedHashMap<String, String> pageTypes = ModsCustomDataSource.getPageTypes();
        dvfPageIndex.setDetailFormatter(new DetailFormatter() {

            @Override
            public String format(Object value, Record record, DetailViewerField field) {
                String number = record.getAttribute(ImportBatchItemDataSource.FIELD_PAGE_NUMBER);
                String type = record.getAttribute(ImportBatchItemDataSource.FIELD_PAGE_TYPE);
                type = (type != null) ? type : ModsCustomDataSource.getDefaultPageType();
                number = (number != null) ? number : "-";
                value = (value != null) ? value : "-";
                return ClientUtils.format("%s: %s<br>Index: %s", pageTypes.get(type), number, value);
            }
        });
        final DetailViewerField dvfThumbnail = new DetailViewerField(ImportBatchItemDataSource.FIELD_THUMBNAIL);
        dvfThumbnail.setImageURLPrefix(RestConfig.URL_DIGOBJECT_THUMBNAIL + "?");
        dvfThumbnail.setType("image");
        thumbGrid.setFields(dvfThumbnail, dvfPageIndex);
        // TileLayoutPolicy.FLOW does not work as expected
        // thumbGrid.setLayoutPolicy(TileLayoutPolicy.FLOW);
        thumbGrid.setTileHeight(128 + 8 + 12 * 2);
        thumbGrid.setTileWidth(120);
        thumbGrid.setContextMenu(Actions.createMenu());

        thumbGrid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                if (selectThumbInProgress) {
                    selectThumbInProgress = false;
                    return ;
                }
                Record[] selection = thumbViewer.getSelection();
                if (selection != null && selection.length == 1) {
//                    LOG.info("THUMB.onSelectionChanged.selection.state: " + event.getState() + ".attrs: " + Arrays.toString(selection[0].getAttributes()));
                    selectListInProgress = true;
                    int selectionIndex = batchItemGrid.getRecordIndex(selection[0]);
                    LOG.fine("thumb selects list: " + selectionIndex);
                    batchItemGrid.selectSingleRecord(selectionIndex);
                    batchItemGrid.scrollToRow(selectionIndex);
                    selectBatchItem(false, selection);
                } else if (selection != null && selection.length > 1) {
                    int[] indexes = new int[selection.length];
                    for (int i = 0; i < indexes.length; i++) {
                        indexes[i] = batchItemGrid.getRecordIndex(selection[i]);
                    }
                    selectListInProgress = true;
                    batchItemGrid.deselectAllRecords();
                    selectListInProgress = true;
                    batchItemGrid.selectRecords(indexes);
                    selectBatchItem(false, selection);
                }
                actionSource.fireEvent();
//                LOG.info("THUMB.onSelectionChanged.selection: " + Arrays.toString(selection));
            }
        });

        thumbGrid.addRecordClickHandler(new com.smartgwt.client.widgets.tile.events.RecordClickHandler() {

            @Override
            public void onRecordClick(com.smartgwt.client.widgets.tile.events.RecordClickEvent event) {
                // always preview last clicked record
                LOG.fine("TG.onRecordClick");
                previewItem(event.getRecord());
            }
        });

        thumbGrid.addKeyPressHandler(new KeyPressHandler() {

            @Override
            public void onKeyPress(KeyPressEvent event) {
                selectAllAction.processEvent(event);
            }
        });

        thumbGrid.addDropHandler(new DropHandler() {

            @Override
            public void onDrop(DropEvent event) {
                reorderTask.reorder(thumbGrid);
            }
        });

        return thumbGrid;
    }

    private int getThumbIndex(Record r) {
        int index = thumbViewer.getRecordList().findIndex(ImportBatchItemDataSource.FIELD_PID,
                r.getAttribute(ImportBatchItemDataSource.FIELD_PID));
        return index;
    }

    private void createActions() {
        foxmlViewAction = new FoxmlViewAction(i18n);
        deleteAction = new DeleteAction(
                new RecordDeletable(batchItemGrid.getDataSource(), i18n), i18n);
        selectAllAction = new SelectAction();
        resumeAction = new AbstractAction(
                i18n.ImportBatchItemEditor_ActionResume_Title(),
                "[SKIN]/actions/next.png",
                i18n.ImportBatchItemEditor_ActionResume_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                if (handler != null) {
                    handler.handleNextAction();
                }
            }
        };
        copyMetadataAction = new DigitalObjectCopyMetadataAction(i18n);
    }

    private ToolStrip createEditorToolBar(ActionSource actionSource) {
        ToolStrip toolbar = Actions.createToolStrip();
        toolbar.addMember(Actions.asIconButton(new RefreshAction(i18n), this));
        toolbar.addMember(Actions.asIconButton(selectAllAction, this));
        toolbar.addMember(Actions.asIconButton(foxmlViewAction, actionSource));
        toolbar.addMember(Actions.asIconButton(deleteAction, actionSource));
        toolbar.addMember(Actions.asIconButton(DigitalObjectFormValidateAction.getInstance(i18n),
                new ValidatableList(batchItemGrid)));
        toolbar.addMember(Actions.asIconButton(copyMetadataAction, actionSource));
        toolbar.addMember(Actions.asIconButton(resumeAction, this));
        return toolbar;
    }

    private Menu createEditorContextMenu(Menu menu, Object contextSource) {
        menu.addItem(Actions.asMenuItem(foxmlViewAction, contextSource, true));
        menu.addItem(Actions.asMenuItem(deleteAction, contextSource, true));
        menu.addItem(Actions.asMenuItem(copyMetadataAction, contextSource, false));
        menu.addItem(Actions.asMenuItem(DigitalObjectFormValidateAction.getInstance(i18n), new ValidatableList(batchItemGrid), false));
        return menu;
    }

    private int getNextSelection() {
        ListGridRecord selectedRecord = batchItemGrid.getSelectedRecord();
        return getNextSelection(selectedRecord);
    }

    private int getNextSelection(Record selectedRecord) {
        RecordList rl = batchItemGrid.getRecordList();
        int length = rl.getLength();
        if (length == 0) {
            return -1;
        }
        int nextSelectionIndex = 0;
        if (selectedRecord != null) {
            int recordIndex = batchItemGrid.getRecordIndex(selectedRecord);
            int nextRecordIndex = recordIndex + 1;
            if (nextRecordIndex >= length) {
                // end of the list
                nextRecordIndex = 0;
            }
            nextSelectionIndex = nextRecordIndex;
        }
        return nextSelectionIndex;
    }

    private void selectBatchItem(final boolean preview, final Record... selections) {
        loadItemInChildEditor(selections);
        if (!preview) {
            return;
        }
        if (selections != null && selections.length == 1) {
            previewItem(selections[0]);
            return ;
        }
        previewItem(null);
    }

    /**
     * Handles a new children selection.
     */
    private void loadItemInChildEditor(Record[] records) {
        actionSource.fireEvent();
        if (records == null || records.length == 0 /*|| originChildren != null*/) {
            childPlaces.goTo(Place.NOWHERE);
        } else {
            Place lastPlace = childPlaces.getWhere();
            DatastreamEditorType lastEditorType = null;
            if (lastPlace instanceof DigitalObjectEditorPlace) {
                DigitalObjectEditorPlace lastDOEPlace = (DigitalObjectEditorPlace) lastPlace;
                lastEditorType = lastDOEPlace.getEditorId();
            }
            lastEditorType = lastEditorType != null
                    ? lastEditorType
                    : DatastreamEditorType.MODS;
            childPlaces.goTo(new DigitalObjectEditorPlace(lastEditorType, records));
        }
    }

    private void previewItem(Record r) {
        DigitalObject dobj = r == null ? null : DigitalObject.createOrNull(r);
        Canvas preview = digitalObjectPreview.getUI();
        if (dobj != null) {
            digitalObjectPreview.edit(dobj);
            preview.show();
            preview.getParentElement().enable();
        } else {
            preview.hide();
            preview.getParentElement().disable();
        }
    }

    private final class SelectAction extends AbstractAction {

        SelectAction() {
            super(i18n.ImportBatchItemEditor_ActionSaveAll_Title(),
                    "[SKIN]/actions/approve.png",
                    i18n.ImportBatchItemEditor_ActionSaveAll_Hint());
        }

        @Override
        public void performAction(ActionEvent event) {
            Object source = event.getSource();
            if (source instanceof ImportBatchItemEditor) {
                selectAll((ImportBatchItemEditor) source);
            }
        }

        public void selectAll(ImportBatchItemEditor editor) {
            batchItemGrid.selectAllRecords();
        }

        /** Runs the action on CTRL+a */
        public void processEvent(BodyKeyPressEvent event) {
            if (!event.isCancelled()) {
                if (processBrowserEvent(event)) {
                    event.cancel();
                }
            }
        }

        /** Runs the action on CTRL+a */
        public void processEvent(KeyPressEvent event) {
            if (!event.isCancelled()) {
                if (processBrowserEvent(event)) {
                    event.cancel();
                }
            }
        }

        private boolean processBrowserEvent(BrowserEvent<?> event) {
            if (event.isCtrlKeyDown()) {
                if ("a".equals(EventHandler.getKeyEventCharacter())) {
                    selectAll(ImportBatchItemEditor.this);
                    return true;
                }
            }
            return false;
        }
    }

    /**
     * Helper to reorder import items. Start with {@link #reorder(java.lang.Object) }.
     */
    private final class ReorderTask implements ScheduledCommand {

        private Object sourceWidget;
        private Record[] originChildren;

        /**
         * Schedules save of reordered records. It expects to be invoked by
         * DropHandlers when widgets still returns origin list of records.
         *
         * @param sourceWidget thumbs or item list
         */
        public void reorder(Object sourceWidget) {
            this.sourceWidget = sourceWidget;
            originChildren = getRecords();
            Scheduler.get().scheduleDeferred(this);
        }

        @Override
        public void execute() {
            save();
        }

        private Record[] getRecords() {
            Record[] records;
            if (sourceWidget == batchItemGrid) {
                records = batchItemGrid.getOriginalResultSet().toArray();
            } else if (sourceWidget == thumbViewer) {
                records = thumbViewer.getData();
            } else {
                throw new IllegalStateException("Unsupported widget: " + sourceWidget);
            }
            return records;
        }

        private void updateWidgets(Object src) {
            if (src == batchItemGrid) {
                DataSource ds = batchItemGrid.getDataSource();
                Record[] records = batchItemGrid.getOriginalResultSet().toArray();
                Record[] copyRecords = ds.copyRecords(records);
                thumbViewer.setData(copyRecords);
            } else if (src == thumbViewer) {
                syncListWithTilesOnReorder();
            } else {
                refreshData();
            }
        }

        private void save() {
            if (originChildren == null) {
                return ;
            }
            Record[] rs = getRecords();
            if (RelationDataSource.equals(originChildren, rs)) {
                originChildren = null;
                sourceWidget = null;
                return ;
            }
            String[] childPids = ClientUtils.toFieldValues(rs, RelationDataSource.FIELD_PID);
            RelationDataSource relationDataSource = RelationDataSource.getInstance();
            DigitalObject root = DigitalObject.create(batchRecord);
            relationDataSource.reorderChildren(root, childPids, new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    if (value != null && value) {
                        updateWidgets(sourceWidget);
                        originChildren = null;
                        sourceWidget = null;
                        StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                    } else {
                        updateWidgets(null);
                    }
                }
            });
        }
    }

    public interface Handler {

        void handleNextAction();
    }

}
