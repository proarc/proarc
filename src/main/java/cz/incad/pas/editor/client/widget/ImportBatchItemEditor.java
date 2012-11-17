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
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TextAreaWrap;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.EventHandler;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.BodyKeyPressEvent;
import com.smartgwt.client.widgets.grid.events.BodyKeyPressHandler;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.tab.events.TabDeselectedEvent;
import com.smartgwt.client.widgets.tab.events.TabDeselectedHandler;
import com.smartgwt.client.widgets.tab.events.TabSelectedEvent;
import com.smartgwt.client.widgets.tab.events.TabSelectedHandler;
import com.smartgwt.client.widgets.tile.TileGrid;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.viewer.DetailFormatter;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.action.AbstractAction;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.DeleteAction;
import cz.incad.pas.editor.client.action.DeleteAction.RecordDeletable;
import cz.incad.pas.editor.client.action.FoxmlViewAction;
import cz.incad.pas.editor.client.action.RefreshAction;
import cz.incad.pas.editor.client.action.Selectable;
import cz.incad.pas.editor.client.ds.DcRecordDataSource;
import cz.incad.pas.editor.client.ds.ImportBatchDataSource.BatchRecord;
import cz.incad.pas.editor.client.ds.ImportBatchItemDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import cz.incad.pas.editor.client.ds.TextDataSource;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public final class ImportBatchItemEditor extends HLayout implements Selectable<Record>, RefreshAction.Refreshable {

    private static final Logger LOG = Logger.getLogger(ImportBatchItemEditor.class.getName());

    private final ClientMessages i18n;

    private final ListGrid batchItemGrid;
    private ListGridField fieldItemModel;
    private final TabSet tabSet;
    private DynamicFormTab[] dfTabs;
    private final TileGrid thumbViewer;
    private final MediaEditor digitalObjectPreview;
    private boolean selectThumbInProgress = false;
    private boolean selectListInProgress = false;
    private boolean selectTabInProgress = false;
    private BatchRecord batchRecord;
    private BatchItemMultiEdit batchItemMultiEdit;
    private FoxmlViewAction foxmlViewAction;
    private DeleteAction deleteAction;

    public ImportBatchItemEditor(ClientMessages i18n) {
        this.i18n = i18n;
        this.setHeight100();
        this.setWidth100();
        
        VLayout layout = new VLayout();
        layout.setShowResizeBar(true);
        layout.setResizeBarTarget("next");
        
        batchItemGrid = createItemList();
        layout.addMember(batchItemGrid);
        
        tabSet = createTabSet();
        tabSet.addTabDeselectedHandler(new TabDeselectedHandler() {

            @Override
            public void onTabDeselected(TabDeselectedEvent event) {
                if (selectTabInProgress) {
                    selectTabInProgress = false;
                    return ;
                }
                event.cancel();
                selectTab(event.getTab(), event.getNewTab());
            }
        });

        tabSet.addTabSelectedHandler(new TabSelectedHandler() {

            @Override
            public void onTabSelected(TabSelectedEvent event) {
                selectTab(event.getTab());
            }
        });

        layout.addMember(tabSet);
        HLayout editorThumbLayout = new HLayout();
        editorThumbLayout.setHeight100();
        editorThumbLayout.addMember(layout);

        thumbViewer = createThumbViewer();
        editorThumbLayout.addMember(thumbViewer);

        VLayout editorThumbToolbarLayout = new VLayout();
        editorThumbToolbarLayout.setShowResizeBar(true);
        editorThumbToolbarLayout.setResizeBarTarget("next");

        createActions();
        ToolStrip editorToolStrip = createEditorToolBar();
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

    public void onShow(BatchRecord batch) {
        this.batchRecord = batch;
        refresh();
    }

    public void onHide(BooleanCallback callback) {
        dfTabs[tabSet.getSelectedTabNumber()].onHide(callback);
    }

    private ListGrid createItemList() {
        final ListGrid grid = new ListGrid();
        grid.setShowResizeBar(true);
        grid.setSelectionType(SelectionStyle.MULTIPLE);
        grid.setCanSort(false);
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

        // issue 7: default BodyKeyPressHandler does not change row focus properly
        // in case of mixing mouse and keyboard navigation.
        // ListGrid.getSelectedRecord and ListGrid.getFocusRow() do not return same result!
        grid.addBodyKeyPressHandler(new BodyKeyPressHandler() {

            @Override
            public void onBodyKeyPress(BodyKeyPressEvent event) {
                if (event.isCtrlKeyDown() || EventHandler.shiftKeyDown()) {
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
                    int tileIndex = thumbViewer.getRecordIndex(selectedRecords[0]);
//                    selectThumbInProgress = true;
                    // use record index instead of ListGridRecord to work around a smartgwt bug
                    thumbViewer.selectRecord(tileIndex);
                    ClientUtils.scrollToTile(thumbViewer, tileIndex);
                } else if (selectedRecords != null && selectedRecords.length > 1) {
                    int[] indexes = new int[selectedRecords.length];
                    for (int i = 0; i < indexes.length; i++) {
                        indexes[i] = thumbViewer.getRecordIndex(selectedRecords[i]);
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
        Criteria criteria = new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, batchRecord.getId());
        batchItemGrid.invalidateCache();
        thumbViewer.invalidateCache();
        previewItem(null);
        dfTabs[tabSet.getSelectedTabNumber()].onShow();

        batchItemGrid.fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    batchItemGrid.selectSingleRecord(0);
                    batchItemGrid.focus();
                }
            }
        });
        thumbViewer.fetchData(criteria);
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
        thumbGrid.setFields(dvfThumbnail, dvfPageIndex);
        thumbGrid.setDataSource(ImportBatchItemDataSource.getInstance());
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
                    LOG.warning("thumb selects list: " + selectionIndex);
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
                    selectBatchItem(false);
                }
//                LOG.info("THUMB.onSelectionChanged.selection: " + Arrays.toString(selection));
            }
        });

        thumbGrid.addRecordClickHandler(new com.smartgwt.client.widgets.tile.events.RecordClickHandler() {

            @Override
            public void onRecordClick(com.smartgwt.client.widgets.tile.events.RecordClickEvent event) {
                // always preview last clicked record
                LOG.warning("TG.onRecordClick");
                previewItem(event.getRecord());
            }
        });

        return thumbGrid;
    }

    private void createActions() {
        batchItemMultiEdit = new BatchItemMultiEdit();
        foxmlViewAction = new FoxmlViewAction(i18n);
        deleteAction = new DeleteAction(
                new RecordDeletable(batchItemGrid.getDataSource(), i18n), i18n);
    }

    private ToolStrip createEditorToolBar() {
        ToolStrip toolbar = Actions.createToolStrip();
        toolbar.addMember(Actions.asIconButton(new RefreshAction(i18n), this));
        toolbar.addMember(Actions.asIconButton(new SelectAction(), this));
        toolbar.addMember(Actions.asIconButton(batchItemMultiEdit, this));
        toolbar.addMember(Actions.asIconButton(foxmlViewAction, this));
        toolbar.addMember(Actions.asIconButton(deleteAction, this));
        return toolbar;
    }

    private Menu createEditorContextMenu(Menu menu, Object contextSource) {
        MenuItem miRedraw = Actions.asMenuItem(new AbstractAction("Reset", null, null) {

            @Override
            public void performAction(ActionEvent event) {
                Object source = event.getSource();
                if (source == batchItemGrid) {
                    batchItemGrid.clear();
                    batchItemGrid.draw();
                } else if (source == thumbViewer) {
                    thumbViewer.clear();
                    thumbViewer.draw();
                }
            }
        }, contextSource);

        menu.addItem(Actions.asMenuItem(batchItemMultiEdit, contextSource, true));
        menu.addItem(Actions.asMenuItem(foxmlViewAction, contextSource, true));
        menu.addItem(Actions.asMenuItem(deleteAction, contextSource, true));
        menu.addItem(new MenuItemSeparator());
        menu.addItem(miRedraw);
        return menu;
    }

    private TabSet createTabSet() {
        TabSet tabSet = new TabSet();
        dfTabs = createTabs();

        for (DynamicFormTab dfTab : dfTabs) {
            tabSet.addTab(dfTab.getWidget());
        }
        Object[] tabControls = createTabControls();
        if (tabControls != null) {
            tabSet.setTabBarControls(tabControls);
        }
        return tabSet;
    }

    private DynamicFormTab[] createTabs() {
        DynamicFormTab dcTab = new DynamicFormTab(
                i18n.ImportBatchItemEditor_TabDublinCore_Title(),
                null,
                createDcForm(), i18n);
        DynamicFormTab noteTab = new DynamicFormTab(
                i18n.ImportBatchItemEditor_TabNote_Title(),
                i18n.ImportBatchItemEditor_TabNote_Hint(),
                createNoteForm(), i18n);
        DynamicFormTab modsTab = new DynamicFormTab(
                i18n.ImportBatchItemEditor_TabMods_Title(),
                null,
                createModsForm(), i18n);
        DynamicFormTab ocrTab = new DynamicFormTab(
                i18n.ImportBatchItemEditor_TabOcr_Title(),
                null,
                createOcrForm(), i18n);
        return new DynamicFormTab[] {
            modsTab,
            noteTab,
            ocrTab,
            dcTab
        };
    }

    private DynamicForm createDcForm() {
        DcRecordDataSource dsDc = DcRecordDataSource.getInstance();
        DCEditor dcEditor = new DCEditor(i18n);
        dcEditor.setDataSource(dsDc);
        dcEditor.setDataPath(DcRecordDataSource.FIELD_DC.getQualifiedName());
        return dcEditor;
    }
    
    private DynamicForm createModsForm() {
        DynamicForm form = new DynamicForm();
        form.setHeight100();
        form.setWidth100();
        form.setBrowserSpellCheck(false);
        form.setSaveOnEnter(true);

        SelectItem pageType = new SelectItem(ImportBatchItemDataSource.FIELD_PAGE_TYPE,
                i18n.PageForm_PageType_Title());
        pageType.setDefaultValue(ModsCustomDataSource.getDefaultPageType());
        pageType.setValueMap(ModsCustomDataSource.getPageTypes());

        IntegerItem pageIndex = new IntegerItem(ImportBatchItemDataSource.FIELD_PAGE_INDEX);
        pageIndex.setTitle(i18n.PageForm_PageIndex_Title());

        TextItem pageNumber = new TextItem(ImportBatchItemDataSource.FIELD_PAGE_NUMBER);
        pageNumber.setTitle(i18n.PageForm_PageNumber_Title());
        pageNumber.setLength(20);

        form.setFields(pageType, pageIndex, pageNumber);
        form.setDataSource(ImportBatchItemDataSource.getInstance());

        IntegerRangeValidator integerRangeValidator = new IntegerRangeValidator();
        integerRangeValidator.setMin(0);
        integerRangeValidator.setMax(Integer.MAX_VALUE);

        pageIndex.setValidators(integerRangeValidator);

        return form;
    }
    
    private DynamicForm createOcrForm() {
        DynamicForm form = new DynamicForm();
        TextDataSource dataSource = TextDataSource.getOcr();
        form.setDataSource(dataSource);
        form.setWidth100();
        form.setHeight100();
        TextAreaItem textAreaItem = new TextAreaItem(TextDataSource.FIELD_CONTENT, "OCR");
        textAreaItem.setColSpan("*");
        textAreaItem.setHeight("*");
        textAreaItem.setWrap(TextAreaWrap.OFF);
        textAreaItem.setShowTitle(false);
        textAreaItem.setWidth("*");
        form.setFields(textAreaItem);
        return form;
    }

    private DynamicForm createNoteForm() {
        DynamicForm form = new DynamicForm();
        TextDataSource dataSource = TextDataSource.getNote();
        form.setDataSource(dataSource);
        form.setWidth100();
        form.setHeight100();
        TextAreaItem textAreaItem = new TextAreaItem(TextDataSource.FIELD_CONTENT, "Note");
        textAreaItem.setColSpan("*");
        textAreaItem.setHeight("*");
        textAreaItem.setWrap(TextAreaWrap.OFF);
        textAreaItem.setShowTitle(false);
        textAreaItem.setWidth("*");
        form.setFields(textAreaItem);
        return form;
    }

    private Object[] createTabControls() {
        return null;
    }

    private int getNextSelection() {
        RecordList rl = batchItemGrid.getRecordList();
        int length = rl.getLength();
        if (length == 0) {
            return -1;
        }
        ListGridRecord selectedRecord = batchItemGrid.getSelectedRecord();
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
        DynamicFormTab dfTab = dfTabs[tabSet.getSelectedTabNumber()];
        dfTab.onChange(new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (!preview) {
                    return;
                }
                if (value != null && value) {
                    if (selections != null && selections.length == 1) {
                        previewItem(selections[0]);
                        return ;
                    }
                }
                previewItem(null);
            }
        }, selections);
    }

    private void previewItem(Record r) {
        String pid = (r == null) ? null : r.getAttribute(ImportBatchItemDataSource.FIELD_PID);
        Canvas preview = digitalObjectPreview.getUI();
        if (pid != null) {
            digitalObjectPreview.edit(pid, batchRecord.getId(), null);
            preview.show();
            preview.getParentElement().enable();
        } else {
            preview.hide();
            preview.getParentElement().disable();
        }
    }

    private void selectTab(Tab newTab) {
//        LOG.info(ClientUtils.format("selectTab: newTab: %s", newTab));
        DynamicFormTab dfTab = dfTabs[tabSet.getTabNumber(newTab.getID())];
        Record[] selections = batchItemGrid.getSelectedRecords();
        dfTab.onShow(selections);
    }

    private void selectTab(Tab oldTab, final Tab newTab) {
        final DynamicFormTab dfTab = dfTabs[tabSet.getTabNumber(oldTab.getID())];
//        LOG.info(ClientUtils.format("switchTabs: oldTab: %s, newTab: %s", oldTab.getTitle(), newTab.getTitle()));
        selectTabInProgress = true;
        dfTab.onHide(new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
//        LOG.info(ClientUtils.format("switchTabs.execute: value: %s, newTab: %s", value, newTab.getTitle()));
                if (value != null && value) {
                    if (newTab != null) {
                        // this will trigger TabDeselectedHandler again
                        tabSet.selectTab(newTab);
                        return ;
                    }
                }
                selectTabInProgress = false;
                dfTab.updateTitle();
            }
        });
    }

    /**
     * Wraps tab form to add submit button and submit handler.
     */
    private final class EditorForm extends VLayout {

        private final DynamicForm form;

        public EditorForm(final DynamicForm form) {
            super(4);
            IButton save = new IButton(i18n.ImportBatchItemEditor_Tab_Submit_Title(), new ClickHandler() {

                @Override
                public void onClick(ClickEvent event) {
                    form.submit();
                }
            });
            save.setLayoutAlign(Alignment.CENTER);
            form.setOverflow(Overflow.AUTO);
            setMembers(form, save);
            this.form = form;

            form.addSubmitValuesHandler(new SubmitValuesHandler() {

                @Override
                public void onSubmitValues(SubmitValuesEvent event) {
                    final int nextSelection = getNextSelection();
                    form.saveData(new DSCallback() {

                        @Override
                        public void execute(DSResponse response, Object rawData, DSRequest request) {
                            if (response.getStatus() == DSResponse.STATUS_SUCCESS) {
                                if (nextSelection >= 0) {
                                    batchItemGrid.selectSingleRecord(nextSelection);
                                    batchItemGrid.scrollToRow(nextSelection);
                                    form.focus();
                                }
                            } else {
                                form.reset();
                                dfTabs[tabSet.getSelectedTabNumber()].updateTitle();
                            }
                        }
                    });
                }
            });
        }

        public DynamicForm getForm() {
            return form;
        }

    }

    private final class DynamicFormTab {
        private final ClientMessages i18n;
        private final Tab tab;
        private final DynamicForm form;
        private final EditorForm eform;
        private String title;
        private final Canvas emptyContent;

        public DynamicFormTab(String title, String hint, DynamicForm form, ClientMessages i18n) {
            this.i18n = i18n;
            this.title = title;
            this.tab = new Tab(title);
            this.tab.setPrompt(hint);
            this.emptyContent = new Canvas();
            this.tab.setPane(emptyContent);
            this.form = form;
            this.eform = new EditorForm(form);
            form.addItemChangedHandler(new ItemChangedHandler() {

                @Override
                public void onItemChanged(ItemChangedEvent event) {
                    updateTitle();
                }
            });
        }

        private void updateTitle() {
            String tabTitle = this.title;
            if (DynamicFormTab.this.form.valuesHaveChanged()) {
                tabTitle += " *";
            }
            LOG.info("updateTitle: " + tabTitle);
            tab.setTitle(tabTitle);

            Map values = form.getValues();
            Map oldValues = form.getOldValues();
            Map changedValues = form.getChangedValues();
            LOG.info(ClientUtils.format("### updateTitle.valuesHaveChanged(): %s\n old: %s\n\n val: %s\n\n new: %s\n",
                    form.valuesHaveChanged(), oldValues, values, changedValues));
        }

        public Tab getWidget() {
            return tab;
        }

        public void onShow(Record... selections) {
            // fetch data
            fetchSelection(selections);
        }

        public void onHide(BooleanCallback hideCallback) {
            hideCallback.execute(true);
        }

        public void onChange(final BooleanCallback changeCallback, final Record... selections) {
            changeCallback.execute(true);
            fetchSelection(selections);
        }

        private void fetchSelection(Record... selections) {
            Canvas pane = tab.getPane();
            Canvas newPane;
            if (selections != null && selections.length == 1) {
                fetchSelection(
                        selections[0].getAttribute(ImportBatchItemDataSource.FIELD_PID),
                        selections[0].getAttribute(ImportBatchItemDataSource.FIELD_BATCHID)
                        );
                newPane = eform;
            } else {
                newPane = emptyContent;
            }

            if (pane != newPane) {
                TabSet ts = tab.getTabSet();
                ts.updateTab(tab, newPane);
            }
        }

        private void fetchSelection(String pid, String batch) {
            LOG.fine("fetch.dc: " + pid);

            Criteria criteria = new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, batch);
            criteria.addCriteria(new Criteria(ImportBatchItemDataSource.FIELD_PID, pid));
            form.fetchData(criteria, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    form.rememberValues();
                    updateTitle();
                }
            });

        }

    }

    private final class BatchItemMultiEdit extends AbstractAction {

        public BatchItemMultiEdit() {
            super(i18n.ImportBatchItemEditor_ActionEdit_Title(),
                    "[SKIN]/actions/edit.png",
                    i18n.ImportBatchItemEditor_ActionEdit_Hint());
        }

        @Override
        public void performAction(ActionEvent event) {
            final Record[] selection = Actions.getSelection(event);
            if (selection == null || selection.length == 0) {
                return ;
            }
            final PageMetadataEditor editor = PageMetadataEditor.getInstance();
            editor.showInWindow(new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    if (value != null && value) {
                        edit(editor, selection);
                    }
                }
            });
        }

        public void edit(PageMetadataEditor editor, Record[] selection) {
            DataSource ds = batchItemGrid.getDataSource();
            Integer indexStart = null;
            Iterator<String> sequence = null;
            String numberFormat = "%s";
            if (editor.getAllowPageIndexes()) {
                indexStart = editor.getIndexStart();
            }
            if (editor.getAllowPageNumbers()) {
                sequence = editor.getSequence();
                String prefix = editor.getPrefix();
                String suffix = editor.getSuffix();
                if (prefix != null) {
                    numberFormat = prefix + numberFormat;
                }
                if (suffix != null) {
                    numberFormat += suffix;
                }
            }
//            RPCManager.startQueue();
            for (Record record : selection) {
                if (editor.getAllowPageIndexes()) {
                    String old = record.getAttributeAsString(ImportBatchItemDataSource.FIELD_PAGE_INDEX);
                    String newVal = indexStart == null ? null : String.valueOf(indexStart++);
                    newVal = (old != null && newVal == null) ? "" : newVal;
                    record.setAttribute(ImportBatchItemDataSource.FIELD_PAGE_INDEX, newVal);
                }
                if (editor.getAllowPageNumbers()) {
                    String old = record.getAttributeAsString(ImportBatchItemDataSource.FIELD_PAGE_NUMBER);
                    String newVal = sequence != null
                            ? ClientUtils.format(numberFormat, sequence.next())
                            : ClientUtils.format(numberFormat, "");
                    newVal = newVal.isEmpty() ? null : newVal;
                    newVal = (old != null && newVal == null) ? "" : newVal;
                    record.setAttribute(ImportBatchItemDataSource.FIELD_PAGE_NUMBER, newVal);
                }
                if (editor.getAllowPageTypes()) {
                    String pageType = editor.getPageType();
                    record.setAttribute(ImportBatchItemDataSource.FIELD_PAGE_TYPE, pageType);
                }
                record = ClientUtils.removeNulls(record);
                ds.updateData(record);
            }
//            RPCManager.sendQueue();
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
    }

}
