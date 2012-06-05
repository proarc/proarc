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

import com.google.gwt.user.client.Timer;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TextAreaWrap;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
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
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.tab.events.TabDeselectedEvent;
import com.smartgwt.client.widgets.tab.events.TabDeselectedHandler;
import com.smartgwt.client.widgets.tab.events.TabSelectedEvent;
import com.smartgwt.client.widgets.tab.events.TabSelectedHandler;
import com.smartgwt.client.widgets.tile.TileGrid;
import com.smartgwt.client.widgets.viewer.DetailFormatter;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.DcRecordDataSource;
import cz.incad.pas.editor.client.ds.ImportBatchDataSource.BatchRecord;
import cz.incad.pas.editor.client.ds.ImportBatchItemDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.ds.TextDataSource;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public final class ImportBatchItemEditor extends HLayout {

    private static final Logger LOG = Logger.getLogger(ImportBatchItemEditor.class.getName());

    private final PasEditorMessages i18nPas;

    private final ListGrid batchItemGrid;
    private ListGridField fieldItemModel;
    private final TabSet tabSet;
    private DynamicFormTab[] dfTabs;
    private final TileGrid thumbViewer;
    private final DigitalObjectPreview digitalObjectPreview;

    public ImportBatchItemEditor(PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
        this.setHeight100();
        this.setWidth100();
        
        VLayout layout = new VLayout();
        layout.setShowResizeBar(true);
        layout.setResizeBarTarget("next");
        
        batchItemGrid = new ListGrid();
        batchItemGrid.setShowResizeBar(true);
        batchItemGrid.setSelectionType(SelectionStyle.MULTIPLE);
        batchItemGrid.setCanSort(false);
        batchItemGrid.setAutoFitFieldWidths(true);
        batchItemGrid.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        batchItemGrid.setLeaveScrollbarGap(false);
        batchItemGrid.setDataSource(ImportBatchItemDataSource.getInstance());

        fieldItemModel = new ListGridField(ImportBatchItemDataSource.FIELD_MODEL,
                i18nPas.ImportBatchItemEditor_ListHeaderModel_Title());
        fieldItemModel.setPrompt(i18nPas.ImportBatchItemEditor_ListHeaderModel_Hint());
        fieldItemModel.setOptionDataSource(MetaModelDataSource.getInstance());
        fieldItemModel.setValueField(MetaModelDataSource.FIELD_PID);
        fieldItemModel.setDisplayField(MetaModelDataSource.FIELD_DISPLAY_NAME);
        fieldItemModel.setAutoFetchDisplayMap(true);
        fieldItemModel.setHidden(true);

        ListGridField fieldPid = new ListGridField(ImportBatchItemDataSource.FIELD_PID,
                i18nPas.ImportBatchItemEditor_ListHeaderPID_Title());
        fieldPid.setPrompt(i18nPas.ImportBatchItemEditor_ListHeaderPID_Hint());
        fieldPid.setHidden(true);

        ListGridField fieldUser = new ListGridField(ImportBatchItemDataSource.FIELD_USER,
                i18nPas.ImportBatchItemEditor_ListHeaderUser_Title());
        fieldUser.setPrompt(i18nPas.ImportBatchItemEditor_ListHeaderUser_Hint());
        fieldUser.setHidden(true);

        ListGridField fieldFilename = new ListGridField(ImportBatchItemDataSource.FIELD_FILENAME,
                i18nPas.ImportBatchItemEditor_ListHeaderFilename_Title());
        fieldFilename.setPrompt(i18nPas.ImportBatchItemEditor_ListHeaderFilename_Hint());

        ListGridField fieldPageIndex = new ListGridField(ImportBatchItemDataSource.FIELD_PAGE_INDEX,
                i18nPas.ImportBatchItemEditor_ListHeaderPageIndex_Title());
        fieldPageIndex.setPrompt(i18nPas.ImportBatchItemEditor_ListHeaderPageIndex_Hint());
        fieldPageIndex.setHidden(true);

        ListGridField fieldPageNumber = new ListGridField(ImportBatchItemDataSource.FIELD_PAGE_NUMBER,
                i18nPas.ImportBatchItemEditor_ListHeaderPageNumber_Title());
        fieldPageNumber.setPrompt(i18nPas.ImportBatchItemEditor_ListHeaderPageNumber_Hint());

        ListGridField fieldPageType = new ListGridField(ImportBatchItemDataSource.FIELD_PAGE_TYPE,
                i18nPas.ImportBatchItemEditor_ListHeaderPageType_Title());
        fieldPageType.setPrompt(i18nPas.ImportBatchItemEditor_ListHeaderPageType_Hint());
        fieldPageType.setEmptyCellValue(ModsCustomDataSource.getPageTypes().get(ModsCustomDataSource.getDefaultPageType()));

        batchItemGrid.setFields(fieldFilename, fieldPageNumber, fieldPageIndex, fieldPageType, fieldPid, fieldItemModel, fieldUser);

        batchItemGrid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                if (selectListInProgress) {
                    selectListInProgress = false;
                    return ;
                }
                ListGridRecord[] selectedRecords = batchItemGrid.getSelectedRecords();
                thumbViewer.deselectAllRecords();
                if (selectedRecords != null && selectedRecords.length == 1) {
                    // select thumbnail just in case of the single selection
                    int tileIndex = thumbViewer.getRecordIndex(selectedRecords[0]);
//                    selectThumbInProgress = true;
                    // use record index instead of ListGridRecord to work around a smartgwt bug
                    thumbViewer.selectRecord(tileIndex);
                    ClientUtils.scrollToTile(thumbViewer, tileIndex);
                }
                selectBatchItem(selectedRecords);
            }

        });

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
        editorThumbLayout.setShowResizeBar(true);
        editorThumbLayout.setResizeBarTarget("next");
        addMember(editorThumbLayout);

        thumbViewer = createThumbViewer();
        editorThumbLayout.addMember(thumbViewer);

        digitalObjectPreview = new DigitalObjectPreview(i18nPas);
        digitalObjectPreview.addBackgroundListeners(thumbViewer);
        Canvas previewLayout = digitalObjectPreview.asCanvas();
        previewLayout.setWidth("40%");
        previewLayout.setHeight100();
//        previewLayout.setShowResizeBar(true);
//        previewLayout.setResizeFrom("L");
        addMember(previewLayout);
    }

    public void onShow(BatchRecord batch) {
        Criteria criteria = new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, batch.getId());
        batchItemGrid.invalidateCache();
        thumbViewer.invalidateCache();
        digitalObjectPreview.selectPreviewField(null);
        dfTabs[tabSet.getSelectedTabNumber()].onShow();

        batchItemGrid.fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (response.getStatus() == DSResponse.STATUS_SUCCESS) {
                    batchItemGrid.selectSingleRecord(0);
                    batchItemGrid.focus();
                }
            }
        });
        thumbViewer.fetchData(criteria);
    }

    public void onHide(BooleanCallback callback) {
        dfTabs[tabSet.getSelectedTabNumber()].onHide(callback);
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
                return ClientUtils.format("Index: %s<br>%s: %s", value, pageTypes.get(type), number);
            }
        });
        final DetailViewerField dvfThumbnail = new DetailViewerField(ImportBatchItemDataSource.FIELD_THUMBNAIL);
        thumbGrid.setFields(dvfThumbnail, dvfPageIndex);
        // TileLayoutPolicy.FLOW does not work as expected
        // thumbGrid.setLayoutPolicy(TileLayoutPolicy.FLOW);
        thumbGrid.setTileHeight(128 + 8 + 12 * 2);
        thumbGrid.setTileWidth(120);
        Menu menu = new Menu();
        MenuItem miSelectAll = new MenuItem(i18nPas.ImportBatchItemEditor_MenuSaveAll_Title());
        miSelectAll.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

            @Override
            public void onClick(MenuItemClickEvent event) {
                thumbGrid.selectAllRecords();
            }
        });
        MenuItem miSelectMatching = new MenuItem(i18nPas.ImportBatchItemEditor_MenuSelectMatching_Title());
        MenuItem miDelete = new MenuItem(i18nPas.ImportBatchItemEditor_MenuDelete_Title());
        miDelete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

            @Override
            public void onClick(MenuItemClickEvent event) {
                final Record[] selection = thumbGrid.getSelection();
                if (selection == null || selection.length == 0) {
                    return ;
                }
                SC.ask(i18nPas.ImportBatchItemEditor_WindowDelete_Title(),
                        i18nPas.ImportBatchItemEditor_WindowDelete_MSG(String.valueOf(selection.length)),
                        new BooleanCallback() {

                    @Override
                    public void execute(Boolean value) {
                        if (value != null && value) {
                            // XXX delete now fires multiple requests; try to do it in single request
                            thumbGrid.removeSelectedData();
                        }
                    }
                });
            }
        });
        MenuItem miEdit = new MenuItem(i18nPas.ImportBatchItemEditor_MenuEdit_Title());
        miEdit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

            @Override
            public void onClick(MenuItemClickEvent event) {
                final PageMetadataEditor editor = PageMetadataEditor.getInstance();
                editor.showInWindow(new BooleanCallback() {

                    @Override
                    public void execute(Boolean value) {
                        if (value != null && value) {
                            Record[] selection = thumbGrid.getSelection();
                            DataSource ds = thumbGrid.getDataSource();
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
//                            RPCManager.startQueue();
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
//                            RPCManager.sendQueue();
                        }
                    }
                });
            }
        });
        menu.setItems(miSelectAll, miSelectMatching, new MenuItemSeparator(), miEdit, miDelete);
        thumbGrid.setContextMenu(menu);

        thumbGrid.setDataSource(ImportBatchItemDataSource.getInstance());

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
                    batchItemGrid.selectSingleRecord(selectionIndex);
                    batchItemGrid.scrollToRow(selectionIndex);
                    selectBatchItem(selection);
                } else if (batchItemGrid.anySelected()) {
                    selectListInProgress = true;
                    batchItemGrid.deselectAllRecords();
                    selectBatchItem();
                }
//                LOG.info("THUMB.onSelectionChanged.selection: " + Arrays.toString(selection));
            }
        });

        return thumbGrid;
    }

    private boolean selectThumbInProgress = false;
    private boolean selectListInProgress = false;
    private ArrayList<Integer> thumbGridSelection = new ArrayList<Integer>();
    private SelectionTimer selectionTimer = new SelectionTimer();

    private final class SelectionTimer extends Timer {

        @Override
        public void run() {
            LOG.severe("TIMER.run selection.length: " + thumbGridSelection.size() + ", TG.selection.length: " + thumbViewer.getSelection().length);
//                LOG.info("THUMB.onSelectionChanged: " + Arrays.toString(record.getAttributes()));
            Record[] selection = thumbViewer.getSelection();
            if (selection != null && selection.length == 1) {
//                LOG.info("THUMB.onSelectionChanged.selection.state: " + event.getState() + ".attrs: " + Arrays.toString(selection[0].getAttributes()));
                selectListInProgress = true;
                int selectionIndex = batchItemGrid.getRecordIndex(selection[0]);
                batchItemGrid.selectSingleRecord(selectionIndex);
                batchItemGrid.scrollToRow(selectionIndex);
                selectBatchItem(selection);
            }
//                LOG.info("THUMB.onSelectionChanged.selection: " + Arrays.toString(selection));
        }

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
                i18nPas.ImportBatchItemEditor_TabDublinCore_Title(),
                null,
                createDcForm(), i18nPas);
        DynamicFormTab noteTab = new DynamicFormTab(
                i18nPas.ImportBatchItemEditor_TabNote_Title(),
                i18nPas.ImportBatchItemEditor_TabNote_Hint(),
                createNoteForm(), i18nPas);
        DynamicFormTab modsTab = new DynamicFormTab(
                i18nPas.ImportBatchItemEditor_TabMods_Title(),
                null,
                createModsForm(), i18nPas);
        DynamicFormTab ocrTab = new DynamicFormTab(
                i18nPas.ImportBatchItemEditor_TabOcr_Title(),
                null,
                createOcrForm(), i18nPas);
        return new DynamicFormTab[] {
            modsTab,
            noteTab,
            ocrTab,
            dcTab
        };
    }

    private DynamicForm createDcForm() {
        DcRecordDataSource dsDc = DcRecordDataSource.getInstance();
        DCEditor dcEditor = new DCEditor(i18nPas);
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
                i18nPas.PageForm_PageType_Title());
        pageType.setDefaultValue(ModsCustomDataSource.getDefaultPageType());
        pageType.setValueMap(ModsCustomDataSource.getPageTypes());

        IntegerItem pageIndex = new IntegerItem(ImportBatchItemDataSource.FIELD_PAGE_INDEX);
        pageIndex.setTitle(i18nPas.PageForm_PageIndex_Title());

        TextItem pageNumber = new TextItem(ImportBatchItemDataSource.FIELD_PAGE_NUMBER);
        pageNumber.setTitle(i18nPas.PageForm_PageNumber_Title());
        pageNumber.setLength(20);

        form.setFields(pageType, pageIndex, pageNumber);
        // XXX replace DS with real MODS service
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

    private void selectBatchItem(final Record... selections) {
        DynamicFormTab dfTab = dfTabs[tabSet.getSelectedTabNumber()];
        dfTab.onChange(new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value != null && value) {
                    if (selections != null && selections.length == 1) {
                        digitalObjectPreview.selectPreviewField(
                                selections[0].getAttribute(ImportBatchItemDataSource.FIELD_PREVIEW));
                        return ;
                    }
                }
                digitalObjectPreview.selectPreviewField(null);
            }
        }, selections);
    }

    private void selectTab(Tab newTab) {
//        LOG.info(ClientUtils.format("selectTab: newTab: %s", newTab));
        DynamicFormTab dfTab = dfTabs[tabSet.getTabNumber(newTab.getID())];
        Record[] selections = batchItemGrid.getSelectedRecords();
        dfTab.onShow(selections);
    }

    private boolean selectTabInProgress = false;

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
            IButton save = new IButton(i18nPas.ImportBatchItemEditor_Tab_Submit_Title(), new ClickHandler() {

                @Override
                public void onClick(ClickEvent event) {
                    form.submit();
                    form.focus();
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
                                    form.focus();
                                }
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
        private final PasEditorMessages i18nPas;
        private final Tab tab;
        private final DynamicForm form;
        private final EditorForm eform;
        private String title;
        private final Canvas emptyContent;

        public DynamicFormTab(String title, String hint, DynamicForm form, PasEditorMessages i18nPas) {
            this.i18nPas = i18nPas;
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
}
