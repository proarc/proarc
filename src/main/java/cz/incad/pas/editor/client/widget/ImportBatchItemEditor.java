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
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.ImageStyle;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TextAreaWrap;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
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
import cz.incad.pas.editor.client.ds.ImportBatchDataSource;
import cz.incad.pas.editor.client.ds.ImportBatchDataSource.BatchRecord;
import cz.incad.pas.editor.client.ds.ImportBatchItemDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.OcrDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * TODO Thumb edit dialog
 * X Page Index
 * Start Sequence:
 * X Page Number
 * Start Sequence: Prefix: Suffix:
 * X Page Type
 * Combo: ListOfIllustrations, TableOfContents, Index, Table, TitlePage, ListOfMaps, NormalPage, Blank, ListOfTables, Advertisement
 *
 * TODO Thumb selection handling (show preview, synchronize ListGrid)
*/
public class ImportBatchItemEditor extends HLayout {

    private static final Logger LOG = Logger.getLogger(ImportBatchItemEditor.class.getName());

    private final PasEditorMessages i18nPas;
//    private DataSource dsBatchItem;

    private final ListGrid batchItemGrid;
    private ListGridField fieldItemModel;
    private final Img preview;
    private final TabSet tabSet;
    private DynamicFormTab[] dfTabs;
    private final TileGrid thumbViewer;
    
    public ImportBatchItemEditor(PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
        this.setHeight100();
        this.setWidth100();
        
        VLayout layout = new VLayout();
        layout.setShowResizeBar(true);
        layout.setResizeBarTarget("next");
        
        batchItemGrid = new ListGrid();
        batchItemGrid.setShowResizeBar(true);
//        batchItemGrid.setSelectionType(SelectionStyle.SINGLE);
//        batchItemGrid.setSelectionType(SelectionStyle.NONE);
        batchItemGrid.setCanSort(false);
        batchItemGrid.setAutoFitFieldWidths(true);
        batchItemGrid.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        batchItemGrid.setLeaveScrollbarGap(false);

        fieldItemModel = new ListGridField(ImportBatchItemDataSource.FIELD_MODEL,
                i18nPas.ImportBatchItemEditor_ListHeaderModel_Title());

        batchItemGrid.setDataSource(ImportBatchItemDataSource.getInstance());
        fieldItemModel.setOptionDataSource(MetaModelDataSource.getInstance());
        fieldItemModel.setValueField(MetaModelDataSource.FIELD_PID);
        fieldItemModel.setDisplayField(MetaModelDataSource.FIELD_DISPLAY_NAME);
        fieldItemModel.setAutoFetchDisplayMap(true);
//        final ListGridField fieldThumbnail = new ListGridField(ImportBatchItemDataSource.FIELD_THUMBNAIL, "Preview");
//        fieldThumbnail.setImageHeight(128/2);
//        fieldThumbnail.setImageWidth(90/2);
//        fieldThumbnail.setHidden(true);
//        batchItemGrid.setCellHeight(fieldThumbnail.getImageHeight() + 2);

        batchItemGrid.setFields(new ListGridField[] {
            new ListGridField(ImportBatchItemDataSource.FIELD_PID, i18nPas.ImportBatchItemEditor_ListHeaderPID_Title()),
//            fieldThumbnail,
            fieldItemModel,
            new ListGridField(ImportBatchItemDataSource.FIELD_USER, i18nPas.ImportBatchItemEditor_ListHeaderUser_Title()),
            new ListGridField(ImportBatchItemDataSource.FIELD_FILENAME, i18nPas.ImportBatchItemEditor_ListHeaderFilename_Title())
        });
//        batchItemGrid.addRecordClickHandler(new RecordClickHandler() {
//
//            @Override
//            public void onRecordClick(RecordClickEvent event) {
//                int record = event.getRecordNum();
//                Boolean selected = batchItemGrid.isSelected(batchItemGrid.getRecord(record));
//                LOG.severe(ClientUtils.format("onRecordClick: %s, selected: %s", record, selected));
//                batchItemGrid.sel
//            }
//        });

        batchItemGrid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
//                if (true) throw new UnsupportedOperationException("Not supported yet.");
//                System.out.println("### PIV.onSelectionUpdated");
                if (selectListInProgress) {
                    selectListInProgress = false;
                    return ;
                }
                ListGridRecord[] selectedRecords = batchItemGrid.getSelectedRecords();
                thumbViewer.deselectAllRecords();
                if (selectedRecords != null && selectedRecords.length == 1) {
                    // select thumbnail just in case of the single selection
//                    int tileIndex = batchItemGrid.getRecordIndex(selectedRecords[0]);
                    int tileIndex = thumbViewer.getRecordIndex(selectedRecords[0]);
//                    selectThumbInProgress = true;
                    // use record index instead of ListGridRecord to work around a smartgwt bug
                    thumbViewer.selectRecord(tileIndex);
                    ClientUtils.scrollToTile(thumbViewer, tileIndex);
                }
                selectBatchItem(selectedRecords);
            }

        });

//        batchItemGrid.addCellClickHandler(new CellClickHandler() {
//
//            @Override
//            public void onCellClick(CellClickEvent event) {
//                System.out.println("### PIV.onCellClick");
//                int rowNum = event.getRowNum();
////                batchItemGrid.selectSingleRecord(rowNum);
//                //                event.cancel();
//            }
//        });

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

//        tabSet = new TabSet();


//        Tab tabDC = new Tab("DC");
//        VLayout dclayout = new VLayout();
//
//        final DcRecordDataSource dsDc = DcRecordDataSource.getInstance();
//
////        final DynamicForm dcEditor = new DCEditorSchema();
//        dcEditor = new DCEditor();
//        dcEditor.setDataSource(dsDc);
//        dcEditor.setDataPath(DcRecordDataSource.FIELD_DC.getQualifiedName());
//
//        dclayout.addMember(dcEditor);
//        IButton commitButton = new IButton("Commit");
//        commitButton.addClickHandler(new ClickHandler() {
//
//            @Override
//            public void onClick(ClickEvent event) {
//                Boolean valuesHaveChanged = dcEditor.valuesHaveChanged();
//                Map oldValues = dcEditor.getOldValues();
//                Record valuesAsRecord = dcEditor.getValuesAsRecord();
//                LOG.info(ClientUtils.dump(dcEditor.getChangedValues(), "", "  ",
//                        new StringBuilder("dcEditor.getChangedValues:\n")).toString());
//                LOG.info(ClientUtils.dump(dcEditor.getValues(), "", "  ",
//                        new StringBuilder("dcEditor.getValues:\n")).toString());
//                LOG.info(ClientUtils.format("%s\n oldValues: %s\n%s",
//                        ClientUtils.dump(valuesAsRecord, "commit, valuesHaveChanged: " + valuesHaveChanged),
//                        oldValues,
//                        dsDc.xmlSerialize(valuesAsRecord, new SerializationContext())
////                        ((DCEditorSchema) dcEditor).schema.xmlSerialize(valuesAsRecord, new SerializationContext())
//                        ));
////                SC.say("dsDc.xmlSerialize", dsDc.xmlSerialize(valuesAsRecord, new SerializationContext()));
//                dcEditor.saveData();
//            }
//        });
//        dclayout.addMember(commitButton);
////        dclayout.setCanFocus(true);
//        dclayout.addFocusChangedHandler(new FocusChangedHandler() {
//
//            @Override
//            public void onFocusChanged(FocusChangedEvent event) {
//                LOG.info("dclayout.onFocusChanged: " + event.getHasFocus());
////                if (event.getHasFocus()) {
////                    dcEditor.focus();
////                }
//            }
//        });
//
//        IButton ValidateButton = new IButton("Validate");
//        ValidateButton.addClickHandler(new ClickHandler() {
//
//            @Override
//            public void onClick(ClickEvent event) {
////                dcEditor.validate();
//
//        dsDc.fetchData(null, new DSCallback() {
//
//            @Override
//            public void execute(DSResponse response, Object rawData, DSRequest request) {
//                Record r = response.getData()[0];
////                LOG.info(ClientUtils.format("fetched dcRecord: \n%s", ClientUtils.dump(r, "")));
//                LOG.info(dsDc.xmlSerialize(r, new SerializationContext()));
//                dcEditor.editRecord(r);
////                dcEditor.editRecord(r.getAttributeAsRecord(DcRecordDataSource.FIELD_DC));
//            }
//        });
//
//
//            }
//
//        });
//        dclayout.addMember(ValidateButton);
//
//        tabDC.setPane(dclayout);
//        tabSet.addTab(tabDC);
        
        layout.addMember(tabSet);
        HLayout editorThumbLayout = new HLayout();
        editorThumbLayout.setHeight100();
        editorThumbLayout.addMember(layout);
        editorThumbLayout.setShowResizeBar(true);
        editorThumbLayout.setResizeBarTarget("next");
        addMember(editorThumbLayout);
//        addMember(layout);

//        VLayout thubmLayout = new VLayout();

        thumbViewer = createThumbViewer();
        editorThumbLayout.addMember(thumbViewer);

//        VLayout previewLayout = new VLayout();
////        preview = new DetailViewer();
//        preview = new TileGrid();
        preview = new Img();
        preview.setImageType(ImageStyle.CENTER);
        preview.setWidth(400);
//        preview.setShowTitle(true);
//        preview.setTitle("Preview");
//        preview.setAltText("Alt Preview");
//        DetailViewerField fieldPreview = new DetailViewerField(ImportBatchItemDataSource.FIELD_PREVIEW, "Preview");
//        fieldPreview.setType("image");
//        fieldPreview.setImageWidth(200);
//        fieldPreview.setShowFileInline(true);
//        preview.setFields(fieldPreview);
////        preview.setDataSource(ImportBatchItemDataSource.getInstance());
//        previewLayout.setMembers(preview);
//        previewLayout.setAlign(Alignment.CENTER);
////        previewLayout.setWidth(200);
//        previewLayout.setOverflow(Overflow.AUTO);
////        previewLayout.setRedrawOnResize(true);
//        addMember(previewLayout);
//        preview.setWidth("50%");
//        preview.setOverflow(Overflow.AUTO);
        HLayout previewLayout = new HLayout();
        previewLayout.setWidth("40%");
        previewLayout.setHeight100();
        previewLayout.setOverflow(Overflow.AUTO);
//        previewLayout.setShowResizeBar(true);
//        previewLayout.setResizeFrom("L");
        previewLayout.addMember(preview);
        addMember(previewLayout);
    }

    public void setBatchItems(BatchRecord batch) {
//        this.dsBatchItem = ds;
//        batchItemGrid.setDataSource(dsBatchItem, batchItemGrid.getFields());
//        fieldItemModel.setOptionDataSource(MetaModelDataSource.getInstance());
//        fieldItemModel.setValueField(MetaModelDataSource.FIELD_PID);
//        fieldItemModel.setDisplayField(MetaModelDataSource.FIELD_DISPLAY_NAME);
//        fieldItemModel.setAutoFetchDisplayMap(true);
//        ds.fetchData(null, null);
        Criteria criteria = new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, batch.getId());
        batchItemGrid.fetchData(criteria);
        thumbViewer.fetchData(criteria);
//        thumbViewer.fetchData();
//        new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, "");
    }

    public void onHide(BooleanCallback callback) {
        dfTabs[tabSet.getSelectedTabNumber()].onHide(callback);
    }

    private TileGrid createThumbViewer() {
        final TileGridEnhanced thumbGrid = new TileGridEnhanced();
        thumbGrid.setWidth(150);
        thumbGrid.setHeight100();
        thumbGrid.setMinWidth(150);
        thumbGrid.setShowEdges(false);
        thumbGrid.setCanReorderTiles(true);
        thumbGrid.setWrapValues(true);
        thumbGrid.setSelectionType(SelectionStyle.MULTIPLE);
//        thumbGrid.setSelectionType(SelectionStyle.SIMPLE);
        // setTileProperties does not work; it replaces default renderer (smartgwt 2.5)
        //thumbGrid.setTileProperties(tileCanvas);
        // setDetailViewerProperties replaces default renderer and it is impossible to customize it not to show field titles (smartgwt 2.5)
        //thumbGrid.setDetailViewerProperties(thumbViewer);

        DetailViewerField dvfPageIndex = new DetailViewerField(ImportBatchItemDataSource.FIELD_PAGE_INDEX);
        dvfPageIndex.setDetailFormatter(new DetailFormatter() {

            @Override
            public String format(Object value, Record record, DetailViewerField field) {
                String number = record.getAttribute(ImportBatchItemDataSource.FIELD_PAGE_NUMBER);
                number = (number != null) ? number : "";
                value = (value != null) ? value : "";
                return ClientUtils.format("%s - %s", value, number);
            }
        });
//        DetailViewerField dvfPageNumber = new DetailViewerField(ImportBatchItemDataSource.FIELD_PAGE_NUMBER);
        DetailViewerField dvfPageType = new DetailViewerField(ImportBatchItemDataSource.FIELD_PAGE_TYPE);
        // ListOfIllustrations, TableOfContents, Index, Table, TitlePage, ListOfMaps, NormalPage, Blank, ListOfTables, Advertisement
        final DetailViewerField dvfThumbnail = new DetailViewerField(ImportBatchItemDataSource.FIELD_THUMBNAIL);
//        dvfThumbnail.setType("image");
//        dvfThumbnail.setShowFileInline(false);
//        dvfThumbnail.setImageURLPrefix(RestConfig.URL_DIGOBJECT_THUMBNAIL + "?pid=");
//        dvfThumbnail.setImageSize(150);
//        dvfThumbnail.setImageHeight(128);
//        dvfThumbnail.setImageWidth(89);
//        dvfThumbnail.setImageHeight(128*2/3);
//        dvfThumbnail.setImageWidth(89*2/3);
        thumbGrid.setFields(dvfThumbnail, dvfPageIndex, dvfPageType);
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
                            Integer numberStart = null;
                            String numberFormat = "%s";
                            if (editor.getAllowPageIndexes()) {
                                indexStart = editor.getIndexStart();
                            }
                            if (editor.getAllowPageNumbers()) {
                                numberStart = editor.getNumberStart();
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
                                    String newVal = numberStart != null
                                            ? ClientUtils.format(numberFormat, numberStart++)
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

        tabSet.setTabBarControls(createTabControls());
//        tabSet.selectTab(dfTabs.length - 1); // OCR
        return tabSet;
    }

    private DynamicFormTab[] createTabs() {
        DynamicFormTab dcTab = new DynamicFormTab(
                i18nPas.ImportBatchItemEditor_TabDublinCore_Title(),
                createDcForm(), i18nPas);
        DynamicFormTab modsTab = new DynamicFormTab(
                i18nPas.ImportBatchItemEditor_TabMods_Title(),
                createModsForm(), i18nPas);
        DynamicFormTab ocrTab = new DynamicFormTab(
                i18nPas.ImportBatchItemEditor_TabOcr_Title(),
                createOcrForm(), i18nPas);
        return new DynamicFormTab[] {
            modsTab,
            dcTab,
            ocrTab
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
//        dynamicForm.setWidth("206px");
//        form.setIsGroup(true);
//        form.setGroupTitle("Group");
        form.setHeight100();
        form.setWidth100();

        SelectItem pageType = new SelectItem("pageType", i18nPas.PageForm_PageType_Title());
//        radioGroupItem.setTooltip("podle ANL by tu mohlo byt mnohem vic typu. Viz http://digit.nkp.cz/DigitizedPeriodicals/DTD/2.10/Periodical.xsd/PeriodicalPage[@Type]");
        pageType.setDefaultValue("NormalPage");
        LinkedHashMap<String, String> pageTypes = new LinkedHashMap<String, String>();
        pageTypes.put("ListOfIllustrations", i18nPas.PageForm_TypeListOfIllustrations_Title());
        pageTypes.put("TableOfContents", i18nPas.PageForm_TypeTableOfContents_Title());
        pageTypes.put("Index", i18nPas.PageForm_TypeIndex_Title());
        pageTypes.put("Table", i18nPas.PageForm_TypeTable_Title());
        pageTypes.put("TitlePage", i18nPas.PageForm_TypeTitlePage_Title());
        pageTypes.put("ListOfMaps", i18nPas.PageForm_TypeListOfMaps_Title());
        pageTypes.put("NormalPage", i18nPas.PageForm_TypeNormalPage_Title());
        pageTypes.put("Blank", i18nPas.PageForm_TypeBlank_Title());
        pageTypes.put("ListOfTables", i18nPas.PageForm_TypeListOfTables_Title());
        pageTypes.put("Advertisement", i18nPas.PageForm_TypeAdvertisement_Title());
        pageType.setValueMap(pageTypes);
        pageType.setDefaultValue("NormalPage");

        IntegerItem pageIndex = new IntegerItem("pageIndex");
        pageIndex.setTitle(i18nPas.PageForm_PageIndex_Title());

        TextItem pageNumber = new TextItem("pageNumber");
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
        OcrDataSource dataSource = OcrDataSource.getInstance();
        form.setDataSource(dataSource);


//        form.setNumCols(1);
        form.setWidth100();
        form.setHeight100();
//        HiddenItem itemPid = new HiddenItem(OcrDataSource.FIELD_PID);
//        HiddenItem itemTimestamp = new HiddenItem(OcrDataSource.FIELD_TIMESTAMP);
        TextAreaItem textAreaItem = new TextAreaItem(OcrDataSource.FIELD_OCR, "OCR");
        textAreaItem.setColSpan("*");
        textAreaItem.setHeight("*");
        textAreaItem.setWrap(TextAreaWrap.OFF);
        textAreaItem.setShowTitle(false);
        textAreaItem.setWidth("*");
        form.setFields(textAreaItem);
        return form;
    }

    private Object[] createTabControls() {
//        btnPreviousObject.setIcon("[SKIN]/actions/back.png");
//        btnPreviousObject.setIcon("[SKIN]/actions/prev.png");
//        btnPreviousObject.setIcon("[SKIN]/TransferIcons/left.png");
        IButton btnPreviousObject = createTabControlButton(
                "[SKIN]/actions/back.png", "p",
                i18nPas.ImportBatchItemEditor_ButtonPrevious_Title(),
                new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                //                LOG.info("FOCUSES: " + ListFormItem.dump(dcEditor.getFields()));
                selectNextBatchItem(false);
            }
        });

//        btnNextObject.setIcon("[SKIN]/actions/next.png");
//        btnNextObject.setIcon("[SKIN]/TransferIcons/right.png");
        IButton btnNextObject = createTabControlButton(
                "[SKIN]/actions/forward.png", "n",
                i18nPas.ImportBatchItemEditor_ButtonNext_Title(),
                new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                selectNextBatchItem(true);
            }
        });

        return new Object[] {btnPreviousObject, btnNextObject,
//                new TransferImgButton(TransferImgButton.LEFT),
//                new TransferImgButton(TransferImgButton.UP)
        };
    }

    private IButton createTabControlButton(String icon, String accessKey, String tooltip, ClickHandler handler) {
        IButton btn = new IButton();
        btn.setIcon(icon);
//        btn.setIcon("[SKIN]/actions/back.png");
//        btn.setIcon("[SKIN]/actions/prev.png");
//        btn.setIcon("[SKIN]/TransferIcons/left.png");
        btn.setShowRollOver(true);
        btn.setShowDisabled(true);
        btn.setShowDown(true);
        btn.setWidth(24);
        btn.setAccessKey(accessKey);
        btn.setTooltip(tooltip);
        btn.addClickHandler(handler);
        return btn;
    }

    private void selectNextBatchItem(boolean forward) {
        RecordList rl = batchItemGrid.getRecordList();
        int length = rl.getLength();
        if (length == 0) {
            return ;
        }

        int increment = forward ? 1 : -1;
        ListGridRecord selectedRecord = batchItemGrid.getSelectedRecord();
        int nextSelectionIndex = 0;
        if (selectedRecord != null) {
            int recordIndex = batchItemGrid.getRecordIndex(selectedRecord);
            int nextRecordIndex = recordIndex + increment;
            if (nextRecordIndex < 0 || nextRecordIndex >= length) {
                // start or end of the list, do nothing
                return ;
            }
            nextSelectionIndex = nextRecordIndex;
        }
        batchItemGrid.selectSingleRecord(nextSelectionIndex);
        batchItemGrid.scrollToRow(nextSelectionIndex);
    }

    private void selectBatchItem(final Record... selections) {
        DynamicFormTab dfTab = dfTabs[tabSet.getSelectedTabNumber()];
        dfTab.onChange(new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value != null && value) {
                    if (selections != null && selections.length == 1) {
                        preview.setSrc(ClientUtils.format("%s?%s",
                                RestConfig.URL_DIGOBJECT_PREVIEW,
                                selections[0].getAttribute(ImportBatchItemDataSource.FIELD_PREVIEW)
                                ));
                    } else {
                        preview.setSrc("");
                    }
                }
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

    private static class DynamicFormTab {
        private final PasEditorMessages i18nPas;
        private final Tab tab;
        private final DynamicForm form;
        private String title;
        private final Canvas emptyContent;

        public DynamicFormTab(String title, DynamicForm form, PasEditorMessages i18nPas) {
            this.i18nPas = i18nPas;
            this.title = title;
            this.tab = new Tab(title);
            this.emptyContent = new Canvas();
            this.tab.setPane(emptyContent);
            this.form = form;
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
            // if change then submit
            // use no selection to prevent fetching
            UpdateProcessor processor = new UpdateProcessor(hideCallback);
            if (form.valuesHaveChanged()) {
                // if change then submit
                processor.updateOrDiscard();
            } else {
                // fetch data
                hideCallback.execute(true);
//                processor.fetchSelection();
            }
        }

        public void onChange(final BooleanCallback changeCallback, final Record... selections) {
            UpdateProcessor processor = new UpdateProcessor(new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    if (value != null && value) {
                        fetchSelection(selections);
                    }
                    changeCallback.execute(value);
                }
            }, selections);

            Map values = form.getValues();
            Map oldValues = form.getOldValues();
            Map changedValues = form.getChangedValues();
            LOG.info(ClientUtils.format("### form.valuesHaveChanged(): %s\n old: %s\n\n val: %s\n\n new: %s",
                    form.valuesHaveChanged(), oldValues, values, changedValues));
            if (form.valuesHaveChanged()) {
                // if change then submit
                processor.updateOrDiscard();
            } else {
                // fetch data
                changeCallback.execute(true);
                processor.fetchSelection();
            }
        }

        private class UpdateProcessor implements BooleanCallback, DSCallback {

            private final BooleanCallback callback;
            private final Record[] selections;

            /**
             * @param callback returns false in case of failed update;
             *          successful update or discard returns true
             */
            public UpdateProcessor(BooleanCallback callback, Record... selections) {
                this.callback = callback;
                this.selections = selections;
            }

            public void updateOrDiscard() {
                if (form.valuesHaveChanged()) {
                    SC.ask(i18nPas.ImportBatchItemEditor_WindowSave_MSG(), this);
                }
            }

            public void updateOrDiscard(Boolean update) {
                if (update != null && update) {
                    form.saveData(this);
                } else {
                    form.clearValues();
                    form.rememberValues();
                    callback.execute(true);
//                    fetchSelection();
                }
            }

            public void fetchSelection() {
                DynamicFormTab.this.fetchSelection(selections);
            }

            private void handleUpdateResponse(DSResponse response) {
                // XXX handle errors
                if (response != null && response.getStatus() != RPCResponse.STATUS_SUCCESS) {
                    callback.execute(false);
                    return ;
                } else {
                    callback.execute(true);
//                    fetchSelection();
                }
//                if (selections != null && selections.length == 1) {
//                    updateTitle();
//                    callback.execute(true);
//                    // this can be async
//                    fetchSelection(selections[0].getAttribute(ImportBatchItemDataSource.FIELD_PID));
//                }
            }

            @Override
            public void execute(Boolean value) {
                updateOrDiscard(value);
            }

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                handleUpdateResponse(response);
            }

        }

        private void fetchSelection(Record... selections) {
            Canvas pane = tab.getPane();
            Canvas newPane;
            if (selections != null && selections.length == 1) {
                Boolean isClientOnlyDataSource = form.getDataSource().getClientOnly();
                if (isClientOnlyDataSource != null && isClientOnlyDataSource) {
                    // useful only for testing purposes
                    form.editRecord(selections[0]);
                    form.focus();
                    form.rememberValues();
                    updateTitle();
                } else {
                    fetchSelection(
                            selections[0].getAttribute(ImportBatchItemDataSource.FIELD_PID),
                            selections[0].getAttribute(ImportBatchItemDataSource.FIELD_BATCHID)
                            );
                }
                newPane = form;
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
//                    Record r = response.getData()[0];
//                    LOG.fine(ClientUtils.format("fetched dcRecord: \n%s", ClientUtils.dump(r.getJsObj())));
//                    LOG.fine(ClientUtils.format("fetched dcRecord: \n%s", ClientUtils.dump(r, "")));
//                    LOG.fine(form.getDataSource().xmlSerialize(r, new SerializationContext()));
//                    form.editRecord(r);
    //                dcEditor.editRecord(r.getAttributeAsRecord(DcRecordDataSource.FIELD_DC));
//                    form.rememberValues();
                    form.focus();
                    form.rememberValues();
                    updateTitle();
    //                LOG.info("FOCUSES: " + ListFormItem.dump(dcEditor.getFields()));
                }
            });

        }

    }
}
