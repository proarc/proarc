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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.widget;

import com.google.gwt.activity.shared.Activity;
import com.google.gwt.activity.shared.ActivityManager;
import com.google.gwt.activity.shared.ActivityMapper;
import com.google.gwt.core.client.Callback;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceController;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;
import com.google.web.bindery.event.shared.SimpleEventBus;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataChangedEvent;
import com.smartgwt.client.data.events.DataChangedHandler;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IconButton;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;
import com.smartgwt.client.widgets.grid.events.RecordDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordDoubleClickHandler;
import com.smartgwt.client.widgets.grid.events.RecordDropEvent;
import com.smartgwt.client.widgets.grid.events.RecordDropHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ItemClickEvent;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.DeleteAction;
import cz.cas.lib.proarc.webapp.client.action.DeleteAction.Deletable;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectCopyMetadataAction;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectCopyMetadataAction.CopySelector;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectFormValidateAction;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectFormValidateAction.ValidatableList;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectNavigateAction;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectNavigateAction.ChildSelector;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.action.SelectionCache;
import cz.cas.lib.proarc.webapp.client.action.UrnNbnAction;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource.RelationChangeEvent;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource.RelationChangeHandler;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditing;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditing.DigitalObjectEditorPlace;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditor;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditor.OptionalEditor;
import cz.cas.lib.proarc.webapp.client.widget.mods.NewIssueEditor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Edits children objects of the digital object.
 * It allows to change order of children, to edit a selected child using
 * particular data stream editor.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectChildrenEditor implements DatastreamEditor,
        Refreshable, Selectable<Record>, CopySelector, ChildSelector {

    /**
     * The boolean attribute to mark the most recently selected record. Useful
     * in case of multiple-selection.
     */
    public static final String LAST_CLICKED_ATTR = "__proarcLastClickedRecord";

    private static final Logger LOG = Logger.getLogger(DigitalObjectChildrenEditor.class.getName());
    private final ClientMessages i18n;
    /** A controller of the enclosing editor. */
    private final PlaceController places;
    private final ListGrid childrenListGrid;
    private final SelectionCache<ListGridRecord> selectionCache;
    private final DigitalObjectEditor childEditor;
    private final HLayout widget;
    private DigitalObject digitalObject;
    private final PlaceController childPlaces;
    private HandlerRegistration listDataChangedHandler;
    private Record[] originChildren;
    /** The last child selections. */
    private SelectionHistory selectionHistory = new SelectionHistory();
    private IconMenuButton addActionButton;
    private IconButton saveActionButton;
    private HandlerRegistration childrenSelectionHandler;
    private RelationDataSource relationDataSource;
    /** Notifies changes in list of children that should be reflected by actions. */
    private final ActionSource actionSource;
    private final DigitalObjectNavigateAction goDownAction;
    private final OptionalEditor preview;
    private Record lastClicked;

    public DigitalObjectChildrenEditor(ClientMessages i18n, PlaceController places, OptionalEditor preview) {
        this.i18n = i18n;
        this.places = places;
        this.preview = preview;
        this.actionSource = new ActionSource(this);
        this.goDownAction = DigitalObjectNavigateAction.child(i18n, places);
        relationDataSource = RelationDataSource.getInstance();
        childrenListGrid = initChildrenListGrid();
        this.selectionCache = SelectionCache.selector(childrenListGrid);
        VLayout childrenLayout = new VLayout();
        childrenLayout.setMembers(childrenListGrid);
        childrenLayout.setWidth("40%");
        childrenLayout.setShowResizeBar(true);

        SimpleEventBus eventBus = new SimpleEventBus();
        childPlaces = new PlaceController(eventBus);
        childEditor = new DigitalObjectEditor(i18n, childPlaces, true);
        ActivityManager activityManager = new ActivityManager(
                new ChildActivities(childEditor), eventBus);

        VLayout editorsLayout = new VLayout();
        VLayout editorsOuterLayout = new VLayout();
//        editorsLayout.setBorder("1px solid grey");
        editorsLayout.addStyleName("defaultBorder");
        editorsOuterLayout.setLayoutLeftMargin(4);
        editorsOuterLayout.setMembers(editorsLayout);
        activityManager.setDisplay(new ChildEditorDisplay(editorsLayout));

        widget = new HLayout();
        widget.setMembers(childrenLayout, editorsOuterLayout);
        relationDataSource.addRelationChangeHandler(new RelationChangeHandler() {

            @Override
            public void onRelationChange(RelationChangeEvent event) {
                // issue 262: isVisible seems to be always true and isAttached is always null.
                // Add test isDrawn that seems to change for dettached widgets.
                if (digitalObject != null && childrenListGrid.isVisible() && childrenListGrid.isDrawn()) {
                    String changedPid = event.getPid();
                    if (changedPid != null) {
                        Record changedRecord = childrenListGrid.getDataAsRecordList()
                                .find(RelationDataSource.FIELD_PID, changedPid);
                        if (changedRecord == null) {
                            // moved object(s)
                            // ListGrid does not remove selection of removed/moved rows
                            // and it does not fire selection change
                            // issue 246: clear selection of moved row
                            childrenListGrid.deselectAllRecords();
                            DigitalObjectCopyMetadataAction.resetSelection();
                            showCopySelection(new Record[0]);
                            return ;
                        }
                    }
                    final ListGridRecord[] selection = childrenListGrid.getSelectedRecords();
                    relationDataSource.updateCaches(digitalObject.getPid(), new BooleanCallback() {

                        @Override
                        public void execute(Boolean value) {
                            // refresh the copy selection as updated records are missing the copy attribute
                            showCopySelection(DigitalObjectCopyMetadataAction.getSelection());
                            // refresh the list selection
                            selectChildren(selection);
                        }
                    });
                }
            }
        });
    }

    @Override
    public void edit(DigitalObject digitalObject) {
        this.digitalObject = digitalObject;
        if (digitalObject == null) {
            return ;
        }
        detachListFromEditor();
        detachListResultSet();
        String pid = digitalObject.getPid();
        Criteria criteria = new Criteria(RelationDataSource.FIELD_ROOT, pid);
        criteria.addCriteria(RelationDataSource.FIELD_PARENT, pid);
        DigitalObjectCopyMetadataAction.resetSelection();
        ResultSet resultSet = childrenListGrid.getResultSet();
        if (resultSet != null) {
            Boolean willFetchData = resultSet.willFetchData(criteria);
            // init editor for cached record when DataArrivedHandler is not called
            if (!willFetchData) {
                showCopySelection(new Record[0]);
                initOnEdit();
            }
        }
        // use DataArrivedHandler instead of callback as it is not called
        // for refresh in SmartGWT 3.0
        childrenListGrid.fetchData(criteria);
        MetaModelDataSource.getModels(false, new Callback<ResultSet, Void>() {

            @Override
            public void onFailure(Void reason) {
            }

            @Override
            public void onSuccess(ResultSet result) {
                Map<?,?> valueMap = result.getValueMap(
                        MetaModelDataSource.FIELD_PID, MetaModelDataSource.FIELD_DISPLAY_NAME);
                childrenListGrid.getField(RelationDataSource.FIELD_MODEL).setValueMap(valueMap);
                createAddMenu(result);
            }
        });
    }

    @Override
    public void focus() {
        childrenListGrid.focus();
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz) || ChildSelector.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        Action addAction = Actions.emptyAction(i18n.DigitalObjectEditor_ChildrenEditor_AddAction_Title(),
                "[SKIN]/actions/add.png",
                i18n.DigitalObjectEditor_ChildrenEditor_AddAction_Hint());
        Action saveAction = new SaveAction(i18n) {

            @Override
            public void performAction(ActionEvent event) {
                save();
            }
        };
        DeleteAction deleteAction = new DeleteAction(new Deletable<Record>() {

            private final Deletable<Record> deletable = DigitalObjectDataSource.createDeletable();

            @Override
            public void delete(Object[] items, Record options) {
                deletable.delete(items, options);
                childrenListGrid.deselectAllRecords();
                DigitalObjectCopyMetadataAction.removeSelection((Record[]) items);
            }

            @Override
            public void delete(Object[] items) {
                delete(items, null);
            }

        }, DigitalObjectDataSource.createDeleteOptionsForm(), i18n);
        addActionButton = Actions.asIconMenuButton(addAction, this);
        return new Canvas[] {
            addActionButton,
            Actions.asIconButton(deleteAction, actionSource),
            Actions.asIconButton(DigitalObjectFormValidateAction.getInstance(i18n),
                    new ValidatableList(childrenListGrid)),
            Actions.asIconButton(new UrnNbnAction(i18n), actionSource),
            Actions.asIconButton(new DigitalObjectCopyMetadataAction(i18n), actionSource),
            saveActionButton = Actions.asIconButton(saveAction, this),
        };
    }

    @Override
    public Canvas getUI() {
        return widget;
    }

    @Override
    public void refresh() {
        ValidatableList.clearRowErrors(childrenListGrid);
        childrenListGrid.invalidateCache();
        edit(digitalObject);
    }

    @Override
    public Record[] getSelection() {
        return originChildren != null ? null : selectionCache.getSelection();
    }

    @Override
    public Record[] getChildSelection() {
        return getSelection();
    }

    @Override
    public void showCopySelection(Record[] records) {
        if (records == null) {
            return ;
        }
        RecordList copySelection = new RecordList(records);
        for (int i = childrenListGrid.getRecords().length - 1; i >= 0; i--) {
            Record item = childrenListGrid.getRecord(i);
            DigitalObject listItem = DigitalObject.create(item);
            Record select = copySelection.find(RelationDataSource.FIELD_PID, listItem.getPid());
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
                childrenListGrid.refreshRow(i);
            }
        }
    }

    private void save() {
        if (originChildren == null) {
            return ;
        }
        Record[] rs = childrenListGrid.getOriginalResultSet().toArray();
        String[] childPids = ClientUtils.toFieldValues(rs, RelationDataSource.FIELD_PID);
        relationDataSource.reorderChildren(digitalObject, childPids, new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value != null && value) {
                    originChildren = null;
                    updateReorderUi(false);
                    StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                }
            }
        });
    }

    /**
     * Handles a new children selection.
     */
    private void onChildSelection(Record[] records) {
        actionSource.fireEvent();
        if (records == null || records.length == 0 || originChildren != null) {
            childPlaces.goTo(Place.NOWHERE);
        } else {
            childPlaces.goTo(new DigitalObjectEditorPlace(null, records));
        }
        if (records == null || records.length <= 1) {
            // in case of multiselection the preview opens the last clicked record
            // see RecordClickHandler.
            preview(records);
        }
    }

    private void preview(Record... records) {
        if (records == null || records.length == 0 || originChildren != null) {
            preview.open();
        } else {
            preview.open(DigitalObject.toArray(records));
        }
    }

    private ListGrid initChildrenListGrid() {
        final ListGrid lg = new ListGrid() {

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
        lg.setDataSource(relationDataSource);
        lg.setFields(
                new ListGridField(RelationDataSource.FIELD_LABEL,
                        i18n.DigitalObjectSearchView_ListHeaderLabel_Title()),
                new ListGridField(RelationDataSource.FIELD_MODEL,
                        i18n.DigitalObjectSearchView_ListHeaderModel_Title()),
                new ListGridField(RelationDataSource.FIELD_PID,
                        i18n.DigitalObjectSearchView_ListHeaderPid_Title())
                );
        lg.getField(RelationDataSource.FIELD_PID).setHidden(true);
        lg.setCanSort(Boolean.FALSE);
        lg.setCanReorderRecords(Boolean.TRUE);
        lg.setShowRollOver(Boolean.FALSE);
        lg.setGenerateDoubleClickOnEnter(Boolean.TRUE);
        ListGridPersistance lgPersistence = new ListGridPersistance("DigitalObjectChildrenEditor.objectList", lg);
        lg.setViewState(lgPersistence.getViewState());

        // ListGrid with enabled grouping prevents record reoredering by dragging! (SmartGWT 3.0)
        // lg.setGroupByField(RelationDataSource.FIELD_MODEL);
        // lg.setGroupStartOpen(GroupStartOpen.ALL);
        lg.addRecordDropHandler(new RecordDropHandler() {

            @Override
            public void onRecordDrop(RecordDropEvent event) {
                Record[] records = childrenListGrid.getOriginalResultSet().toArray();
                if (originChildren == null) {
                    // takes the list snapshot before first reorder
                    originChildren = records;
                }
            }
        });
        lg.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                if (event.getStartRow() == 0) {
                    initOnEdit();
                }
            }
        });
        lg.addRecordDoubleClickHandler(new RecordDoubleClickHandler() {

            @Override
            public void onRecordDoubleClick(RecordDoubleClickEvent event) {
                ActionEvent evt = new ActionEvent(actionSource.getSource());
                goDownAction.performAction(evt);
            }
        });
        lg.addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                // NOTE: RecordClickEvent is fired after SelectionUpdatedEvent!
                if (!event.isCancelled() && originChildren == null) {
                    ListGridRecord r = event.getRecord();
                    r.setAttribute(LAST_CLICKED_ATTR, Boolean.TRUE);
                    if (lastClicked != null && r != lastClicked) {
                        lastClicked.setAttribute(LAST_CLICKED_ATTR, Boolean.FALSE);
                    }
                    lastClicked = r;
                    Record[] selection = getSelection();
                    if (selection != null && selection.length > 1) {
                        // single selection is handled by onChildSelection
                        preview(r);
                    }
                }
            }
        });
        return lg;
    }

    private void initOnEdit() {
//        LOG.info("initOnEdit");
        originChildren = null;
        lastClicked = null;
        updateReorderUi(false);
        attachListResultSet();
        // select first
        if (!childrenListGrid.getOriginalResultSet().isEmpty()) {
            Scheduler.get().scheduleDeferred(new ScheduledCommand() {

                @Override
                public void execute() {
                    // defer the select as it is ignored after refresh in onDataArrived
                    selectChildFromHistory();
                }
            });
        }
    }

    private void selectChildFromHistory() {
        int row = -1;
        Place where = places.getWhere();
        if (where instanceof DigitalObjectEditorPlace) {
            String selectPid = ((DigitalObjectEditorPlace) where).getSelectChildPid();
            if (selectPid != null) {
                int index = childrenListGrid.getRecordList().findIndex(RelationDataSource.FIELD_PID, selectPid);
                if (index > -1) {
                    row = index;
                }
            }
        }
        if (row < 0) {
            String selectPid = selectionHistory.getSelection(digitalObject.getPid());
            if (selectPid != null) {
                int index = childrenListGrid.getRecordList().findIndex(RelationDataSource.FIELD_PID, selectPid);
                if (index > -1) {
                    row = index;
                }
            }
        }
        if (row < 0) {
            row = 0;
        }
        childrenListGrid.scrollToRow(row);
        childrenListGrid.selectSingleRecord(row);
        childrenListGrid.focus();
    }

    private void selectChildren(Record[] selection) {
        RecordList rl = childrenListGrid.getRecordList();
        ArrayList<Record> newSelection = new ArrayList<Record>(selection.length);
        for (Record r : selection) {
            Record found = rl.find(RelationDataSource.FIELD_PID, r.getAttribute(RelationDataSource.FIELD_PID));
            if (found != null) {
                newSelection.add(found);
            }
        }
        if (!newSelection.isEmpty()) {
            childrenListGrid.selectRecords(newSelection.toArray(new Record[newSelection.size()]));
        }
    }

    /**
     * Opens child editor according to list selection.
     */
    private void attachListToEditor() {
        if (childrenSelectionHandler != null) {
            childrenSelectionHandler.removeHandler();
        }
        childrenSelectionHandler = childrenListGrid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                ListGridRecord[] records = selectionCache.setSelection();
                selectionHistory.select(digitalObject.getPid(), records);
                onChildSelection(records);
            }
        });
    }

    private void detachListFromEditor() {
        if (childrenSelectionHandler != null) {
            childrenSelectionHandler.removeHandler();
            childrenSelectionHandler = null;
            childPlaces.goTo(Place.NOWHERE);
            preview();
        }
    }

    /**
     * Listens to changes of original list result set (reorder, fetch).
     * It tracks order changes together with {@link ListGrid#addRecordDropHandler }
     * in {@link #initChildrenListGrid() }
     */
    private void attachListResultSet() {
        listDataChangedHandler = childrenListGrid.getOriginalResultSet().addDataChangedHandler(new DataChangedHandler() {

            @Override
            public void onDataChanged(DataChangedEvent event) {
                if (originChildren != null) {
                    // compare origin with new children
                    boolean equals = RelationDataSource.equals(
                            originChildren,
                            childrenListGrid.getOriginalResultSet().toArray());
                    // enable Save button
                    // disable remove and add
                    updateReorderUi(!equals);
                }
            }
        });
    }

    private void detachListResultSet() {
        if (listDataChangedHandler != null) {
            listDataChangedHandler.removeHandler();
            listDataChangedHandler = null;
        }
    }

    private void updateReorderUi(boolean reordered) {
        actionSource.fireEvent();
        saveActionButton.setVisible(reordered);
        addActionButton.setVisible(!reordered);
        if (reordered) {
            detachListFromEditor();
        } else {
            attachListToEditor();
        }
    }

    private void attachAddSubmenu(MenuItem mi) {
        Menu sm = new Menu();
        MenuItem miAddSingle = new MenuItem(i18n.DigitalObjectEditor_ChildrenEditor_CreateAction_Title());
        MenuItem miAddMultiple = new MenuItem(i18n.DigitalObjectEditor_ChildrenEditor_CreateMoreAction_Title());
        sm.addItemClickHandler((event) -> {
            MetaModelRecord mmr = MetaModelRecord.get(mi);
            if (event.getItem() == miAddSingle) {
                addChild(mmr, Collections.emptyMap());
            } else if (event.getItem() == miAddMultiple) {
                new NewIssueEditor(i18n).showWindow(params -> {
                    addChild(mmr, params.toMap());
                });
            }
        });
        sm.addItem(miAddSingle);
        sm.addItem(miAddMultiple);
        mi.setSubmenu(sm);
    }

    private void createAddMenu(ResultSet models) {
        Menu menuAdd = MetaModelDataSource.createMenu(models, true);
        menuAdd.setCanSelectParentItems(true);
        Arrays.stream(menuAdd.getItems())
                .filter(mi -> "model:ndkperiodicalissue".equals(mi.getAttribute(MetaModelDataSource.FIELD_PID)))
                .forEach(mi -> attachAddSubmenu(mi));
        addActionButton.setMenu(menuAdd);
        menuAdd.addItemClickHandler((ItemClickEvent event) -> {
            MetaModelRecord mmr = MetaModelRecord.get(event.getItem());
            addChild(mmr, Collections.emptyMap());
        });
    }

    private void addChild(MetaModelRecord model, Map<String, Object> params) {
        Record record = new Record(params);
        record.setAttribute(DigitalObjectDataSource.FIELD_MODEL, model.getId());
        record.setAttribute(RelationDataSource.FIELD_PARENT, digitalObject.getPid());
        DigitalObjectDataSource.getInstance().addData(record, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    final Record r = data[0];
                    DSRequest dsRequest = new DSRequest();
                    dsRequest.setOperationType(DSOperationType.ADD);
                    RelationDataSource.getInstance().updateCaches(response, dsRequest);
                    Scheduler.get().scheduleDeferred(new ScheduledCommand() {

                        @Override
                        public void execute() {
                            childrenListGrid.selectSingleRecord(r);
                            int recordIndex = childrenListGrid.getRecordIndex(r);
                            childrenListGrid.scrollToRow(recordIndex);
                        }
                    });
                }
            }
        });
    }

    public static final class ChildActivities implements ActivityMapper {

        private final DigitalObjectEditor childEditor;

        public ChildActivities(DigitalObjectEditor childEditor) {
            this.childEditor = childEditor;
        }

        @Override
        public Activity getActivity(Place place) {
            if (place instanceof DigitalObjectEditorPlace) {
                DigitalObjectEditorPlace editorPlace = (DigitalObjectEditorPlace) place;
                return new DigitalObjectEditing(editorPlace, childEditor);
            }
            return null;
        }

    }

    public static final class ChildEditorDisplay implements AcceptsOneWidget {

        private final Layout display;

        public ChildEditorDisplay(Layout display) {
            this.display = display;
        }

        @Override
        public void setWidget(IsWidget w) {
            Widget asWidget = Widget.asWidgetOrNull(w);
            if (asWidget instanceof Canvas) {
                ClientUtils.setMembers(display, (Canvas) asWidget);
            } else if (asWidget == null) {
                display.removeMembers(display.getMembers());
            } else {
                throw new IllegalStateException("Unsupported widget: " + asWidget.getClass());
            }
        }

    }

    /**
     * Keeps the history of child selections of edited parents.
     */
    private static final class SelectionHistory {

        /**
         * The PID to PID mapping.
         */
        private final Map<String, String> cache = new HashMap<String, String>();

        public void select(String pid, Record[] selection) {
            if (selection == null || selection.length == 0) {
                select(pid, (String) null);
            } else {
                DigitalObject dobj = DigitalObject.create(selection[0]);
                select(pid, dobj.getPid());
            }
        }

        public void select(String pid, String selection) {
            cache.put(pid, selection);
        }

        public String getSelection(String pid) {
            return cache.get(pid);
        }
    }

}
