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
package cz.incad.pas.editor.client.widget;

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
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataChangedEvent;
import com.smartgwt.client.data.events.DataChangedHandler;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IconButton;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.RecordDropEvent;
import com.smartgwt.client.widgets.grid.events.RecordDropHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.action.Action;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.action.SaveAction;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.ds.RelationDataSource.RelationChangeEvent;
import cz.incad.pas.editor.client.ds.RelationDataSource.RelationChangeHandler;
import cz.incad.pas.editor.client.presenter.DigitalObjectEditing;
import cz.incad.pas.editor.client.presenter.DigitalObjectEditing.DigitalObjectEditorPlace;
import cz.incad.pas.editor.client.presenter.DigitalObjectEditor;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi.DatastreamEditorType;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Edits children objects of the digital object.
 * It allows to change order of children, to edit a selected child using
 * particular data stream editor.
 *
 * XXX add new child
 * XXX remove selected children
 * XXX batch edit of selected children (change device, change parent, MODS edit)
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectChildrenEditor implements DatastreamEditor, Refreshable {

    private static final Logger LOG = Logger.getLogger(DigitalObjectChildrenEditor.class.getName());
    private final ClientMessages i18n;
    private final ListGrid childrenListGrid;
    private final DigitalObjectEditor childEditor;
    private final HLayout widget;
    private String pid;
    private final PlaceController childPlaces;
    private DatastreamEditorType lastEditorType;
    private HandlerRegistration listDataChangedHandler;
    private Record[] originChildren;
    private IconButton saveActionButton;
    private HandlerRegistration childrenSelectionHandler;
    private RelationDataSource relationDataSource;

    public DigitalObjectChildrenEditor(ClientMessages i18n) {
        this.i18n = i18n;
        relationDataSource = RelationDataSource.getInstance();
        childrenListGrid = initChildrenListGrid();
        VLayout childrenLayout = new VLayout();
        childrenLayout.setMembers(childrenListGrid);
        childrenLayout.setWidth("40%");

        SimpleEventBus eventBus = new SimpleEventBus();
        childPlaces = new PlaceController(eventBus);
        ActivityManager activityManager = new ActivityManager(new ChildActivities(), eventBus);

        VLayout editorsLayout = new VLayout();
        VLayout editorsOuterLayout = new VLayout();
        childEditor = new DigitalObjectEditor(i18n, childPlaces, true);
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
                if (pid != null && childrenListGrid.isVisible()) {
                    relationDataSource.updateCaches(pid);
                }
            }
        });
    }

    @Override
    public void edit(String pid, String batchId, MetaModelRecord model) {
        this.pid = pid;
        if (pid == null) {
            return ;
        }
        detachListFromEditor();
        detachListResultSet();
        Criteria criteria = new Criteria(RelationDataSource.FIELD_ROOT, pid);
        criteria.addCriteria(RelationDataSource.FIELD_PARENT, pid);
        ResultSet resultSet = childrenListGrid.getResultSet();
        if (resultSet != null) {
            Boolean willFetchData = resultSet.willFetchData(criteria);
            // init editor for cached record when DataArrivedHandler is not called
            if (!willFetchData) {
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
            }
        });
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        Action saveAction = new SaveAction(i18n) {

            @Override
            public void performAction(ActionEvent event) {
                save();
            }
        };
        return new Canvas[] {
            saveActionButton = Actions.asIconButton(saveAction, this),
        };
    }

    @Override
    public Canvas getUI() {
        return widget;
    }

    @Override
    public void refresh() {
        childrenListGrid.invalidateCache();
        edit(pid, null, null);
    }

    private void save() {
        if (originChildren == null) {
            return ;
        }
        Record[] rs = childrenListGrid.getOriginalResultSet().toArray();
        String[] childPids = RelationDataSource.toFieldValues(rs, RelationDataSource.FIELD_PID);
        relationDataSource.reorderChildren(pid, childPids, new BooleanCallback() {

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

    private ListGrid initChildrenListGrid() {
        ListGrid lg = new ListGrid();
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
        return lg;
    }

    private void initOnEdit() {
//        LOG.info("initOnEdit");
        originChildren = null;
        updateReorderUi(false);
        attachListResultSet();
        // select first
        if (!childrenListGrid.getOriginalResultSet().isEmpty()) {
            Scheduler.get().scheduleDeferred(new ScheduledCommand() {

                @Override
                public void execute() {
                    // defer the select as it is ignored after refresh in onDataArrived
                    childrenListGrid.selectSingleRecord(0);
                }
            });
        }
    }

    /**
     * Opens child editor according to list selection.
     */
    private void attachListToEditor() {
        childrenSelectionHandler = childrenListGrid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                ListGridRecord[] records = childrenListGrid.getSelectedRecords();
                if (records == null || records.length == 0) {
                    childPlaces.goTo(Place.NOWHERE);
                } else if (records.length == 1) {
//                    LOG.severe(ClientUtils.dump(records[0], "onSelectionUpdated"));
                    String childPid = records[0].getAttribute(RelationDataSource.FIELD_PID);
                    lastEditorType = lastEditorType == null ? DatastreamEditorType.MODS : lastEditorType;
                    childPlaces.goTo(new DigitalObjectEditorPlace(lastEditorType, childPid));
                }
            }
        });
    }

    private void detachListFromEditor() {
        if (childrenSelectionHandler != null) {
            childrenSelectionHandler.removeHandler();
            childrenSelectionHandler = null;
            childPlaces.goTo(Place.NOWHERE);
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
        saveActionButton.setVisible(reordered);
        if (reordered) {
            detachListFromEditor();
        } else {
            attachListToEditor();
        }
    }

    private final class ChildActivities implements ActivityMapper {

        @Override
        public Activity getActivity(Place place) {
            if (place instanceof DigitalObjectEditorPlace) {
                DigitalObjectEditorPlace editorPlace = (DigitalObjectEditorPlace) place;
                lastEditorType = editorPlace.getEditorId();
                return new DigitalObjectEditing(editorPlace, childPlaces, childEditor, i18n);
            }
            return null;
        }

    }

    private static final class ChildEditorDisplay implements AcceptsOneWidget {

        private final Layout display;

        public ChildEditorDisplay(Layout display) {
            this.display = display;
        }

        @Override
        public void setWidget(IsWidget w) {
            Widget asWidget = Widget.asWidgetOrNull(w);
            if (asWidget instanceof Canvas) {
                display.setMembers((Canvas) asWidget);
            } else if (asWidget == null) {
                display.removeMembers(display.getMembers());
            } else {
                throw new IllegalStateException("Unsupported widget: " + asWidget.getClass());
            }
        }

    }

}
