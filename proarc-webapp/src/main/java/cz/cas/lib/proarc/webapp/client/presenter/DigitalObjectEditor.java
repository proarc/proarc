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
package cz.cas.lib.proarc.webapp.client.presenter;

import com.google.gwt.activity.shared.ActivityManager;
import com.google.gwt.core.client.Callback;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceController;
import com.google.web.bindery.event.shared.SimpleEventBus;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionType;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IconButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.events.VisibilityChangedEvent;
import com.smartgwt.client.widgets.events.VisibilityChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripSeparator;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.ClientUtils.SweepTask;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectEditAction;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectEditAction.AcceptFilter;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectNavigateAction;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectNavigateAction.ChildSelector;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadEvent;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadHandler;
import cz.cas.lib.proarc.webapp.client.event.HasEditorLoadHandlers;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditing.DigitalObjectEditorPlace;
import cz.cas.lib.proarc.webapp.client.widget.BatchDatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.DatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectAdministrationEditor;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectChildrenEditor;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectChildrenEditor.ChildActivities;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectChildrenEditor.ChildEditorDisplay;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectParentEditor;
import cz.cas.lib.proarc.webapp.client.widget.MediaEditor;
import cz.cas.lib.proarc.webapp.client.widget.TextEditor;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.logging.Logger;

/**
 * Edits digital object data streams.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectEditor implements Refreshable, Selectable<Record>,
        HasEditorLoadHandlers, ChildSelector {

    private static final Logger LOG = Logger.getLogger(DigitalObjectEditor.class.getName());
    private final ClientMessages i18n;
    private final VLayout widget;
    private final Label lblHeader;
    private final ToolStrip toolbar;
    private ToolStripSeparator customToolbarSeparator;
    private final VLayout editorContainer;
    private VLayout optionalEditorContainer;
    private EditorDescriptor currentEditor;
    private OptionalEditor optionalEditor;
    private IconButton optionalEditorSwitch;
    /** currently edited object {PID, MODEL_OBJECT}; should be replaced with some interface */
    private Record[] selection;
    private final EnumMap<DatastreamEditorType, EditorDescriptor> editorCache;
    private final ActionSource actionSource;
    private final PlaceController places;
    private boolean importView;
    private boolean embeddedView;
    private boolean optionalView;
    private HandlerManager handlerManager;
    private Label unsupportedEditor;
    /** Holds the last used editor type. */
    private DatastreamEditorType lastEditorType;

    public DigitalObjectEditor(ClientMessages i18n, PlaceController places) {
        this(i18n, places, false);
    }

    public DigitalObjectEditor(ClientMessages i18n, PlaceController places, boolean embedded) {
        this.i18n = i18n;
        this.places = places;
        this.editorCache = new EnumMap<DatastreamEditorType, EditorDescriptor>(DatastreamEditorType.class);
        this.widget = new VLayout();
        this.lblHeader = new Label();
        lblHeader.setAutoHeight();
        lblHeader.setPadding(4);
        lblHeader.setStyleName(Editor.CSS_PANEL_DESCRIPTION_TITLE);
        this.actionSource = new ActionSource(this);
        this.embeddedView = embedded;
        this.toolbar = Actions.createToolStrip();
        this.editorContainer = new VLayout();
        editorContainer.setLayoutMargin(4);
        editorContainer.setWidth100();
        editorContainer.setHeight100();

        widget.addMember(lblHeader);
        widget.addMember(toolbar);

        if (embedded) {
            widget.addMember(editorContainer);
        } else {
            editorContainer.setResizeBarTarget("next");
            HLayout multiView = new HLayout();
            multiView.setWidth100();
            multiView.setHeight100();
            multiView.setLayoutMargin(4);
            multiView.addMember(editorContainer);
            initOptionalEditor(multiView);
            widget.addMember(multiView);
        }
    }

    private void initOptionalEditor(Layout multiView) {
        if (embeddedView) {
            return ;
        }
        editorContainer.setShowResizeBar(true);
        VLayout optionalEditorInnerContainer = new VLayout();
        optionalEditorContainer = new VLayout();
        optionalEditor = new OptionalEditor(i18n, optionalEditorInnerContainer);
        optionalEditorInnerContainer.addStyleName("defaultBorder");
        optionalEditorContainer.setLayoutMargin(4);
        optionalEditorContainer.setWidth100();
        optionalEditorContainer.setVisible(false);
        optionalEditorContainer.setMinMemberSize(200);
        optionalEditorContainer.setMembers(optionalEditorInnerContainer);
        multiView.addMember(optionalEditorContainer);
        optionalEditorContainer.addDrawHandler(new DrawHandler() {

            @Override
            public void onDraw(DrawEvent event) {
//                LOG.warning("onDraw: " + widget.getID() + ", drawn: " + optionalEditorContainer.isDrawn()
//                        + ", isVisible: " + optionalEditorContainer.isVisible()
//                        + ", isSelected: " + optionalEditorSwitch.isSelected()
//                        );
                if (optionalEditor.isEnabled()) {
                    // Ignore events thrown while the editor is enabled as it breaks
                    // the layout hierarchy in case browser history usage.
                    // onDraw is necessary as VisibilityChangedEvent is not fired
                    // before drawing a widget.
                    return ;
                }
                switchOptionalEditor(true);
            }
        });
        // switch the editor on/off by clicking the resize bar
        optionalEditorContainer.addVisibilityChangedHandler(new VisibilityChangedHandler() {

            @Override
            public void onVisibilityChanged(VisibilityChangedEvent event) {
//                LOG.warning("onVisibilityChanged: " + widget.getID() + ", visible: " + event.getIsVisible());
                switchOptionalEditor(event.getIsVisible());
            }
        });
    }

    public Canvas getUI() {
        return widget;
    }

    /**
     * Shows UI for the passed editor type and fetches digital objects.
     * @param type optional type. If {@code null} the last type or a reasonable default is used
     * @param pids digital objects to fetch
     */
    public void edit(DatastreamEditorType type, Record[] pids) {
        if (pids == null || pids.length == 0) {
            // this should occur just in case someone breakes URL in browser.
            notifyMissingPid(type);
            return ;
        }

        OpenEditorTask task = new OpenEditorTask(pids);
        edit(type, task, pids.length > 1);
    }

    /**
     * {@link #edit(cz.cas.lib.proarc.common.object.model.DatastreamEditorType, com.smartgwt.client.data.Record[])
     * description}
     */
    public void edit(DatastreamEditorType type, String... pids) {
        if (pids.length == 0) {
            // this should occur just in case someone breakes URL in browser.
            notifyMissingPid(type);
            return ;
        }

        OpenEditorTask task = new OpenEditorTask(pids);
        edit(type, task, pids.length > 1);
    }

    private void notifyMissingPid(DatastreamEditorType type) {
        ClientUtils.severe(LOG, "invalid edit parameters: %s, no pid", type);
        SC.warn("Invalid URL!");
        places.goTo(Place.NOWHERE);
    }

    private void edit(DatastreamEditorType type, OpenEditorTask task, boolean multiselection) {
        this.selection = null;
        if (type == null) {
//            ClientUtils.warning(LOG, "missing type, objects: %s", task.toString());
            // reasonable default
            if (lastEditorType != null) {
                type = lastEditorType;
            } else if (optionalView) {
                type = multiselection ? DatastreamEditorType.PARENT : DatastreamEditorType.MEDIA;
            } else {
                type = multiselection ? DatastreamEditorType.PARENT : DatastreamEditorType.MODS;
            }
        }
        lastEditorType = type;

        EditorDescriptor previousEditor = currentEditor;
        currentEditor = getDatastreamEditor(type);
        updateToolbar(previousEditor, currentEditor);
        task.start();
    }

    public void focus() {
        if (currentEditor != null) {
            currentEditor.getEditor().focus();
        }
    }

    private void setSelection(Record[] records) {
        this.selection = records;
        actionSource.fireEvent();
    }

    private void openEditor() {
        Scheduler.get().scheduleDeferred(new ScheduledCommand() {

            @Override
            public void execute() {
                openEditorImpl();
            }
        });
    }

    private void openEditorImpl() {
        final DatastreamEditor editor = currentEditor.getEditor();
        final Record[] records = getSelection();
        DigitalObject[] dobjs = records == null ? new DigitalObject[0] : DigitalObject.toArray(records);
        if (dobjs.length > 1) {
            setDescription(currentEditor.getTitle(),
                    i18n.DigitalObjectEditor_MultiSelection_Title(String.valueOf(dobjs.length)),
                    null);
            BatchDatastreamEditor beditor = editor.getCapability(BatchDatastreamEditor.class);
            if (beditor != null) {
                beditor.edit(DigitalObject.toArray(records));
                ClientUtils.setMembers(editorContainer, editor.getUI());
            } else {
                openUnsupportedEditor();
            }
        } else {
            MetaModelRecord model = dobjs[0].getModel();
            setDescription(currentEditor.getTitle(), getLabel(records[0]), model);
            if (model.isSupportedDatastream(currentEditor.getType().name())) {
                editor.edit(dobjs[0]);
                ClientUtils.setMembers(editorContainer, editor.getUI());
            } else {
                openUnsupportedEditor();
            }
        }
        openOptionalEditor(dobjs);
//        editorContainer.show();
    }

    private void openOptionalEditor(DigitalObject... dobj) {
        if (optionalEditor == null) {
            return ;
        }
        if (currentEditor != null && currentEditor.getType() == DatastreamEditorType.CHILDREN) {
            DigitalObject[] children = DigitalObject.toArray(getChildSelection());
            optionalEditor.open(children);
        } else {
            optionalEditor.open(dobj);
        }
    }

    private void openUnsupportedEditor() {
        if (currentEditor != null) {
            updateToolbar(currentEditor, null);
            currentEditor = null;
        }
        editorContainer.setMembers(getUnsupportedEditor());
    }

    private Label getUnsupportedEditor() {
        if (unsupportedEditor == null) {
            unsupportedEditor = new Label();
            unsupportedEditor.setContents(i18n.DigitalObjectEditor_UnsupportedEditor_Msg());
            unsupportedEditor.setIcon("[SKIN]/Dialog/warn.png");
            unsupportedEditor.setIconSize(2 * 16);
            unsupportedEditor.setLayoutAlign(Alignment.CENTER);
            unsupportedEditor.setLayoutAlign(VerticalAlignment.CENTER);
            unsupportedEditor.setHeight100();
            unsupportedEditor.setWidth100();
            unsupportedEditor.setAlign(Alignment.CENTER);
        }
        return unsupportedEditor;
    }

    @Override
    public void refresh() {
        if (currentEditor == null) {
            return ;
        }
        Refreshable r = currentEditor.getEditor().getCapability(Refreshable.class);
        if (r != null) {
            r.refresh();
        }
    }

    @Override
    public Record[] getSelection() {
        return selection;
    }

    @Override
    public Record[] getChildSelection() {
        Record[] result = null;
        if (currentEditor != null) {
            ChildSelector selector = currentEditor.getEditor().getCapability(ChildSelector.class);
            if (selector != null) {
                result = selector.getChildSelection();
            }
        }
        return result;
    }

    private void updateToolbar(EditorDescriptor oldEditor, EditorDescriptor newEditor) {
        if (customToolbarSeparator == null) {
            initToolbar(toolbar, actionSource);
        }
        if (oldEditor == newEditor) {
            return ;
        }
        if (oldEditor != null) {
            toolbar.removeMembers(oldEditor.getToolbarItems());
        }
        Canvas[] customToolbar = newEditor == null ? new Canvas[0] : newEditor.getToolbarItems();
        if (customToolbar.length > 0 && !(customToolbar[0] instanceof ToolStripSeparator)) {
            customToolbarSeparator.setVisible(true);
        } else {
            customToolbarSeparator.setVisible(false);
        }
        int addCustomIdx = toolbar.getMemberNumber(customToolbarSeparator);
        for (Canvas item : customToolbar) {
            toolbar.addMember(item, ++addCustomIdx);
        }
    }

    private ToolStrip initToolbar(ToolStrip t, ActionSource source) {
        RefreshAction refreshAction = new RefreshAction(i18n);
        DigitalObjectEditAction modsEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabMods_Title(),
                i18n.DigitalObjectEditAction_Hint(),
                null,
                DatastreamEditorType.MODS,
                embeddedView ? new AcceptFilter(true, true) : new AcceptFilter(false, false),
                places);
        DigitalObjectEditAction ocrEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabOcr_Title(),
                i18n.DigitalObjectEditAction_Hint(),
                null,
                DatastreamEditorType.OCR, places);
        DigitalObjectEditAction noteEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabNote_Title(),
                i18n.ImportBatchItemEditor_TabNote_Hint(),
                null,
                DatastreamEditorType.NOTE, places);
        DigitalObjectEditAction parentEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_ParentAction_Title(),
                i18n.DigitalObjectEditAction_Hint(),
                null,
                DatastreamEditorType.PARENT,
                // support multiple selection just in case DigitalObjectEditor is embeded
                embeddedView ? new AcceptFilter(true, true) : new AcceptFilter(false, false),
                places);
        DigitalObjectEditAction mediaEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_MediaAction_Title(),
                i18n.DigitalObjectEditor_MediaAction_Hint(),
                null,
                DatastreamEditorType.MEDIA, places);
        DigitalObjectEditAction childrenEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_ChildrenAction_Title(),
                i18n.DigitalObjectEditor_ChildrenAction_Hint(),
                null,
                DatastreamEditorType.CHILDREN, places);
        DigitalObjectEditAction atmEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_AdministrationAction_Title(),
                i18n.DigitalObjectEditor_AdministrationAction_Hint(),
                null,
                DatastreamEditorType.ATM,
                new AcceptFilter(true, true),
                places);
        if (embeddedView) {
            Action actionMore = Actions.emptyAction(i18n.ActionsMenu_Title(),
                    null, null);
            IconMenuButton actionsMenu = Actions.asIconMenuButton(actionMore, source);
            t.addMember(actionsMenu);
            Menu menu = Actions.createMenu();
            menu.addItem(Actions.asMenuItem(refreshAction, source, false));
//            if (!optionalView) {
                menu.addItem(Actions.asMenuItem(modsEditAction, source, false));
//            }
            menu.addItem(Actions.asMenuItem(noteEditAction, source, false));
            if (!importView/* && !optionalView*/) {
                menu.addItem(Actions.asMenuItem(parentEditAction, source, false));
            }
            if (!importView) {
                menu.addItem(Actions.asMenuItem(mediaEditAction, source, false));
            }
            menu.addItem(Actions.asMenuItem(ocrEditAction, source, false));
            menu.addItem(Actions.asMenuItem(atmEditAction, source, false));
            actionsMenu.setMenu(menu);
        } else {
            t.addMember(Actions.asIconButton(refreshAction, source));
            Action actionEditors = Actions.emptyAction(i18n.EditorsAction_Title(),
                    "[SKIN]/actions/edit.png", i18n.EditorsAction_Hint());
            IconMenuButton btnEditors = Actions.asIconMenuButton(actionEditors, source);
            Menu menuEditors = Actions.createMenu();
            menuEditors.addItem(Actions.asMenuItem(modsEditAction, source, false));
            menuEditors.addItem(Actions.asMenuItem(noteEditAction, source, false));
            menuEditors.addItem(Actions.asMenuItem(parentEditAction, source, false));
            menuEditors.addItem(Actions.asMenuItem(mediaEditAction, source, false));
            menuEditors.addItem(Actions.asMenuItem(ocrEditAction, source, false));
            menuEditors.addItem(Actions.asMenuItem(childrenEditAction, source, false));
            menuEditors.addItem(Actions.asMenuItem(atmEditAction, source, false));
            btnEditors.setMenu(menuEditors);
            t.addMember(btnEditors);
            DigitalObjectNavigateAction parentAction = DigitalObjectNavigateAction.parent(i18n, places);
            DigitalObjectNavigateAction childAction = DigitalObjectNavigateAction.child(i18n, places);
            DigitalObjectNavigateAction prevSiblingAction = DigitalObjectNavigateAction.previous(i18n, places);
            DigitalObjectNavigateAction nextSiblingAction = DigitalObjectNavigateAction.next(i18n, places);
            t.addMember(Actions.asIconButton(parentAction, source));
            t.addMember(Actions.asIconButton(childAction, source));
            t.addMember(Actions.asIconButton(prevSiblingAction, source));
            t.addMember(Actions.asIconButton(nextSiblingAction, source));
        }
        customToolbarSeparator = new ToolStripSeparator();
        customToolbarSeparator.setVisible(false);
        t.addMember(customToolbarSeparator);
        if (!embeddedView) {
            t.addFill();
            optionalEditorSwitch = Actions.asIconButton(new SwitchOptionalEditorAction(), source);
            optionalEditorSwitch.setSelected(false);
            optionalEditorSwitch.setActionType(SelectionType.CHECKBOX);
            optionalEditorSwitch.setShowSelectedIcon(true);
            t.addMember(optionalEditorSwitch);
        }
        return t;
    }

    private void switchOptionalEditor(boolean state) {
        if (optionalEditor == null || state == optionalEditor.isEnabled()) {
            return ;
        }
        optionalEditorSwitch.setSelected(state);
        optionalEditor.setEnabled(optionalEditorSwitch.getSelected());
        if (state) {
            DigitalObject[] selection = DigitalObject.toArray(getSelection());
            openOptionalEditor(selection);
        }
    }

    /**
     * Use to customize editor for batch import items.
     */
    public void setImportView(boolean importView) {
        this.importView = importView;
    }

    /**
     * Use to customize editor as an embedded view.
     */
    public void setEmbeddedView(boolean embeddedView) {
        this.embeddedView = embeddedView;
    }

    /**
     * Use to customize editor as an optional view.
     */
    public void setOptionalView(boolean optionalView) {
        this.optionalView = optionalView;
    }

    private EditorDescriptor getDatastreamEditor(DatastreamEditorType type) {
        EditorDescriptor desc = editorCache.get(type);
        if (desc != null) {
            return desc;
        }
        DatastreamEditor deditor = null;
        String title = "";
        switch (type) {
            case OCR:
                title = i18n.ImportBatchItemEditor_TabOcr_Title();
                deditor = TextEditor.ocr(i18n);
                break;
            case NOTE:
                title = i18n.ImportBatchItemEditor_TabNote_Title();
                deditor = TextEditor.note(i18n);
                break;
            case MEDIA:
                title = i18n.DigitalObjectEditor_MediaEditor_Title();
                deditor = new MediaEditor(i18n, this.getClass().getName());
                break;
            case MODS:
                title = i18n.ImportBatchItemEditor_TabMods_Title();
                deditor = new ModsMultiEditor(i18n);
                break;
            case PARENT:
                title = i18n.DigitalObjectEditor_ParentEditor_Title();
                deditor = new DigitalObjectParentEditor(i18n);
                break;
            case CHILDREN:
                title = i18n.DigitalObjectEditor_ChildrenEditor_Title();
                deditor = new DigitalObjectChildrenEditor(i18n, places, optionalEditor);
                break;
            case ATM:
                title = i18n.DigitalObjectEditor_AdministrationEditor_Title();
                deditor = new DigitalObjectAdministrationEditor(i18n);
                break;
        }
        title = ClientUtils.format("<b>%s</b>", title);
        desc = new EditorDescriptor(deditor, title, type);
        editorCache.put(type, desc);
        attachDatastreamEditor(deditor);
        return desc;
    }

    /**
     * Forwards editor events.
     */
    private void attachDatastreamEditor(DatastreamEditor deditor) {
        if (deditor instanceof HasEditorLoadHandlers) {
            ((HasEditorLoadHandlers) deditor).addEditorLoadHandler(new EditorLoadHandler() {

                @Override
                public void onEditorLoad(EditorLoadEvent evt) {
                    fireEvent(evt);
                }
            });
        }
    }

    private void setDescription(String editorTitle, String objectLabel, MetaModelRecord mr) {
        String content;
        if (mr != null) {
            // Editor Name - Model - Label
            String model = mr.getDisplayName();
            content = ClientUtils.format("%s - %s: %s", editorTitle, model, objectLabel);
        } else {
            // Editor Name - Label
            content = ClientUtils.format("%s: %s", editorTitle, objectLabel);
        }
        lblHeader.setContents(content);
    }

    private String getLabel(Record r) {
        return r == null ? "[ERROR]" : r.getAttribute(SearchDataSource.FIELD_LABEL);
    }

    @Override
    public HandlerRegistration addEditorLoadHandler(EditorLoadHandler handler) {
        return ensureHandlers().addHandler(EditorLoadEvent.TYPE, handler);
    }

    @Override
    public void fireEvent(GwtEvent<?> event) {
        if (handlerManager != null) {
            handlerManager.fireEvent(event);
        }
    }

    private HandlerManager ensureHandlers() {
        return handlerManager == null ? handlerManager = createHandlerManager()
                : handlerManager;
    }

    private HandlerManager createHandlerManager() {
        return new HandlerManager(this);
    }

    /** Holds already created editor and its toolbar */
    private static final class EditorDescriptor {

        private final DatastreamEditor editor;
        private final Canvas[] toolbarItems;
        private final String title;
        private final DatastreamEditorType type;

        EditorDescriptor(DatastreamEditor editor, String title, DatastreamEditorType type) {
            this.editor = editor;
            toolbarItems = editor.getToolbarItems();
            this.title = title;
            this.type = type;
        }

        public DatastreamEditor getEditor() {
            return editor;
        }

        public DatastreamEditorType getType() {
            return type;
        }

        public String getTitle() {
            return title;
        }

        public Canvas[] getToolbarItems() {
            return toolbarItems;
        }

    }

    /**
     * Opens editor when object description and model object are fetched.
     */
    private final class OpenEditorTask extends SweepTask implements Callback<ResultSet, Void> {

        private RecordList searchList;
        private ResultSet modelResultSet;
        private final String[] pids;
        private final Record[] digitalObjects;

        /**
         * It will fetch model and other attributes for each PID.
         */
        public OpenEditorTask(String[] pids) {
            this(null, pids);
        }

        /**
         * No fetch. It will use records as digital objects.
         */
        public OpenEditorTask(Record[] digitalObjects) {
            this(digitalObjects, null);
        }

        private OpenEditorTask(Record[] digitalObjects, String[] pids) {
            this.pids = pids;
            this.digitalObjects = digitalObjects;
        }

        public void start() {
            expect();
            if (pids != null) {
                initSearchList(pids);
            }
            initModels();
            release();
        }

        private void initModels() {
            expect();
            MetaModelDataSource.getModels(false, this);
        }

        private void initSearchList(String[] pids) {
            expect();
            SearchDataSource.getInstance().find(pids, new Callback<ResultSet, Void>() {

                @Override
                public void onFailure(Void reason) {
                    searchList = new RecordList();
                    release();
                }

                @Override
                public void onSuccess(ResultSet result) {
                    searchList = result;
                    release();
                }
            });
        }

        /**
         * Collects responses and opens editor.
         */
        @Override
        protected void processing() {
            Record[] records = processRecords();
            if (records == null) {
                return ;
            }
            setSelection(records);
            openEditor();
        }

        private Record[] processRecords() {
            Record[] records;
            if (pids != null) {
                records = searchList.toArray();
                String error = checkSearchedRecordsConsistency(records);
                if (error != null) {
                    SC.warn(error);
                    places.goTo(Place.NOWHERE);
                    return null;
                }
            } else {
                records = digitalObjects;
            }
            return records;
        }

        private String checkSearchedRecordsConsistency(Record[] records) {
            String error = null;
            HashSet<String> pidSet = new HashSet<String>(Arrays.asList(pids));
            for (Record record : records) {
                String recordPid = record.getAttribute(SearchDataSource.FIELD_PID);
                if (!pidSet.remove(recordPid)) {
                    error = ClientUtils.format("PID %s not requested!", recordPid);
                    break;
                } else if (SearchDataSource.isDeleted(record)) {
                    error = ClientUtils.format("PID %s is deleted!", recordPid);
                    break;
                }
            }
            if (error == null && !pidSet.isEmpty()) {
                error = ClientUtils.format("PID %s not found!", pidSet.toString());
            }
            return error;
        }

        @Override
        public void onFailure(Void reason) {
            modelResultSet = new ResultSet();
            release();
        }

        @Override
        public void onSuccess(ResultSet result) {
            modelResultSet = result;
            release();
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            if (pids != null) {
                sb.append("pids: ").append(Arrays.toString(pids));
            } else {
                String[] recordPids = ClientUtils.toFieldValues(digitalObjects, SearchDataSource.FIELD_PID);
                sb.append("records: ").append(Arrays.toString(recordPids));
            }
            return super.toString();
        }
    }

    public static final class OptionalEditor {

        private final PlaceController embeddedPlaces;
        private boolean editorEnabled;

        private OptionalEditor(ClientMessages i18n, Layout previewContainer) {
            SimpleEventBus eventBus = new SimpleEventBus();
            embeddedPlaces = new PlaceController(eventBus);
            DigitalObjectEditor embeddedEditor = new DigitalObjectEditor(i18n, embeddedPlaces, true);
            embeddedEditor.setOptionalView(true);
            ActivityManager activityManager = new ActivityManager(
                    new ChildActivities(embeddedEditor), eventBus);
            activityManager.setDisplay(new ChildEditorDisplay(previewContainer));
        }

        public void open(DigitalObject... objects) {
            open(null, objects);
        }

        public void open(DatastreamEditorType editor, DigitalObject... objects) {
            if (!editorEnabled) {
                return ;
            }
            DigitalObject openObject = findRecentSelection(objects);
            if (openObject == null) {
                embeddedPlaces.goTo(Place.NOWHERE);
                return ;
            }
//            LOG.log(Level.SEVERE, "# openOptionalEditor: " + objects.length, new IllegalStateException(openObject.toString()));
            embeddedPlaces.goTo(new DigitalObjectEditorPlace(editor, openObject));
        }

        private DigitalObject findRecentSelection(DigitalObject... objects) {
            if (objects == null || objects.length == 0 || objects[0] == null) {
                return null;
            } else if (objects.length == 1) {
                return objects[0];
            }
            for (DigitalObject object : objects) {
                Record record = object.getRecord();
                Boolean isLastSelection = record.getAttributeAsBoolean(DigitalObjectChildrenEditor.LAST_CLICKED_ATTR);
                if (isLastSelection != null && isLastSelection) {
                    return object;
                }
            }
            return null;
        }

        public boolean isEnabled() {
            return editorEnabled;
        }

        public void setEnabled(boolean editorEnabled) {
            this.editorEnabled = editorEnabled;
        }
    }

    private final class SwitchOptionalEditorAction extends AbstractAction {

        public SwitchOptionalEditorAction() {
            super(i18n.DigitalObjectEditor_OptionalEditorAction_Title(),
                    null,
                    i18n.DigitalObjectEditor_OptionalEditorAction_Hint());
        }

        @Override
        public void performAction(ActionEvent event) {
            boolean visible = optionalEditorContainer.isVisible();
            // fires VisibilityChangedEvent
            // see optionalEditorContainer.addVisibilityChangedHandler in initOptionalEditor
            optionalEditorContainer.setVisible(!visible);
        }

    }

}
