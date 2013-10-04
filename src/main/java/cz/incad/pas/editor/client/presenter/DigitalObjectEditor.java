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
package cz.incad.pas.editor.client.presenter;

import com.google.gwt.core.client.Callback;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceController;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripSeparator;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.ClientUtils.SweepTask;
import cz.incad.pas.editor.client.action.AbstractAction;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.Actions.ActionSource;
import cz.incad.pas.editor.client.action.DigitalObjectEditAction;
import cz.incad.pas.editor.client.action.DigitalObjectEditAction.AcceptFilter;
import cz.incad.pas.editor.client.action.DigitalObjectOpenParentAction;
import cz.incad.pas.editor.client.action.RefreshAction;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.action.Selectable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.SearchDataSource;
import cz.incad.pas.editor.client.widget.BatchDatastreamEditor;
import cz.incad.pas.editor.client.widget.DatastreamEditor;
import cz.incad.pas.editor.client.widget.DigitalObjectChildrenEditor;
import cz.incad.pas.editor.client.widget.DigitalObjectParentEditor;
import cz.incad.pas.editor.client.widget.MediaEditor;
import cz.incad.pas.editor.client.widget.TextEditor;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi.DatastreamEditorType;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.logging.Logger;

/**
 * Edits digital object data streams.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectEditor implements Refreshable, Selectable<Record> {

    private static final Logger LOG = Logger.getLogger(DigitalObjectEditor.class.getName());
    private final ClientMessages i18n;
    private final VLayout widget;
    private final Label lblHeader;
    private final ToolStrip toolbar;
    private ToolStripSeparator customToolbarSeparator;
    private final VLayout editorContainer;
    private EditorDescriptor currentEditor;
    /** currently edited object {PID, MODEL_OBJECT}; should be replaced with some interface */
    private Record[] selection;
    private final EnumMap<DatastreamEditorType, EditorDescriptor> editorCache;
    private final ActionSource actionSource;
    private final PlaceController places;

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
        lblHeader.setStyleName("pasWizardTitle");
        this.actionSource = new ActionSource(this);
        this.toolbar = createToolbar(actionSource, embedded);
        this.editorContainer = new VLayout();
        editorContainer.setLayoutMargin(4);
        editorContainer.setWidth100();
        editorContainer.setHeight100();
        widget.addMember(lblHeader);
        widget.addMember(toolbar);
        widget.addMember(editorContainer);
    }

    public Canvas getUI() {
        return widget;
    }

    public void edit(DatastreamEditorType type, Record[] pids) {
        if (pids == null || pids.length == 0) {
            // this should occur just in case someone breakes URL in browser.
            notifyMissingPid(type);
            return ;
        }

        OpenEditorTask task = new OpenEditorTask(pids);
        edit(type, task, pids.length > 1);
    }

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
            ClientUtils.warning(LOG, "missing type, objects: %s", task.toString());
            // reasonable default
            type = multiselection ? DatastreamEditorType.PARENT : DatastreamEditorType.MODS;
        }

        editorContainer.hide();

        EditorDescriptor previousEditor = currentEditor;
        currentEditor = getDatastreamEditor(type);
        updateToolbar(previousEditor, currentEditor);
        task.start();
    }

    private void setSelection(Record[] records) {
        this.selection = records;
        actionSource.fireEvent();
    }

    private void openEditor() {
        final DatastreamEditor editor = currentEditor.getEditor();
        final Record[] records = getSelection();
        Scheduler.get().scheduleDeferred(new ScheduledCommand() {

            @Override
            public void execute() {
                if (records.length > 1) {
                    BatchDatastreamEditor beditor = editor.getCapability(BatchDatastreamEditor.class);
                    if (beditor != null) {
                        beditor.edit(records, null);
                    }
                } else {
                    String pid = records[0].getAttribute(SearchDataSource.FIELD_PID);
                    MetaModelRecord mr = MetaModelDataSource.getModel(records[0]);
                    editor.edit(pid, null, mr);
                }
                editorContainer.setMembers(editor.getUI());
                editorContainer.show();
            }
        });
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

    private void updateToolbar(EditorDescriptor oldEditor, EditorDescriptor newEditor) {
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
        for (Canvas item : customToolbar) {
            toolbar.addMember(item);
        }
    }

    private ToolStrip createToolbar(ActionSource source, boolean tiny) {
        RefreshAction refreshAction = new RefreshAction(i18n);
        DigitalObjectEditAction modsEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabMods_Title(),
                i18n.DigitalObjectEditAction_Hint(),
                null,
                DatastreamEditorType.MODS, places);
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
                tiny ? new AcceptFilter(true, true) : new AcceptFilter(false, false),
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
        ToolStrip t = Actions.createToolStrip();
        if (tiny) {
            IconMenuButton actionsMenu = Actions.asIconMenuButton(new AbstractAction(
                    i18n.ActionsMenu_Title(), null, null) {

                @Override
                public void performAction(ActionEvent event) {
                    throw new UnsupportedOperationException("Not supported.");
                }
            }, source);
            t.addMember(actionsMenu);
            Menu menu = Actions.createMenu();
            menu.addItem(Actions.asMenuItem(refreshAction, source, false));
            menu.addItem(Actions.asMenuItem(modsEditAction, source, false));
            menu.addItem(Actions.asMenuItem(noteEditAction, source, false));
            menu.addItem(Actions.asMenuItem(parentEditAction, source, false));
            menu.addItem(Actions.asMenuItem(mediaEditAction, source, false));
            menu.addItem(Actions.asMenuItem(ocrEditAction, source, false));
            actionsMenu.setMenu(menu);
        } else {
            t.addMember(Actions.asIconButton(refreshAction, source));
            t.addMember(Actions.asIconButton(modsEditAction, source));
            t.addMember(Actions.asIconButton(noteEditAction, source));
            t.addMember(Actions.asIconButton(parentEditAction, source));
            t.addMember(Actions.asIconButton(mediaEditAction, source));
            t.addMember(Actions.asIconButton(ocrEditAction, source));
            t.addMember(Actions.asIconButton(childrenEditAction, source));
            DigitalObjectOpenParentAction openParentAction = new DigitalObjectOpenParentAction(i18n, places);
            t.addMember(Actions.asIconButton(openParentAction, source));
        }
        customToolbarSeparator = new ToolStripSeparator();
        customToolbarSeparator.setVisible(false);
        t.addMember(customToolbarSeparator);
        return t;
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
                deditor = new MediaEditor(i18n);
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
                deditor = new DigitalObjectChildrenEditor(i18n, places);
                break;
        }
        title = ClientUtils.format("<b>%s</b>", title);
        desc = new EditorDescriptor(deditor, title, type);
        editorCache.put(type, desc);
        return desc;
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
            if (records.length == 1) {
                MetaModelRecord model = MetaModelDataSource.getModel(records[0]);
                setDescription(currentEditor.getTitle(), getLabel(records[0]), model);
                if (!model.isSupportedDatastream(currentEditor.getType().name())) {
                    // XXX this should query current action, not model
                    updateToolbar(currentEditor, null);
                    currentEditor = null;
                    return ;
                }
            } else {
                setDescription(currentEditor.getTitle(),
                        i18n.DigitalObjectEditor_MultiSelection_Title(String.valueOf(records.length)),
                        null);
                BatchDatastreamEditor beditor = currentEditor.getEditor().getCapability(BatchDatastreamEditor.class);
                if (beditor == null) {
                    // let the user choose proper batch editor
                    updateToolbar(currentEditor, null);
                    currentEditor = null;
                    return ;
                }
            }
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

        private String getLabel(Record r) {
            return r == null ? "[ERROR]" : r.getAttribute(SearchDataSource.FIELD_LABEL);
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

}
