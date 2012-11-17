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

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceController;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataArrivedEvent;
import com.smartgwt.client.data.events.DataArrivedHandler;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripSeparator;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.Actions.ActionSource;
import cz.incad.pas.editor.client.action.DigitalObjectEditAction;
import cz.incad.pas.editor.client.action.RefreshAction;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.action.Selectable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.SearchDataSource;
import cz.incad.pas.editor.client.widget.DatastreamEditor;
import cz.incad.pas.editor.client.widget.DigitalObjectParentEditor;
import cz.incad.pas.editor.client.widget.MediaEditor;
import cz.incad.pas.editor.client.widget.TextEditor;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi.DatastreamEditorType;
import java.util.EnumMap;
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
    private Record selection;
    private final EnumMap<DatastreamEditorType, EditorDescriptor> editorCache;
    private final ActionSource actionSource;
    private ResultSet modelResultSet;
    private String pid;
    private String modelId;
    private final PlaceController places;

    public DigitalObjectEditor(ClientMessages i18n, PlaceController places) {
        this.i18n = i18n;
        this.places = places;
        this.editorCache = new EnumMap<DatastreamEditorType, EditorDescriptor>(DatastreamEditorType.class);
        this.widget = new VLayout();
        this.lblHeader = new Label();
        lblHeader.setAutoHeight();
        lblHeader.setPadding(4);
        lblHeader.setStyleName("pasWizardTitle");
        this.actionSource = new ActionSource(this);
        this.toolbar = createToolbar(actionSource);
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

    public void edit(DatastreamEditorType type, Record selection) {
        this.selection = selection;
        pid = selection == null ? null : selection.getAttribute(SearchDataSource.FIELD_PID);
        if (type == null || pid == null) {
            // this should occur just in case someone breakes URL in browser.
            ClientUtils.severe(LOG, "invalid edit parameters: %s, %s, %s", type, pid, selection);
            SC.warn("Invalid URL!");
            places.goTo(Place.NOWHERE);
            return ;
        }
        modelId = selection.getAttribute(SearchDataSource.FIELD_MODEL);

        editorContainer.hide();

        initModels();

        EditorDescriptor previousEditor = currentEditor;
        currentEditor = getDatastreamEditor(type);
        updateToolbar(previousEditor, currentEditor);
        if (modelResultSet.lengthIsKnown()) {
            edit(pid, getModel(modelResultSet, modelId));
        }
    }

    private void initModels() {
        if (modelResultSet == null) {
            modelResultSet = MetaModelDataSource.getModels();
            modelResultSet.addDataArrivedHandler(new DataArrivedHandler() {

                @Override
                public void onDataArrived(DataArrivedEvent event) {
                    edit(pid, getModel(modelResultSet, modelId));
                }
            });
        }
    }

    private static MetaModelRecord getModel(ResultSet rs, String modelId) {
        MetaModelRecord m = MetaModelRecord.get(rs.findByKey(modelId));
        if (m == null) {
            throw new IllegalStateException("Invalid model ID: " + modelId);
        }
        return m;
    }

    private void edit(final String pid, final MetaModelRecord mr) {
        setDesctiption(currentEditor.getTitle(), selection, mr);
        actionSource.fireEvent();
        final DatastreamEditor editor = currentEditor.getEditor();
        Scheduler.get().scheduleDeferred(new ScheduledCommand() {

            @Override
            public void execute() {
                editor.edit(pid, null, mr);
                editorContainer.setMembers(editor.getUI());
                editorContainer.show();
            }
        });
    }

    @Override
    public void refresh() {
        Refreshable r = currentEditor.getEditor().getCapability(Refreshable.class);
        if (r != null) {
            r.refresh();
        }
    }

    @Override
    public Record[] getSelection() {
        return MetaModelDataSource.addModelObjectField(
                SearchDataSource.FIELD_MODEL, new Record[] { selection });
    }

    private void updateToolbar(EditorDescriptor oldEditor, EditorDescriptor newEditor) {
        if (oldEditor != null) {
            toolbar.removeMembers(oldEditor.getToolbarItems());
        }
        Canvas[] customToolbar = newEditor.getToolbarItems();
        if (customToolbar.length > 0 && !(customToolbar[0] instanceof ToolStripSeparator)) {
            customToolbarSeparator.setVisible(true);
        } else {
            customToolbarSeparator.setVisible(false);
        }
        for (Canvas item : customToolbar) {
            toolbar.addMember(item);
        }
    }

    private ToolStrip createToolbar(ActionSource source) {
        RefreshAction refreshAction = new RefreshAction(i18n);
        DigitalObjectEditAction modsEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabMods_Title(), DatastreamEditorType.MODS, i18n);
        DigitalObjectEditAction ocrEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabOcr_Title(), DatastreamEditorType.OCR, i18n);
        DigitalObjectEditAction noteEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabNote_Title(), DatastreamEditorType.NOTE, i18n);
        DigitalObjectEditAction parentEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_ParentAction_Title(), DatastreamEditorType.PARENT, i18n);
        DigitalObjectEditAction mediaEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_MediaAction_Title(),
                i18n.DigitalObjectEditor_MediaAction_Hint(),
                DatastreamEditorType.MEDIA);
        ToolStrip t = Actions.createToolStrip();
        t.addMember(Actions.asIconButton(refreshAction, source));
        t.addMember(Actions.asIconButton(modsEditAction, source));
        t.addMember(Actions.asIconButton(noteEditAction, source));
        t.addMember(Actions.asIconButton(parentEditAction, source));
        t.addMember(Actions.asIconButton(mediaEditAction, source));
        t.addMember(Actions.asIconButton(ocrEditAction, source));
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
        }
        title = ClientUtils.format("<b>%s</b>", title);
        desc = new EditorDescriptor(deditor, title);
        editorCache.put(type, desc);
        return desc;
    }

    private void setDesctiption(String editorTitle, Record r, MetaModelRecord mr) {
        // Editor Name - Model - Label
        String label = r.getAttribute(SearchDataSource.FIELD_LABEL);
        String modelId = r.getAttribute(SearchDataSource.FIELD_MODEL);
        String model = mr == null ? modelId : mr.getDisplayName();
        String content = ClientUtils.format("%s - %s: %s", editorTitle, model, label);
        lblHeader.setContents(content);
    }

    /** Holds already created editor and its toolbar */
    private static final class EditorDescriptor {

        private final DatastreamEditor editor;
        private final Canvas[] toolbarItems;
        private final String title;

        EditorDescriptor(DatastreamEditor editor, String title) {
            this.editor = editor;
            toolbarItems = editor.getToolbarItems();
            this.title = title;
        }

        public DatastreamEditor getEditor() {
            return editor;
        }

        public String getTitle() {
            return title;
        }

        public Canvas[] getToolbarItems() {
            return toolbarItems;
        }

    }

}
