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

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataArrivedEvent;
import com.smartgwt.client.data.events.DataArrivedHandler;
import com.smartgwt.client.types.PromptStyle;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.action.AbstractAction;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.Actions.ActionSource;
import cz.incad.pas.editor.client.action.DataStreamExportAction;
import cz.incad.pas.editor.client.action.DeleteAction;
import cz.incad.pas.editor.client.action.DeleteAction.Deletable;
import cz.incad.pas.editor.client.action.DigitalObjectEditAction;
import cz.incad.pas.editor.client.action.FoxmlViewAction;
import cz.incad.pas.editor.client.action.KrameriusExportAction;
import cz.incad.pas.editor.client.ds.DigitalObjectDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import cz.incad.pas.editor.client.widget.DigitalObjectSearchView;
import cz.incad.pas.editor.client.widget.DigitalObjectTreeView;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi.DatastreamEditorType;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * The component allows to search digital objects and perform actions on
 * search results.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectManager {

    private final ClientMessages i18n;
    private final VLayout widget;
    private final DigitalObjectSearchView foundView;
    private final DigitalObjectTreeView treeView;
    private FoxmlViewAction foxmlAction;
    private KrameriusExportAction krameriusExportAction;
    private DataStreamExportAction fullDataStreamExportAction;
    private DataStreamExportAction rawDataStreamExportAction;
    private DeleteAction deleteAction;
    private DigitalObjectEditAction ocrEditAction;
    private DigitalObjectEditAction noteEditAction;
    private DigitalObjectEditAction modsEditAction;
    private DigitalObjectEditAction parentEditAction;
    private DigitalObjectEditAction mediaEditAction;
    private boolean initialized;
    private ResultSet modelResultSet;

    public DigitalObjectManager(ClientMessages i18n) {
        this.i18n = i18n;
        widget = new VLayout(4);
        widget.setLayoutMargin(4);
        widget.setWidth100();
        widget.setHeight100();

        foundView = new DigitalObjectSearchView(i18n);
        foundView.getGrid().setSelectionType(SelectionStyle.MULTIPLE);
        final ActionSource listSource = new ActionSource(foundView);
        foundView.getGrid().addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                final ListGridRecord[] selectedRecords = foundView.getGrid().getSelectedRecords();
                listSource.fireEvent();
                if (selectedRecords != null && selectedRecords.length == 1) {
                    String pid = selectedRecords[0].getAttribute(RelationDataSource.FIELD_PID);
                    treeView.setRoot(pid);
                }
            }
        });

        treeView = new DigitalObjectTreeView(i18n);
        treeView.getTree().setSelectionType(SelectionStyle.MULTIPLE);
        final ActionSource treeSource = new ActionSource(treeView);
        treeView.getTree().addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                treeSource.fireEvent();
            }
        });

        widget.addMember(foundView.asWidget());
        widget.addMember(treeView.asWidget());
        createActions();
        initToolbar(foundView.getToolbar(), listSource);
        initToolbar(treeView.getToolbar(), treeSource);
        initContextMenu(foundView.getGrid().getContextMenu(), listSource);
        initContextMenu(treeView.getTree().getContextMenu(), treeSource);
        Actions.fixListGridContextMenu(foundView.getGrid());
        Actions.fixListGridContextMenu(treeView.getTree());
    }

    public void init() {
        if (initialized) {
            return ;
        }
        initialized = true;
        initModels();
        foundView.setModels(modelResultSet);
        treeView.setModels(modelResultSet);
        foundView.onShow();
        treeView.setRoot(null);
    }

    private void initModels() {
        if (modelResultSet != null) {
            return ;
        }
        modelResultSet = MetaModelDataSource.getModels(true);
        modelResultSet.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                Map valueMap = modelResultSet.getValueMap(
                        MetaModelDataSource.FIELD_PID, MetaModelDataSource.FIELD_DISPLAY_NAME);
                treeView.setModels(valueMap);
                foundView.setModels(valueMap);
            }
        });
    }

    public VLayout getUI() {
        return widget;
    }

    private void createActions() {
        foxmlAction = new FoxmlViewAction(i18n);
        krameriusExportAction = new KrameriusExportAction(i18n);
        fullDataStreamExportAction = DataStreamExportAction.full(i18n);
        rawDataStreamExportAction = DataStreamExportAction.raw(i18n);
        deleteAction = new DeleteAction(new MultiRecordDeletable(), i18n);
        ocrEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabOcr_Title(), DatastreamEditorType.OCR, i18n);
        noteEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabNote_Title(), DatastreamEditorType.NOTE, i18n);
        modsEditAction = new DigitalObjectEditAction(
                i18n.ImportBatchItemEditor_TabMods_Title(), DatastreamEditorType.MODS, i18n);
        parentEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_ParentAction_Title(), DatastreamEditorType.PARENT, i18n);
        mediaEditAction = new DigitalObjectEditAction(
                i18n.DigitalObjectEditor_MediaAction_Title(),
                i18n.DigitalObjectEditor_MediaAction_Hint(),
                DatastreamEditorType.MEDIA);
    }
    
    /**
     * export (Kramerius, Datastream), edit(MODS, Hierarchy), delete, view (Datastream)
     */
    private void initToolbar(ToolStrip toolbar, ActionSource actionSource) {
        final AbstractAction exportMenuAction = new AbstractAction(
                i18n.ExportsAction_Title(), "[SKIN]/actions/save.png", null) {

            @Override
            public boolean accept(ActionEvent event) {
                Object[] selection = Actions.getSelection(event);
                return selection != null && selection.length > 0;
            }
            
            @Override
            public void performAction(ActionEvent event) {
                // choose default action iff supported
            }
        };
        IconMenuButton btnExport = Actions.asIconMenuButton(exportMenuAction, actionSource);
        Menu menuExport = Actions.createMenu();
        menuExport.addItem(Actions.asMenuItem(krameriusExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(fullDataStreamExportAction, actionSource, false));
        menuExport.addItem(Actions.asMenuItem(rawDataStreamExportAction, actionSource, false));
        btnExport.setMenu(menuExport);

        toolbar.addSeparator();
        toolbar.addMember(Actions.asIconButton(modsEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(noteEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(parentEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(mediaEditAction, actionSource));
        toolbar.addMember(Actions.asIconButton(ocrEditAction, actionSource));
        toolbar.addSeparator();
        toolbar.addMember(Actions.asIconButton(foxmlAction, actionSource));
        toolbar.addMember(btnExport);
        toolbar.addMember(Actions.asIconButton(deleteAction, actionSource));
    }

    private void initContextMenu(Menu menu, ActionSource actionSource) {
        menu.addItem(Actions.asMenuItem(modsEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(noteEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(parentEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(mediaEditAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(ocrEditAction, actionSource, false));
        menu.addItem(new MenuItemSeparator());
        menu.addItem(Actions.asMenuItem(foxmlAction, actionSource, true));
        menu.addItem(new MenuItemSeparator());
        menu.addItem(Actions.asMenuItem(krameriusExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(fullDataStreamExportAction, actionSource, false));
        menu.addItem(Actions.asMenuItem(rawDataStreamExportAction, actionSource, false));
        menu.addItem(new MenuItemSeparator());
        menu.addItem(Actions.asMenuItem(deleteAction, actionSource, true));
    }

    /**
     * XXX ask for delete hierarchy, prune or setInactive
     * XXX setDeleted should use UPDATE instead of DELETE?
     */
    private final class MultiRecordDeletable implements Deletable {

        @Override
        public void delete(Object[] items) {
            ArrayList<String> pids = new ArrayList<String>(items.length);
            for (Object item : items) {
                if (item instanceof Record) {
                    String pid = ((Record) item).getAttribute(DigitalObjectDataSource.FIELD_PID);
                    if (pid != null) {
                        pids.add(pid);
                    }
                }
            }
            if (!pids.isEmpty()) {
                Record record = new Record();
                record.setAttribute(DigitalObjectDataSource.FIELD_PID,
                        pids.toArray(new String[pids.size()]));
                delete(record);
            }
        }

        private void delete(Record query) {
            HashMap<String, String> deleteParams = new HashMap<String, String>();
            deleteParams.put(DigitalObjectResourceApi.DELETE_PURGE_PARAM,
                    Boolean.FALSE.toString());
            deleteParams.put(DigitalObjectResourceApi.DELETE_HIERARCHY_PARAM,
                    Boolean.TRUE.toString());
            DSRequest dsRequest = new DSRequest();
            dsRequest.setPromptStyle(PromptStyle.DIALOG);
            dsRequest.setPrompt(i18n.DeleteAction_Deleting_Msg());
            dsRequest.setParams(deleteParams);
            DigitalObjectDataSource.getInstance().removeData(query, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    if (RestConfig.isStatusOk(response)) {
                        foundView.getGrid().invalidateCache();
                    }
                }
            }, dsRequest);
        }

    }

}
