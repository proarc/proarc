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

import com.google.gwt.core.client.Callback;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.tree.TreeNode;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.action.AbstractAction;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.RefreshAction;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.action.Selectable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import cz.incad.pas.editor.client.ds.SearchDataSource;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi.SearchType;
import java.util.LinkedHashMap;

/**
 * UI to show and change parent for given digital object or import batch.
 * 
 * @author Jan Pokorsky
 */
public final class ImportParentChooser {

    private final ClientMessages i18n;
    private ImportParentHandler handler;
    private final DigitalObjectSearchView foundView;
    private final DigitalObjectTreeView treeView;
    private final DynamicForm selectionForm;
    private AbstractAction selectParentAction;
    private final VLayout widget;
    private Record newParent;
    private Record oldParent;
    private boolean loadFailed;
    
    public ImportParentChooser(ClientMessages i18n) {
        this.i18n = i18n;
        this.widget = new VLayout(4);
        widget.setLayoutMargin(4);
        widget.setWidth100();
        widget.setHeight100();

        selectionForm = createSelectionForm();
        foundView = new DigitalObjectSearchView(i18n);
        treeView = new DigitalObjectTreeView(i18n);

        foundView.getGrid().setSelectionType(SelectionStyle.SINGLE);
        foundView.getGrid().addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                final ListGridRecord selectedRecord = foundView.getGrid().getSelectedRecord();
                if (selectedRecord != null) {
                    String pid = selectedRecord.getAttribute(RelationDataSource.FIELD_PID);
                    treeView.setRoot(pid);
                }
            }
        });

        widget.addMember(selectionForm);
        widget.addMember(foundView.asWidget());
        widget.addMember(treeView.asWidget());
        createActions();
        initToolbar(foundView.getToolbar(), foundView);
        initToolbar(treeView.getToolbar(), treeView);
        initContextMenu(foundView.getGrid().getContextMenu(), foundView);
        initContextMenu(treeView.getTree().getContextMenu(), treeView);
    }

    /**
     * Registers listener for parent changes.
     *
     * @param handler change handler
     */
    public void setHandler(ImportParentHandler handler) {
        this.handler = handler;
    }

    /**
     * Sets digital object to show its parent.
     *
     * @param pid digital object ID
     */
    public void setDigitalObject(String pid) {
        init(pid, null);
    }

    /**
     * Sets import batch to show its parent.
     *
     * @param batchId import batch ID
     */
    public void setImport(String batchId) {
        init(null, batchId);
    }

    private void init(String pid, String batchId) {
        oldParent = null;
        newParent = null;
        loadFailed = true;
        fetchModels(false);
        loadParentSelection(pid, batchId);
        foundView.onShow();
    }

    private void fetchModels(boolean reload) {
        MetaModelDataSource.getModels(reload, new Callback<ResultSet, Void>() {

            @Override
            public void onFailure(Void reason) {
            }

            @Override
            public void onSuccess(ResultSet modelResultSet) {
                LinkedHashMap<?, ?> valueMap = ClientUtils.getValueMap(modelResultSet,
                        MetaModelDataSource.FIELD_PID, MetaModelDataSource.FIELD_DISPLAY_NAME);
                treeView.setModels(valueMap);
                foundView.setModels(valueMap);
                selectionForm.getField(SearchDataSource.FIELD_MODEL)
                        .setValueMap(valueMap);
            }
        });
    }

    /**
     * Gets origin parent selection. It tries to use record from TreeGrid
     * to propagate add/remove of record to view properly.
     */
    public Record getOldParent() {
        if (oldParent != null && oldParent.getAttribute(RelationDataSource.FIELD_ID) == null) {
            TreeNode node = treeView.getTree().getTree().find(RelationDataSource.FIELD_PID, asPid(oldParent));
            String id = node == null ? null : node.getAttribute(RelationDataSource.FIELD_ID);
            if (id != null) {
                oldParent.setAttribute(RelationDataSource.FIELD_ID, id);
            }
        }
        return oldParent;
    }

    /**
     * @see #getOldParent
     */
    public String getOldParentPid() {
        return asPid(oldParent);
    }

    /**
     * @see #getSelectedParent
     */
    public String getSelectedParentPid() {
        Record selectedParent = getSelectedParent();
        return selectedParent == null
                ? null
                : selectedParent.getAttribute(SearchDataSource.FIELD_PID);
    }
    
    /**
     * Gets current selection of parent object. It tries to use record from TreeGrid
     * to propagate add/remove of record to view properly.
     */
    public Record getSelectedParent() {
        return newParent;
    }

    /**
     * Resets old/new parent values. It sould be called after storing of current selection.
     * @param newParent
     */
    public void onSave(Record newParent) {
        this.newParent = newParent;
        this.oldParent = newParent;
    }

    private static String asPid(Record r) {
        return r == null ? null : r.getAttribute(RelationDataSource.FIELD_PID);
    }

    /**
     * Any change of parent object selection?
     */
    public boolean isChanged() {
        if (loadFailed) {
            return false;
        } else {
            String newPid = asPid(newParent);
            String oldPid = asPid(oldParent);
            return (oldPid == null ? newPid != null : !oldPid.equals(newPid));
        }
    }
    
    public Canvas getUI() {
        return widget;
    }

    private void loadParentSelection(final String pid, String batchId) {
        if (pid == null && batchId == null) {
            selectionForm.clearValues();
            return ;
        }
        Criteria criteria = new Criteria(
                DigitalObjectResourceApi.SEARCH_TYPE_PARAM, SearchType.PARENT.toString());
        if (pid != null && !pid.isEmpty()) {
            criteria.addCriteria(DigitalObjectResourceApi.SEARCH_PID_PARAM, pid);
        }
        if (batchId != null && !batchId.isEmpty()) {
            criteria.addCriteria(DigitalObjectResourceApi.SEARCH_BATCHID_PARAM, batchId);
        }
        SearchDataSource.getInstance().fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    if (data != null && data.length > 0) {
                        newParent = oldParent = data[0];
                        selectionForm.editRecord(data[0]);
                    } else {
                        selectionForm.clearValues();
                    }
                    loadFailed = false;
                }
            }
        });
    }

    private DynamicForm createSelectionForm() {
        final DynamicForm form = new DynamicForm();
        form.setAutoWidth();
        form.setNumCols(7);
        form.setBrowserSpellCheck(false);
        form.setCanEdit(false);
        form.setCanFocus(false);
        form.setGroupTitle(i18n.ImportParentChooser_SelectionForm_Title());
        form.setIsGroup(true);
        form.setTitleWidth(1); // to compute real width of titles
        TextItem model = new TextItem(SearchDataSource.FIELD_MODEL,
                i18n.DigitalObjectSearchView_ListHeaderModel_Title());
        TextItem pid = new TextItem(SearchDataSource.FIELD_PID,
                i18n.DigitalObjectSearchView_ListHeaderPid_Title());
        TextItem label = new TextItem(SearchDataSource.FIELD_LABEL,
                i18n.DigitalObjectSearchView_ListHeaderLabel_Title());
        label.setWidth(400);
        ButtonItem clear = new ButtonItem("clear",
                i18n.ImportParentChooser_SelectionForm_Clear_Title());
        clear.setTooltip(i18n.ImportParentChooser_SelectionForm_Clear_Hint());
        clear.setStartRow(false);
        clear.setCanEdit(true);
        clear.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                form.clearValues();
                newParent = null;
                handler.onParentSelectionUpdated();
            }
        });
        form.setItems(label, model, pid, clear);
        return form;
    }

    private void createActions() {
        selectParentAction = new AbstractAction(
                i18n.ImportParentChooser_SelectAction_Title(),
                "[SKIN]/actions/approve.png",
                i18n.ImportParentChooser_SelectAction_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                Record[] selection = Actions.getSelection(event);
                if (selection != null && selection.length == 1) {
                    newParent = treeView.getTree().getTree().find(
                            RelationDataSource.FIELD_PID, selection[0].getAttribute(RelationDataSource.FIELD_PID));
                    selectionForm.editRecord(selection[0]);
                    handler.onParentSelectionUpdated();
                }
            }
        };
    }

    private void initToolbar(ToolStrip toolbar, Selectable<Record> source) {
        toolbar.addMember(Actions.asIconButton(new RefreshAction(i18n),
                new RefreshableView((Refreshable) source)));
        toolbar.addMember(Actions.asIconButton(selectParentAction, source));
    }

    private void initContextMenu(Menu menu, Selectable<Record> source) {
        menu.addItem(Actions.asMenuItem(selectParentAction, source));
    }

    public interface ImportParentHandler {
        void onParentSelectionUpdated();
    }

    private final class RefreshableView implements Refreshable {

        private final Refreshable delegate;

        RefreshableView(Refreshable delegate) {
            this.delegate = delegate;
        }

        @Override
        public void refresh() {
            fetchModels(true);
            delegate.refresh();
        }
    }

}
