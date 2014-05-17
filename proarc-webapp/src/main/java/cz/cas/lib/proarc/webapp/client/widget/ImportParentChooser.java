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
package cz.cas.lib.proarc.webapp.client.widget;

import com.google.gwt.core.client.Callback;
import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
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
    private final SelectionView selectionView;
    private AbstractAction selectParentAction;
    private final VLayout widget;
    private Record newParent;
    private Record oldParent;
    private boolean loadFailed;
    private boolean parentOwnerCheck = false;
    
    public ImportParentChooser(ClientMessages i18n) {
        this.i18n = i18n;
        this.widget = new VLayout(4);
        widget.setWidth100();
        widget.setHeight100();
        widget.setOverflow(Overflow.AUTO);

        selectionView = createSelectionView(i18n);
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

        widget.addMember(selectionView);
        Canvas foundViewWidget = foundView.asWidget();
        foundViewWidget.setShowResizeBar(true);
        widget.addMember(foundViewWidget);
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

    /**
     * Sets whether check owner of parent object and logged user are same.
     * @param check
     */
    public void setParentOwnerCheck(boolean check) {
        this.parentOwnerCheck = check;
    }

    public void focus() {
        foundView.getGrid().focus();
    }

    private void init(String pid, String batchId) {
        oldParent = null;
        newParent = null;
        loadFailed = true;
        fetchModels(false);
        loadParentSelection(pid, batchId);
        foundView.onShow();
    }

    private void fetchModels(final boolean reload) {
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
                selectionView.setModels(valueMap);
                if (!reload) {
                    // init the view filter with the first modelId on first show
                    if (!valueMap.isEmpty()) {
                        Object firstModel = valueMap.keySet().iterator().next();
                        foundView.setFilterModel(firstModel);
                    }
                    foundView.refresh();
                }
            }
        });
    }

    /**
     * Gets origin parent selection.
     */
    public Record getOldParent() {
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
            selectionView.setSelection(null);
            return ;
        }

        SearchDataSource.getInstance().findParent(pid, batchId, new Callback<ResultSet, Void>() {

            @Override
            public void onFailure(Void reason) {
            }

            @Override
            public void onSuccess(ResultSet result) {
                if (result.isEmpty()) {
                    selectionView.setSelection(null);
                } else {
                    newParent = oldParent = result.first();
                    selectionView.setSelection(newParent);
                }
                loadFailed = false;
            }
        });
    }

    private SelectionView createSelectionView(ClientMessages i18n) {
        final SelectionView view = new SelectionView(i18n);
        view.addClearClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                setParentSelection(null);
            }
        });
        return view;
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
                    onParentSelection(selection[0]);
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

    private void onParentSelection(final Record selection) {
        String parentOwner = selection.getAttribute(SearchDataSource.FIELD_OWNER);
        String username = Editor.getInstance().getUser().getAttribute(UserDataSource.FIELD_USERNAME);
        if (parentOwnerCheck && !username.equals(parentOwner)) {
            SC.ask(i18n.ImportParentChooser_SelectAction_ParentOwnerCheck_Msg(),
                    new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    if (value != null && value) {
                        setParentSelection(selection);
                    }
                }
            });
        } else {
            setParentSelection(selection);
        }
    }

    private void setParentSelection(Record selection) {
        if (selection != null) {
            newParent = treeView.getTree().getTree().find(
                    RelationDataSource.FIELD_PID, selection.getAttribute(RelationDataSource.FIELD_PID));
        } else {
            newParent = null;
        }
        selectionView.setSelection(selection);
        handler.onParentSelectionUpdated();
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

    /**
     * Shows selected parent object.
     */
    private final static class SelectionView extends VLayout {

        private final Canvas selection;
        private LinkedHashMap<?, ?> models;
        private Record parentRecord;
        private final IButton clear;
        private final ClientMessages i18n;

        SelectionView(ClientMessages i18n) {
            this.i18n = i18n;
            setAutoHeight();
            setIsGroup(true);
            setGroupTitle(i18n.ImportParentChooser_SelectionForm_Title());
            setLayoutMargin(4);
            selection = new Canvas();
            selection.setWidth100();
            selection.setAutoHeight();
            selection.setMargin(4);
            selection.setCanSelectText(Boolean.TRUE);
            clear = new IButton(i18n.ImportParentChooser_SelectionForm_Clear_Title());
            clear.setTooltip(i18n.ImportParentChooser_SelectionForm_Clear_Hint());
            clear.setAutoFit(Boolean.TRUE);
            setMembers(selection, clear);
        }

        public HandlerRegistration addClearClickHandler(ClickHandler handler) {
            return clear.addClickHandler(handler);
        }

        private void setSelection(Record parentRecord) {
            this.parentRecord = parentRecord;
            if (parentRecord == null) {
                selection.setContents(i18n.ImportParentChooser_EmptySelection_Msg());
                return ;
            }
            String model = parentRecord.getAttribute(SearchDataSource.FIELD_MODEL);
            if (models != null) {
                Object obj = models.get(model);
                if (obj != null) {
                    model = String.valueOf(obj);
                }
            }
            selection.setContents(ClientUtils.format("%s: <b>%s</b>, %s",
                    model,
                    parentRecord.getAttribute(SearchDataSource.FIELD_LABEL),
                    parentRecord.getAttribute(SearchDataSource.FIELD_PID)
                    ));
        }

        public void setModels(LinkedHashMap<?, ?> models) {
            this.models = models;
            setSelection(parentRecord);
        }

    }


}
