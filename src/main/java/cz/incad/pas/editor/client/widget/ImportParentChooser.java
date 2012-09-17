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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionStyle;
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
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.action.AbstractAction;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.Selectable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import cz.incad.pas.editor.client.ds.SearchDataSource;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi.SearchType;

public final class ImportParentChooser extends VLayout {

    private final PasEditorMessages i18nPas;
    private ImportParentHandler handler;
    private final DigitalObjectSearchView foundView;
    private final DigitalObjectTreeView treeView;
    private final DynamicForm selectionForm;
    private AbstractAction selectParentAction;
    
    public ImportParentChooser(PasEditorMessages i18nPas) {
        super(4);
        this.i18nPas = i18nPas;
        setLayoutMargin(4);
        setWidth100();
        setHeight100();

        selectionForm = createSelectionForm();
        foundView = new DigitalObjectSearchView(i18nPas);
        treeView = new DigitalObjectTreeView(i18nPas);

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

        addMember(selectionForm);
        addMember(foundView.asWidget());
        addMember(treeView.asWidget());
        createActions();
        initToolbar(foundView.getToolbar(), foundView);
        initToolbar(treeView.getToolbar(), treeView);
        initContextMenu(foundView.getGrid().getContextMenu(), foundView);
        initContextMenu(treeView.getTree().getContextMenu(), treeView);
    }

    public void setHandler(ImportParentHandler handler) {
        this.handler = handler;
    }

    public void setDataSource(final String parentPid) {
        loadParentSelection(parentPid);
        treeView.loadModels();
        foundView.onShow();
        treeView.setRoot(parentPid);
    }

    public Record getSelectedParent() {
        return selectionForm.getValuesAsRecord();
    }

    private void loadParentSelection(final String parentPid) {
        if (parentPid == null) {
            selectionForm.clearValues();
            return ;
        }
        Criteria criteria = new Criteria(
                DigitalObjectResourceApi.SEARCH_TYPE_PARAM, SearchType.PIDS.toString());
        criteria.addCriteria(DigitalObjectResourceApi.SEARCH_PID_PARAM, parentPid);
        SearchDataSource.getInstance().fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    if (data != null && data.length > 0) {
                        selectionForm.editRecord(data[0]);
                    } else {
                        selectionForm.clearValues();
                    }
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
        form.setGroupTitle(i18nPas.ImportParentChooser_SelectionForm_Title());
        form.setIsGroup(true);
        form.setTitleWidth(1); // to compute real width of titles
        TextItem model = new TextItem(SearchDataSource.FIELD_MODEL,
                i18nPas.DigitalObjectSearchView_ListHeaderModel_Title());
        model.setOptionDataSource(MetaModelDataSource.getInstance());
        model.setValueField(MetaModelDataSource.FIELD_PID);
        model.setDisplayField(MetaModelDataSource.FIELD_DISPLAY_NAME);
        TextItem pid = new TextItem(SearchDataSource.FIELD_PID,
                i18nPas.DigitalObjectSearchView_ListHeaderPid_Title());
        TextItem label = new TextItem(SearchDataSource.FIELD_LABEL,
                i18nPas.DigitalObjectSearchView_ListHeaderLabel_Title());
        label.setWidth(400);
        ButtonItem clear = new ButtonItem("clear",
                i18nPas.ImportParentChooser_SelectionForm_Clear_Title());
        clear.setTooltip(i18nPas.ImportParentChooser_SelectionForm_Clear_Hint());
        clear.setStartRow(false);
        clear.setCanEdit(true);
        clear.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                form.clearValues();
                handler.onParentSelectionUpdated();
            }
        });
        form.setItems(label, model, pid, clear);
        return form;
    }

    private void createActions() {
        selectParentAction = new AbstractAction(
                i18nPas.ImportParentChooser_SelectAction_Title(),
                "[SKIN]/actions/approve.png",
                i18nPas.ImportParentChooser_SelectAction_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                Record[] selection = Actions.getSelection(event);
                if (selection != null && selection.length == 1) {
                    selectionForm.editRecord(selection[0]);
                    handler.onParentSelectionUpdated();
                }
            }
        };
    }

    private void initToolbar(ToolStrip toolbar, Selectable<Record> source) {
        toolbar.addMember(Actions.asIconButton(selectParentAction, source));
    }

    private void initContextMenu(Menu menu, Selectable<Record> source) {
        menu.addItem(Actions.asMenuItem(selectParentAction, source));
    }

    public interface ImportParentHandler {
        void onParentSelectionUpdated();
    }

}
