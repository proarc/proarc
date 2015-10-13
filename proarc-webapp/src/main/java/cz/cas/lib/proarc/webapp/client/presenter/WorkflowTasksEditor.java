/*
 * Copyright (C) 2015 Jan Pokorsky
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

import com.google.gwt.place.shared.PlaceController;
import com.google.gwt.user.client.ui.Widget;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.AutoFitTextAreaItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowTaskDataSource;

/**
 * Edits tasks of the workflow.
 *
 * @author Jan Pokorsky
 */
public class WorkflowTasksEditor {

    private final ClientMessages i18n;
    private WorkflowTasksView view;

    public WorkflowTasksEditor(ClientMessages i18n, PlaceController places) {
        this.i18n = i18n;
    }

    public Canvas getUI() {
        if (view == null) {
            view = new WorkflowTasksView(i18n);
            view.init();
        }
        return view.getWidget();
    }

    private static final class WorkflowTasksView {

        private final ClientMessages i18n;
        private final Canvas widget;
        private ListGrid taskGrid;
        private WorkflowTaskFormView taskFormView;

        public WorkflowTasksView(ClientMessages i18n) {
            this.i18n = i18n;
            this.widget = createMainLayout();
        }

        public Canvas getWidget() {
            return widget;
        }

        public void init() {
            taskGrid.fetchData();
        }

        private Canvas createMainLayout() {
            VLayout main = new VLayout();
            main.addMember(createPanelLabel());
            // filter
            main.addMember(createFilter());
            // toolbar
            main.addMember(createJobsToolbar());
            // list + item
            main.addMember(createJobLayout());
            return main;
        }

        private Canvas createJobLayout() {
            HLayout l = new HLayout();
            l.addMember(createTaskList());
            l.addMember(createTaskFormLayout());
            return l;
        }

        private Label createPanelLabel() {
            Label lblHeader = new Label();
            String title = ClientUtils.format("<b>%s</b>", "Správa úkolů");//i18n.DigitalObjectManager_Title());
            lblHeader.setContents(title);
            lblHeader.setAutoHeight();
            lblHeader.setPadding(4);
            lblHeader.setStyleName(Editor.CSS_PANEL_DESCRIPTION_TITLE);
            return lblHeader;
        }

        private DynamicForm createFilter() {
            DynamicForm form = new DynamicForm();
            form.setBrowserSpellCheck(false);
            form.setValidateOnExit(true);
            form.setSaveOnEnter(true);
            form.setAutoHeight();
            form.setWidth100();
            form.setNumCols(3);
            // ????
            return form;
        }

        private ToolStrip createJobsToolbar() {
            ToolStrip toolbar = Actions.createToolStrip();
            RefreshAction refreshAction = new RefreshAction(i18n);
            SaveAction saveAction = new SaveAction(i18n) {

                @Override
                public void performAction(ActionEvent event) {
                }
            };

            AbstractAction addAction = new AbstractAction("Nový",//i18n.DeviceManager_Add_Title(),
                    "[SKIN]/actions/add.png", "Nový záměr") {//i18n.DeviceManager_Add_Hint()) {

                @Override
                public void performAction(ActionEvent event) {
                }
            };
            toolbar.addMember(Actions.asIconButton(refreshAction, this));
//            toolbar.addMember(Actions.asIconButton(addAction, this));
            toolbar.addMember(Actions.asIconButton(saveAction, this));
            return toolbar;
        }

        private ListGrid createTaskList() {
            final ListGrid grid = new ListGrid();
            grid.setDataSource(WorkflowTaskDataSource.getInstance(),
                    new ListGridField(WorkflowTaskDataSource.FIELD_LABEL, 150),
                    new ListGridField(WorkflowTaskDataSource.FIELD_TYPE),
                    new ListGridField(WorkflowTaskDataSource.FIELD_STATE),
                    new ListGridField(WorkflowTaskDataSource.FIELD_PRIORITY),
                    new ListGridField(WorkflowTaskDataSource.FIELD_OWNER),
                    new ListGridField(WorkflowTaskDataSource.FIELD_CREATED),
                    new ListGridField(WorkflowTaskDataSource.FIELD_MODIFIED),
                    new ListGridField(WorkflowTaskDataSource.FIELD_ID),
                    new ListGridField(WorkflowTaskDataSource.FIELD_NOTE),
                    new ListGridField(WorkflowTaskDataSource.FIELD_JOB_ID),
                    new ListGridField(WorkflowTaskDataSource.FIELD_JOB_LABEL)
                    );

            grid.addDataArrivedHandler(new DataArrivedHandler() {

                @Override
                public void onDataArrived(DataArrivedEvent event) {
                    int startRow = event.getStartRow();
                    int endRow = event.getEndRow();
                    if (startRow == 0 && endRow >= 0) {
                        grid.focus();
                        grid.selectSingleRecord(0);
                    } else if (endRow < 0) {
                        grid.deselectAllRecords();
                    }
                }
            });
            grid.setSelectionType(SelectionStyle.SINGLE);
            grid.setCanSort(false);
            grid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

                @Override
                public void onSelectionUpdated(SelectionUpdatedEvent event) {
                    taskFormView.setTask(taskGrid.getSelectedRecord());
                }
            });
            taskGrid = grid;
            return grid;
        }

        private Canvas createTaskFormLayout() {
            taskFormView = new WorkflowTaskFormView(i18n);
            return taskFormView.getWidget();
        }

    }

    private static final class WorkflowTaskFormView {

        private final ClientMessages i18n;
        private final Canvas widget;
        private DynamicForm taskForm;
        private ListGrid paramGrid;
        private ListGrid materialGrid;

        public WorkflowTaskFormView(ClientMessages i18n) {
            this.i18n = i18n;
            this.widget = createMainLayout();
        }

        public Canvas getWidget() {
            return widget;
        }

        public void setTask(Record task) {
            if (task != null) {
                taskForm.clearErrors(true);
                taskForm.editRecord(task);
                setParameters(task.getAttributeAsRecordArray(WorkflowTaskDataSource.FIELD_PARAMETERS));
                setMaterials(task.getAttributeAsRecordArray(WorkflowTaskDataSource.FIELD_MATERIALS));
            } else {
                taskForm.clearValues();
                setParameters(new Record[0]);
                setMaterials(new Record[0]);
            }
//            taskForm.fetchData();
        }

        private void setParameters(Record[] records) {
            paramGrid.setData(records);
        }

        private void setMaterials(Record[] records) {
            materialGrid.setData(records);
        }

        private Canvas createMainLayout() {
            VLayout main = new VLayout();
            main.addMember(createForm());
            main.addMember(createParameterList());
            main.addMember(createMaterialList());
            return main;
        }

        private Widget createForm() {
            taskForm = new DynamicForm();
            taskForm.setDataSource(WorkflowTaskDataSource.getInstance());
            taskForm.setNumCols(3);
            taskForm.setColWidths("*", "*", "*");
            taskForm.setTitleOrientation(TitleOrientation.TOP);

            StaticTextItem jobLabel = new StaticTextItem(WorkflowTaskDataSource.FIELD_JOB_LABEL);
            jobLabel.setColSpan("*");
            jobLabel.setWidth("*");
            jobLabel.setShowTitle(false);
            jobLabel.setTextBoxStyle(Editor.CSS_PANEL_DESCRIPTION_TITLE);

            SelectItem owner = new SelectItem(WorkflowTaskDataSource.FIELD_OWNER);
            owner.setOptionDataSource(UserDataSource.getInstance());
            owner.setValueField(UserDataSource.FIELD_USERNAME);
            owner.setDisplayField(UserDataSource.FIELD_USERNAME);
            owner.setWidth("*");

            AutoFitTextAreaItem note = new AutoFitTextAreaItem(WorkflowTaskDataSource.FIELD_NOTE);
            note.setStartRow(true);
            note.setColSpan("*");
            note.setWidth("*");

            SelectItem priority = new SelectItem(WorkflowTaskDataSource.FIELD_PRIORITY);

            TextItem label = new TextItem(WorkflowTaskDataSource.FIELD_LABEL);
            label.setWidth("*");

            taskForm.setFields(jobLabel,
                    label,
                    new TextItem(WorkflowTaskDataSource.FIELD_ID),
                    new TextItem(WorkflowTaskDataSource.FIELD_TYPE),
                    owner,
                    new SelectItem(WorkflowTaskDataSource.FIELD_STATE),
                    priority,
                    new TextItem(WorkflowTaskDataSource.FIELD_CREATED),
                    new TextItem(WorkflowTaskDataSource.FIELD_MODIFIED),
                    note);
            return taskForm;
        }

        private Widget createParameterList() {
            paramGrid = new ListGrid();
            ListGridField value = new ListGridField("value","Hodnota");
            value.setCanEdit(true);
            paramGrid.setFields(new ListGridField("label", "Název parametru"), value);
            return paramGrid;
        }

        private Widget createMaterialList() {
            materialGrid = new ListGrid() {

                @Override
                protected Canvas getExpansionComponent(final ListGridRecord record) {
                    String type = record.getAttribute("type");
                    DynamicForm form = null;
                    if ("folder".equals(type)) {
                        form = createFolderForm();
                    } else if ("physicalDocument".equals(type)) {
                        form = createPhysicalDocumentForm();
                    } else if ("digitalDocument".equals(type)) {
                        form = createDigitalDocumentForm();
                    }
                    if (form != null) {
                        return bindExpansinonForm(form, record);
                    } else {
                        return super.getExpansionComponent(record);
                    }
                }

            };
            materialGrid.setFields(
                    new ListGridField("label","Název materiálu"),
                    new ListGridField("type", "Typ"),
                    new ListGridField("value", "Hodnota"),
                    new ListGridField("note", "Poznámka")
            );
            materialGrid.setExpansionMode(ExpansionMode.DETAIL_FIELD);
            materialGrid.setCanExpandRecords(true);
            return materialGrid;
        }

        private DynamicForm bindExpansinonForm(final DynamicForm form, final Record record) {
            form.addDrawHandler(new DrawHandler() {

                @Override
                public void onDraw(DrawEvent event) {
                    form.editRecord(record);
                }
            });
            return form;
        }

        private DynamicForm createExpansionForm() {
            DynamicForm form = new DynamicForm();
            return form;
        }

        private DynamicForm createFolderForm() {
            DynamicForm form = createExpansionForm();
            TextItem path = new TextItem("path", "Cesta");
            path.setWidth("*");
            form.setItems(path);
            return form;
        }

        private DynamicForm createPhysicalDocumentForm() {
            DynamicForm form = createExpansionForm();
            TextAreaItem xml = new TextAreaItem("xml", "XML");
            xml.setWidth("*");
            form.setItems(new TextItem("barCode", "Čárový kód"),
                    new TextItem("field001", "Pole 001"),
                    new TextItem("rdczId", "RD CZ ID"),
                    xml);
            return form;
        }

        private DynamicForm createDigitalDocumentForm() {
            DynamicForm form = createExpansionForm();
            TextItem pid = new TextItem("pid", "PID");
            pid.setWidth("*");
            form.setItems(pid);
            return form;
        }

    }
}
