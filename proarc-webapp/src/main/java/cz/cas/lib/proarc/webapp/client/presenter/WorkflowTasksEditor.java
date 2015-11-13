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
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.validator.IsFloatValidator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridEditorContext;
import com.smartgwt.client.widgets.grid.ListGridEditorCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.common.workflow.model.Material;
import cz.cas.lib.proarc.common.workflow.model.ValueType;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.DisplayType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ValueMapDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowMaterialDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowParameterDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowTaskDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowJobPlace;

/**
 * Edits tasks of the workflow.
 *
 * @author Jan Pokorsky
 */
public class WorkflowTasksEditor {

    private final ClientMessages i18n;
    private WorkflowTasksView view;
    private final PlaceController places;

    public WorkflowTasksEditor(ClientMessages i18n, PlaceController places) {
        this.i18n = i18n;
        this.places = places;
    }

    public Canvas getUI() {
        if (view == null) {
            view = new WorkflowTasksView(i18n, this);
            view.init();
        }
        return view.getWidget();
    }

    private void onOpenJob() {
        places.goTo(new WorkflowJobPlace());
    }

    private static final class WorkflowTasksView implements Refreshable {

        private final ClientMessages i18n;
        private final Canvas widget;
        private ListGrid taskGrid;
        private WorkflowTaskFormView taskFormView;
        private final WorkflowTasksEditor handler;

        public WorkflowTasksView(ClientMessages i18n, WorkflowTasksEditor handler) {
            this.i18n = i18n;
            this.handler = handler;
            this.widget = createMainLayout();
        }

        public Canvas getWidget() {
            return widget;
        }

        public void init() {
            taskGrid.fetchData();
        }

        @Override
        public void refresh() {
            taskGrid.invalidateCache();
        }

        private Canvas createMainLayout() {
            VLayout main = new VLayout();
            main.addMember(createPanelLabel());
            // filter
            main.addMember(createFilter());
            // toolbar
            main.addMember(createTasksToolbar());
            // list + item
            main.addMember(createTaskLayout());
            return main;
        }

        private Canvas createTaskLayout() {
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

        private ToolStrip createTasksToolbar() {
            ToolStrip toolbar = Actions.createToolStrip();
            RefreshAction refreshAction = new RefreshAction(i18n);
            SaveAction saveAction = new SaveAction(i18n) {

                @Override
                public void performAction(ActionEvent event) {
                }
            };

            toolbar.addMember(Actions.asIconButton(refreshAction, this));
            toolbar.addMember(Actions.asIconButton(saveAction, this));
            return toolbar;
        }

        private ListGrid createTaskList() {
            final ListGrid grid = new ListGrid();
            grid.setShowFilterEditor(true);
            grid.setFilterOnKeypress(true);
            grid.setCanSort(true);
            grid.setDataSource(WorkflowTaskDataSource.getInstance(),
                    new ListGridField(WorkflowTaskDataSource.FIELD_LABEL),
                    new ListGridField(WorkflowTaskDataSource.FIELD_TYPE),
                    new ListGridField(WorkflowTaskDataSource.FIELD_STATE, 50),
                    new ListGridField(WorkflowTaskDataSource.FIELD_PRIORITY, 50),
                    new ListGridField(WorkflowTaskDataSource.FIELD_OWNER, 50),
                    new ListGridField(WorkflowTaskDataSource.FIELD_CREATED, 100),
                    new ListGridField(WorkflowTaskDataSource.FIELD_MODIFIED, 100),
                    new ListGridField(WorkflowTaskDataSource.FIELD_ID, 30),
                    new ListGridField(WorkflowTaskDataSource.FIELD_NOTE),
                    new ListGridField(WorkflowTaskDataSource.FIELD_JOB_ID, 30),
                    new ListGridField(WorkflowTaskDataSource.FIELD_JOB_LABEL)
                    );

            grid.getField(WorkflowTaskDataSource.FIELD_LABEL).setWidth("80%");
            grid.getField(WorkflowTaskDataSource.FIELD_LABEL).setCanFilter(false);
            grid.getField(WorkflowTaskDataSource.FIELD_LABEL).setCanSort(false);

            grid.getField(WorkflowTaskDataSource.FIELD_OWNER).setCanFilter(true);
            grid.getField(WorkflowTaskDataSource.FIELD_OWNER).setCanSort(false);
            SelectItem owner = new SelectItem();
            owner.setOptionDataSource(UserDataSource.getInstance());
            owner.setValueField(UserDataSource.FIELD_ID);
            owner.setDisplayField(UserDataSource.FIELD_USERNAME);
            grid.getField(WorkflowTaskDataSource.FIELD_OWNER).setFilterEditorProperties(owner);

            grid.getField(WorkflowTaskDataSource.FIELD_STATE).setCanFilter(true);
            grid.getField(WorkflowTaskDataSource.FIELD_STATE).setCanSort(false);

            grid.getField(WorkflowTaskDataSource.FIELD_CREATED).setCanFilter(false);
            grid.getField(WorkflowTaskDataSource.FIELD_CREATED).setCanSort(true);

            grid.getField(WorkflowTaskDataSource.FIELD_MODIFIED).setCanFilter(false);
            grid.getField(WorkflowTaskDataSource.FIELD_MODIFIED).setCanSort(true);

            grid.getField(WorkflowTaskDataSource.FIELD_NOTE).setCanFilter(false);
            grid.getField(WorkflowTaskDataSource.FIELD_NOTE).setCanSort(false);

            grid.getField(WorkflowTaskDataSource.FIELD_JOB_LABEL).setCanFilter(false);
            grid.getField(WorkflowTaskDataSource.FIELD_JOB_LABEL).setCanSort(true);

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
            taskFormView = new WorkflowTaskFormView(i18n, handler);
            return taskFormView.getWidget();
        }

    }

    private static final class WorkflowTaskFormView {

        private final ClientMessages i18n;
        private final Canvas widget;
        private DynamicForm taskForm;
        private ListGrid paramGrid;
        private WorkflowMaterialView materialView;
        private final WorkflowTasksEditor handler;

        public WorkflowTaskFormView(ClientMessages i18n, WorkflowTasksEditor handler) {
            this.i18n = i18n;
            this.handler = handler;
            this.widget = createMainLayout();
        }

        public Canvas getWidget() {
            return widget;
        }

        public void setTask(Record task) {
            if (task != null) {
                taskForm.clearErrors(true);
                taskForm.editRecord(task);
                String taskId = task.getAttribute(WorkflowTaskDataSource.FIELD_ID);
                setParameters(taskId);
                materialView.getMaterialGrid().fetchData(
                        new Criteria(WorkflowModelConsts.MATERIALFILTER_TASKID, taskId));
            } else {
                taskForm.clearValues();
                setParameters(null);
                setMaterials(new Record[0]);
            }
//            taskForm.fetchData();
        }

        private void setParameters(String taskId) {
            if (taskId != null) {
                paramGrid.discardAllEdits();
                paramGrid.invalidateCache();
                paramGrid.invalidateRecordComponents();
                paramGrid.fetchData(new Criteria(WorkflowModelConsts.PARAMETERPROFILE_TASKID, taskId));
            } else {
                paramGrid.setData(new Record[0]);
            }
        }

        private void setMaterials(Record[] records) {
            materialView.getMaterialGrid().setData(records);
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

//            StaticTextItem jobLabel = new StaticTextItem(WorkflowTaskDataSource.FIELD_JOB_LABEL);
            LinkItem jobLabel = new LinkItem(WorkflowTaskDataSource.FIELD_JOB_LABEL);
            jobLabel.setColSpan("*");
            jobLabel.setWidth("*");
            jobLabel.setShowTitle(false);
            jobLabel.setTextBoxStyle(Editor.CSS_PANEL_DESCRIPTION_TITLE);
            jobLabel.setReadOnlyTextBoxStyle(Editor.CSS_PANEL_DESCRIPTION_TITLE);
            jobLabel.setTarget("javascript");
            jobLabel.setTooltip("Kliknutím přejdete na záměr.");
            jobLabel.addClickHandler(new ClickHandler() {

                @Override
                public void onClick(ClickEvent event) {
                    handler.onOpenJob();
                }
            });

            SelectItem owner = new SelectItem(WorkflowTaskDataSource.FIELD_OWNER);
            owner.setOptionDataSource(UserDataSource.getInstance());
            owner.setValueField(UserDataSource.FIELD_USERNAME);
            owner.setDisplayField(UserDataSource.FIELD_USERNAME);
            owner.setWidth("*");

            TextAreaItem note = new TextAreaItem(WorkflowTaskDataSource.FIELD_NOTE);
            note.setStartRow(true);
            note.setColSpan("*");
            note.setWidth("*");
            note.setHeight(40);

            TextItem label = new TextItem(WorkflowTaskDataSource.FIELD_LABEL);
            label.setWidth("*");

            taskForm.setFields(jobLabel,
                    label,
                    owner,
                    new SelectItem(WorkflowTaskDataSource.FIELD_STATE),
                    new TextItem(WorkflowTaskDataSource.FIELD_CREATED),
                    new TextItem(WorkflowTaskDataSource.FIELD_MODIFIED),
                    new TextItem(WorkflowTaskDataSource.FIELD_PRIORITY),
                    note);
            return taskForm;
        }

        private Widget createParameterList() {
            paramGrid = new ListGrid();
            paramGrid.setDataSource(WorkflowParameterDataSource.getInstance());
            paramGrid.setCanEdit(true);
//            paramGrid.setEditByCell(true);
            paramGrid.setEditEvent(ListGridEditEvent.CLICK);
//            paramGrid.setValidateByCell(true);
//            paramGrid.setValidateOnChange(true);
            paramGrid.setHeight("30%");
            paramGrid.setAutoSaveEdits(false);
//            paramGrid.setAlwaysShowEditors(true);
            paramGrid.setEditorCustomizer(new ListGridEditorCustomizer() {

                @Override
                public FormItem getEditor(ListGridEditorContext context) {
                    return getParamValueEditor(context);
                }
            });
            return paramGrid;
        }

        private FormItem getParamValueEditor(ListGridEditorContext context) {
            ListGridField editField = context.getEditField();
            if (!WorkflowParameterDataSource.FIELD_VALUE.equals(editField.getName())) {
                return context.getDefaultProperties();
            }
            ListGridRecord editedRecord = context.getEditedRecord();
            ValueType valueType = ValueType.fromString(
                    editedRecord.getAttribute(WorkflowModelConsts.PARAMETER_VALUETYPE));
            DisplayType displayType = DisplayType.fromString(
                    editedRecord.getAttribute(WorkflowModelConsts.PARAMETER_DISPLAYTYPE));

            FormItem fi = createFormItem(displayType, editedRecord);

            System.out.println("###" + editedRecord.getAttribute(WorkflowModelConsts.PARAMETER_PROFILENAME)
                    + ", required: " + editedRecord.getAttributeAsBoolean(WorkflowModelConsts.PARAMETER_REQUIRED));
            fi.setRequired(editedRecord.getAttributeAsBoolean(WorkflowModelConsts.PARAMETER_REQUIRED));
            if (valueType == ValueType.NUMBER && displayType != DisplayType.CHECKBOX) {
                fi.setValidators(new IsFloatValidator());
            }
//            fi.setValidateOnExit(true);
            return fi;
        }

        private FormItem createFormItem(DisplayType displayType, Record profile) {
            switch (displayType) {
                case SELECT:
                    SelectItem si = new SelectItem();
                    setOptions(si, profile);
                    return si;
                case COMBOBOX:
                    ComboBoxItem cbi = new ComboBoxItem();
                    setOptions(cbi, profile);
                    cbi.setLength(2000);
                    return cbi;
                case CHECKBOX:
                    CheckboxItem ci = new CheckboxItem();
                    ci.setShowLabel(false);
                    return ci;
                case TEXTAREA:
                    TextAreaItem tai = new TextAreaItem();
                    tai.setLength(2000);
                    return tai;
                case TEXT:
                default:
                    TextItem ti = new TextItem();
                    ti.setLength(2);
                    return ti;
            }
        }

        private void setOptions(FormItem item, Record profile) {
            String dataSourceId = profile.getAttribute(WorkflowModelConsts.PARAMETER_VALUEMAPID);
            if (dataSourceId != null) {
                DataSource ds = ValueMapDataSource.getInstance().getOptionDataSource(dataSourceId);
                item.setValueField(profile.getAttribute(WorkflowModelConsts.PARAMETER_OPTION_VALUE_FIELD));
                item.setOptionDataSource(ds);
                item.setDisplayField(profile.getAttribute(WorkflowModelConsts.PARAMETER_OPTION_DISPLAY_FIELD));
            }
        }

        private Widget createMaterialList() {
            materialView = new WorkflowMaterialView(i18n);
            return materialView.getWidget();
        }

    }

    public static class WorkflowMaterialView {

        private final ClientMessages i18n;
        private ListGrid materialGrid;
        private final Widget widget;
        private final boolean jobMaterial;

        public WorkflowMaterialView(ClientMessages i18n) {
            this(i18n, false);
        }

        public WorkflowMaterialView(ClientMessages i18n, boolean jobMaterial) {
            this.i18n = i18n;
            this.jobMaterial = jobMaterial;
            widget = createMaterialList();
        }

        public Widget getWidget() {
            return widget;
        }

        public ListGrid getMaterialGrid() {
            return materialGrid;
        }

        private Widget createMaterialList() {
            materialGrid = new ListGrid() {

                @Override
                protected Canvas getExpansionComponent(final ListGridRecord record) {
                    String type = record.getAttribute(WorkflowMaterialDataSource.FIELD_TYPE);
                    DynamicForm form = null;
                    if (Material.Type.FOLDER.name().equals(type)) {
                        form = createFolderForm();
                    } else if (Material.Type.PHYSICAL_DOCUMENT.name().equals(type)) {
                        form = createPhysicalDocumentForm();
                    } else if (Material.Type.DIGITAL_OBJECT.name().equals(type)) {
                        form = createDigitalDocumentForm();
                    }
                    if (form != null) {
                        return bindExpansinonForm(form, record);
                    } else {
                        return super.getExpansionComponent(record);
                    }
                }

            };
            materialGrid.setDataSource(WorkflowMaterialDataSource.getInstance(),
                    new ListGridField(WorkflowMaterialDataSource.FIELD_PROFILENAME),
                    new ListGridField(WorkflowMaterialDataSource.FIELD_VALUE),
                    new ListGridField(WorkflowMaterialDataSource.FIELD_WAY),
                    new ListGridField(WorkflowMaterialDataSource.FIELD_NOTE),
                    new ListGridField(WorkflowMaterialDataSource.FIELD_ID)
            );
            materialGrid.getField(WorkflowMaterialDataSource.FIELD_WAY).setHidden(jobMaterial);
            materialGrid.setExpansionMode(ExpansionMode.DETAIL_FIELD);
            materialGrid.setCanExpandRecords(true);
            materialGrid.setCanExpandMultipleRecords(false);
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
            TextItem path = new TextItem(WorkflowMaterialDataSource.FIELD_FOLDER_PATH);
            path.setWidth("*");
            form.setDataSource(WorkflowMaterialDataSource.getInstance(),
                    path);
            return form;
        }

        private DynamicForm createPhysicalDocumentForm() {
            DynamicForm form = createExpansionForm();
            TextAreaItem xml = new TextAreaItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_METADATA);
            xml.setWidth("*");
            form.setDataSource(WorkflowMaterialDataSource.getInstance(),
                    new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_CATALOG),
                    new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_BARCODE),
                    new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_FIELD001),
                    new TextItem(WorkflowMaterialDataSource.FIELD_PHYSICAL_RDCZID),
                    xml);
            return form;
        }

        private DynamicForm createDigitalDocumentForm() {
            DynamicForm form = createExpansionForm();
            TextItem pid = new TextItem(WorkflowMaterialDataSource.FIELD_DIGITAL_PID);
            pid.setWidth("*");
            form.setDataSource(WorkflowMaterialDataSource.getInstance(),
                    pid);
            return form;
        }
    }
}
