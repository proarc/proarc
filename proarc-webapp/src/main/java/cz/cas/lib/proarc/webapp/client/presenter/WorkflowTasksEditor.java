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

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.place.shared.PlaceController;
import com.google.gwt.user.client.ui.Widget;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.CriteriaPolicy;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.FetchMode;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.DateTimeItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.validator.IsFloatValidator;
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
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.ValueType;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.DisplayType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.ErrorHandler;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ValueMapDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowMaterialDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowParameterDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowTaskDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowJobPlace;
import cz.cas.lib.proarc.webapp.client.widget.ListGridPersistance;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import java.math.BigDecimal;
import java.util.Iterator;
import java.util.Map;

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

    private void onSave(WorkflowTaskFormView taskFormView) {
        if (taskFormView.validate()) {
            view.setExpectUpdateOperation(true);
            DSRequest req = new DSRequest();
            req.setWillHandleError(true);
            final DynamicForm taskForm = taskFormView.getTask();
            taskForm.saveData(new DSCallback() {

                @Override
                public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                    boolean statusOk = RestConfig.isStatusOk(dsResponse);
                    if (statusOk) {
                        StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                        view.refreshState();
                        String taskId = taskForm.getValueAsString(WorkflowTaskDataSource.FIELD_ID);
                        view.refreshParameters(taskId);
                    } else if (RestConfig.isConcurrentModification(dsResponse)) {
                        SC.ask(i18n.SaveAction_ConcurrentErrorAskReload_Msg(), new BooleanCallback() {

                            @Override
                            public void execute(Boolean value) {
                                if (value != null && value) {
                                    view.editSelection();
                                }
                            }
                        });
                    } else {
                        ErrorHandler.warn(dsResponse, dsRequest);
                    }
                    taskForm.focus();
                }
            }, req);
        }
    }

    private static final class WorkflowTasksView implements Refreshable {

        private final ClientMessages i18n;
        private final Canvas widget;
        private ListGrid taskGrid;
        private WorkflowTaskFormView taskFormView;
        private final WorkflowTasksEditor handler;
        private ListGridPersistance taskListPersistance;
        private final ActionSource actionSource = new ActionSource(this);
        private boolean isUpdateOperation;
        private ListGridRecord lastSelection;

        public WorkflowTasksView(ClientMessages i18n, WorkflowTasksEditor handler) {
            this.i18n = i18n;
            this.handler = handler;
            this.widget = createMainLayout();
        }

        public Canvas getWidget() {
            return widget;
        }

        public void init() {
            taskGrid.fetchData(taskListPersistance.getFilterCriteria());
        }

        @Override
        public void refresh() {
            taskGrid.invalidateCache();
        }

        public void editSelection() {
            taskFormView.setTask(taskGrid.getSelectedRecord());
            refreshState();
        }

        public void refreshState() {
            actionSource.fireEvent();
        }

        public void refreshParameters(String taskId) {
            taskFormView.setParameters(taskId);
        }

        /**
         * Set to {@code true} before saving a job. It is a hack not to select
         * the job again inside {@code onDataArrived}.
         */
        public void setExpectUpdateOperation(boolean isUpdateOperation) {
            this.isUpdateOperation = isUpdateOperation;
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
                public boolean accept(ActionEvent event) {
                    return handler != null
                            && taskGrid.getSelectedRecords().length > 0
                            && taskFormView.isChanged();
                }

                @Override
                public void performAction(ActionEvent event) {
                    handler.onSave(taskFormView);
                }
            };

            toolbar.addMember(Actions.asIconButton(refreshAction, this));
            toolbar.addMember(Actions.asIconButton(saveAction, actionSource));
            return toolbar;
        }

        private ListGrid createTaskList() {
            final ListGrid grid = new ListGrid();
            taskListPersistance = new ListGridPersistance("WorkflowTasksView.taskList", grid);
            grid.setShowFilterEditor(true);
            grid.setFilterOnKeypress(true);
            grid.setCanSort(true);

            grid.setDataFetchMode(FetchMode.PAGED);
            ResultSet rs = new ResultSet();
            rs.setCriteriaPolicy(CriteriaPolicy.DROPONCHANGE);
            rs.setUseClientFiltering(false);
            rs.setUseClientSorting(false);
            grid.setDataProperties(rs);

            grid.setDataSource(WorkflowTaskDataSource.getInstance(),
                    new ListGridField(WorkflowTaskDataSource.FIELD_LABEL),
                    new ListGridField(WorkflowTaskDataSource.FIELD_TYPE),
                    new ListGridField(WorkflowTaskDataSource.FIELD_STATE, 50),
                    new ListGridField(WorkflowTaskDataSource.FIELD_PRIORITY, 60),
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

            grid.getField(WorkflowTaskDataSource.FIELD_CREATED).setCanFilter(true);
            grid.getField(WorkflowTaskDataSource.FIELD_CREATED).setCanSort(true);

            grid.getField(WorkflowTaskDataSource.FIELD_MODIFIED).setCanFilter(true);
            grid.getField(WorkflowTaskDataSource.FIELD_MODIFIED).setCanSort(true);

            grid.getField(WorkflowTaskDataSource.FIELD_NOTE).setCanFilter(false);
            grid.getField(WorkflowTaskDataSource.FIELD_NOTE).setCanSort(false);

            grid.getField(WorkflowTaskDataSource.FIELD_JOB_LABEL).setCanFilter(false);
            grid.getField(WorkflowTaskDataSource.FIELD_JOB_LABEL).setCanSort(true);

            grid.addDataArrivedHandler(new DataArrivedHandler() {

                @Override
                public void onDataArrived(DataArrivedEvent event) {
                    if (isUpdateOperation) {
                        isUpdateOperation = false;
                        return ;
                    }
                    int startRow = event.getStartRow();
                    int endRow = event.getEndRow();
                    if (startRow == 0 && endRow >= 0) {
                        updateSelection();
                        grid.focus();
                    } else if (endRow < 0) {
                        grid.deselectAllRecords();
                    }
                }
            });
            grid.setSelectionType(SelectionStyle.SINGLE);
            grid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

                @Override
                public void onSelectionUpdated(SelectionUpdatedEvent event) {
                    lastSelection = taskGrid.getSelectedRecord();
                    editSelection();
                }
            });
            grid.setViewState(taskListPersistance.getViewState());
            taskGrid = grid;
            return grid;
        }

        private void updateSelection() {
            RecordList rl = taskGrid.getRecordList();
            if (rl.isEmpty()) {
                return ;
            }
            if (lastSelection == null) {
                taskGrid.selectSingleRecord(0);
                return ;
            }
            Record newRec = rl.find(WorkflowTaskDataSource.FIELD_ID,
                    lastSelection.getAttribute(WorkflowTaskDataSource.FIELD_ID));
            if (newRec != null) {
                taskGrid.selectSingleRecord(newRec);
                int rowNum = taskGrid.getRecordIndex(newRec);
                if (rowNum >= 0) {
                    taskGrid.scrollToRow(rowNum);
                }
            }
        }

        private Canvas createTaskFormLayout() {
            taskFormView = new WorkflowTaskFormView(i18n, handler);
            taskFormView.setItemChangedHandler(new ItemChangedHandler() {

                @Override
                public void onItemChanged(ItemChangedEvent event) {
                    refreshState();
                }
            });
            return taskFormView.getWidget();
        }

    }

    private static final class WorkflowTaskFormView {

        private final ClientMessages i18n;
        private final Canvas widget;
        private DynamicForm taskForm;
        private WorkflowMaterialView materialView;
        private final WorkflowTasksEditor handler;
        private DynamicForm paramForm;
        private VLayout paramContainer;
        private ItemChangedHandler itemChangedHandler;

        public WorkflowTaskFormView(ClientMessages i18n, WorkflowTasksEditor handler) {
            this.i18n = i18n;
            this.handler = handler;
            this.widget = createMainLayout();
        }

        public Canvas getWidget() {
            return widget;
        }

        /** Notifies about form changes. */
        public void setItemChangedHandler(ItemChangedHandler itemChangedHandler) {
            this.itemChangedHandler = itemChangedHandler;
            taskForm.addItemChangedHandler(itemChangedHandler);
        }

        public boolean isChanged() {
            return taskForm.valuesHaveChanged() || paramForm.valuesHaveChanged();
        }

        public DynamicForm getTask() {
            return taskForm;
        }

        public boolean validate() {
            if (taskForm.validate() && paramForm.validate()) {
                Map<?,?> params = paramForm.getValues();
                for (Iterator<?> it = params.values().iterator(); it.hasNext();) {
                    Object paramValue = it.next();
                    if (paramValue instanceof Map) {
                        it.remove();
                    }
                }
                if (params.isEmpty()) {
                    taskForm.clearValue(WorkflowTaskDataSource.FIELD_PARAMETERS);
                } else {
                    taskForm.setValue(WorkflowTaskDataSource.FIELD_PARAMETERS, params);
                }
                return true;
            } else {
                return false;
            }
        }

        public void setTask(Record task) {
            if (task != null) {
                String taskId = task.getAttribute(WorkflowTaskDataSource.FIELD_ID);
                taskForm.clearErrors(true);
                taskForm.fetchData(new Criteria(WorkflowTaskDataSource.FIELD_ID, taskId));
                setParameters(taskId);
                materialView.getMaterialGrid().fetchData(
                        new Criteria(WorkflowModelConsts.MATERIALFILTER_TASKID, taskId));
            } else {
                taskForm.clearValues();
                setParameters(null);
                setMaterials(new Record[0]);
            }
        }

        private void setParameters(String taskId) {
            if (taskId != null) {
                DSRequest dsRequest = new DSRequest();
                dsRequest.setWillHandleError(true);
                WorkflowParameterDataSource.getInstance().fetchData(
                        new Criteria(WorkflowModelConsts.PARAMETERPROFILE_TASKID, taskId),
                        new DSCallback() {

                    @Override
                    public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                        if (RestConfig.isStatusOk(dsResponse)) {
                            setParameterRecords(dsResponse.getData());
                        } else {
                            setParameterRecords(new Record[0]);
                            ErrorHandler.warn(dsResponse, dsRequest);
                        }
                    }
                }, dsRequest);
            } else {
                setParameterRecords(new Record[0]);
            }
        }

        private void setMaterials(Record[] records) {
            materialView.getMaterialGrid().setData(records);
        }

        private Canvas createMainLayout() {
            VLayout forms = new VLayout();
            forms.setOverflow(Overflow.AUTO);
            forms.addMember(createForm());
            forms.addMember(createParameterList());

            VLayout main = new VLayout();
            main.addMember(forms);
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
            owner.setValueField(UserDataSource.FIELD_ID);
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
                    new SelectItem(WorkflowTaskDataSource.FIELD_PRIORITY),
                    note);
            return taskForm;
        }

        private DynamicForm createDefaultParameterForm() {
            DynamicForm df = new DynamicForm();
//            StaticTextItem msg = new StaticTextItem();
//            msg.setShowTitle(false);
//            msg.setValue("No parameter!");
//            df.setItems(msg);
            return df;
        }

        private DynamicForm createParameterForm(Record[] records) {
            if (records == null || records.length == 0) {
                return createDefaultParameterForm();
            }
            DynamicForm df = new DynamicForm();
            df.setUseFlatFields(true);
            df.setWrapItemTitles(false);
            df.setTitleOrientation(TitleOrientation.TOP);
            df.setNumCols(2);
            FormItem[] items = new FormItem[records.length];
            Record values = new Record();
            for (int i = 0; i < records.length; i++) {
                Record record = records[i];
                ValueType valueType = ValueType.fromString(
                        record.getAttribute(WorkflowModelConsts.PARAMETER_VALUETYPE));
                DisplayType displayType = DisplayType.fromString(
                        record.getAttribute(WorkflowModelConsts.PARAMETER_DISPLAYTYPE));
                displayType = valueType == ValueType.DATETIME ? DisplayType.DATETIME : displayType;

                String paramName = record.getAttribute(WorkflowParameterDataSource.FIELD_NAME);
                String fieldName = "f" + i;
                items[i] = createFormItem(record, valueType, displayType);
                items[i].setName(fieldName);
                // use dataPath to solve cases here the valid JSON name is not a valid javascript ID (param.id).
                items[i].setDataPath("/" + paramName);
                items[i].setTitle(record.getAttribute(WorkflowModelConsts.PARAMETER_PROFILELABEL));
                Object val = getParameterValue(record, valueType, displayType);
                if (val != null) {
                    values.setAttribute(paramName, val);
                }
            }
            df.setItems(items);
            df.editRecord(values);
            df.addItemChangedHandler(itemChangedHandler);
            return df;
        }

        private Object getParameterValue(Record record, ValueType valueType, DisplayType displayType) {
            Object val = record.getAttributeAsObject(WorkflowParameterDataSource.FIELD_VALUE);
            if (valueType == ValueType.DATETIME && val instanceof String) {
                DateTimeFormat format = DateTimeFormat.getFormat(PredefinedFormat.ISO_8601);
                val = format.parse((String) val);
            } else if (displayType == DisplayType.CHECKBOX && val instanceof String) {
                if (Boolean.TRUE.toString().equalsIgnoreCase((String) val)) {
                    val = true;
                } else if (Boolean.FALSE.toString().equalsIgnoreCase((String) val)) {
                    val = false;
                } else {
                    try {
                        val = new BigDecimal((String) val).compareTo(BigDecimal.ZERO) > 0;
                    } catch (NumberFormatException e) {
                        // ignore
                    }
                }
            } else if (displayType == DisplayType.CHECKBOX && val instanceof Number) {
                val = ((Number) val).doubleValue() > 0;
            }
            return val;
        }

        private Widget createParameterList() {
            paramContainer = new VLayout();
            paramContainer.setAutoHeight();
            setParameterRecords(null);
            return paramContainer;
        }

        private void setParameterRecords(Record[] records) {
            if (paramForm != null) {
                paramForm.markForDestroy();
            }
            paramForm = createParameterForm(records);
            paramContainer.setMembers(paramForm);
        }

        private FormItem createFormItem(Record editedRecord, ValueType valueType, DisplayType displayType) {
            FormItem fi = createFormItem(displayType, editedRecord);

            fi.setRequired(editedRecord.getAttributeAsBoolean(WorkflowModelConsts.PARAMETER_REQUIRED));
            if (valueType == ValueType.NUMBER && displayType != DisplayType.CHECKBOX) {
                fi.setValidators(new IsFloatValidator());
            }
            return fi;
        }

        private FormItem createFormItem(DisplayType displayType, Record profile) {
            String name = profile.getAttribute(WorkflowParameterDataSource.FIELD_NAME);
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
                    // the width must be set otherwise it overflows the form
                    ci.setWidth(150);
                    ci.setAllowEmptyValue(true);
                    return ci;
                case TEXTAREA:
                    TextAreaItem tai = new TextAreaItem();
                    tai.setStartRow(true);
                    tai.setEndRow(true);
                    tai.setLength(2000);
                    tai.setColSpan("*");
                    tai.setWidth("*");
                    tai.setHeight(30);
                    return tai;
                case DATETIME:
                    DateTimeItem di = new DateTimeItem();
                    return di;
                case TEXT:
                default:
                    TextItem ti = new TextItem(name);
                    ti.setLength(2000);
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
                    if (MaterialType.FOLDER.name().equals(type)) {
                        form = createFolderForm();
                    } else if (MaterialType.PHYSICAL_DOCUMENT.name().equals(type)) {
                        form = createPhysicalDocumentForm();
                    } else if (MaterialType.DIGITAL_OBJECT.name().equals(type)) {
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
