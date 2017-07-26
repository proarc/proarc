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
package cz.cas.lib.proarc.webapp.client.widget.workflow;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.user.client.ui.Widget;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.DateTimeItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.IsFloatValidator;
import com.smartgwt.client.widgets.form.validator.RequiredIfFunction;
import com.smartgwt.client.widgets.form.validator.RequiredIfValidator;
import com.smartgwt.client.widgets.form.validator.Validator;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.common.workflow.model.Task.State;
import cz.cas.lib.proarc.common.workflow.model.ValueType;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.DisplayType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.ErrorHandler;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ValueMapDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowParameterDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowTaskDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowTasksEditor;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowTaskFormView implements Refreshable {

    private final ClientMessages i18n;
    private final Canvas widget;
    private DynamicForm taskForm;
    private SelectItem stateItem;
    private WorkflowMaterialView materialView;
    private final WorkflowTasksEditor handler;
    private DynamicForm paramForm;
    private final RequiredIfFunction requiredFunc;
    private VLayout paramContainer;
    private ItemChangedHandler itemChangedHandler;
    private final ActionSource actionSource = new ActionSource(this);

    public WorkflowTaskFormView(ClientMessages i18n, WorkflowTasksEditor handler) {
        this.i18n = i18n;
        this.handler = handler;
        // params are required just in case of the finished state of the task
        this.requiredFunc = new RequiredIfFunction() {

            @Override
            public boolean execute(FormItem formItem, Object value) {
                return State.FINISHED.name().equals(
                        taskForm.getValueAsString(WorkflowTaskDataSource.FIELD_STATE));
            }
        };
        this.widget = createMainLayout();
        setItemChangedHandler();
    }

    public Canvas getWidget() {
        return widget;
    }

    public boolean isChanged() {
        return taskForm.valuesHaveChanged() || paramForm.valuesHaveChanged();
    }

    public DynamicForm getTask() {
        return taskForm;
    }

    @Override
    public void refresh() {
        Record task = taskForm.getValuesAsRecord();
        if (task.getAttribute(WorkflowTaskDataSource.FIELD_ID) != null) {
            setTask(task);
        }
    }

    public void refreshState() {
        actionSource.fireEvent();
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
            taskForm.fetchData(new Criteria(WorkflowTaskDataSource.FIELD_ID, taskId), new DSCallback() {

                @Override
                public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                    Record dataRecord = null;
                    if (RestConfig.isStatusOk(dsResponse)) {
                        Record[] records = dsResponse.getData();
                        if (records.length > 0) {
                            dataRecord = records[0];
                        }
                    }
                    setPermitedTaskStates(dataRecord);
                    refreshState();
                }
            });
            setParameters(taskId);
            materialView.setTaskMaterials(taskId);
        } else {
            taskForm.clearValues();
            setParameters(null);
            materialView.setEmptyMaterials();
        }
        widget.setDisabled(task == null);
        refreshState();
    }

    private void setPermitedTaskStates(Record task) {
        State state = State.WAITING;
        try {
            if (task != null) {
                state = State.valueOf(task.getAttribute(WorkflowTaskDataSource.FIELD_STATE));
            }
        } catch (IllegalArgumentException e) {
            state = State.WAITING;
        }
        EnumSet<State> permitted;
        switch(state) {
            case READY:
            case STARTED:
                permitted = EnumSet.of(State.READY, State.STARTED, State.FINISHED, State.CANCELED);
                break;
            case CANCELED:
            case FINISHED:
                permitted = EnumSet.of(State.FINISHED, State.CANCELED);
                break;
            case WAITING:
            default:
                permitted = EnumSet.of(State.WAITING, State.READY, State.STARTED, State.FINISHED, State.CANCELED);
                break;
        }
        stateItem.setValueMap(fillTaskStates(permitted));
    }

    private LinkedHashMap<String, String> fillTaskStates(EnumSet<State> permitted) {
        LinkedHashMap<String, String> vm = new LinkedHashMap<String, String>();
        LinkedHashMap<String, String> allTaskStates = WorkflowTaskDataSource.getInstance().getAllTaskStates();
        for (State s : permitted) {
            String label = allTaskStates.get(s.name());
            label = label != null ? label : s.name();
            vm.put(s.name(), label);
        }
        return vm;
    }

    public void setParameters(String taskId) {
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

    /** Notifies about form changes. */
    private void setItemChangedHandler() {
        this.itemChangedHandler = new ItemChangedHandler() {

            @Override
            public void onItemChanged(ItemChangedEvent event) {
                refreshState();
            }
        };
        taskForm.addItemChangedHandler(itemChangedHandler);
    }

    private Canvas createMainLayout() {
        VLayout forms = new VLayout();
        forms.setOverflow(Overflow.AUTO);
        forms.addMember(createForm());
        forms.addMember(createParameterList());
        forms.setShowResizeBar(true);
        forms.setResizeBarTarget("next");

        VLayout main = new VLayout();
        main.addMember(createTaskToolbar());
        main.addMember(forms);
        main.addMember(createMaterialList());
        return main;
    }

    private ToolStrip createTaskToolbar() {
        ToolStrip toolbar = Actions.createToolStrip();
        RefreshAction refreshAction = new RefreshAction(i18n);
        SaveAction saveAction = new SaveAction(i18n) {

            @Override
            public boolean accept(ActionEvent event) {
                return handler != null
                        && taskForm.getValue(WorkflowTaskDataSource.FIELD_ID) != null
                        && isChanged();
            }

            @Override
            public void performAction(ActionEvent event) {
                handler.onSave(WorkflowTaskFormView.this);
            }
        };
        AbstractAction openJobAction = new AbstractAction(
                i18n.WorkflowTask_View_OpenJobAction_Title(),
                "[SKIN]/actions/edit.png",
                i18n.WorkflowTask_View_OpenJobAction_Hint()) {

            @Override
            public boolean accept(ActionEvent event) {
                return handler != null
                        && taskForm.getValue(WorkflowTaskDataSource.FIELD_ID) != null;
            }

            @Override
            public void performAction(ActionEvent event) {
                String jobId = taskForm.getValueAsString(WorkflowTaskDataSource.FIELD_JOB_ID);
                handler.onOpenJob(jobId);
            }
        };

        toolbar.addMember(Actions.asIconButton(refreshAction, this));
        toolbar.addMember(Actions.asIconButton(openJobAction, actionSource));
        toolbar.addMember(Actions.asIconButton(saveAction, actionSource));
        return toolbar;
    }

    private Widget createForm() {
        taskForm = new DynamicForm();
        taskForm.setDataSource(WorkflowTaskDataSource.getInstance());
        taskForm.setNumCols(3);
        taskForm.setColWidths("*", "*", "*");
        taskForm.setTitleOrientation(TitleOrientation.TOP);
        taskForm.setItemHoverWidth(300);

        StaticTextItem jobLabel = new StaticTextItem(WorkflowTaskDataSource.FIELD_JOB_LABEL);
        jobLabel.setColSpan("*");
        jobLabel.setWidth("*");
        jobLabel.setShowTitle(false);
        jobLabel.setReadOnlyTextBoxStyle(Editor.CSS_HEADER_INSIDE_FORM);
        jobLabel.setTextBoxStyle(Editor.CSS_HEADER_INSIDE_FORM);

        final SelectItem owner = new SelectItem(WorkflowTaskDataSource.FIELD_OWNER);
        owner.setOptionDataSource(UserDataSource.getInstance());
        owner.setValueField(UserDataSource.FIELD_ID);
        owner.setDisplayField(UserDataSource.FIELD_USERNAME);
        owner.setWidth("*");
        owner.setValidators(new RequiredIfValidator(requiredFunc));

        TextAreaItem note = new TextAreaItem(WorkflowTaskDataSource.FIELD_NOTE);
        note.setStartRow(true);
        note.setColSpan("*");
        note.setWidth("*");

        // title tooltip is broken in SmartGWT 4.0
        final FormItemIcon taskHelpIcon = new FormItemIcon();
        taskHelpIcon.setSrc("[SKIN]/actions/help.png");
        taskHelpIcon.setTabIndex(-1);
        taskHelpIcon.setNeverDisable(true);
        taskHelpIcon.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                String hint = taskForm.getValueAsString(WorkflowTaskDataSource.FIELD_HINT);
                taskHelpIcon.setPrompt(hint);
                return hint != null;
            }
        });
        TextItem label = new TextItem(WorkflowTaskDataSource.FIELD_LABEL);
        label.setWidth("*");
        label.setIcons(taskHelpIcon);

        stateItem = new SelectItem(WorkflowTaskDataSource.FIELD_STATE);
        stateItem.addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                owner.redraw();
                paramForm.markForRedraw();
            }
        });

        taskForm.setFields(jobLabel,
                label,
                owner,
                stateItem,
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
        df.setNumCols(3);
        df.setColWidths("*", "*", "*");
        df.setItemHoverWidth(300);
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
            // use dataPath to solve cases where the valid JSON name is not a valid javascript ID (param.id).
            items[i].setDataPath("/" + paramName);
            items[i].setTitle(record.getAttribute(WorkflowModelConsts.PARAMETER_PROFILELABEL));
            items[i].setTooltip(record.getAttribute(WorkflowModelConsts.PARAMETER_PROFILEHINT));
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

        Boolean required = editedRecord.getAttributeAsBoolean(WorkflowModelConsts.PARAMETER_REQUIRED);
        ArrayList<Validator> validators = new ArrayList<Validator>();
        if (required != null && required) {
            validators.add(new RequiredIfValidator(requiredFunc));
        }
        if (valueType == ValueType.NUMBER && displayType != DisplayType.CHECKBOX) {
            validators.add(new IsFloatValidator());
        }
        if (!validators.isEmpty()) {
            fi.setValidators(validators.toArray(new Validator[validators.size()]));
        }
        return fi;
    }

    private FormItem createFormItem(DisplayType displayType, Record profile) {
        String name = profile.getAttribute(WorkflowParameterDataSource.FIELD_NAME);
        switch (displayType) {
            case SELECT:
                SelectItem si = new SelectItem();
                setOptions(si, profile);
                si.setWidth("*");
                return si;
            case COMBOBOX:
                ComboBoxItem cbi = new ComboBoxItem();
                setOptions(cbi, profile);
                cbi.setLength(2000);
                cbi.setWidth("*");
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
                ti.setWidth("*");
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
        materialView = new WorkflowMaterialView(i18n, this.getClass().getSimpleName());
        return materialView.getWidget();
    }

}
