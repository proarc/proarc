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

import com.google.gwt.user.client.ui.Widget;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.CriteriaPolicy;
import com.smartgwt.client.types.FetchMode;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.RecordDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordDoubleClickHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ItemClickEvent;
import com.smartgwt.client.widgets.menu.events.ItemClickHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfileConsts;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowJobDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowProfileDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowTaskDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowJobsEditor;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowJobPlace;
import cz.cas.lib.proarc.webapp.client.widget.CanvasSizePersistence;
import cz.cas.lib.proarc.webapp.client.widget.ListGridPersistance;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowJobFormView implements Refreshable {

    private final ClientMessages i18n;
    private final Canvas widget;
    private DynamicForm jobForm;
    private WorkflowMaterialView materialView;
    private ListGrid taskView;
    private Menu addTaskMenu;
    private WorkflowJobsEditor handler;
    private Record lastJob;
    private final ActionSource actionSource = new ActionSource(this);
    private Action editTaskAction;

    public WorkflowJobFormView(ClientMessages i18n) {
        this.i18n = i18n;
        this.widget = createMainLayout();
    }

    public Canvas getWidget() {
        return widget;
    }

    public DynamicForm getJobValues() {
        return jobForm;
    }

    public ListGrid getTasks() {
        return taskView;
    }

    public void setHandler(WorkflowJobsEditor handler) {
        this.handler = handler;
    }

    @Override
    public void refresh() {
        taskView.invalidateCache();
        setJob(lastJob);
    }

    public void refreshState() {
        actionSource.fireEvent();
    }

    public void setJob(final Record job) {
        this.lastJob = job;
        fetchAddTaskMenu(null);
        if (job != null) {
            String jobId = job.getAttribute(WorkflowJobDataSource.FIELD_ID);
            jobForm.clearErrors(true);
            jobForm.fetchData(new Criteria(WorkflowJobDataSource.FIELD_ID, jobId), new DSCallback() {

                @Override
                public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                    refreshState();
                }
            });
            taskView.fetchData(
                    new Criteria(WorkflowModelConsts.TASK_FILTER_JOBID, jobId),
                    new DSCallback() {

                @Override
                public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                    if (RestConfig.isStatusOk(dsResponse)) {
                        Record[] records = dsResponse.getData();
                        if (records.length > 0) {
                            fetchAddTaskMenu(job.getAttribute(WorkflowJobDataSource.FIELD_PROFILE_ID));
                        }
                    }
                }
            });
            materialView.setJobMaterials(jobId);
        } else {
            jobForm.clearValues();
            materialView.setEmptyMaterials();
            taskView.setData(new Record[0]);
        }
        widget.setDisabled(job == null);
        refreshState();
    }

    private void fetchAddTaskMenu(final String jobName) {
        addTaskMenu.setData(new Record[0]);
        if (jobName == null) {
            return ;
        }
        WorkflowProfileDataSource.getInstance().getTasks(false, jobName, (tasks) -> {
                addTaskMenu.setData(tasks);
        });
    }

    private Canvas createMainLayout() {
        VLayout main = new VLayout();
        main.addMember(createJobToolbar());
        main.addMember(createForm());
        main.addMember(createTaskList());
        main.addMember(createMaterialList());
        return main;
    }

    private ToolStrip createJobToolbar() {
        ToolStrip toolbar = Actions.createToolStrip();
        RefreshAction refreshAction = new RefreshAction(i18n);
        SaveAction saveAction = createSaveAction();
        Action parentAction = createParentAction();

        toolbar.addMember(Actions.asIconButton(refreshAction, this));
        toolbar.addMember(Actions.asIconButton(parentAction, actionSource));
        toolbar.addMember(Actions.asIconButton(saveAction, actionSource));
        return toolbar;
    }

    private String getParentId() {
        String id = lastJob == null ? null : lastJob.getAttribute(WorkflowJobDataSource.FIELD_PARENTID);
        return id;
    }

    private SaveAction createSaveAction() {
        return new SaveAction(i18n) {

            @Override
            public boolean accept(ActionEvent event) {
                return handler != null
                        && jobForm.getValue(WorkflowJobDataSource.FIELD_ID) != null
                        && getJobValues().valuesHaveChanged();
            }

            @Override
            public void performAction(ActionEvent event) {
                if (handler != null) {
                    handler.onSave(WorkflowJobFormView.this);
                }
            }
        };
    }

    private Action createParentAction() {
        return new AbstractAction(i18n.WorkflowJob_FormView_OpenParentAction_Title(),
                Page.getAppDir() + "images/16/next_up.png",
                i18n.WorkflowJob_FormView_OpenParentAction_Hint()) {

            @Override
            public boolean accept(ActionEvent event) {
                return handler != null && getParentId() != null;
            }

            @Override
            public void performAction(ActionEvent event) {
                handler.open(new WorkflowJobPlace().setJobId(getParentId()));
            }

        };
    }

    private Widget createForm() {
        jobForm = new DynamicForm();
        jobForm.setDataSource(WorkflowJobDataSource.getInstance());
        jobForm.setNumCols(3);
        jobForm.setColWidths("*", "*", "*");
        jobForm.setTitleOrientation(TitleOrientation.TOP);
        jobForm.setItemHoverWidth(300);
        jobForm.setShowResizeBar(true);

        CanvasSizePersistence sizePersistence = new CanvasSizePersistence("WorkflowJobFormView.form", jobForm);
        jobForm.setHeight(sizePersistence.getHeight());

        SelectItem owner = new SelectItem(WorkflowJobDataSource.FIELD_OWNER);
        owner.setOptionDataSource(UserDataSource.getInstance());
        owner.setValueField(UserDataSource.FIELD_ID);
        owner.setDisplayField(UserDataSource.FIELD_USERNAME);

//        AutoFitTextAreaItem note = new AutoFitTextAreaItem(WorkflowJobDataSource.FIELD_NOTE);
        // There is a bug in AutoFitTextAreaItem. It does not resize properly.
        TextAreaItem note = new TextAreaItem(WorkflowJobDataSource.FIELD_NOTE);
        note.setStartRow(true);
        note.setColSpan("*");
        note.setWidth("*");
        note.setMinHeight(50);
        note.setHeight("*");

        // title tooltip is broken in SmartGWT 4.0
        final FormItemIcon jobHelpIcon = new FormItemIcon();
        jobHelpIcon.setSrc("[SKIN]/actions/help.png");
        jobHelpIcon.setTabIndex(-1);
        jobHelpIcon.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                String hint = jobForm.getValueAsString(WorkflowJobDataSource.FIELD_PROFILE_HINT);
                jobHelpIcon.setPrompt(hint);
                return hint != null;
            }
        });
        final TextItem label = new TextItem(WorkflowJobDataSource.FIELD_LABEL);
        label.setColSpan("*");
        label.setWidth("*");
        label.setIcons(jobHelpIcon);

        jobForm.setFields(label,
                new SelectItem(WorkflowJobDataSource.FIELD_STATE),
                new SelectItem(WorkflowJobDataSource.FIELD_PRIORITY),
                new TextItem(WorkflowJobDataSource.FIELD_PROFILE_ID),
                owner,
                new TextItem(WorkflowJobDataSource.FIELD_FINANCED),
                new TextItem(WorkflowJobDataSource.FIELD_ID),
                new TextItem(WorkflowJobDataSource.FIELD_CREATED),
                new TextItem(WorkflowJobDataSource.FIELD_MODIFIED),
                note
                );
        jobForm.addItemChangedHandler(new ItemChangedHandler() {

            @Override
            public void onItemChanged(ItemChangedEvent event) {
                refreshState();
            }
        });
        jobForm.addSubmitValuesHandler(new SubmitValuesHandler() {

            @Override
            public void onSubmitValues(SubmitValuesEvent event) {
                if (handler != null) {
                    handler.onSave(WorkflowJobFormView.this);
                }
            }
        });
        jobForm.setSaveOnEnter(true);
        return jobForm;
    }

    private Widget createTaskList() {
        taskView = new ListGrid();
        taskView.setCanSort(false);
        taskView.setDataFetchMode(FetchMode.BASIC);
        taskView.setGenerateDoubleClickOnEnter(true);
        taskView.setShowResizeBar(true);
        taskView.setResizeBarTarget("next");
        ListGridPersistance taskViewPersistance = new ListGridPersistance("WorkflowJobFormView.taskList", taskView);

        ResultSet rs = new ResultSet();
        rs.setCriteriaPolicy(CriteriaPolicy.DROPONCHANGE);
        rs.setUseClientFiltering(false);
        rs.setUseClientSorting(true);
        taskView.setDataProperties(rs);

        taskView.setDataSource(WorkflowTaskDataSource.getInstance(),
                new ListGridField(WorkflowTaskDataSource.FIELD_LABEL),
                new ListGridField(WorkflowTaskDataSource.FIELD_OWNER, 50),
                new ListGridField(WorkflowTaskDataSource.FIELD_STATE, 50),
                new ListGridField(WorkflowTaskDataSource.FIELD_TYPE),
                new ListGridField(WorkflowTaskDataSource.FIELD_PRIORITY, 60),
                new ListGridField(WorkflowTaskDataSource.FIELD_CREATED, 100),
                new ListGridField(WorkflowTaskDataSource.FIELD_MODIFIED, 100),
                new ListGridField(WorkflowTaskDataSource.FIELD_ID, 30),
                new ListGridField(WorkflowTaskDataSource.FIELD_NOTE)
        );
        taskView.getField(WorkflowTaskDataSource.FIELD_TYPE).setHidden(true);
        taskView.getField(WorkflowTaskDataSource.FIELD_PRIORITY).setHidden(true);
        taskView.getField(WorkflowTaskDataSource.FIELD_CREATED).setHidden(true);
        taskView.getField(WorkflowTaskDataSource.FIELD_MODIFIED).setHidden(true);
        taskView.getField(WorkflowTaskDataSource.FIELD_ID).setHidden(true);
        taskView.getField(WorkflowTaskDataSource.FIELD_NOTE).setHidden(true);
        taskView.setViewState(taskViewPersistance.getViewState());

        taskView.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                refreshState();
            }
        });
        taskView.addRecordDoubleClickHandler(new RecordDoubleClickHandler() {

            @Override
            public void onRecordDoubleClick(RecordDoubleClickEvent event) {
                ActionEvent evt = new ActionEvent(actionSource);
                if (editTaskAction.accept(evt)) {
                    editTaskAction.performAction(evt);
                }
            }
        });

        this.editTaskAction = new AbstractAction(
                i18n.WorkflowJob_FormView_EditTaskAction_Title(),
                "[SKIN]/actions/edit.png",
                i18n.WorkflowJob_FormView_EditTaskAction_Hint()) {

            @Override
            public boolean accept(ActionEvent event) {
                return taskView.getSelectedRecords().length == 1;
            }

            @Override
            public void performAction(ActionEvent event) {
                ListGridRecord selectedRecord = taskView.getSelectedRecord();
                handler.onGotoTask(selectedRecord.getAttribute(WorkflowTaskDataSource.FIELD_ID));
            }
        };
        Menu ctxMenu = Actions.createMenu();
        ctxMenu.addItem(createAddTaskMenuItem());
        ctxMenu.addItem(Actions.asMenuItem(editTaskAction, actionSource, false));
        taskView.setContextMenu(ctxMenu);
        Actions.fixListGridContextMenu(taskView);
        return taskView;
    }

    private MenuItem createAddTaskMenuItem() {
        addTaskMenu = new Menu();
        addTaskMenu.addItemClickHandler(new ItemClickHandler() {

            @Override
            public void onItemClick(ItemClickEvent event) {
                if (handler != null) {
                    Record taskDef = event.getRecord();
                    Record newTask = new Record();
                    newTask.setAttribute(WorkflowModelConsts.TASK_JOBID,
                            jobForm.getValue(WorkflowModelConsts.JOB_ID));
                    newTask.setAttribute(WorkflowModelConsts.TASK_PROFILENAME,
                            taskDef.getAttribute(WorkflowProfileConsts.NAME));
                    handler.onCreateNewTask(WorkflowJobFormView.this, newTask);
                }
            }
        });
        MenuItem addTaskMenuItem = Actions.asMenuItem(new AbstractAction(
                i18n.WorkflowJob_FormView_AddTaskAction_Title(),
                "[SKIN]/actions/add.png",
                i18n.WorkflowJob_FormView_AddTaskAction_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
            }
        }, taskView);
        addTaskMenuItem.setSubmenu(addTaskMenu);
        return addTaskMenuItem;
    }

    private Widget createMaterialList() {
        materialView = new WorkflowMaterialView(i18n, true, this.getClass().getSimpleName());
        return materialView.getWidget();
    }

}
