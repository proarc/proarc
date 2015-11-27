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
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.CriteriaPolicy;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.FetchMode;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.AutoFitTextAreaItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.RecordDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordDoubleClickHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ItemClickEvent;
import com.smartgwt.client.widgets.menu.events.ItemClickHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfileConsts;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.ErrorHandler;
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
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowJobPlace;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowNewJobPlace;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowTaskPlace;
import cz.cas.lib.proarc.webapp.client.widget.ListGridPersistance;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.client.widget.workflow.WorkflowMaterialView;

/**
 * Edits jobs of the workflow.
 *
 * @author Jan Pokorsky
 */
public class WorkflowJobsEditor {

    private final ClientMessages i18n;
    private WorkflowJobView view;
    private final PlaceController places;

    public WorkflowJobsEditor(ClientMessages i18n, PlaceController places) {
        this.i18n = i18n;
        this.places = places;
    }

    public Canvas getUI() {
        if (view == null) {
            view = new WorkflowJobView(i18n);
            view.setHandler(this);
        }
        return view.getWidget();
    }

    public void open(WorkflowJobPlace place) {
        if (view != null) {
            view.edit(place.getJobId());
        }
    }

    private void onGotoTask(String taskId) {
        places.goTo(new WorkflowTaskPlace().setTaskId(taskId));
    }

    private void onCreateNew() {
        places.goTo(new WorkflowNewJobPlace());
    }

    private void onCreateNewTask(final WorkflowJobFormView jobFormView, Record taskDef) {
        DSRequest req = new DSRequest();
        req.setWillHandleError(true);
        jobFormView.taskView.addData(taskDef, new DSCallback() {

            @Override
            public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                boolean statusOk = RestConfig.isStatusOk(dsResponse);
                if (statusOk) {
                    StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                    // reload tasks to get the proper order of tasks
                    jobFormView.refresh();
//                } else if (RestConfig.isConcurrentModification(dsResponse)) {
                } else {
                    ErrorHandler.warn(dsResponse, dsRequest);
                }
            }
        }, req);
    }

    private void onSave(WorkflowJobFormView jobFormView) {
        final DynamicForm vm = jobFormView.getValues();
        if (vm.validate()) {
            view.setExpectUpdateOperation(true);
            DSRequest req = new DSRequest();
            req.setWillHandleError(true);
            vm.saveData(new DSCallback() {

                @Override
                public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                    boolean statusOk = RestConfig.isStatusOk(dsResponse);
                    if (statusOk) {
                        StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                        view.refreshState();
                        // invalidate the task cache as it may contain an outdated job name
                        DSResponse resetCache = new DSResponse();
                        resetCache.setInvalidateCache(true);
                        resetCache.setOperationType(DSOperationType.UPDATE);
                        WorkflowTaskDataSource.getInstance().updateCaches(resetCache);
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
                    vm.focus();
                }
            }, req);
        }
    }

    private static final class WorkflowJobView implements Refreshable {

        private final ClientMessages i18n;
        private final Canvas widget;
        private ListGrid jobGrid;
        private ListGridPersistance jobsPersistance;
        private WorkflowJobFormView jobFormView;
        private WorkflowJobsEditor handler;
        private final ActionSource actionSource = new ActionSource(this);
        private boolean isUpdateOperation;
        private boolean isDataInitialized;
        private ListGridRecord lastSelection;

        public WorkflowJobView(ClientMessages i18n) {
            this.i18n = i18n;
            this.widget = createMainLayout();
        }

        public Canvas getWidget() {
            return widget;
        }

        private void init() {
            if (!isDataInitialized) {
                isDataInitialized = true;
                jobGrid.fetchData(jobsPersistance.getFilterCriteria());
            }
        }

        public void edit(String jobId) {
            if (jobId == null) {
                init();
                return ;
            }
            int jobRec = jobGrid.findIndex(
                    new AdvancedCriteria(WorkflowJobDataSource.FIELD_ID, OperatorId.EQUALS, jobId));
            if (jobRec >= 0) {
                jobGrid.selectSingleRecord(jobRec);
                jobGrid.scrollToRow(jobRec);
            } else {
                lastSelection = null;
                jobGrid.deselectAllRecords();
                Record r = new Record();
                r.setAttribute(WorkflowJobDataSource.FIELD_ID, jobId);
                jobFormView.setJob(r);
            }
        }

        public void setHandler(WorkflowJobsEditor handler) {
            this.handler = handler;
            jobFormView.setHandler(handler);
        }

        @Override
        public void refresh() {
            if (isDataInitialized) {
                jobGrid.invalidateCache();
            } else {
                init();
            }
        }

        public void editSelection() {
            jobFormView.setJob(jobGrid.getSelectedRecord());
            refreshState();
        }

        public void refreshState() {
            actionSource.fireEvent();
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
            main.addMember(createJobsToolbar());
            // list + item
            main.addMember(createJobLayout());
            return main;
        }

        private Canvas createJobLayout() {
            HLayout l = new HLayout();
            l.addMember(createJobList());
            l.addMember(createJobFormLayout());
            return l;
        }

        private Label createPanelLabel() {
            Label lblHeader = new Label();
            String title = ClientUtils.format("<b>%s</b>", "Správa záměrů");//i18n.DigitalObjectManager_Title());
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
            SaveAction saveAction = createSaveAction();

            AbstractAction addAction = new AbstractAction("Nový",//i18n.DeviceManager_Add_Title(),
                    "[SKIN]/actions/add.png", "Nový záměr") {//i18n.DeviceManager_Add_Hint()) {

                @Override
                public void performAction(ActionEvent event) {
                    if (handler != null) {
                        handler.onCreateNew();
                    }
                }
            };
            toolbar.addMember(Actions.asIconButton(refreshAction, this));
            toolbar.addMember(Actions.asIconButton(addAction, this));
            toolbar.addMember(Actions.asIconButton(saveAction, actionSource));
            return toolbar;
        }

        private SaveAction createSaveAction() {
            return new SaveAction(i18n) {

                @Override
                public boolean accept(ActionEvent event) {
                    return handler != null
                            && jobGrid.getSelectedRecords().length > 0
                            && jobFormView.getValues().valuesHaveChanged();
                }

                @Override
                public void performAction(ActionEvent event) {
                    if (handler != null) {
                        handler.onSave(jobFormView);
                    }
                }
            };
        }

        private ListGrid createJobList() {
            jobGrid = new ListGrid();
            jobsPersistance = new ListGridPersistance("WorkflowJobView.jobList", jobGrid);
            jobGrid.setSelectionType(SelectionStyle.SINGLE);
            jobGrid.setShowFilterEditor(true);
            jobGrid.setFilterOnKeypress(true);
            jobGrid.setFilterLocalData(false);
            jobGrid.setCanSort(true);
            jobGrid.setDataFetchMode(FetchMode.PAGED);
            ResultSet rs = new ResultSet();
            rs.setCriteriaPolicy(CriteriaPolicy.DROPONCHANGE);
            rs.setUseClientFiltering(false);
            rs.setUseClientSorting(false);
            jobGrid.setDataProperties(rs);
            jobGrid.setDataSource(WorkflowJobDataSource.getInstance(),
                    new ListGridField(WorkflowJobDataSource.FIELD_LABEL),
                    new ListGridField(WorkflowJobDataSource.FIELD_ID, 30),
                    new ListGridField(WorkflowJobDataSource.FIELD_STATE, 50),
                    new ListGridField(WorkflowJobDataSource.FIELD_PROFILE_ID, 80),
                    new ListGridField(WorkflowJobDataSource.FIELD_OWNER, 50),
                    new ListGridField(WorkflowJobDataSource.FIELD_PRIORITY, 60),
                    new ListGridField(WorkflowJobDataSource.FIELD_CREATED, 100),
                    new ListGridField(WorkflowJobDataSource.FIELD_MODIFIED, 100),
                    new ListGridField(WorkflowJobDataSource.FIELD_FINANCED, 100),
                    new ListGridField(WorkflowJobDataSource.FIELD_NOTE)
                    );

            jobGrid.getField(WorkflowJobDataSource.FIELD_LABEL).setWidth("80%");
            jobGrid.getField(WorkflowJobDataSource.FIELD_LABEL).setFilterOnKeypress(false);

            jobGrid.getField(WorkflowJobDataSource.FIELD_STATE).setCanSort(false);

            jobGrid.getField(WorkflowJobDataSource.FIELD_PROFILE_ID).setCanSort(false);
            SelectItem profileFilter = new SelectItem();
            profileFilter.setOptionDataSource(WorkflowProfileDataSource.getInstance());
            profileFilter.setValueField(WorkflowProfileDataSource.FIELD_ID);
            profileFilter.setDisplayField(WorkflowProfileDataSource.FIELD_LABEL);
            jobGrid.getField(WorkflowJobDataSource.FIELD_PROFILE_ID).setFilterEditorProperties(profileFilter);

            jobGrid.getField(WorkflowJobDataSource.FIELD_OWNER).setCanSort(false);
            SelectItem owner = new SelectItem();
            owner.setOptionDataSource(UserDataSource.getInstance());
            owner.setValueField(UserDataSource.FIELD_ID);
            owner.setDisplayField(UserDataSource.FIELD_USERNAME);
            jobGrid.getField(WorkflowJobDataSource.FIELD_OWNER).setFilterEditorProperties(owner);

            jobGrid.getField(WorkflowJobDataSource.FIELD_FINANCED).setCanFilter(false);
            jobGrid.getField(WorkflowJobDataSource.FIELD_FINANCED).setCanSort(false);

            jobGrid.getField(WorkflowJobDataSource.FIELD_NOTE).setCanFilter(false);
            jobGrid.getField(WorkflowJobDataSource.FIELD_NOTE).setCanSort(false);

            jobGrid.addDataArrivedHandler(new DataArrivedHandler() {

                @Override
                public void onDataArrived(DataArrivedEvent event) {
                    if (isUpdateOperation) {
                        isUpdateOperation = false;
                        return ;
                    }
                    int startRow = event.getStartRow();
                    int endRow = event.getEndRow();
                    if (startRow == 0 && endRow >= 0) {
                        jobGrid.focus();
                        updateSelection();
                    } else if (endRow < 0) {
                        jobGrid.deselectAllRecords();
                    }
                }
            });
            jobGrid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

                @Override
                public void onSelectionUpdated(SelectionUpdatedEvent event) {
                    lastSelection = jobGrid.getSelectedRecord();
                    editSelection();
                }
            });
            jobGrid.setViewState(jobsPersistance.getViewState());
            return jobGrid;
        }

        private void updateSelection() {
            RecordList rl = jobGrid.getRecordList();
            if (rl.isEmpty()) {
                return ;
            }
            if (lastSelection == null) {
                jobGrid.selectSingleRecord(0);
                return ;
            }
            Record newRec = rl.find(WorkflowJobDataSource.FIELD_ID,
                    lastSelection.getAttribute(WorkflowJobDataSource.FIELD_ID));
            if (newRec != null) {
                jobGrid.selectSingleRecord(newRec);
                int rowNum = jobGrid.getRecordIndex(newRec);
                if (rowNum >= 0) {
                    jobGrid.scrollToRow(rowNum);
                }
            }
        }

        private Canvas createJobFormLayout() {
            jobFormView = new WorkflowJobFormView(i18n);
            jobFormView.getValues().addItemChangedHandler(new ItemChangedHandler() {

                @Override
                public void onItemChanged(ItemChangedEvent event) {
                    refreshState();
                }
            });
            jobFormView.getValues().addSubmitValuesHandler(new SubmitValuesHandler() {

                @Override
                public void onSubmitValues(SubmitValuesEvent event) {
                    if (handler != null) {
                        handler.onSave(jobFormView);
                    }
                }
            });
            jobFormView.getValues().setSaveOnEnter(true);
            return jobFormView.getWidget();
        }

    }

    private static final class WorkflowJobFormView implements Refreshable {

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

        public void setHandler(WorkflowJobsEditor handler) {
            this.handler = handler;
        }

        @Override
        public void refresh() {
            taskView.invalidateCache();
            setJob(lastJob);
        }

        public void setJob(Record job) {
            this.lastJob = job;
            fetchAddTaskMenu(null);
            if (job != null) {
                String jobId = job.getAttribute(WorkflowJobDataSource.FIELD_ID);
                jobForm.clearErrors(true);
                jobForm.fetchData(new Criteria(WorkflowJobDataSource.FIELD_ID, jobId));
                taskView.fetchData(
                        new Criteria(WorkflowModelConsts.TASK_FILTER_JOBID, jobId),
                        new DSCallback() {

                    @Override
                    public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                        if (RestConfig.isStatusOk(dsResponse)) {
                            Record[] records = dsResponse.getData();
                            if (records.length > 0) {
                                fetchAddTaskMenu(records[0].getAttribute(WorkflowJobDataSource.FIELD_PROFILE_ID));
                            }
                        }
                    }
                });
                materialView.getMaterialGrid().invalidateCache();
                materialView.getMaterialGrid().fetchData(
                        new Criteria(WorkflowModelConsts.MATERIALFILTER_JOBID, jobId));
            } else {
                jobForm.clearValues();
                materialView.getMaterialGrid().setData(new Record[0]);
                taskView.setData(new Record[0]);
            }
            widget.setDisabled(job == null);
            actionSource.fireEvent();
        }

        public DynamicForm getValues() {
            return jobForm;
        }

        private void fetchAddTaskMenu(final String jobName) {
            addTaskMenu.setData(new Record[0]);
            if (jobName == null) {
                return ;
            }
            WorkflowProfileDataSource.getInstance().fetchData(
                    new Criteria(WorkflowProfileConsts.NAME, jobName),
                    new DSCallback() {

                @Override
                public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                    Record[] taskRecs = new Record[0];
                    if (RestConfig.isStatusOk(dsResponse)) {
                        Record jobRec = dsResponse.getDataAsRecordList().find(WorkflowProfileConsts.NAME, jobName);
                        if (jobRec != null) {
                            taskRecs = jobRec.getAttributeAsRecordArray(WorkflowProfileConsts.JOBVIEW_TASK);
                        }
                    }
                    addTaskMenu.setData(taskRecs);
                }
            }, null);
        }

        private Canvas createMainLayout() {
            VLayout main = new VLayout();
            main.addMember(createForm());
            main.addMember(createTaskList());
            main.addMember(createMaterialList());
            return main;
        }

        private Widget createForm() {
            jobForm = new DynamicForm();
            jobForm.setDataSource(WorkflowJobDataSource.getInstance());
            jobForm.setNumCols(3);
            jobForm.setColWidths("*", "*", "*");
            jobForm.setTitleOrientation(TitleOrientation.TOP);

            SelectItem owner = new SelectItem(WorkflowJobDataSource.FIELD_OWNER);
            owner.setOptionDataSource(UserDataSource.getInstance());
            owner.setValueField(UserDataSource.FIELD_ID);
            owner.setDisplayField(UserDataSource.FIELD_USERNAME);

            AutoFitTextAreaItem note = new AutoFitTextAreaItem(WorkflowJobDataSource.FIELD_NOTE);
            note.setStartRow(true);
            note.setColSpan("*");
            note.setWidth("*");
            
            TextItem label = new TextItem(WorkflowJobDataSource.FIELD_LABEL);
            label.setColSpan("*");
            label.setWidth("*");

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
            return jobForm;
        }

        private Widget createTaskList() {
            taskView = new ListGrid();
            taskView.setCanSort(false);
            taskView.setDataFetchMode(FetchMode.BASIC);
            taskView.setGenerateDoubleClickOnEnter(true);

            ResultSet rs = new ResultSet();
            rs.setCriteriaPolicy(CriteriaPolicy.DROPONCHANGE);
            rs.setUseClientFiltering(false);
            rs.setUseClientSorting(true);
            taskView.setDataProperties(rs);

            taskView.setDataSource(WorkflowTaskDataSource.getInstance(),
                    new ListGridField(WorkflowTaskDataSource.FIELD_LABEL),
                    new ListGridField(WorkflowTaskDataSource.FIELD_OWNER, 50),
                    new ListGridField(WorkflowTaskDataSource.FIELD_STATE, 50)
            );
            taskView.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

                @Override
                public void onSelectionUpdated(SelectionUpdatedEvent event) {
                    actionSource.fireEvent();
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

            this.editTaskAction = new AbstractAction("Editovat", "[SKIN]/actions/edit.png", null) {

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
                    "Přidat krok", "[SKIN]/actions/add.png", null) {

                @Override
                public void performAction(ActionEvent event) {
                }
            }, taskView);
            addTaskMenuItem.setSubmenu(addTaskMenu);
            return addTaskMenuItem;
        }

        private Widget createMaterialList() {
            materialView = new WorkflowMaterialView(i18n, true);
            return materialView.getWidget();
        }

    }

}
