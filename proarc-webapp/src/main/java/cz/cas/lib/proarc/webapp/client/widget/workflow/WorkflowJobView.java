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

import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.CriteriaPolicy;
import com.smartgwt.client.types.FetchMode;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.events.ItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowJobDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowProfileDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowJobsEditor;
import cz.cas.lib.proarc.webapp.client.widget.CanvasSizePersistence;
import cz.cas.lib.proarc.webapp.client.widget.ListGridPersistance;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.shared.rest.WorkflowResourceApi;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowJobView implements Refreshable {

    private final ClientMessages i18n;
    private final Canvas widget;
    private ListGrid jobGrid;
    private ListGrid subjobGrid;
    private boolean ignoreSubjobSelection;
    private ListGridPersistance jobsPersistance;
    private ListGridPersistance subjobsPersistance;
    private WorkflowJobFormView jobFormView;
    private WorkflowJobsEditor handler;
    private final ActionSource actionSource = new ActionSource(this);
    private boolean isUpdateOperation;
    private boolean isDataInitialized;
    private ListGridRecord lastSelection;
    private IconMenuButton addSubjobButton;

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
            jobGrid.setViewState(jobsPersistance.getViewState());
            jobGrid.fetchData(jobsPersistance.getFilterCriteria());
            subjobGrid.setViewState(subjobsPersistance.getViewState());
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
            loadSubjobs(r);
        }
    }

    public void setHandler(WorkflowJobsEditor handler) {
        this.handler = handler;
        jobFormView.setHandler(handler);
    }

    @Override
    public void refresh() {
        if (isDataInitialized) {
            WorkflowProfileDataSource.getInstance().getProfileResultSet(true);
            jobGrid.invalidateCache();
        } else {
            init();
        }
    }

    public void editSelection() {
        ListGridRecord selection = jobGrid.getSelectedRecord();
        jobFormView.setJob(selection);
        loadSubjobs(selection);
        refreshState();
    }

    private void loadSubjobs(Record job) {
        try {
            ignoreSubjobSelection = true;
            subjobGrid.deselectAllRecords();
            if (job != null) {
                String id = job.getAttribute(WorkflowJobDataSource.FIELD_ID);
                subjobGrid.fetchData(new Criteria(WorkflowJobDataSource.FIELD_PARENTID, id));
            } else {
                subjobGrid.setData(new Record[0]);
            }
        } finally {
            ignoreSubjobSelection = false;
        }
    }

    public void editSubjobSelection() {
        jobFormView.setJob(subjobGrid.getSelectedRecord());
        actionSource.fireEvent();
        jobFormView.refreshState();
    }

    public void refreshState() {
        fetchAddSubjobMenu(jobGrid.getSelectedRecord());
        actionSource.fireEvent();
        jobFormView.refreshState();
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
        // list + item
        main.addMember(createJobLayout());
        return main;
    }

    private Canvas createJobLayout() {
        VLayout left = new VLayout();
        left.addMember(createJobsToolbar());
        left.addMember(createJobList());
        left.addMember(createSubjobList());
        left.setShowResizeBar(true);

        CanvasSizePersistence sizePersistence = new CanvasSizePersistence("WorkflowJobFormView.jobLayout", left);
        left.setWidth(sizePersistence.getWidth());

        HLayout l = new HLayout();
        l.addMember(left);
        l.addMember(createJobFormLayout());
        return l;
    }

    private Label createPanelLabel() {
        Label lblHeader = new Label();
        String title = ClientUtils.format("<b>%s</b>", i18n.WorkflowJob_View_Title());
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

        AbstractAction addAction = new AbstractAction(i18n.WorkflowJob_View_NewAction_Title(),
                "[SKIN]/actions/add.png", i18n.WorkflowJob_View_NewAction_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                if (handler != null) {
                    handler.onCreateNew();
                }
            }
        };
        Action addSubjobAction = Actions.emptyAction(i18n.WorkflowJob_View_NewSubjobAction_Title(),
                "[SKIN]/actions/add.png", i18n.WorkflowJob_View_NewSubjobAction_Hint());

        toolbar.addMember(Actions.asIconButton(refreshAction, this));
        toolbar.addMember(Actions.asIconButton(addAction, this));
        addSubjobButton = Actions.asIconMenuButton(addSubjobAction, this);
        addSubjobButton.setVisible(false);
        toolbar.addMember(addSubjobButton);
        return toolbar;
    }

    private void fetchAddSubjobMenu(Record job) {
        if (job == null
                || job.getAttribute(WorkflowJobDataSource.FIELD_PARENTID) != null
                || !Job.State.OPEN.name().equals(job.getAttribute(WorkflowJobDataSource.FIELD_STATE))
                ) {
            addSubjobButton.setVisible(false);
            return ;
        }
        String jobName = job.getAttribute(WorkflowJobDataSource.FIELD_PROFILE_ID);
        if (jobName == null) {
            return ;
        }
        WorkflowProfileDataSource.getInstance().getSubjobs(false, jobName, (subjobs) -> {
            Menu menu = createSubjobMenu(subjobs);
            addSubjobButton.setVisible(menu != null);
            addSubjobButton.setMenu(menu);
        });
    }

    private Menu createSubjobMenu(Record[] subjobs) {
        if (subjobs == null || subjobs.length == 0) {
            return null;
        }
        Menu menu = new Menu();
        menu.setData(subjobs);
        menu.addItemClickHandler((ItemClickEvent event) -> {
            Record subjob = event.getRecord();
            Record job = jobGrid.getSelectedRecord();
            createSubjob(job, subjob);
        });
        return menu;
    }

    private void createSubjob(Record job, Record subjob) {
        Record query = new Record();
        query.setAttribute(WorkflowResourceApi.NEWJOB_PARENTID, job.getAttribute(WorkflowJobDataSource.FIELD_ID));
        query.setAttribute(WorkflowResourceApi.NEWJOB_PROFILE, subjob.getAttribute(WorkflowProfileDataSource.FIELD_ID));
        WorkflowJobDataSource ds = WorkflowJobDataSource.getInstance();
        ds.addData(query, (dsResponse, data, dsRequest) -> {
            if (RestConfig.isStatusOk(dsResponse)) {
                StatusView.getInstance().show(i18n.DigitalObjectCreator_FinishedStep_Done_Msg());
                Record[] records = dsResponse.getData();
                if (records.length > 0) {
                    int idx = subjobGrid.findIndex(new AdvancedCriteria(WorkflowJobDataSource.FIELD_ID,
                            OperatorId.EQUALS, records[0].getAttribute(WorkflowJobDataSource.FIELD_ID)));
                    subjobGrid.selectSingleRecord(idx);
                    subjobGrid.scrollToRow(idx);
                }
            }
        });
    }

    private ListGrid createSubjobList() {
        ListGrid g = new ListGrid();
        subjobGrid = g;
        subjobsPersistance = new ListGridPersistance("WorkflowJobView.subjobList", g);

        CanvasSizePersistence sizePersistence = new CanvasSizePersistence("WorkflowJobView.subjobList", g);
        g.setHeight(sizePersistence.getHeight());

        g.setSelectionType(SelectionStyle.SINGLE);
        g.setCanGroupBy(false);
        g.setDataFetchMode(FetchMode.BASIC);
        g.setDataSource(WorkflowJobDataSource.getInstance(),
                new ListGridField(WorkflowJobDataSource.FIELD_LABEL),
                new ListGridField(WorkflowJobDataSource.FIELD_ID, 30),
                new ListGridField(WorkflowJobDataSource.FIELD_STATE, 50),
                new ListGridField(WorkflowJobDataSource.FIELD_PROFILE_ID, 80),
                new ListGridField(WorkflowJobDataSource.FIELD_OWNER, 50),
                new ListGridField(WorkflowJobDataSource.FIELD_PRIORITY, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_CREATED, 100),
                new ListGridField(WorkflowJobDataSource.FIELD_MODIFIED, 100),
                new ListGridField(WorkflowJobDataSource.FIELD_FINANCED, 100),
                new ListGridField(WorkflowJobDataSource.FIELD_MBARCODE, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MDETAIL, 100),
                new ListGridField(WorkflowJobDataSource.FIELD_MFIELD001, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MISSUE, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MSIGLA, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MSIGNATURE, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MVOLUME, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MYEAR, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_NOTE)
                );
        g.setSortField(WorkflowJobDataSource.FIELD_CREATED);
        g.setSortDirection(SortDirection.ASCENDING);

        SelectItem profileFilter = new SelectItem();
        profileFilter.setOptionDataSource(WorkflowProfileDataSource.getInstance());
        profileFilter.setValueField(WorkflowProfileDataSource.FIELD_ID);
        profileFilter.setDisplayField(WorkflowProfileDataSource.FIELD_LABEL);
        g.getField(WorkflowJobDataSource.FIELD_PROFILE_ID).setFilterEditorProperties(profileFilter);

        SelectItem owner = new SelectItem();
        owner.setOptionDataSource(UserDataSource.getInstance());
        owner.setValueField(UserDataSource.FIELD_ID);
        owner.setDisplayField(UserDataSource.FIELD_USERNAME);
        g.getField(WorkflowJobDataSource.FIELD_OWNER).setFilterEditorProperties(owner);

        g.addSelectionUpdatedHandler((SelectionUpdatedEvent event) -> {
            if (!ignoreSubjobSelection) {
                editSubjobSelection();
            }
            ignoreSubjobSelection = false;
        });
        return g;
    }

    private ListGrid createJobList() {
        jobGrid = new ListGrid();
        jobsPersistance = new ListGridPersistance("WorkflowJobView.jobList", jobGrid);
        jobGrid.setSelectionType(SelectionStyle.SINGLE);
        jobGrid.setShowFilterEditor(true);
        jobGrid.setAllowFilterOperators(false);
        jobGrid.setFilterOnKeypress(true);
        jobGrid.setFilterLocalData(false);
        jobGrid.setCanSort(true);
        jobGrid.setCanGroupBy(false);
        jobGrid.setShowResizeBar(true);
        jobGrid.setResizeBarTarget("next");
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
                new ListGridField(WorkflowJobDataSource.FIELD_MBARCODE, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MDETAIL, 100),
                new ListGridField(WorkflowJobDataSource.FIELD_MFIELD001, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MISSUE, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MSIGLA, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MSIGNATURE, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MVOLUME, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_MYEAR, 60),
                new ListGridField(WorkflowJobDataSource.FIELD_NOTE)
                );

        jobGrid.getField(WorkflowJobDataSource.FIELD_LABEL).setWidth("80%");
        jobGrid.getField(WorkflowJobDataSource.FIELD_LABEL).setFilterOnKeypress(false);

        jobGrid.getField(WorkflowJobDataSource.FIELD_ID).setFilterOperator(OperatorId.EQUALS);

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

        jobGrid.addDataArrivedHandler((DataArrivedEvent event) -> {
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
        });
        jobGrid.addSelectionUpdatedHandler((SelectionUpdatedEvent event) -> {
            lastSelection = jobGrid.getSelectedRecord();
            editSelection();
        });
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
        return jobFormView.getWidget();
    }

}
