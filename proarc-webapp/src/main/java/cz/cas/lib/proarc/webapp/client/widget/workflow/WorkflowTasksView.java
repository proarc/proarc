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
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.CriteriaPolicy;
import com.smartgwt.client.types.FetchMode;
import com.smartgwt.client.types.MultipleAppearance;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.HoverCustomizer;
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
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfileConsts;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ValueMapDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowTaskDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowTasksEditor;
import cz.cas.lib.proarc.webapp.client.widget.ListGridPersistance;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowTasksView implements Refreshable {

    private final ClientMessages i18n;
    private final Canvas widget;
    private ListGrid taskGrid;
    private WorkflowTaskFormView taskFormView;
    private final WorkflowTasksEditor handler;
    private ListGridPersistance taskListPersistance;
    private final ActionSource actionSource = new ActionSource(this);
    private boolean isUpdateOperation;
    private boolean isDataInitialized;
    private ListGridRecord lastSelection;

    public WorkflowTasksView(ClientMessages i18n, WorkflowTasksEditor handler) {
        this.i18n = i18n;
        this.handler = handler;
        this.widget = createMainLayout();
    }

    public Canvas getWidget() {
        return widget;
    }

    private void init() {
        if (!isDataInitialized) {
            isDataInitialized = true;
            taskGrid.fetchData(taskListPersistance.getFilterCriteria());
        }
    }

    @Override
    public void refresh() {
        if (isDataInitialized) {
            taskGrid.invalidateCache();
        } else {
            init();
        }
    }

    public void edit(String taskId) {
        if (taskId == null) {
            init();
            return ;
        }
        int taskRec = taskGrid.findIndex(
                new AdvancedCriteria(WorkflowTaskDataSource.FIELD_ID, OperatorId.EQUALS, taskId));
        if (taskRec >= 0) {
            taskGrid.selectSingleRecord(taskRec);
            taskGrid.scrollToRow(taskRec);
        } else {
            lastSelection = null;
            taskGrid.deselectAllRecords();
            Record r = new Record();
            r.setAttribute(WorkflowTaskDataSource.FIELD_ID, taskId);
            taskFormView.setTask(r);
        }
    }

    public void editSelection() {
        taskFormView.setTask(taskGrid.getSelectedRecord());
        refreshState();
    }

    public void refreshState() {
        actionSource.fireEvent();
        taskFormView.refreshState();
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
//        main.addMember(createFilter());
        main.addMember(createTaskLayout());
        return main;
    }

    private Canvas createTaskLayout() {
        VLayout left = new VLayout();
        left.addMember(createToolbar());
        left.addMember(createTaskList());
        HLayout l = new HLayout();
        l.addMember(left);
        l.addMember(createTaskFormLayout());
        return l;
    }

    private Label createPanelLabel() {
        Label lblHeader = new Label();
        String title = ClientUtils.format("<b>%s</b>", i18n.WorkflowTask_View_Title());
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

    private ToolStrip createToolbar() {
        ToolStrip toolbar = Actions.createToolStrip();
        RefreshAction refreshAction = new RefreshAction(i18n);

        toolbar.addMember(Actions.asIconButton(refreshAction, this));
        return toolbar;
    }

    private ListGrid createTaskList() {
        final ListGrid grid = new ListGrid();
        taskListPersistance = new ListGridPersistance("WorkflowTasksView.taskList", grid);
        grid.setShowFilterEditor(true);
        grid.setAllowFilterOperators(false);
        grid.setFilterOnKeypress(true);
        grid.setCanSort(true);
        grid.setCanGroupBy(false);

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
        grid.getField(WorkflowTaskDataSource.FIELD_LABEL).setCanFilter(true);
        grid.getField(WorkflowTaskDataSource.FIELD_LABEL).setCanSort(false);
        grid.getField(WorkflowTaskDataSource.FIELD_LABEL).setFilterEditorProperties(createTaskFilterEditor());

        grid.getField(WorkflowTaskDataSource.FIELD_OWNER).setCanFilter(true);
        grid.getField(WorkflowTaskDataSource.FIELD_OWNER).setCanSort(false);
        SelectItem owner = createMultiSelectColumnFilter();
        owner.setOptionDataSource(UserDataSource.getInstance());
        owner.setValueField(UserDataSource.FIELD_ID);
        owner.setDisplayField(UserDataSource.FIELD_USERNAME);
        grid.getField(WorkflowTaskDataSource.FIELD_OWNER).setFilterEditorProperties(owner);

        grid.getField(WorkflowTaskDataSource.FIELD_PRIORITY).setFilterEditorProperties(createMultiSelectColumnFilter());

        grid.getField(WorkflowTaskDataSource.FIELD_STATE).setCanFilter(true);
        grid.getField(WorkflowTaskDataSource.FIELD_STATE).setCanSort(false);
        grid.getField(WorkflowTaskDataSource.FIELD_STATE).setFilterEditorProperties(createMultiSelectColumnFilter());

        grid.getField(WorkflowTaskDataSource.FIELD_CREATED).setCanFilter(true);
        grid.getField(WorkflowTaskDataSource.FIELD_CREATED).setCanSort(true);

        grid.getField(WorkflowTaskDataSource.FIELD_MODIFIED).setCanFilter(true);
        grid.getField(WorkflowTaskDataSource.FIELD_MODIFIED).setCanSort(true);

        grid.getField(WorkflowTaskDataSource.FIELD_NOTE).setCanFilter(false);
        grid.getField(WorkflowTaskDataSource.FIELD_NOTE).setCanSort(false);

        grid.getField(WorkflowTaskDataSource.FIELD_JOB_LABEL).setCanFilter(true);
        grid.getField(WorkflowTaskDataSource.FIELD_JOB_LABEL).setFilterOnKeypress(false);
        grid.getField(WorkflowTaskDataSource.FIELD_JOB_LABEL).setCanSort(true);

        grid.getField(WorkflowTaskDataSource.FIELD_TYPE).setCanFilter(false);
        grid.getField(WorkflowTaskDataSource.FIELD_TYPE).setCanSort(false);

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

    private SelectItem createMultiSelectColumnFilter() {
        SelectItem options = new SelectItem();
        options.setMultiple(true);
        options.setMultipleAppearance(MultipleAppearance.PICKLIST);
        return options;
    }

    private FormItem createTaskFilterEditor() {
        SelectItem taskOptions = createMultiSelectColumnFilter();
        taskOptions.setMultiple(true);
        taskOptions.setMultipleAppearance(MultipleAppearance.PICKLIST);
        taskOptions.setOptionDataSource(ValueMapDataSource.getInstance()
                .getOptionDataSource(WorkflowProfileConsts.WORKFLOWITEMVIEW_TASKS_VALUEMAP));
        taskOptions.setValueField(WorkflowProfileConsts.NAME);
        taskOptions.setDisplayField(WorkflowProfileConsts.TITLE_EL);
        taskOptions.setPickListFields(new ListGridField(WorkflowProfileConsts.TITLE_EL));
        taskOptions.getPickListFields()[0].setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                Boolean disabled = record.getAttributeAsBoolean(WorkflowProfileConsts.DISABLED);
                if (value == null) {
                    return null;
                } else if (disabled != null && disabled) {
                    return "<s>" + value + "</s>";
                } else {
                    return value.toString();
                }
            }
        });

        ListGrid profilePickListProperties = new ListGrid();
        profilePickListProperties.setCanHover(true);
        profilePickListProperties.setShowHover(true);
        profilePickListProperties.setHoverWidth(300);
        profilePickListProperties.setHoverCustomizer(new HoverCustomizer() {

            @Override
            public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                String hint = record.getAttribute(WorkflowProfileConsts.HINT_EL);
                String name = record.getAttribute(WorkflowProfileConsts.NAME);
                String title = record.getAttribute(WorkflowProfileConsts.TITLE_EL);
                StringBuilder sb = new StringBuilder();
                sb.append("<b>").append(title).append("</b>");
                sb.append("<br><i>").append(name).append("</i></br>");
                if (hint != null) {
                    sb.append("<p>").append(hint).append("</p>");
                }
                return sb.toString();
            }
        });
        taskOptions.setPickListProperties(profilePickListProperties);
        return taskOptions;
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
        return taskFormView.getWidget();
    }

}
