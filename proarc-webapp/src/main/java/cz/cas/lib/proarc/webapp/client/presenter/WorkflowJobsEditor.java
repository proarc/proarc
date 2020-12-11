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
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.grid.ListGrid;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ErrorHandler;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowJobDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowJobPlace;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowNewJobPlace;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowTaskPlace;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.client.widget.workflow.WorkflowJobFormView;
import cz.cas.lib.proarc.webapp.client.widget.workflow.WorkflowJobView;

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

    public void onGotoTask(String taskId) {
        places.goTo(new WorkflowTaskPlace().setTaskId(taskId));
    }

    public void onCreateNew() {
        places.goTo(new WorkflowNewJobPlace());
    }

    public void onCreateNewTask(final WorkflowJobFormView jobFormView, Record taskDef) {
        DSRequest req = new DSRequest();
        req.setWillHandleError(true);
        jobFormView.getTasks().addData(taskDef, new DSCallback() {

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

    public void onSave(WorkflowJobFormView jobFormView) {
        final DynamicForm vm = jobFormView.getJobValues();
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
                        //view.refreshState();
                        // invalidate the task cache as it may contain an outdated job name
                        //DSResponse resetCache = new DSResponse();
                        //resetCache.setInvalidateCache(true);
                        //resetCache.setOperationType(DSOperationType.UPDATE);
                       // WorkflowTaskDataSource.getInstance().updateCaches(resetCache);
                        jobFormView.refresh();
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

    public void onCreateNewSubjob(Record query, ListGrid subjobGrid) {
        DSRequest dsRequest = new DSRequest();
        dsRequest.setWillHandleError(true);
        WorkflowJobDataSource.getInstance().addData(query, new DSCallback() {
            @Override
            public void execute(DSResponse dsResponse, Object o, DSRequest dsRequest) {
                if (RestConfig.isStatusOk(dsResponse)) {
                    StatusView.getInstance().show(i18n.DigitalObjectCreator_FinishedStep_Done_Msg());
                    Record[] records = dsResponse.getData();
                    String model = records[0].getAttribute(WorkflowJobDataSource.FIELD_MODEL);
                    if (records.length > 0) {
                        int idx = subjobGrid.findIndex(new AdvancedCriteria(WorkflowJobDataSource.FIELD_ID,
                                OperatorId.EQUALS, records[0].getAttribute(WorkflowJobDataSource.FIELD_ID)));
                        subjobGrid.selectSingleRecord(idx);
                        subjobGrid.scrollToRow(idx);
                    }
                    String jobId = records.length == 0 ? null : records[0].getAttribute(WorkflowJobDataSource.FIELD_ID);
                    places.goTo(new WorkflowManaging.WorkflowNewJobEditPlace().setJobId(jobId).setModelPid(model));
                }
            }
        }, dsRequest);
    }
}
