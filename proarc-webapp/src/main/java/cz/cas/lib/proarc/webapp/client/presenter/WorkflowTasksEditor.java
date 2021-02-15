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
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ErrorHandler;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowJobDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowTaskDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowJobPlace;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowTaskPlace;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.client.widget.workflow.WorkflowTaskFormView;
import cz.cas.lib.proarc.webapp.client.widget.workflow.WorkflowTasksView;

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
        }
        return view.getWidget();
    }

    public void open(WorkflowTaskPlace place) {
        if (view != null) {
            view.edit(place.getTaskId());
        }
    }

    public void onOpenJob(String jobId) {
        places.goTo(new WorkflowJobPlace().setJobId(jobId));
    }

    public void onSave(WorkflowTaskFormView taskFormView) {
        if (taskFormView.validate()) {
            view.setExpectUpdateOperation(true);
            DSRequest req = new DSRequest();
            req.setWillHandleError(true);
            final DynamicForm taskForm = taskFormView.getTask();
            Object oldState = taskForm.getOldValues().get(WorkflowTaskDataSource.FIELD_STATE);
            String newState = taskForm.getValueAsString(WorkflowTaskDataSource.FIELD_STATE);
            final boolean stateChanged = !newState.equals(oldState);
            taskForm.saveData(new DSCallback() {

                @Override
                public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                    boolean statusOk = RestConfig.isStatusOk(dsResponse);
                    if (statusOk) {
                        view.refreshState();
                        String taskId = taskForm.getValueAsString(WorkflowTaskDataSource.FIELD_ID);
                        view.refreshParameters(taskId);
                        if (stateChanged) {
                            StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                            DSResponse reset = new DSResponse();
                            reset.setOperationType(DSOperationType.UPDATE);
                            reset.setInvalidateCache(true);
                            WorkflowTaskDataSource.getInstance().updateCaches(reset);
                            WorkflowJobDataSource.getInstance().updateCaches(reset);
                        }
                        places.goTo(new WorkflowJobPlace());
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

}
