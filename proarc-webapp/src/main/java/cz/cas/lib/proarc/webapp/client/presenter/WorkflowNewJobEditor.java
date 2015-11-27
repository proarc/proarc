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
import com.smartgwt.client.data.Record;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.widgets.Canvas;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ErrorHandler;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowJobDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowManaging.WorkflowJobPlace;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.client.widget.workflow.WorkflowNewJobView;
import java.util.Map;

/**
 * Creates a new workflow job.
 *
 * @author Jan Pokorsky
 */
public class WorkflowNewJobEditor {

    private final ClientMessages i18n;
    private WorkflowNewJobView view;
    private final PlaceController places;

    public WorkflowNewJobEditor(ClientMessages i18n, PlaceController places) {
        this.i18n = i18n;
        this.places = places;
    }

    public Canvas getUI() {
        if (view == null) {
            view = new WorkflowNewJobView(i18n);
            view.setHandler(this);
        }
        return view.getWidget();
    }

    public void init() {
        if (view != null) {
            view.init();
        }
    }

    public void onCreateNew(Record query) {
        DSRequest dsRequest = new DSRequest();
        dsRequest.setWillHandleError(true);
        WorkflowJobDataSource.getInstance().addData(query, new DSCallback() {

            @Override
            public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                if (RestConfig.isStatusOk(dsResponse)) {
                    StatusView.getInstance().show(i18n.DigitalObjectCreator_FinishedStep_Done_Msg());
                    Record[] records = dsResponse.getData();
                    String jobId = records.length == 0 ? null : records[0].getAttribute(WorkflowJobDataSource.FIELD_ID);
                    places.goTo(new WorkflowJobPlace().setJobId(jobId));
                } else {
                    if (RPCResponse.STATUS_VALIDATION_ERROR == dsResponse.getStatus()) {
                        Map<?,?> errors = dsResponse.getErrors();
                        view.getOptions().setErrors(errors, true);
                    } else {
                        ErrorHandler.warn(dsResponse, dsRequest);
                    }
                }
            }
        }, dsRequest);
    }

}
