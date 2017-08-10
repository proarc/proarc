/*
 * Copyright (C) 2017 Martin Rumanek
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

import com.google.gwt.core.client.Callback;
import com.google.gwt.place.shared.PlaceController;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;


public class WorkflowNewJobEditor {
    private final ClientMessages i18n;
    private final PlaceController places;
    private final VLayout uiContainer;

    private final ModsMultiEditor editor;

    public WorkflowNewJobEditor(ClientMessages i18n, PlaceController places) {
        this.i18n = i18n;
        this.places = places;
        uiContainer = new VLayout();
        editor = new ModsMultiEditor(i18n);
        ToolStrip toolbar = Actions.createToolStrip();
        toolbar.setMembers(editor.getToolbarItems());

        uiContainer.addMembers(toolbar, editor.getUI());

    }

    public void open(WorkflowManaging.WorkflowNewJobEditPlace place) {

        MetaModelDataSource.getModels(false, new Callback<ResultSet, Void>() {

            @Override
            public void onFailure(Void reason) {
            }

            @Override
            public void onSuccess(ResultSet result) {
                Record modelRecord = MetaModelDataSource.getModels().findByKey(place.getModelPid());
                MetaModelDataSource.MetaModelRecord.get(modelRecord);
                MetaModelDataSource.MetaModelRecord metaModel = new MetaModelDataSource.MetaModelRecord(modelRecord);
                Record record = new Record();
                record.setAttribute(WorkflowModelConsts.JOB_ID, place.getJobId());
                record.setAttribute(DigitalObjectDataSource.FIELD_MODEL, place.getModelPid());
                record.setAttributeAsJavaObject(MetaModelDataSource.FIELD_MODELOBJECT, metaModel);
                editor.edit(DigitalObjectDataSource.DigitalObject.create(record));
            }
        });
    }

    public Canvas getUI() {
        return uiContainer;
    }

}
