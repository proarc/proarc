/*
 * Copyright (C) 2022 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.action.administration.indexObjects;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.ds.IndexAllObjectsDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import java.util.logging.Logger;

/**
 * @author Lukas Sykora
 */
public final class IndexAllObjectsAction extends AbstractAction {

    private static final Logger LOG = Logger.getLogger(IndexAllObjectsAction.class.getName());

    private final ClientMessages i18n;

    public IndexAllObjectsAction(ClientMessages i18n) {
        super(i18n.DigitalObjectIndexAllObjectsAction_Title(), null, i18n.DigitalObjectIndexAllObjectsAction_FinishMessage());
        this.i18n = i18n;
    }

    @Override
    public boolean accept(ActionEvent event) {
        if (!(Editor.getInstance().hasPermission("proarc.permission.admin") || Editor.getInstance().hasPermission(UserRole.ROLE_SUPERADMIN))) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] records = Actions.getSelection(event);
        Record localRecord = null;
        for (Record record : records) {
            localRecord = record;
        }
        askAndIndexAll(localRecord);
    }

    private void askAndIndexAll(Record localRecord) {
        SC.ask(i18n.DigitalObjectIndexAllObjectsActionAsk_Title(),
                i18n.DigitalObjectIndexAllObjectsActionAsk_Message(),
                new BooleanCallback() {

                    @Override
                    public void execute(Boolean value) {
                        if (value != null && value) {
                            indexAll(localRecord);
                        }
                    }
                });
    }

    private void indexAll(Record record) {
        DSRequest dsRequest = new DSRequest();
        dsRequest.setHttpMethod("POST");
        IndexAllObjectsDataSource ds = IndexAllObjectsDataSource.getInstance();
        if (record == null) {
            record = new Record();
        }
        ds.addData(record, new DSCallback() {
            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (hasValidationError(response)) {
                    handleValidations(response);
                } else if (RestConfig.isStatusOk(response)) {
                    StatusView.getInstance().show(i18n.DigitalObjectIndexAllObjectsAction_FinishMessage());
                }
            }
        }, dsRequest);
    }
}
