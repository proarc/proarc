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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.action.administration.lockModels;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.LockObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;

/**
 * Lock Object.
 *
 * @author Lukas Sykora
 */
public class LockObjectAction extends AbstractAction {

    private final ClientMessages i18n;

    public LockObjectAction(ClientMessages i18n) {
        this(i18n, i18n.LockObjectAction_Title(),
                "16/locked.png",
                i18n.LockObjectAction_Hint());
    }

    public LockObjectAction(ClientMessages i18n, String title, String icon, String tooltip) {
        super(title, icon, tooltip);
        this.i18n = i18n;
    }

    @Override
    public boolean accept(ActionEvent event) {
        if (!(Editor.getInstance().hasPermission("proarc.permission.admin") ||
                Editor.getInstance().hasPermission(UserRole.ROLE_SUPERADMIN) ||
                Editor.getInstance().hasPermission(UserRole.PERMISSION_RUN_LOCK_OBJECT_FUNCTION))) {
            return false;
        }
        Object[] selection = Actions.getSelection(event);
        if (selection != null && selection instanceof Record[]) {
            Record[] records = (Record[]) selection;
            return acceptRecords(records);
        }
        return true;
    }

    private boolean acceptRecords(Record[] records) {
        for (Record record : records) {
            DigitalObject dobj = DigitalObject.createOrNull(record);
            if (dobj != null) {
                boolean isLocked = dobj.isLocked();
                if (isLocked) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] records = Actions.getSelection(event);
        Record record = new Record();
        for (Record recordLocal : records){
            DigitalObject dobj = DigitalObject.createOrNull(recordLocal);
            if (dobj != null) {
                record = recordLocal;
                continue;
            }
        }
        register(record);
    }

    private void register(Record record) {
        DSRequest dsRequest = new DSRequest();
        dsRequest.setHttpMethod("POST");
        LockObjectDataSource ds = LockObjectDataSource.lockObject();
        ds.addData(record, new DSCallback() {
            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    StatusView.getInstance().show(i18n.LockObjectAction_Done_Msg());
                    SearchDataSource.getInstance().updateCaches(response, request);
                    RelationDataSource.getInstance().updateCaches(response, request);
                }
            }
        }, dsRequest);
    }
}
