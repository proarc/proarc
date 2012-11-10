/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.incad.pas.editor.client.action;

import com.google.gwt.user.client.Window;
import com.smartgwt.client.data.Record;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ds.ImportBatchItemDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;

/**
 * Displays digital object in FOXML format. Uses {@link Selectable} on
 * the event source to get object to show.
 *
 * @author Jan Pokorsky
 */
public final class FoxmlViewAction extends AbstractAction {

    public FoxmlViewAction(ClientMessages i18n) {
        super(i18n.FoxmlViewAction_Title(), "[SKIN]/actions/view.png", i18n.FoxmlViewAction_Hint());
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        return selection != null && selection.length == 1;
    }

    @Override
    public void performAction(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        if (selection != null && selection.length > 0 && selection[0] instanceof Record) {
            view((Record) selection[0]);
        }
    }

    public void view(Record selection) {
        String pid = selection.getAttribute(ImportBatchItemDataSource.FIELD_PID);
        String batchId = selection.getAttribute(ImportBatchItemDataSource.FIELD_BATCHID);
        view(pid, batchId);
    }

    public void view(String pid, String batchId) {
        if (pid == null) {
            throw new IllegalArgumentException("pid");
        }
        StringBuilder sb = new StringBuilder();
        sb.append(RestConfig.URL_DIGOBJECT_DISSEMINATION);
        sb.append('?').append(DigitalObjectResourceApi.DIGITALOBJECT_PID).append('=').append(pid);
        if (batchId != null) {
            sb.append('&').append(DigitalObjectResourceApi.BATCHID_PARAM).append('=').append(batchId);
        }
        Window.open(sb.toString(), "_blanc", "");
    }

}
