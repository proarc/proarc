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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.client.action;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.PromptStyle;
import com.smartgwt.client.util.SC;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ds.ExportDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import cz.incad.pas.editor.client.ds.SearchDataSource;
import cz.incad.pas.editor.shared.rest.ExportResourceApi;
import java.util.ArrayList;
import java.util.List;

/**
 * Exports selected digital objects in Kramerius 4 format.
 *
 * It expects the event source to implement {@link Selectable}.
 *
 * @author Jan Pokorsky
 */
public final class KrameriusExportAction extends AbstractAction {

    private final ClientMessages i18n;

    public KrameriusExportAction(ClientMessages i18n) {
        super(i18n.KrameriusExportAction_Title(), null, i18n.KrameriusExportAction_Hint());
        this.i18n = i18n;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] selection = Actions.getSelection(event);
        if (selection != null && selection.length > 0) {
            ArrayList<String> pids = new ArrayList<String>(selection.length);
            for (Record record : selection) {
                String pid = record.getAttribute(SearchDataSource.FIELD_PID);
                if (pid != null) {
                    pids.add(pid);
                }
            }
            export(pids);
        }
    }

    private void export(List<String> pids) {
        DataSource ds = ExportDataSource.getKramerius4();
        Record export = new Record();
        export.setAttribute(ExportResourceApi.KRAMERIUS4_PID_PARAM,
                pids.toArray(new String[pids.size()]));
        DSRequest dsRequest = new DSRequest();
        dsRequest.setPromptStyle(PromptStyle.DIALOG);
        dsRequest.setPrompt(i18n.KrameriusExportAction_Add_Msg());
        ds.addData(export, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    SC.say(i18n.KrameriusExportAction_AddDone_Msg(
                            data[0].getAttribute(ExportResourceApi.RESULT_TARGET)));
                }
            }
        }, dsRequest);
    }

}
