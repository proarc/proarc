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
package cz.cas.lib.proarc.webapp.client.action.export;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.ExportDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import static cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi.KRAMERIUS4_POLICY_PARAM;

/**
 * Exports selected digital objects in Kramerius 4 format.
 *
 * It expects the event source to implement {@link Selectable}.
 *
 * @author Jan Pokorsky
 */
public final class KrameriusExportAction extends ExportAction {

    public static final String K4_POLICY_PUBLIC = "policy:public";
    public static final String K4_POLICY_PRVIATE = "policy:private";

    private RadioGroupItem rgi;

    public KrameriusExportAction(ClientMessages i18n) {
        super(i18n, i18n.KrameriusExportAction_Title(), null, i18n.KrameriusExportAction_Hint());
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        return selection != null && selection.length > 0 && selection instanceof Record[]
                && acceptMods((Record[]) selection);
    }

    /** Accepts only if all records are objects with description metadata in MODS format */
    private boolean acceptMods(Record[] records) {
        boolean accept = false;
        for (Record record : records) {
            DigitalObject dobj = DigitalObject.createOrNull(record);
            if (dobj != null) {
                MetaModelRecord model = dobj.getModel();
                String modelId = dobj.getModelId();
                if (modelId != null && (modelId.contains("ndkmusicdocument")
                        || modelId.contains("ndksong") || modelId.contains("ndktrack")
                        || modelId.contains("ndkaudiopage") || modelId.contains("ndkphonographcylinder"))) {
                    accept = false;
                    continue;
                }
               if (model != null && ModsConstants.NS.equals(model.getMetadataFormat()) && !model.getId().startsWith("model:ndke")) {
                    accept = true;
                    continue;
                }
            }
            accept = false;
            break;
        }
        return accept;
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
        //dsRequest.setPromptStyle(PromptStyle.DIALOG);
        //dsRequest.setPrompt(i18n.KrameriusExportAction_Add_Msg());
        dsRequest.setShowPrompt(false);
        dsAddData(ds, export, new DSCallback() {

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

    @Override
    protected List<FormItem> createExportFormOptions() {
        List<FormItem> formItems = new ArrayList<>();

        //RadioGroupItem requires specifically LinkedHashMap
        LinkedHashMap<String, String> radioButtonMap = new LinkedHashMap<>();
        radioButtonMap.put(K4_POLICY_PUBLIC, i18n.ExportAction_Request_K4_Policy_Public());
        radioButtonMap.put(K4_POLICY_PRVIATE, i18n.ExportAction_Request_K4_Policy_Private());

        rgi = new RadioGroupItem();
        rgi.setTitle(i18n.ExportAction_Request_K4_Policy_Msg());
        rgi.setValueMap(radioButtonMap);
        rgi.setDefaultValue(K4_POLICY_PUBLIC);
        rgi.setVertical(false);
        formItems.add(rgi);

        return formItems;
    }

    @Override
    protected void setRequestOptions(Record record) {
        record.setAttribute(KRAMERIUS4_POLICY_PARAM, rgi.getValueAsString());
    }
}
