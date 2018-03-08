package cz.cas.lib.proarc.webapp.client.action.export;

import com.smartgwt.client.data.*;
import com.smartgwt.client.types.PromptStyle;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.ds.*;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import static cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi.KRAMERIUS4_POLICY_PARAM;

/**
 * @author Jakub Kremlacek
 */
public class KWISExportAction extends ExportAction {

    private final ClientMessages i18n;

    private RadioGroupItem rgi;

    public KWISExportAction(ClientMessages i18n) {
        super(i18n, i18n.KWISExportAction_Title(), null, i18n.KWISExportAction_Hint());
        this.i18n = i18n;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] selection = Actions.getSelection(event);

        ArrayList<String> pids = new ArrayList<String>(selection.length);
        for (Record record : selection) {
            String pid = record.getAttribute(SearchDataSource.FIELD_PID);
            if (pid != null) {
                pids.add(pid);
            }
        }

        DataSource ds = ExportDataSource.getKWIS();
        Record export = new Record();
        export.setAttribute(ExportResourceApi.KWIS_PID_PARAM,
                pids.toArray(new String[pids.size()]));
        DSRequest dsRequest = new DSRequest();
        //dsRequest.setPromptStyle(PromptStyle.DIALOG);
        //dsRequest.setPrompt(i18n.KWISExportAction_Add_Msg());
        dsRequest.setShowPrompt(false);
        dsAddData(ds, export, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    SC.say(i18n.KWISExportAction_AddDone_Msg(
                            data[0].getAttribute(ExportResourceApi.RESULT_TARGET)));
                }
            }
        }, dsRequest);
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        return selection != null && selection.length > 0 && selection instanceof Record[]
                && acceptMods((Record[]) selection);
    }

    /**
     * Accepts only if all records are objects with description metadata in MODS format
     */
    private boolean acceptMods(Record[] records) {
        boolean accept = false;
        for (Record record : records) {
            DigitalObjectDataSource.DigitalObject dobj = DigitalObjectDataSource.DigitalObject.createOrNull(record);
            if (dobj != null) {
                MetaModelDataSource.MetaModelRecord model = dobj.getModel();
                if (model != null && ModsConstants.NS.equals(model.getMetadataFormat())) {
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
    protected List<FormItem> createExportFormOptions() {
        List<FormItem> formItems = new ArrayList<>();

        //RadioGroupItem requires specifically LinkedHashMap
        LinkedHashMap<String, String> radioButtonMap = new LinkedHashMap<>();
        radioButtonMap.put(KrameriusExportAction.K4_POLICY_PUBLIC, i18n.ExportAction_Request_K4_Policy_Public());
        radioButtonMap.put(KrameriusExportAction.K4_POLICY_PRVIATE, i18n.ExportAction_Request_K4_Policy_Private());

        rgi = new RadioGroupItem();
        rgi.setTitle(i18n.ExportAction_Request_K4_Policy_Msg());
        rgi.setValueMap(radioButtonMap);
        rgi.setDefaultValue(KrameriusExportAction.K4_POLICY_PUBLIC);
        rgi.setVertical(false);
        formItems.add(rgi);

        return formItems;
    }

    @Override
    protected void setRequestOptions(Record record) {
        record.setAttribute(KRAMERIUS4_POLICY_PARAM, rgi.getValueAsString());
    }
}