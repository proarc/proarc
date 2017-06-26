package cz.cas.lib.proarc.webapp.client.action;

import com.smartgwt.client.data.*;
import com.smartgwt.client.types.PromptStyle;
import com.smartgwt.client.util.SC;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.*;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;

import java.util.ArrayList;

/**
 * @author Jakub Kremlacek
 */
public class KWISExportAction extends AbstractAction {

    private final ClientMessages i18n;

    public KWISExportAction(ClientMessages i18n) {
        super(i18n.KWISExportAction_Title(), null, i18n.KWISExportAction_Hint());
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
        dsRequest.setPromptStyle(PromptStyle.DIALOG);
        dsRequest.setPrompt(i18n.KWISExportAction_Add_Msg());
        ds.addData(export, new DSCallback() {

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
}