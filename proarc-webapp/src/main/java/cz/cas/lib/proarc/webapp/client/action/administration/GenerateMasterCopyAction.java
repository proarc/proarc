/*
 * Copyright (C) 2019 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.client.action.administration;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.GenerateJP2DataSource;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;

import java.util.*;

/**
 * Create new Master Copy from RAW
 *
 * @author Lukas Sykora
 */
public class GenerateMasterCopyAction extends AbstractAction {

    private final ClientMessages i18n;

    public static final Set<String> TITLE_MODELS = Collections.unmodifiableSet(new HashSet<>(
            Arrays.asList(NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_MONOGRAPHVOLUME,
                    NdkPlugin.MODEL_MONOGRAPHTITLE, NdkAudioPlugin.MODEL_MUSICDOCUMENT,
                    K4Plugin.MODEL_PERIODICAL, K4Plugin.MODEL_MONOGRAPH, K4Plugin.MODEL_MONOGRAPHUNIT,
                    OldPrintPlugin.MODEL_MONOGRAPHTITLE, OldPrintPlugin.MODEL_VOLUME,
                    ChroniclePlugin.MODEL_CHRONICLETITLE, ChroniclePlugin.MODEL_CHRONICLEVOLUME
            )));

    public GenerateMasterCopyAction(ClientMessages i18n) {
        this(i18n, i18n.MasterCopyAdministrationAction_Title(),
                "[SKIN]/headerIcons/transfer.png",
                i18n.MasterCopyAdministrationAction_Hint());
    }

    public GenerateMasterCopyAction(ClientMessages i18n, String title, String icon, String tooltip) {
        super(title, icon, tooltip);
        this.i18n = i18n;
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        boolean accept = false;
        if (selection != null && selection instanceof Record[]) {
            Record[] records = (Record[]) selection;
            accept = acceptModel(records);
        }
        return accept;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] records = Actions.getSelection(event);
        String modelId = "";
        String pidOld = "";
        Record record = new Record();
        for (Record recordLocal : records){
            DigitalObject dobj = DigitalObject.createOrNull(recordLocal);
            if (dobj != null) {
                modelId = dobj.getModelId();
                pidOld = dobj.getPid();
                record = recordLocal;
                continue;
            }
        }
        register(pidOld, pidOld, modelId, record);
    }

    private boolean acceptModel(Record[] records) {
        boolean accept = false;
        for (Record record : records) {
            DigitalObject dobj = DigitalObject.createOrNull(record);
            if (dobj != null) {
                String modelId = dobj.getModelId();
                if (modelId != null && TITLE_MODELS.contains(modelId)) {
                    accept = true;
                    continue;
                }
            }
            accept = false;
            break;
        }
        return accept;
    }

    private void register(String pidOld, String pidNew, String modelId, Record record) {
        DSRequest dsRequest = new DSRequest();
        dsRequest.setHttpMethod("POST");
        GenerateJP2DataSource ds = GenerateJP2DataSource.getInstance("mastercopy");
        ds.addData(record, new DSCallback() {
            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    StatusView.getInstance().show(i18n.MasterCopyAdministrationAction_FinishStep_Msg());
                    GenerateJP2DataSource.getInstance("mastercopy").updateCaches(response, request);
                    SearchDataSource.getInstance().updateCaches(response, request);
                    RelationDataSource.getInstance().updateCaches(response, request);
                }
            }
        }, dsRequest);
    }
}
