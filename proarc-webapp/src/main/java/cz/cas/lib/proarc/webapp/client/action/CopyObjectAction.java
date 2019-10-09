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
package cz.cas.lib.proarc.webapp.client.action;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.CopyObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;

import java.util.*;

/**
 * Copy Object.
 *
 * @author Lukas Sykora
 */
public class CopyObjectAction extends AbstractAction {

    private final ClientMessages i18n;

    public static final Set<String> COPY_MODELS = Collections.unmodifiableSet(new HashSet<>(
            Arrays.asList(NdkPlugin.MODEL_PERIODICALISSUE, NdkPlugin.MODEL_MONOGRAPHVOLUME,
                    NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, NdkEbornPlugin.MODEL_EPERIODICALISSUE
            )));

    public CopyObjectAction(ClientMessages i18n) {
        this(i18n, i18n.CopyObjectAction_Title(),
                "16/copy.png",
                i18n.CopyObjectAction_Hint());
    }

    public CopyObjectAction(ClientMessages i18n, String title, String icon, String tooltip) {
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
                if (modelId != null && COPY_MODELS.contains(modelId)) {
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
        CopyObjectDataSource ds = CopyObjectDataSource.getInstance();
        ds.addData(record, new DSCallback() {
            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    StatusView.getInstance().show(i18n.DigitalObjectCreator_FinishedStep_Done_Msg());
                    CopyObjectDataSource.getInstance().updateCaches(response, request);
                    SearchDataSource.getInstance().updateCaches(response, request);
                    RelationDataSource.getInstance().updateCaches(response, request);
                }
            }
        }, dsRequest);
    }

}
