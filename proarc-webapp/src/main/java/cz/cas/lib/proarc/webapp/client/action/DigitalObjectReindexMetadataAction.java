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
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectReindexMetadataDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Selects all digital objects and use them as a template.
 * In order to display the selection an action source should implement
 *
 * <p>For now it supports {@code model:page}.
 *
 * @author Lukas Sykora
 */
public class DigitalObjectReindexMetadataAction extends AbstractAction{

    private final ClientMessages i18n;

    private static final Set<String> REINDEX_MODELS = Collections.unmodifiableSet(new HashSet<>(
            Arrays.asList(NdkPlugin.MODEL_PAGE, NdkPlugin.MODEL_NDK_PAGE, NdkAudioPlugin.MODEL_PAGE, OldPrintPlugin.MODEL_PAGE)));

    public DigitalObjectReindexMetadataAction(ClientMessages i18n) {
        this(i18n, i18n.DigitalObjectReindexMetadataAction_Title(),
                "[SKIN]/RichTextEditor/text_list_numbers.png",
                i18n.DigitalObjectReindexMetadataAction_Hint());
    }

    private DigitalObjectReindexMetadataAction(ClientMessages i18n, String title, String icon, String tooltip) {
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

    private boolean acceptModel(Record[] records) {
        boolean accept = false;
        for (Record record : records) {
            DigitalObjectDataSource.DigitalObject dobj = DigitalObjectDataSource.DigitalObject.createOrNull(record);
            if (dobj != null) {
                String modelId = dobj.getModelId();
                if (modelId != null && REINDEX_MODELS.contains(modelId)) {
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
        Record[] records = Actions.getSelection(event);
        String modelId = "";
        String pidOld = "";
        Record record = new Record();
        for (Record recordLocal : records){
            DigitalObjectDataSource.DigitalObject dobj = DigitalObjectDataSource.DigitalObject.createOrNull(recordLocal);
            if (dobj != null) {
                modelId = dobj.getModelId();
                pidOld = dobj.getPid();
                record = recordLocal;
            }
        }
        register(pidOld, pidOld, modelId, record);
    }

    private void register(String pidOld, String pidNew, String modelId, Record record) {
        DSRequest dsRequest = new DSRequest();
        dsRequest.setHttpMethod("PUT");
        DigitalObjectReindexMetadataDataSource ds = DigitalObjectReindexMetadataDataSource.getInstance();
        ds.addData(record, new DSCallback() {
            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    StatusView.getInstance().show(i18n.DigitalObjectReindex_FinishedStep_Done_Msg());
                    DigitalObjectReindexMetadataDataSource.getInstance().updateCaches(response, request);
                }
            }
        }, dsRequest);
    }
}
