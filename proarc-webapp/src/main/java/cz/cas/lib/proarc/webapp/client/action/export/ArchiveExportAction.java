/*
 * Copyright (C) 2015 Jan Pokorsky
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
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.util.SC;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.ExportDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;

/**
 * It archives the selected objects.
 *
 * @author Jan Pokorsky
 */
public class ArchiveExportAction extends ExportAction {

    public ArchiveExportAction(ClientMessages i18n) {
        this(i18n, i18n.ArchiveExportAction_Title(), null, i18n.ArchiveExportAction_Hint());
    }

    public ArchiveExportAction(ClientMessages i18n, String title, String icon, String tooltip) {
        super(i18n, title, icon, tooltip);
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        boolean accept = false;
        if (selection != null && selection instanceof Record[]) {
            Record[] records = (Record[]) selection;
            accept = accept(records);
        }
        return accept;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] records = Actions.getSelection(event);
        String[] pids = ClientUtils.toFieldValues(records, ExportResourceApi.ARCHIVE_PID_PARAM);
        askForExportOptions(pids);
    }

    private boolean accept(Record[] records) {
        boolean accept = false;
        for (Record record : records) {
            DigitalObject dobj = DigitalObject.createOrNull(record);
            if (dobj != null) {
//                MetaModelRecord model = dobj.getModel();
//                String metadataFormat = model.getMetadataFormat();
                String modelId = dobj.getModelId();
                if (isAcceptableModel(modelId)) {
                    accept = true;
                    continue;
                }
            }
            accept = false;
            break;
        }
        return accept;
    }

    protected boolean isAcceptableModel(String modelId) {
        return modelId != null && isNdkModel(modelId, false, false);
    }

    private boolean isNdkModel(String modelId, boolean withNdkEborn, boolean withNdkSoundrecording) {
//        if (modelId != null && (modelId.contains("ndkmusicdocument")
//                || modelId.contains("ndksong") || modelId.contains("ndktrack")
//                || modelId.contains("ndkaudiopage") || modelId.contains("ndkphonographcylinder"))) {
//            return withNdkSoundrecording;
//        }
        if (withNdkEborn && modelId != null && modelId.startsWith("model:ndke")) {
            return true;
        }
        if (modelId != null && modelId.startsWith("model:ndk")) {
            return true;
        }
        return false;
    }

    private void askForExportOptions(String[] pids) {
        if (pids == null || pids.length == 0) {
            return ;
        }
        Record export = new Record();
        setAttributes(export, pids);
        exportOrValidate(export);
    }

    protected void setAttributes(Record export, String[] pids) {
        export.setAttribute(ExportResourceApi.ARCHIVE_PID_PARAM, pids);
    }

    private void exportOrValidate(final Record export) {
        DSRequest dsRequest = new DSRequest();
        //dsRequest.setPromptStyle(PromptStyle.DIALOG);
        //dsRequest.setPrompt(i18n.KrameriusExportAction_Add_Msg());
        dsRequest.setShowPrompt(false);
        DataSource ds = ExportDataSource.getArchive();
        dsAddData(ds, export, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    RecordList erl = errorsFromExportResult(data);
                    if (erl.isEmpty()) {
                        String target = "";
                        if (data != null && data.length > 0) {
                            target = data[0].getAttribute(ExportResourceApi.RESULT_TARGET);
                        }
                        SC.say(i18n.ExportAction_Title(), i18n.ExportAction_ProcessPlanned_Msg());
                    } else {
                        DesaExportAction.ExportResultWidget.showErrors(erl.toArray());
                    }
                }
            }
        }, dsRequest);
    }

    private RecordList errorsFromExportResult(Record[] exportResults) {
        RecordList recordList = new RecordList();
        for (Record result : exportResults) {
            Record[] errors = result.getAttributeAsRecordArray(ExportResourceApi.RESULT_ERRORS);
            if (errors != null && errors.length > 0) {
                recordList.addList(errors);
            }
        }
        return recordList;
    }

}
