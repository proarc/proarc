/*
 * Copyright (C) 2014 Jan Pokorsky
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

import com.google.gwt.core.client.Callback;
import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.PromptStyle;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TextAreaWrap;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.ExportDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;
import java.util.Arrays;
import java.util.HashSet;

/**
 * The DESA export action.
 *
 * @author Jan Pokorsky
 */
public class DesaExportAction extends AbstractAction {

    private static final HashSet<String> MODELS = new HashSet<String>(Arrays.asList(
            "model:desFolder", "model:desInternalRecord", "model:desExternalRecord",
            "model:derFolder", "model:derDocument"));
    private final ClientMessages i18n;
    private final ExportType type;

    private enum ExportType { DOWNLOAD, EXPORT, VALIDATION}

    /**
     * The action to export or validate digital objects.
     */
    public static DesaExportAction export(ClientMessages i18n) {
        return new DesaExportAction(i18n, i18n.DesaExportAction_Title(),
                i18n.DesaExportAction_Hint(), ExportType.VALIDATION);
    }

    /**
     * The action to export and download a digital object to client.
     */
    public static DesaExportAction download(ClientMessages i18n) {
        return new DesaExportAction(i18n, i18n.DesaDownloadExportAction_Title(),
                i18n.DesaDownloadExportAction_Hint(), ExportType.DOWNLOAD);
    }

    DesaExportAction(ClientMessages i18n, String title, String hint, ExportType type) {
        super(title, null, hint);
        this.i18n = i18n;
        this.type = type;
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        boolean accept = false;
        if (selection != null && selection instanceof Record[]) {
            Record[] records = (Record[]) selection;
            if (ExportType.DOWNLOAD == type) {
                accept = records.length == 1 && acceptDesa(records);
            } else if (records.length > 0 && acceptDesa(records)) {
                accept = true;
            }
        }
        return accept;
    }

    private boolean acceptDesa(Record[] records) {
        boolean accept = false;
        for (Record record : records) {
            DigitalObject dobj = DigitalObject.createOrNull(record);
            if (dobj != null) {
//                MetaModelRecord model = dobj.getModel();
//                String metadataFormat = model.getMetadataFormat();
                String modelId = dobj.getModelId();
                // XXX hack; it needs support to query model/object for action availability
                if (MODELS.contains(modelId)) {
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
        String[] pids = ClientUtils.toFieldValues(records, ExportResourceApi.DESA_PID_PARAM);
        switch (type) {
            case DOWNLOAD:
                download(pids);
                break;
            default:
                askForExportOptions(pids);
        }
    }

    private void download(final String[] pids) {
        Record export = new Record();
        export.setAttribute(ExportResourceApi.DESA_PID_PARAM, pids[0]);
        export.setAttribute(ExportResourceApi.DESA_FORDOWNLOAD_PARAM, true);
        DSRequest dsRequest = new DSRequest();
        dsRequest.setPromptStyle(PromptStyle.DIALOG);
        dsRequest.setPrompt(i18n.KrameriusExportAction_Add_Msg());
        DataSource ds = ExportDataSource.getDesa();
        ds.addData(export, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    Record[] errors = data[0].getAttributeAsRecordArray(ExportResourceApi.RESULT_ERRORS);
                    if (errors != null && errors.length > 0) {
                        ExportResultWidget.showErrors(errors);
                    } else {
                        String token = data[0].getAttribute(ExportResourceApi.RESULT_TOKEN);
                        openResult(pids[0], token);
                    }
                }
            }
        }, dsRequest);
    }

    private void askForExportOptions(String[] pids) {
        if (pids == null || pids.length == 0) {
            return ;
        }
        Record export = new Record();
        export.setAttribute(ExportResourceApi.DESA_PID_PARAM, pids);
        ExportOptionsWidget.showOptions(export, new Callback<Record, Void>() {

            @Override
            public void onFailure(Void reason) {
                // no-op
            }

            @Override
            public void onSuccess(Record result) {
                exportOrValidate(result);
            }
        });
    }

    private void exportOrValidate(final Record export) {
        DSRequest dsRequest = new DSRequest();
        dsRequest.setPromptStyle(PromptStyle.DIALOG);
        dsRequest.setPrompt(i18n.KrameriusExportAction_Add_Msg());
        DataSource ds = ExportDataSource.getDesa();
        ds.addData(export, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    RecordList erl = errorsFromExportResult(data);
                    if (erl.isEmpty()) {
                        String dryRun = export.getAttribute(ExportResourceApi.DESA_DRYRUN_PARAM);
                        SC.say(dryRun == null
                                ? i18n.DesaExportAction_ExportDone_Msg()
                                : i18n.DesaExportAction_ValidationDone_Msg());
                    } else {
                        ExportResultWidget.showErrors(erl.toArray());
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

    private void openResult(String pid, String token) {
        if (pid == null) {
            throw new IllegalArgumentException("pid");
        }
        StringBuilder sb = new StringBuilder();
        sb.append(RestConfig.URL_EXPORT_DESA);
        sb.append('?').append(ExportResourceApi.DESA_PID_PARAM).append('=').append(pid);
        sb.append('&').append(ExportResourceApi.RESULT_TOKEN).append('=').append(token);
        com.google.gwt.user.client.Window.open(sb.toString(), "_blanc", "");
    }

    // ask to send to DESA already exported?
    // ask to keep folder with export, ask for folder name?
    /**
     * Displays the form to collect options for a new export.
     */
    static final class ExportOptionsWidget {

        private static ExportOptionsWidget INSTANCE;

        private final ClientMessages i18n;
        private final DynamicForm form;
        private final Label label;
        private Window window;
        private Callback<Record, Void> callback;

        public static void showOptions(Record init, Callback<Record, Void> callback) {
            if (INSTANCE == null) {
                INSTANCE = new ExportOptionsWidget();
            }
            INSTANCE.showWindow(init, callback);
        }

        public ExportOptionsWidget() {
            i18n = GWT.create(ClientMessages.class);
            label = new Label();
            label.setMargin(5);
            label.setWidth100();
            label.setAutoHeight();
            label.setWrap(false);
            form = new DynamicForm();
            CheckboxItem hierarchy = new CheckboxItem(ExportResourceApi.DESA_HIERARCHY_PARAM,
                    i18n.ExportOptionsWidget_Hierarchy_Title());
            hierarchy.setPrompt(i18n.ExportOptionsWidget_Hierarchy_Hint());
            form.setFields(hierarchy);
        }

        private void showWindow(Record init, Callback<Record, Void> callback) {
            if (window == null) {
                VLayout container = new VLayout();
                container.setMembers(label, form, createButtons());
                container.setMargin(5);
                window = new Window();
                window.setAutoCenter(true);
                window.setAutoSize(true);
                window.setIsModal(true);
                window.addItem(container);
                window.setTitle(i18n.ExportOptionsWidget_Window_Title());
                window.setShowMinimizeButton(false);
                window.setKeepInParentRect(true);
                window.setShowModalMask(true);
            }
            window.show();
            form.clearValues();
            form.focus();

            String[] pids = init.getAttributeAsStringArray(ExportResourceApi.DESA_PID_PARAM);
            label.setContents(i18n.ExportOptionsWidget_Selection_Title(String.valueOf(pids.length)));
            form.editRecord(init);
            this.callback = callback;
            window.addCloseClickHandler(new CloseClickHandler() {

                @Override
                public void onCloseClick(CloseClickEvent event) {
                    finish().onFailure(null);
                }
            });
        }

        private Callback<Record, Void> finish() {
            window.hide();
            Callback<Record, Void> c = callback;
            callback = null;
            return c;
        }

        private Canvas createButtons() {
            SmartGwtMessages i18nSgwt = ClientUtils.createSmartGwtMessages();
            IButton btnExport = new IButton(i18n.ExportOptionsWidget_ExportBtn_Title(), new com.smartgwt.client.widgets.events.ClickHandler() {

                @Override
                public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                    Record r = new Record(form.getValues());
                    finish().onSuccess(r);
                }
            });
            btnExport.setPrompt(i18n.ExportOptionsWidget_ExportBtn_Hint());
            IButton btnValidate = new IButton(i18n.ExportOptionsWidget_ValidateBtn_Title(), new com.smartgwt.client.widgets.events.ClickHandler() {

                @Override
                public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                    Record r = new Record(form.getValues());
                    r.setAttribute(ExportResourceApi.DESA_DRYRUN_PARAM, true);
                    finish().onSuccess(r);
                }
            });
            btnValidate.setPrompt(i18n.ExportOptionsWidget_ValidateBtn_Hint());
            IButton btnCancel = new IButton(i18nSgwt.dialog_CancelButtonTitle(), new com.smartgwt.client.widgets.events.ClickHandler() {

                @Override
                public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                    finish().onFailure(null);
                }
            });

            HStack btnLayout = new HStack(5);
            btnLayout.setAutoHeight();
            btnLayout.setLayoutTopMargin(20);
            btnLayout.setLayoutAlign(Alignment.CENTER);
            btnLayout.setMembers(btnValidate, btnExport, btnCancel);
            return btnLayout;
        }

    }

    /**
     * Presents errors of the digital objects export result. Use {@link #showErrors} for
     * shared instance.
     * @see cz.cas.lib.proarc.webapp.server.rest.ExportResource.ExportError
     */
    static final class ExportResultWidget {

        private final ClientMessages i18n;
        private static ExportResultWidget INSTANCE;
        private final ListGrid grid;
        private final Canvas widget;
        private final DynamicForm logForm;
        private Window window;

        /**
         * Shows errors in the shared window instance.
         * @param result array of errors
         */
        public static void showErrors(Record[] result) {
            if (INSTANCE == null) {
                INSTANCE = new ExportResultWidget();
            }
            INSTANCE.getGrid().setData(result);
            INSTANCE.getGrid().selectSingleRecord(result[0]);
            INSTANCE.showWindow();
        }

        public ExportResultWidget() {
            i18n = GWT.create(ClientMessages.class);
            VLayout vLayout = new VLayout();
            vLayout.setWidth100();
            vLayout.setHeight100();
            grid = new ListGrid();
            grid.setSelectionType(SelectionStyle.SINGLE);
            grid.setFixedRecordHeights(false);
            grid.setWrapCells(true);
            ListGridField pidField = new ListGridField(ExportResourceApi.RESULT_ERROR_PID,
                    i18n.ExportResultWidget_PID_Title());

            ListGridField errorField = new ListGridField(ExportResourceApi.RESULT_ERROR_MESSAGE,
                    i18n.ExportResultWidget_Message_Title());
            grid.setFields(pidField, errorField);
            grid.addSelectionChangedHandler(new SelectionChangedHandler() {

                @Override
                public void onSelectionChanged(SelectionEvent event) {
                    logForm.editSelectedData(grid);
                }
            });
            logForm = createLogForm();
            vLayout.setMembers(grid, logForm);
            this.widget = vLayout;
        }

        public ListGrid getGrid() {
            return grid;
        }

        public Canvas getWidget() {
            return widget;
        }

        private void showWindow() {
            if (window == null) {
                window = new Window();
                window.setWidth(600);
                window.setHeight(400);
                window.setAutoCenter(true);
                window.setIsModal(true);
                window.addItem(widget);
                window.setTitle(i18n.ExportResultWidget_Window_Title());
                window.setShowMinimizeButton(false);
                window.setShowMaximizeButton(true);
                window.setKeepInParentRect(true);
                window.setShowModalMask(true);
                window.setCanDragResize(true);
            }
            window.show();
            grid.focus();
        }

        private DynamicForm createLogForm() {
            DynamicForm form = new DynamicForm();
            form.setBrowserSpellCheck(false);
            form.setCanEdit(false);
            form.setWidth100();
            form.setHeight("40%");
            TextAreaItem textAreaItem = new TextAreaItem(ExportResourceApi.RESULT_ERROR_LOG);
            textAreaItem.setColSpan("*");
            textAreaItem.setHeight("*");
            textAreaItem.setWrap(TextAreaWrap.OFF);
            textAreaItem.setShowTitle(false);
            textAreaItem.setWidth("*");
            textAreaItem.setCanEdit(false);
            form.setItems(textAreaItem);
            return form;
        }

    }
}
