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

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.PromptStyle;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TextAreaWrap;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemValueFormatter;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.UrnNbnDataSource;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Set;

/**
 * Registers NDK digital object(s) in the URN:NBN resolver.
 *
 * @author Jan Pokorsky
 */
public class UrnNbnAction extends AbstractAction {

    private final ClientMessages i18n;

    public static final Set<String> URNNBN_MODELS = Collections.unmodifiableSet(new HashSet<>(
            Arrays.asList(NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALVOLUME, NdkPlugin.MODEL_PERIODICALISSUE,
                    NdkPlugin.MODEL_PERIODICALSUPPLEMENT, NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_MONOGRAPHVOLUME,
                    NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkPlugin.MODEL_CARTOGRAPHIC, NdkPlugin.MODEL_SHEETMUSIC,
                    NdkPlugin.MODEL_ARTICLE, NdkPlugin.MODEL_CHAPTER, NdkPlugin.MODEL_PICTURE,
                    NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, NdkEbornPlugin.MODEL_EPERIODICALISSUE,
                    NdkEbornPlugin.MODEL_EARTICLE, NdkEbornPlugin.MODEL_ECHAPTER
                    )));

    public UrnNbnAction(ClientMessages i18n) {
        this(i18n, i18n.UrnNbnAction_Title(),
                "[SKIN]/actions/add.png",
                i18n.UrnNbnAction_Hint());
    }
    public UrnNbnAction(ClientMessages i18n, String title, String icon, String tooltip) {
        super(title, icon, tooltip);
        this.i18n = i18n;
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);
        boolean accept = false;
        if (selection != null && selection instanceof Record[]) {
            Record[] records = (Record[]) selection;
            accept = acceptNdk(records);
        }
        return accept;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] records = Actions.getSelection(event);
        String[] pids = ClientUtils.toFieldValues(records, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        // show registrators?
        askForRegisterOptions(pids);
    }

    private boolean acceptNdk(Record[] records) {
        boolean accept = false;
        for (Record record : records) {
            DigitalObject dobj = DigitalObject.createOrNull(record);
            if (dobj != null) {
//                MetaModelRecord model = dobj.getModel();
//                String metadataFormat = model.getMetadataFormat();
                String modelId = dobj.getModelId();
                // XXX hack; it needs support to query model/object for action availability
                if (modelId != null && URNNBN_MODELS.contains(modelId)) {
                    accept = true;
                    continue;
                }
            }
            accept = false;
            break;
        }
        return accept;
    }

    private void askForRegisterOptions(String[] pids) {
        if (pids == null || pids.length == 0) {
            return ;
        }
        final Record register = new Record();
        register.setAttribute(DigitalObjectResourceApi.DIGITALOBJECT_PID, pids);
        SC.ask(i18n.UrnNbnAction_Window_Title(), i18n.UrnNbnAction_Window_Msg(),
                new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value == Boolean.TRUE) {
                    register(register);
                }
            }
        });
    }

    private void register(final Record register) {
        DSRequest dsRequest = new DSRequest();
        dsRequest.setPromptStyle(PromptStyle.DIALOG);
        dsRequest.setPrompt(i18n.UrnNbnAction_Wait_Msg());
        DataSource ds = UrnNbnDataSource.getInstance();
        ds.addData(register, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    ExportResultWidget.showErrors(data);
                }
            }
        }, dsRequest);
    }

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
            INSTANCE.showWindow(result);
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
            grid.setShowClippedValuesOnHover(true);
            ListGridField pidField = new ListGridField(DigitalObjectResourceApi.DIGITALOBJECT_PID,
                    i18n.ExportResultWidget_PID_Title());

//            ListGridField errorField = new ListGridField(DigitalObjectResourceApi.URNNBN_ITEM_MESSAGE,
//                    i18n.ExportResultWidget_Message_Title());
            ListGridField urnNbnField = new ListGridField(DigitalObjectResourceApi.URNNBN_ITEM_URNNBN,
                    i18n.UrnNbnAction_Result_UrnNbn_Title());
            ListGridField labelField = new ListGridField(DigitalObjectResourceApi.MEMBERS_ITEM_LABEL,
                    i18n.UrnNbnAction_Result_Label_Title());
            ListGridField modelField = new ListGridField(DigitalObjectResourceApi.DIGITALOBJECT_MODEL,
                    i18n.UrnNbnAction_Result_Model_Title());
            ListGridField statusField = new ListGridField(DigitalObjectResourceApi.URNNBN_ITEM_STATUSTYPE,
                    i18n.UrnNbnAction_Result_Error_Title());
            ListGridField warningField = new ListGridField(DigitalObjectResourceApi.URNNBN_ITEM_WARNING,
                    i18n.UrnNbnAction_Result_Status_Title(), 50);
            warningField.setCellAlign(Alignment.CENTER);
            warningField.setEmptyCellValue(":-)");
            HashMap<String, String> statusValues = new HashMap<String, String>();
            statusValues.put("true", ":-|");
            statusValues.put("false", ":-(");
            warningField.setValueMap(statusValues);

            grid.setFields(labelField, modelField, statusField, urnNbnField, warningField, pidField);

            grid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

                @Override
                public void onSelectionUpdated(SelectionUpdatedEvent event) {
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

        private void showWindow(Record[] result) {
            if (window == null) {
                window = new Window();
                window.setWidth(600);
                window.setHeight(400);
                window.setAutoCenter(true);
                window.setIsModal(true);
                window.addItem(widget);
                window.setTitle(i18n.UrnNbnAction_Window_Title());
                window.setShowMinimizeButton(false);
                window.setShowMaximizeButton(true);
                window.setKeepInParentRect(true);
                window.setShowModalMask(true);
                window.setCanDragResize(true);
            }
            window.show();
            logForm.clearValues();
            grid.setData(result);
            grid.deselectAllRecords();
            if (result != null && result.length > 0) {
                INSTANCE.getGrid().selectSingleRecord(result[0]);
            }
            LinkedHashMap<?, ?> valueMap = ClientUtils.getValueMap(
                    MetaModelDataSource.getModels(),
                    MetaModelDataSource.FIELD_PID,
                    MetaModelDataSource.FIELD_DISPLAY_NAME);
            grid.getField(DigitalObjectResourceApi.DIGITALOBJECT_MODEL)
                    .setValueMap(valueMap);
            grid.focus();
        }

        private DynamicForm createLogForm() {
            DynamicForm form = new DynamicForm();
            form.setBrowserSpellCheck(false);
            form.setCanEdit(false);
            form.setWidth100();
            form.setHeight("40%");
            TextAreaItem textAreaItem = new TextAreaItem(DigitalObjectResourceApi.URNNBN_ITEM_MESSAGE);
            textAreaItem.setColSpan("*");
            textAreaItem.setHeight("*");
            textAreaItem.setWrap(TextAreaWrap.OFF);
            textAreaItem.setShowTitle(false);
            textAreaItem.setWidth("*");
            textAreaItem.setCanEdit(false);
            textAreaItem.setEditorValueFormatter(new FormItemValueFormatter() {

                @Override
                public String formatValue(Object value, Record record, DynamicForm form, FormItem item) {
                    if (value == null) {
                        String urnnbn = record.getAttribute(DigitalObjectResourceApi.URNNBN_ITEM_URNNBN);
                        return urnnbn;
                    }
                    return String.valueOf(value);
                }
            });
            form.setItems(textAreaItem);
            return form;
        }

    }

}
