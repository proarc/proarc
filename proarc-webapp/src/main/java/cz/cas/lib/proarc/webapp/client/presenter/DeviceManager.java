/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.presenter;

import com.google.gwt.place.shared.PlaceController;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.DeleteAction;
import cz.cas.lib.proarc.webapp.client.action.DeleteAction.RecordDeletable;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.DeviceDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;

/**
 * The device manager UI to search, add, remove or edit devices.
 *
 * @author Jan Pokorsky
 */
public final class DeviceManager {

    private DeviceManagerWidget ui;
    private final ClientMessages i18n;

    public DeviceManager(ClientMessages i18n, PlaceController places) {
        this.i18n = i18n;
    }

    public void init() {
        ui.refresh();
    }

    public Canvas getUI() {
        if (ui == null) {
            ui = new DeviceManagerWidget(i18n);
        }
        return ui.asWidget();
    }

    private static final class DeviceManagerWidget implements Refreshable, Selectable<Record> {

        private final ClientMessages i18n;
        private final VLayout widget;
        private final ListGrid deviceList;
        private final DynamicForm form;
        private final ActionSource actionSource;
        private Record lastSelection;
        /** The flag cancels onDataArrived as onDataArrived is invoked before addData callback. */
        private boolean addingDevice = false;

        public DeviceManagerWidget(ClientMessages i18n) {
            this.i18n = i18n;
            actionSource = new ActionSource(this);
            widget = new VLayout();

            Label lblHeader = new Label();
            String title = ClientUtils.format("<b>%s</b>", i18n.DeviceManager_Title());
            lblHeader.setContents(title);
            lblHeader.setAutoHeight();
            lblHeader.setPadding(4);
            lblHeader.setStyleName(Editor.CSS_PANEL_DESCRIPTION_TITLE);

            ToolStrip toolbar = createToolbar(i18n);
            deviceList = createDeviceList(i18n);
            form = createForm();

            HLayout hLayout = new HLayout();
            deviceList.setWidth100();
            form.setWidth100();
            hLayout.setMembers(deviceList, form);

            widget.addMember(lblHeader);
            widget.addMember(toolbar);
            widget.addMember(hLayout);
        }

        public VLayout asWidget() {
            return widget;
        }

        @Override
        public void refresh() {
            form.clearValues();
            deviceList.invalidateCache();
            deviceList.fetchData();
        }

        @Override
        public Record[] getSelection() {
            return deviceList.getSelectedRecords();
        }

        private ListGrid createDeviceList(ClientMessages i18n) {
            final ListGrid lg = new ListGrid();
            lg.setSelectionType(SelectionStyle.SINGLE);
            ListGridField fieldLabel = new ListGridField(DeviceDataSource.FIELD_LABEL);
            lg.setDataSource(DeviceDataSource.getInstance(), fieldLabel);
            lg.setSortField(DeviceDataSource.FIELD_LABEL);
            lg.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

                @Override
                public void onSelectionUpdated(SelectionUpdatedEvent event) {
                    onDeviceSelect();
                }

            });
            lg.addDataArrivedHandler(new DataArrivedHandler() {

                @Override
                public void onDataArrived(DataArrivedEvent event) {
                    if (addingDevice) {
                        return ;
                    }
                    int index = 0;
                    boolean notEmpty = !lg.getDataAsRecordList().isEmpty();
                    if (lastSelection != null && notEmpty) {
                        String lastId = lastSelection.getAttribute(DeviceDataSource.FIELD_ID);
                        index = lg.getDataAsRecordList().findIndex(DeviceDataSource.FIELD_ID, lastId);
                        index = Math.max(0, index);
                    }
                    if (notEmpty) {
                        lg.scrollToRow(index);
                        lg.selectSingleRecord(index);
                    } else {
                        actionSource.fireEvent();
                    }
                }
            });
            return lg;
        }

        private void onDeviceSelect() {
            ListGridRecord record = deviceList.getSelectedRecord();
            lastSelection = record;
            if (record != null) {
                form.editRecord(record);
            } else {
                form.clearValues();
            }
            actionSource.fireEvent();
        }

        private DynamicForm createForm() {
            DynamicForm df = new DynamicForm();
            df.setMargin(4);
            df.setNumCols(1);
            df.setTitleOrientation(TitleOrientation.TOP);
            df.setBrowserSpellCheck(false);
            df.setSaveOnEnter(true);
            df.setDataSource(DeviceDataSource.getInstance());
            TextItem fieldId = new TextItem(DeviceDataSource.FIELD_ID);
            fieldId.setWidth(250);
            fieldId.setCanEdit(false);
            TextItem fieldLabel = new TextItem(DeviceDataSource.FIELD_LABEL);
            fieldLabel.setRequired(true);
            fieldLabel.setWidth("*");
            df.setItems(fieldId, fieldLabel);
            df.addSubmitValuesHandler(new SubmitValuesHandler() {

                @Override
                public void onSubmitValues(SubmitValuesEvent event) {
                    save();
                }
            });
            return df;
        }

        private ToolStrip createToolbar(ClientMessages i18n) {
            RefreshAction refreshAction = new RefreshAction(i18n);

            AbstractAction addAction = new AbstractAction(i18n.DeviceManager_Add_Title(),
                    "[SKIN]/actions/add.png", i18n.DeviceManager_Add_Hint()) {

                @Override
                public void performAction(ActionEvent event) {
                    addDevice();
                }
            };

            SaveAction saveAction = new SaveAction(i18n) {
                @Override
                public void performAction(ActionEvent event) {
                    save();
                }

                @Override
                public boolean accept(ActionEvent event) {
                    return getSelection().length == 1;
                }
            };

            DeleteAction deleteAction = new DeleteAction(
                    new RecordDeletable(DeviceDataSource.getInstance(), i18n),
                    i18n);

            ToolStrip t = Actions.createToolStrip();
            t.addMember(Actions.asIconButton(refreshAction, this));
            t.addMember(Actions.asIconButton(addAction, this));
            t.addMember(Actions.asIconButton(deleteAction, actionSource));
            t.addMember(Actions.asIconButton(saveAction, actionSource));
            return t;
        }

        private void save() {
            ListGridRecord r = deviceList.getSelectedRecord();
            if (r != null) {
                save(r);
            }
        }

        private void save(Record record) {
            if (!form.validate()) {
                return ;
            }
            form.saveData(new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    if (RestConfig.isStatusOk(response)) {
                        StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                    }
                }
            });
        }

        private void addDevice() {
            addingDevice = true;
            deviceList.addData(new Record(), new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    addingDevice = false;
                    if (RestConfig.isStatusOk(response)) {
                        StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                        Record r = response.getData()[0];
                        deviceList.selectSingleRecord(r);
                    }
                }
            });
        }

    }

}
