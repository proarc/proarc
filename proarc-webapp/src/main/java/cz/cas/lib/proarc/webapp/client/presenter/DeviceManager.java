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
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.ReadOnlyDisplayAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
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
import cz.cas.lib.proarc.webapp.client.action.SaveAction.Savable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction.SaveValidation;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.DeviceDataSource;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.client.widget.form.FormGenerator;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;

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
        /** The flag to do not fetch data after update. */
        private boolean updatingDevice = false;
        private final DynamicForm descriptionForm;
        ValuesManager valuesManager;

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
            descriptionForm = createDescriptionForm();
            valuesManager = new ValuesManager();
            valuesManager.addMember(form);
            valuesManager.addMember(descriptionForm);
            valuesManager.setDataSource(DeviceDataSource.getInstance());
            VLayout forms = new VLayout();
            forms.setOverflow(Overflow.AUTO);
            forms.setMembers(form, descriptionForm);

            HLayout hLayout = new HLayout();
            deviceList.setWidth100();
            forms.setWidth100();
            hLayout.setMembers(deviceList, forms);

            widget.addMember(lblHeader);
            widget.addMember(toolbar);
            widget.addMember(hLayout);
        }

        public VLayout asWidget() {
            return widget;
        }

        @Override
        public void refresh() {
            valuesManager.clearValues();
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
            if (updatingDevice) {
                return ;
            }
            ListGridRecord record = deviceList.getSelectedRecord();
            lastSelection = record;
            if (record != null) {
                fetchDescription(record.getAttribute(DeviceDataSource.FIELD_ID));
            } else {
                valuesManager.clearValues();
                descriptionForm.clearValues();
            }
            actionSource.fireEvent();
        }

        private void fetchDescription(String id) {
            Criteria criteria = new Criteria(DeviceDataSource.FIELD_ID, id);
            valuesManager.fetchData(criteria, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object dataObj, DSRequest dsRequest) {
                    if (RestConfig.isStatusOk(response)) {
                        Record[] data = response.getData();
                        if (data != null && data.length == 1) {
                            setDescription(null);
                        }
                    }
                }
            });
        }

        private void setDescription(Record deviceRecord) {
            clearErrors(true);
            if (deviceRecord != null) {
                valuesManager.editRecord(deviceRecord);
            }
        }

        private DynamicForm createForm() {
            DynamicForm df = new DynamicForm();
            df.setMargin(4);
            df.setNumCols(1);
            df.setTitleOrientation(TitleOrientation.TOP);
            df.setBrowserSpellCheck(false);
            df.setDataSource(DeviceDataSource.getInstance());
            TextItem fieldId = new TextItem(DeviceDataSource.FIELD_ID);
            fieldId.setWidth(280);
            fieldId.setCanEdit(false);
            fieldId.setReadOnlyDisplay(ReadOnlyDisplayAppearance.STATIC);
            TextItem fieldLabel = new TextItem(DeviceDataSource.FIELD_LABEL);
            fieldLabel.setRequired(true);
            fieldLabel.setWidth("*");
            df.setItems(fieldId, fieldLabel);
            return df;
        }

        private DynamicForm createDescriptionForm() {
            Form f = new Form();
            f.getFields().add(
                new FieldBuilder("description").setMaxOccurrences(1)
                    .addField(new FieldBuilder("ImageCaptureMetadata").setMaxOccurrences(1)
                        .addField(new FieldBuilder("GeneralCaptureInformation").setTitle("General Capture Information - M").setMaxOccurrences(1)
                            .setHint("Základní údaje o skenování.")
                            // dateTimeCreated, typeOfDateType - should be taken from scan?
//                            .addField(new FieldBuilder("dateTimeCreated").setMaxOccurrences(1).createField())
                            // imageProducer, stringType(value, use)
                            .addField(new FieldBuilder("imageProducer").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Image Producer - M").setMaxOccurrences(1).setRequired(true)
                                    .setType(Field.TEXT).setHint("Entita provádějící skenování. Např. The National Library of the Czech Republic, osoba apod.").createField())
                            .createField()) // imageProducer
                            // captureDevice, typeOfCaptureDeviceType extends captureDeviceType
                            .addField(new FieldBuilder("captureDevice").setMaxOccurrences(1)
                                // @use, value(enum)
                                .addField(new FieldBuilder("value").setTitle("Capture Device - MA").setMaxOccurrences(1).setType(Field.SELECT)
                                    .setHint("Typ skenovacího zařízení.")
                                    .addMapValue("TRANSMISSION_SCANNER", "transmission scanner")
                                    .addMapValue("REFLECTION_PRINT_SCANNER", "reflection print scanner")
                                    .addMapValue("DIGITAL_STILL_CAMERA", "digital still camera")
                                    .addMapValue("STILL_FROM_VIDEO", "still from video")
                                .createField()) // value
                            .createField()) // captureDevice
                        .createField()) // GeneralCaptureInformation

                        // ScannerCapture
                        .addField(new FieldBuilder("ScannerCapture").setTitle("Scanner Capture - M").setMaxOccurrences(1)
                            .setHint("Údaje o skeneru.")
                            // scannerManufacturer, stringType(value, use)
                            .addField(new FieldBuilder("scannerManufacturer").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Scanner Manufacturer - M").setMaxOccurrences(1).setRequired(true)
                                    .setType(Field.COMBO).setHint("Výrobce skeneru, např. 4DigitalBooks, Treventus, Zeutschel.")
                                    .addMapValue("Zeutschel", "Zeutschel")
                                    .addMapValue("Treventus", "Treventus")
                                    .addMapValue("4DigitalBooks", "4DigitalBooks")
                                .createField()) // value
                            .createField()) // imageProducer
                            // ScannerModel
                            .addField(new FieldBuilder("ScannerModel").setTitle("Scanner Model - M").setMaxOccurrences(1)
                                .setHint("Údaje o konkrétním typu skeneru.")
                                // scannerModelName, stringType
                                .addField(new FieldBuilder("scannerModelName").setMaxOccurrences(1)
                                    .addField(new FieldBuilder("value").setTitle("Scanner Model Name - M").setMaxOccurrences(1).setRequired(true)
                                        .setType(Field.TEXT).setHint("Jméno modelové řady skeneru, např. DL.")
                                    .createField()) // value
                                .createField()) // scannerModelName
                                // scannerModelNumber, stringType
                                .addField(new FieldBuilder("scannerModelNumber").setMaxOccurrences(1)
                                    .addField(new FieldBuilder("value").setTitle("Scanner Model Number - M").setMaxOccurrences(1).setRequired(true)
                                        .setType(Field.TEXT).setHint("Číslo/označení modelu, např. 3000.")
                                    .createField()) // value
                                .createField()) // scannerModelNumber
                                // scannerModelNumber, stringType
                                .addField(new FieldBuilder("scannerModelSerialNo").setMaxOccurrences(1)
                                    .addField(new FieldBuilder("value").setTitle("Scanner Model Serial No - M").setMaxOccurrences(1).setRequired(true)
                                        .setType(Field.TEXT).setHint("Výrobní číslo skeneru, např. E4R0003649.")
                                    .createField()) // value
                                .createField()) // scannerModelSerialNo
                            .createField()) // ScannerModel
                            // MaximumOpticalResolution
                            .addField(new FieldBuilder("MaximumOpticalResolution").setTitle("Maximum Optical Resolution - M").setMaxOccurrences(1)
                                .setHint("Údaje o maximálním optickém rozlišení skeneru.")
                                // xOpticalResolution, positiveIntegerType extends positiveInteger extends xsd:integer
                                .addField(new FieldBuilder("xOpticalResolution").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("X Optical Resolution - M").setMaxOccurrences(1).setRequired(true)
                                        .setType(Field.TEXT).setHint("Optické rozlišení na ose x, např. 300.")
                                    .createField()) // value
                                .createField()) // xOpticalResolution
                                // yOpticalResolution, positiveIntegerType extends positiveInteger extends xsd:integer
                                .addField(new FieldBuilder("yOpticalResolution").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("Y Optical Resolution - M").setMaxOccurrences(1).setRequired(true)
                                        .setType(Field.TEXT).setHint("Optické rozlišení na ose y, např. 300.")
                                    .createField()) // value
                                .createField()) // yOpticalResolution
                                // opticalResolutionUnit, typeOfOpticalResolutionUnitType extends opticalResolutionUnitType base opticalResolutionUnitType
                                .addField(new FieldBuilder("opticalResolutionUnit").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("Optical Resolution Unit - M").setMaxOccurrences(1).setRequired(true)
                                        .setType(Field.SELECT).setHint("Jednotka optického rozlišení, např. inch (in.).").setWidth("150")
                                        .addMapValue("CM", "cm")
                                        .addMapValue("IN", "in.")
                                        .addMapValue("NO_ABSOLUTE_UNIT", "no absolute unit")
                                    .createField()) // value
                                .createField()) // opticalResolutionUnit
                            .createField()) // MaximumOpticalResolution
                            // scannerSensor, typeOfScannerSensorType extends scannerSensorType(enum)
                            .addField(new FieldBuilder("scannerSensor").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("Scanner Sensor - M").setMaxOccurrences(1).setRequired(true)
                                        .setType(Field.SELECT).setHint("Popis typu snímacího senzoru skenovacího zařízení.")
                                        .addMapValue("UNDEFINED", "undefined")
                                        .addMapValue("MONOCHROME_LINEAR", "MonochromeLinear")
                                        .addMapValue("COLOR_TRI_LINEAR", "ColorTriLinear")
                                        .addMapValue("COLOR_SEQUENTIAL_LINEAR", "ColorSequentialLinear")
                                        .addMapValue("MONOCHROME_AREA", "MonochromeArea")
                                        .addMapValue("ONE_CHIP_COLOUR_AREA", "OneChipColourArea")
                                        .addMapValue("TWO_CHIP_COLOR_AREA", "TwoChipColorArea")
                                        .addMapValue("THREE_CHIP_COLOR_AREA", "ThreeChipColorArea")
                                        .addMapValue("COLOR_SEQUENTIAL_AREA", "ColorSequentialArea")
                                    .createField()) // value
                            .createField()) // scannerSensor
                            .addField(new FieldBuilder("ScanningSystemSoftware").setTitle("Scanning System Software - M").setMaxOccurrences(1)
                                .setHint("Údaje o softwaru skenovacího zařízení.")
                                // scanningSoftwareName, stringType
                                .addField(new FieldBuilder("scanningSoftwareName").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("Scanning Software Name - M").setMaxOccurrences(1).setRequired(true)
                                        .setType(Field.TEXT).setHint("Název softwaru.")
                                    .createField()) // value
                                .createField()) // scanningSoftwareName
                                // scanningSoftwareVersionNo, stringType
                                .addField(new FieldBuilder("scanningSoftwareVersionNo").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("Scanning Software Version No - M").setMaxOccurrences(1)
                                        .setType(Field.TEXT).setHint("Číslo verze softwaru, např. 3.7.").setRequired(true)
                                    .createField()) // value
                                .createField()) // scanningSoftwareVersionNo
                            .createField()) // ScanningSystemSoftware
                        .createField()) // ScannerCapture

                        .addField(new FieldBuilder("DigitalCameraCapture").setTitle("Digital Camera Capture - MA").setMaxOccurrences(1)
                            .setHint("Údaje o snímacím zařízení (fotoaparát).<p>Povinné, pokud je používán fotoaparát a není používán skener.")
                            // digitalCameraManufacturer, stringType
                            .addField(new FieldBuilder("digitalCameraManufacturer").setMaxOccurrences(1)
                                // value, @use
                                .addField(new FieldBuilder("value").setTitle("Digital Camera Manufacturer - M").setMaxOccurrences(1)
                                    .setType(Field.TEXT).setHint("Výrobce fotoaparátu, např. Canon.")
                                .createField()) // value
                            .createField()) // digitalCameraManufacturer
                            .addField(new FieldBuilder("DigitalCameraModel").setTitle("DigitalCameraModel").setMaxOccurrences(1)
                                .setHint("Popis modelu fotoaparátu")
                                // digitalCameraModelName, stringType
                                .addField(new FieldBuilder("digitalCameraModelName").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("Digital Camera Model Name - M").setMaxOccurrences(1)
                                        .setType(Field.TEXT).setHint("Název modelové řady, např. EOS.")
                                    .createField()) // value
                                .createField()) // digitalCameraModelName
                                // digitalCameraModelNumber, stringType
                                .addField(new FieldBuilder("digitalCameraModelNumber").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("Digital Camera Model Number - M").setMaxOccurrences(1)
                                        .setType(Field.TEXT).setHint("Označení modelu fotoaparátu, např. 1000D.")
                                    .createField()) // value
                                .createField()) // digitalCameraModelNumber
                                // digitalCameraModelSerialNo, stringType
                                .addField(new FieldBuilder("digitalCameraModelSerialNo").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("Digital Camera Model Serial No - M").setMaxOccurrences(1)
                                        .setType(Field.TEXT).setHint("výrobní číslo přístroje, např. E12345.")
                                    .createField()) // value
                                .createField()) // digitalCameraModelNumber
                            .createField()) // DigitalCameraModel
                            // cameraSensor, typeOfCameraSensorType extends cameraSensorType(enum)
                            .addField(new FieldBuilder("cameraSensor").setMaxOccurrences(1)
                                    // value, @use
                                    .addField(new FieldBuilder("value").setTitle("Camera Sensor - M").setMaxOccurrences(1)
                                        .setType(Field.SELECT).setHint("Typ senzoru fotoaparátu.")
                                        .addMapValue("UNDEFINED", "undefined")
                                        .addMapValue("MONOCHROME_AREA", "MonochromeArea")
                                        .addMapValue("ONE_CHIP_COLOR_AREA", "OneChipColorArea")
                                        .addMapValue("TWO_CHIP_COLOR_AREA", "TwoChipColorArea")
                                        .addMapValue("THREE_CHIP_COLOR_AREA", "ThreeChipColorArea")
                                        .addMapValue("MONOCHROME_LINEAR", "MonochromeLinear")
                                        .addMapValue("COLOR_TRI_LINEAR", "ColorTriLinear")
                                        .addMapValue("COLOR_SEQUENTIAL_LINEAR", "ColorSequentialLinear")
                                    .createField()) // value
                            .createField()) // cameraSensor
                        .createField()) // DigitalCameraCapture

                    .createField()) // ImageCaptureMetadata
                .createField()); // mix
            return new FormGenerator(f, LanguagesDataSource.activeLocale()).generateForm();
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
                if (!form.validate()) {
                    return ;
                }
                SaveAction.saveTask(new Savable() {

                    @Override
                    public void save(BooleanCallback result) {
                        saveImpl(result);
                    }

                    @Override
                    public void validate(BooleanCallback result) {
                        Boolean valid = descriptionForm.validate();
                        result.execute(valid);
                    }
                }, ClientUtils.EMPTY_BOOLEAN_CALLBACK, true, SaveValidation.ASK, i18n);
            }
        }

        private void saveImpl(final BooleanCallback callback) {
            Record update = new Record(valuesManager.getValues());
            update = ClientUtils.normalizeData(update);
            updatingDevice = true;
            DeviceDataSource.getInstance().updateData(update, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    updatingDevice = false;
                    boolean status = RestConfig.isStatusOk(response);
                    if (status) {
                        StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                        Record[] data = response.getData();
                        if (data != null && data.length == 1) {
                            Record deviceRecord = data[0];
                            setDescription(deviceRecord);
                        }
                    }
                    callback.execute(status);
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

        private void clearErrors(boolean show) {
            form.clearErrors(show);
            descriptionForm.clearErrors(show);
        }

    }

}
