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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.BooleanItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.ds.DeviceDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectAdministrationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.LinkedHashMap;

/**
 * Edits administration and technical meta data of digital object.
 * It support modification of single object and batch modification of
 * selected values (for now device) as well.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectAdministrationEditor implements BatchDatastreamEditor, Refreshable {

    private DigitalObject[] digitalObjects;
    private Object activeEditor;
    private PlainEditor plainEditor;
    private BatchEditor batchEditor;
    private final ClientMessages i18n;
    private VLayout container;

    public DigitalObjectAdministrationEditor(ClientMessages i18n) {
        this.i18n = i18n;
        this.container = new VLayout();
    }

    @Override
    public void edit(DigitalObject[] items) {
        this.digitalObjects = items;
        if (items == null || items.length == 0) {
            setActiveEditor(null);
        } else if (items.length == 1) {
            setActiveEditor(getPlainEditor());
            plainEditor.getForm().fetchData(items[0].toCriteria());
        } else {
            setActiveEditor(getBatchEditor());
            DynamicForm form = batchEditor.getForm();
            form.editNewRecord();
        }
    }

    @Override
    public void edit(DigitalObject digitalObject) {
        DigitalObject[] objs = digitalObject == null ? null : new DigitalObject[] { digitalObject };
        edit(objs);
    }

    @Override
    public void focus() {
        if (activeEditor instanceof PlainEditor) {
            plainEditor.getForm().focus();
        } else if (activeEditor instanceof BatchEditor) {
            batchEditor.getForm().focus();
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz) || BatchDatastreamEditor.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        SaveAction saveAction = new SaveAction(i18n) {

            @Override
            public void performAction(ActionEvent event) {
                save();
            }
        };
        return new Canvas[] {
            Actions.asIconButton(saveAction, this)
        };
    }

    @Override
    public Canvas getUI() {
        return container;
    }

    @Override
    public void refresh() {
        getPlainEditor().invalidateCache();
        edit(digitalObjects);
    }

    public void save() {
        if (activeEditor instanceof BatchEditor) {
            saveBatchEditor();
        } else if (activeEditor instanceof PlainEditor) {
            savePlainEditor();
        }
    }

    private void savePlainEditor() {
        DynamicForm form = getPlainEditor().getForm();
        if (!form.valuesHaveChanged()) {
            // nothing to save, ignore
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

    private void saveBatchEditor() {
        DynamicForm form = getBatchEditor().getForm();
        String device = form.getValueAsString(DigitalObjectAdministrationDataSource.FIELD_DEVICE);
        String[] pids = DigitalObject.toPidArray(digitalObjects);
        Record update = new Record();
        update.setAttribute(DigitalObjectAdministrationDataSource.FIELD_PID, pids);
        String batchId = digitalObjects[0].getBatchId();
        if (batchId != null) {
            update.setAttribute(DigitalObjectResourceApi.ATM_ITEM_BATCHID, batchId);
        }
        update.setAttribute(DigitalObjectAdministrationDataSource.FIELD_DEVICE, device);
        DigitalObjectAdministrationDataSource.getInstance().updateData(update, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                }
            }
        });
    }

    private void setActiveEditor(Object editor) {
        this.activeEditor = editor;
        if (editor instanceof BatchEditor) {
            container.setMembers(batchEditor.getWidget());
        } else if (editor instanceof PlainEditor) {
            container.setMembers(plainEditor.getWidget());
        } else {
            container.setMembers((Canvas) null);
        }
    }

    private PlainEditor getPlainEditor() {
        if (plainEditor == null) {
            plainEditor = new PlainEditor(i18n);
        }
        return plainEditor;
    }

    private BatchEditor getBatchEditor() {
        if (batchEditor == null) {
            batchEditor = new BatchEditor(i18n);
        }
        return batchEditor;
    }

    private static final class PlainEditor {

        private final VLayout widget;
        private final DynamicForm form;

        public PlainEditor(ClientMessages i18n) {
            widget = new VLayout();
            form = new DynamicForm();
            form.setDataSource(DigitalObjectAdministrationDataSource.getInstance());
            form.setBrowserSpellCheck(false);

            TextItem pid = new TextItem(DigitalObjectAdministrationDataSource.FIELD_PID,
                    i18n.DigitalObjectEditor_AdministrationEditor_Pid_Title());
            pid.setWidth("*");
            pid.setCanEdit(Boolean.FALSE);
            TextItem model = new TextItem(DigitalObjectAdministrationDataSource.FIELD_MODEL,
                    i18n.DigitalObjectEditor_AdministrationEditor_Model_Title());
            model.setCanEdit(Boolean.FALSE);
            TextItem owner = new TextItem(DigitalObjectAdministrationDataSource.FIELD_OWNER,
                    i18n.DigitalObjectEditor_AdministrationEditor_Owner_Title());
            owner.setCanEdit(Boolean.FALSE);
            TextItem creationDate = new TextItem(DigitalObjectAdministrationDataSource.FIELD_CREATED,
                    i18n.DigitalObjectEditor_AdministrationEditor_Created_Title());
            creationDate.setCanEdit(Boolean.FALSE);
            TextItem modificationDate = new TextItem(DigitalObjectAdministrationDataSource.FIELD_MODIFIED,
                    i18n.DigitalObjectEditor_AdministrationEditor_Modified_Title());
            modificationDate.setCanEdit(Boolean.FALSE);

            SelectItem device = new SelectItem(DigitalObjectAdministrationDataSource.FIELD_DEVICE,
                    i18n.DigitalObjectEditor_AdministrationEditor_Device_Title());
            device.setWidth(250);
            DeviceDataSource.setOptionDataSource(device);
            device.setAllowEmptyValue(true);
            device.setEmptyDisplayValue(ClientUtils.format("<i>&lt;%s&gt;</i>",
                    i18n.DigitalObjectEditor_AdministrationEditor_NoDeviceSelection_Title()));

            TextItem filename = new TextItem(DigitalObjectAdministrationDataSource.FIELD_FILENAME,
                    i18n.DigitalObjectEditor_AdministrationEditor_File_Title());
            filename.setWidth("*");
            filename.setCanEdit(Boolean.FALSE);

            TextItem export = new TextItem(DigitalObjectAdministrationDataSource.FIELD_EXPORT,
                    i18n.DigitalObjectEditor_AdministrationEditor_Export_Title());
            export.setWidth("*");
            export.setCanEdit(Boolean.FALSE);

            TextItem ndkExport = new TextItem(DigitalObjectAdministrationDataSource.FIELD_NDK_EXPORT,
                    i18n.DigitalObjectEditor_AdministrationEditor_Ndk_Export_Title());
            ndkExport.setWidth("*");
            ndkExport.setCanEdit(Boolean.FALSE);

            TextItem archiveExport = new TextItem(DigitalObjectAdministrationDataSource.FIELD_ARCHIVE_EXPORT,
                    i18n.DigitalObjectEditor_AdministrationEditor_Archive_Export_Title());
            archiveExport.setWidth("*");
            archiveExport.setCanEdit(Boolean.FALSE);

            TextItem krameriusExport = new TextItem(DigitalObjectAdministrationDataSource.FIELD_KRAMERIUS_EXPORT,
                    i18n.DigitalObjectEditor_AdministrationEditor_Kramerius_Export_Title());
            krameriusExport.setWidth("*");
            krameriusExport.setCanEdit(Boolean.FALSE);

            TextItem crossrefExport = new TextItem(DigitalObjectAdministrationDataSource.FIELD_CROSSREF_EXPORT,
                    i18n.DigitalObjectEditor_AdministrationEditor_Crossref_Export_Title());
            crossrefExport.setWidth("*");
            crossrefExport.setCanEdit(Boolean.FALSE);

            ComboBoxItem organization = new ComboBoxItem(DigitalObjectAdministrationDataSource.FIELD_ORGANIZATION,
                    i18n.UsersView_ListHeader_Organization_Title());
            organization.setWidth(250);
            organization.setValueMap(Organization.getMap());

            ComboBoxItem user = new ComboBoxItem(DigitalObjectAdministrationDataSource.FIELD_USER,
                    i18n.UsersView_ListHeader_Proccesor_Title());
            user.setWidth(250);
            user.setOptionDataSource(UserDataSource.getInstance());
            user.setValueField(UserDataSource.FIELD_USERNAME);
            user.setDisplayField(UserDataSource.FIELD_USERNAME);

            ComboBoxItem status = new ComboBoxItem(DigitalObjectAdministrationDataSource.FIELD_STATUS,
                    i18n.DigitalObjectEditor_AdministrationEditor_Status_Title());
            status.setWidth(250);
            status.setValueMap(DigitalObjectState.getMap(i18n));

            BooleanItem locked = new BooleanItem(DigitalObjectAdministrationDataSource.FIELD_LOCKED,
                    i18n.DigitalObjectEditor_AdministrationAction_Locked());
            locked.setWidth(250);
            locked.setCanEdit(Boolean.FALSE);

            TextItem lockedBy = new TextItem(DigitalObjectAdministrationDataSource.FIELD_LOCKED_BY,
                    i18n.DigitalObjectEditor_AdministrationAction_Locked_By());
            lockedBy.setWidth("*");
            lockedBy.setCanEdit(Boolean.FALSE);

            DateItem lockedDate = new DateItem(DigitalObjectAdministrationDataSource.FIELD_LOCKED_DATE,
                    i18n.DigitalObjectEditor_AdministrationAction_Locked_Datum());
            lockedDate.setWidth("*");
            lockedDate.setCanEdit(Boolean.FALSE);

            ComboBoxItem donator = new ComboBoxItem(DigitalObjectAdministrationDataSource.FIELD_DONATOR,
            i18n.DigitalObjectEditor_AdministrationAction_Donator_Title());
            LinkedHashMap<String, String> valueMap = new LinkedHashMap<>();
            valueMap.put("norway", "Norské fondy");
            donator.setValueMap(valueMap);
            donator.setDefaultValue("norway");
            donator.setWidth(250);

            form.setItems(pid, model, owner, creationDate, modificationDate, device, filename, export,  ndkExport, krameriusExport, archiveExport, crossrefExport, organization, user, status, locked, lockedBy, lockedDate, donator);
            widget.setMembers(form);
        }

        public VLayout getWidget() {
            return widget;
        }

        public DynamicForm getForm() {
            return form;
        }

        public void invalidateCache() {
            form.invalidateCache();
            form.getField(DigitalObjectAdministrationDataSource.FIELD_DEVICE)
                    .invalidateDisplayValueCache();
        }

    }

    private static final class BatchEditor {

        private final VLayout widget;
        private final DynamicForm form;

        public BatchEditor(ClientMessages i18n) {
            widget = new VLayout();
            form = new DynamicForm();
            form.setBrowserSpellCheck(false);

            TextItem owner = new TextItem(DigitalObjectAdministrationDataSource.FIELD_OWNER);
            owner.setCanEdit(Boolean.FALSE);

            SelectItem device = new SelectItem(DigitalObjectAdministrationDataSource.FIELD_DEVICE,
                    i18n.ImportSourceChooser_OptionScanner_Title());
            DeviceDataSource.setOptionDataSource(device);
            device.setWidth(250);
            device.setAllowEmptyValue(true);
            device.setEmptyDisplayValue(ClientUtils.format("<i>&lt;%s&gt;</i>",
                    i18n.DigitalObjectEditor_AdministrationEditor_NoDeviceSelection_Title()));

            form.setItems(device);
            widget.setMembers(form);
        }

        public VLayout getWidget() {
            return widget;
        }

        public DynamicForm getForm() {
            return form;
        }

        public void invalidateCache() {
            form.invalidateCache();
            form.getField(DigitalObjectAdministrationDataSource.FIELD_DEVICE)
                    .invalidateDisplayValueCache();
        }

    }

}
