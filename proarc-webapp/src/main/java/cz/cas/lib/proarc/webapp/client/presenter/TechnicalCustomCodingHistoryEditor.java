/*
 * Copyright (C) 2020 Lukas Sykora
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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.TechnicalCodingHistoryCustomDataSource;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadEvent;
import cz.cas.lib.proarc.webapp.client.widget.AbstractDatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.technicalMetadata.TechnicalMetadataForms;
import java.util.HashMap;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

public class TechnicalCustomCodingHistoryEditor extends AbstractDatastreamEditor implements RefreshAction.Refreshable {

    private static final Logger LOG = Logger.getLogger(TechnicalCustomCodingHistoryEditor.class.getName());

    private final ClientMessages i18n;
    private final HashMap<String, DynamicForm> editorCache;
    private DynamicForm activeEditor;
    private final Canvas warning = new Canvas();
    private TechnicalCodingHistoryCustomDataSource.DescriptionMetadata metadata;
    private final VLayout widget;
    private Boolean showFetchPrompt;
    private DigitalObjectDataSource.DigitalObject digitalObject;
    private String formPrefix = "";

    public TechnicalCustomCodingHistoryEditor(ClientMessages i18n) {
        this.i18n = i18n;
        this.editorCache = new HashMap<String, DynamicForm>();
        this.widget = new VLayout();
        this.widget.setOverflow(Overflow.AUTO);

        String wmsg = ClientUtils.format("<div class='proarcMetadataFormWarning'>%s</div>",
                i18n.DigitalObjectEditor_DescriptionFormEditor_FormSaveWarning_Msg());
        warning.setContents(wmsg);
        warning.setAutoHeight();
    }

    @Override
    public void edit(DigitalObjectDataSource.DigitalObject digitalObject) {
        edit(digitalObject, ClientUtils.EMPTY_BOOLEAN_CALLBACK);
    }

    public void edit(DigitalObjectDataSource.DigitalObject digitalObject, BooleanCallback loadCallback) {
        this.digitalObject = digitalObject;
        metadata = null;
        activeEditor = getCustomForm(digitalObject.getModel());
        if (activeEditor != null) {
            ClientUtils.setMembers(widget, warning, activeEditor);
            loadCustom(activeEditor, digitalObject, loadCallback);
        } else {
            widget.setMembers();
        }
    }

    private DynamicForm getCustomForm(MetaModelDataSource.MetaModelRecord model) {
        final String editorId = getFormPrefix() + model.getEditorId();
        DynamicForm editor = editorCache.get(editorId);
        if (editor == null) {
            editor = createCustomForm(model);
            editorCache.put(editorId, editor);
        }
        return editor;
    }

    private DynamicForm createCustomForm(MetaModelDataSource.MetaModelRecord model) {
        String metadataFormat = model.getMetadataFormat();
        DynamicForm form = null;
        form = new TechnicalMetadataForms(i18n).getFormExtension(model);
        if (form == null) {
            ClientUtils.warning(LOG, "Unknown model editor: %s, editor: %s, format: %s",
                    model.getId(), model.getEditorId(), metadataFormat);
        }
        return form;
    }

    private void loadCustom(final DynamicForm editor, DigitalObjectDataSource.DigitalObject dobj, final BooleanCallback loadCallback) {
        loadCustom(editor, dobj.getPid(), dobj.getBatchId(), dobj.getModel(), loadCallback);
    }

    private void loadCustom(final DynamicForm editor, String pid, String batchId,
                            MetaModelDataSource.MetaModelRecord model, final BooleanCallback loadCallback) {

        metadata = null;
        Criteria pidCriteria = new Criteria(TechnicalCodingHistoryCustomDataSource.FIELD_PID, pid);
        Criteria criteria = new Criteria(MetaModelDataSource.FIELD_EDITOR, model.getEditorId());
        criteria.addCriteria(pidCriteria);
        if (batchId != null) {
            criteria.addCriteria(TechnicalCodingHistoryCustomDataSource.FIELD_BATCHID, batchId);
        }
        ClientUtils.fine(LOG, "loadCustom pid: %s, batchId: %s, editor: %s", pid, batchId, model.getEditorId());
        DSRequest request = new DSRequest();
        if (showFetchPrompt != null) {
            request.setShowPrompt(showFetchPrompt);
        }
        TechnicalCodingHistoryCustomDataSource.getInstance().fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                handleFetch(response, editor, loadCallback);
            }
        }, request);
    }

    private void handleFetch(DSResponse response, DynamicForm editor, BooleanCallback loadCallback) {
        if (RestConfig.isStatusOk(response)) {
            Record[] data = response.getData();
            if (LOG.isLoggable(Level.FINE)) {
                ClientUtils.fine(LOG, "fetch custom data: %s", ClientUtils.dump(data));
            }
            if (data != null && data.length == 1) {
                Record customRecord = data[0];
                TechnicalCodingHistoryCustomDataSource.DescriptionMetadata dm = new TechnicalCodingHistoryCustomDataSource.DescriptionMetadata(customRecord);
                Record customModsRecord = dm.getDescription();
                if (customModsRecord != null) {
                    metadata = dm;
                    editor.editRecord(customModsRecord);
                    editor.clearErrors(true);
                    loadCallback.execute(Boolean.TRUE);
                    fireEvent(new EditorLoadEvent(false));
                    return ;
                }
            } else {
                String msg = data != null && data.length > 1
                        ? "Unexpected data in server response!"
                        : "No data in server response!";
                SC.warn(msg);
            }
        }
        widget.setMembers();
        loadCallback.execute(Boolean.FALSE);
        fireEvent(new EditorLoadEvent(true));
    }

    public String getFormPrefix() {
        return formPrefix;
    }

    @Override
    public void refresh() {
        loadCustom(activeEditor, digitalObject, ClientUtils.EMPTY_BOOLEAN_CALLBACK);
    }



    @Override
    public void focus() {
        if (activeEditor != null) {
            activeEditor.focus();
        }
    }

    @Override
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (RefreshAction.Refreshable.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        return new Canvas[0];
    }

    @Override
    public Canvas getUI() {
        return widget;
    }

    public void save(BooleanCallback callback) {
        save(callback, false, SaveAction.SaveValidation.ASK);
    }

    public void save(final BooleanCallback callback, boolean ask, SaveAction.SaveValidation strategy) {
        if (metadata == null) {
            callback.execute(Boolean.TRUE);
            return ;
        }
        SaveAction.saveTask(new SaveAction.Savable() {

            @Override
            public void save(BooleanCallback result) {
                saveImpl(result);
            }

            @Override
            public void validate(BooleanCallback result) {
                Boolean valid = activeEditor.validate();
                result.execute(valid);
            }
        }, callback, ask, strategy, i18n);
    }

    private void saveImpl(final BooleanCallback callback) {
        Record r = new Record(activeEditor.getValues());
        if (LOG.isLoggable(Level.FINE)) {
            ClientUtils.fine(LOG, "saveCustomData: %s", ClientUtils.dump(r.getJsObj()));
        }
        r = ClientUtils.normalizeData(r);
        metadata.setDescription(r);
        if (LOG.isLoggable(Level.FINE)) {
            ClientUtils.fine(LOG, "saveCustomRecord: %s", ClientUtils.dump(metadata.getWrapper().getJsObj()));
        }
        TechnicalCodingHistoryCustomDataSource.DescriptionSaveHandler dsh = new TechnicalCodingHistoryCustomDataSource.DescriptionSaveHandler() {

            @Override
            protected void onSave(TechnicalCodingHistoryCustomDataSource.DescriptionMetadata dm) {
                super.onSave(dm);
                if (dm != null) {
                    metadata = dm;
                    Record customModsRecord = dm.getDescription();
                    if (customModsRecord != null) {
                        // refresh editor with server values
                        activeEditor.editRecord(customModsRecord);
                    }
                }
                callback.execute(true);
                activeEditor.focus();
                Optional.ofNullable(dm).ifPresent(f  -> refresh());
            }

            @Override
            protected void onConcurrencyError() {
                SC.ask(i18n.SaveAction_ConcurrentErrorAskReload_Msg(), new BooleanCallback() {

                    @Override
                    public void execute(Boolean value) {
                        callback.execute(false);
                        activeEditor.focus();
                        if (value != null && value) {
                            refresh();
                        }
                    }
                });
            }

            @Override
            protected void onError() {
                super.onError();
                callback.execute(false);
            }

        };
        TechnicalCodingHistoryCustomDataSource.getInstance().saveDescription(metadata, dsh, true);
    }

    public void setFormPrefix(String s) {
        this.formPrefix = s;
    }

    public DynamicForm getCustomForm() {
        return activeEditor;
    }
}
