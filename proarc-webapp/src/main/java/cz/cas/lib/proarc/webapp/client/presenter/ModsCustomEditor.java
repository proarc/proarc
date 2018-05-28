    /*
 * Copyright (C) 2012 Jan Pokorsky
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

import com.google.gwt.event.shared.HandlerRegistration;
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
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.action.SaveAction.Savable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction.SaveValidation;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource.DescriptionMetadata;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource.DescriptionSaveHandler;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadEvent;
import cz.cas.lib.proarc.webapp.client.widget.AbstractDatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.dc.DcEditor;
import cz.cas.lib.proarc.webapp.client.widget.mods.MonographForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.MonographUnitForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkForms;
import cz.cas.lib.proarc.webapp.client.widget.mods.PageForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.PeriodicalForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.PeriodicalIssueForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.PeriodicalVolumeForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.bdm.BornDigitalForms;
import cz.cas.lib.proarc.webapp.client.widget.mods.oldprint.OldPrintForms;
import cz.cas.lib.proarc.webapp.client.widget.nsesss.NsesssV2Form;

import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Edits subset of MODS data. It shows different form for each dig. object model.
 *
 * @author Jan Pokorsky
 */
// XXX rename to DescriptionFormEditor
public final class ModsCustomEditor extends AbstractDatastreamEditor implements Refreshable {

    private static final Logger LOG = Logger.getLogger(ModsCustomEditor.class.getName());

    private final ClientMessages i18n;
    private final HashMap<String, DynamicForm> editorCache;
    private DynamicForm activeEditor;
    private final Canvas warning = new Canvas();
    private DescriptionMetadata metadata;
    private final VLayout widget;
    private Boolean showFetchPrompt;
    private DigitalObject digitalObject;
    private String formPrefix = "";

    public ModsCustomEditor(ClientMessages i18n) {
        this.i18n = i18n;
        this.editorCache = new HashMap<String, DynamicForm>();
        this.widget = new VLayout();
        this.widget.setOverflow(Overflow.AUTO);

        String wmsg = ClientUtils.format("<div class='proarcMetadataFormWarning'>%s</div>",
                i18n.DigitalObjectEditor_DescriptionFormEditor_FormSaveWarning_Msg());
        warning.setContents(wmsg);
        warning.setAutoHeight();
    }

    /**
     * @see #edit(DigitalObject, BooleanCallback)
     */
    @Override
    public void edit(DigitalObject digitalObject) {
        edit(digitalObject, ClientUtils.EMPTY_BOOLEAN_CALLBACK);
    }

    /**
     * Loads a digital object into the given MODS custom form according to object's model.
     *
     * @param digitalObject digital object
     * @param loadCallback listens to load status
     */
    public void edit(DigitalObject digitalObject, BooleanCallback loadCallback) {
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

    @Override
    public void focus() {
        if (activeEditor != null) {
            activeEditor.focus();
        }
    }

    /**
     * Validates loaded digital object.
     * @return values are valid
     */
    public boolean isValidDigitalObject() {
        return activeEditor.valuesAreValid(false);
    }

    /**
     * Overrides {@link DSRequest#setShowPrompt(java.lang.Boolean)}
     * for the fetch operation.
     * <p>Disable prompt in case of a batch processing. Otherwise it blocks
     * the browser event queue and {@link cz.cas.lib.proarc.webapp.client.widget.ProgressTracker}
     * widgets might not respond.
     */
    public void setShowFetchPrompt(Boolean showFetchPrompt) {
        this.showFetchPrompt = showFetchPrompt;
    }

    /**
     * @see #setShowFetchPrompt(java.lang.Boolean)
     */
    public Boolean isShowFetchPrompt() {
        return showFetchPrompt != null ? showFetchPrompt : new DSRequest().getShowPrompt();
    }

    @Override
    public void refresh() {
        loadCustom(activeEditor, digitalObject, ClientUtils.EMPTY_BOOLEAN_CALLBACK);
    }

    public void save(final BooleanCallback callback) {
        save(callback, false, SaveValidation.ASK);
    }

    /**
     * Stores editor content.
     *
     * @param callback notifies whether the save was successful
     * @param ask ask user before the save
     * @param strategy validation strategy
     * @see SaveAction#saveTask
     */
    public void save(final BooleanCallback callback, boolean ask, SaveValidation strategy) {
        if (metadata == null) {
            callback.execute(Boolean.TRUE);
            return ;
        }
        SaveAction.saveTask(new Savable() {

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
        // do not use customForm.getValuesAsRecord()
        Record r = new Record(activeEditor.getValues());
        if (LOG.isLoggable(Level.FINE)) {
            ClientUtils.fine(LOG, "saveCustomData: %s", ClientUtils.dump(r.getJsObj()));
        }
        r = ClientUtils.normalizeData(r);
        metadata.setDescription(r);
        if (LOG.isLoggable(Level.FINE)) {
            ClientUtils.fine(LOG, "saveCustomRecord: %s", ClientUtils.dump(metadata.getWrapper().getJsObj()));
        }
        DescriptionSaveHandler dsh = new DescriptionSaveHandler() {

            @Override
            protected void onSave(DescriptionMetadata dm) {
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
                refresh();
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
        // workflow has a separate api endpoint
        if (this.digitalObject.getWorkflowJobId() != null) {
            WorkflowModsCustomDataSource.getInstance().saveDescription(metadata, digitalObject.getModelId(), digitalObject.getWorkflowJobId(), dsh, true);
        } else {
            ModsCustomDataSource.getInstance().saveDescription(metadata, dsh, true);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz)) {
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

    @Override
    public HandlerRegistration addSubmitValuesHandler(SubmitValuesHandler handler) {
        return activeEditor.addSubmitValuesHandler(handler);
    }

    /**
     * Gets active custom form.
     * @return editor or {@code null} if unsupported
     */
    public DynamicForm getCustomForm() {
        return activeEditor;
    }

    public String getFormPrefix() {
        return formPrefix;
    }

    /**
     * Sets prefix to address other than the prime form in
     * {@link #getCustomForm(MetaModelDataSource.MetaModelRecord)}.
     */
    public void setFormPrefix(String formPrefix) {
        this.formPrefix = formPrefix;
    }

    private DynamicForm getCustomForm(MetaModelRecord model) {
        final String editorId = getFormPrefix() + model.getEditorId();
        DynamicForm editor = editorCache.get(editorId);
        if (editor == null) {
            editor = createCustomForm(model);
            editorCache.put(editorId, editor);
        }
        return editor;
    }

    private DynamicForm createCustomForm(MetaModelRecord model) {
        String metadataFormat = model.getMetadataFormat();
        DynamicForm form = null;
        if (ModsConstants.NS.equals(metadataFormat)) {
            form = new NdkForms(i18n).getForm(model);
            if (form == null) {
                form = new BornDigitalForms(i18n).getForm(model, getFormPrefix());
            }
            if (form == null) {
                form = new OldPrintForms(i18n).getForm(model);
            }
            if (form == null) {
                // obsolete K4 forms as a fallback
                form = createModsForm(model.getEditorId());
            }
        } else if (DcConstants.NS_OAIDC.equals(metadataFormat)) {
            form = new DcEditor(i18n, model).getForm();
        } else if ("http://www.mvcr.cz/nsesss/v2".equals(metadataFormat)) {
            form = new NsesssV2Form(i18n, model).getForm();
        }
        if (form == null) {
            ClientUtils.warning(LOG, "Uknown model editor: %s, editor: %s, format: %s",
                    model.getId(), model.getEditorId(), metadataFormat);
        }
        return form;
    }

    private DynamicForm createModsForm(String editorId) {
        DynamicForm form = null;
        if (ModsCutomEditorType.EDITOR_PAGE.equals(editorId)) {
            form = new PageForm(i18n);
        } else if (ModsCutomEditorType.EDITOR_PERIODICAL.equals(editorId)) {
            form = new PeriodicalForm(i18n);
        } else if (ModsCutomEditorType.EDITOR_MONOGRAPH.equals(editorId)) {
            form = new MonographForm(i18n);
        } else if (ModsCutomEditorType.EDITOR_PERIODICAL_VOLUME.equals(editorId)) {
            form = new PeriodicalVolumeForm(i18n);
        } else if (ModsCutomEditorType.EDITOR_PERIODICAL_ISSUE.equals(editorId)) {
            form = new PeriodicalIssueForm(i18n);
        } else if (ModsCutomEditorType.EDITOR_MONOGRAPH_UNIT.equals(editorId)) {
            form = new MonographUnitForm(i18n);
        }
        return form;
    }

    private void loadCustom(final DynamicForm editor, DigitalObject dobj, final BooleanCallback loadCallback) {
        if (dobj.getWorkflowJobId() != null) {
            loadCustom(editor, dobj.getWorkflowJobId(), dobj.getModel(), loadCallback);
        } else {
            loadCustom(editor, dobj.getPid(), dobj.getBatchId(), dobj.getModel(), loadCallback);
        }
    }

    /**
     * Loads a digital object from the workflow
     *
     * @param editor form
     * @param workflowJobId identifier of digital object in workflow
     * @param model model of digital object (do in workflow has not saved model)
     * @param loadCallback listens to load status
     */
    private void loadCustom(final DynamicForm editor, Long workflowJobId,
                            MetaModelDataSource.MetaModelRecord model, final BooleanCallback loadCallback) {

        Criteria criteria = new Criteria(MetaModelDataSource.FIELD_EDITOR, model.getEditorId());
        Criteria workflowJobIdCriteria = new Criteria(WorkflowModelConsts.MATERIALFILTER_JOBID, workflowJobId.toString());
        criteria.addCriteria(workflowJobIdCriteria);
        Criteria modelIdCriteria = new Criteria(MetaModelDataSource.FIELD_MODELOBJECT, model.getId());
        criteria.addCriteria(modelIdCriteria);
        DSRequest request = new DSRequest();
        if (showFetchPrompt != null) {
            request.setShowPrompt(showFetchPrompt);
        }

        WorkflowModsCustomDataSource.getInstance().fetchData(criteria, new DSCallback() {
            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                handleFetch(response, editor, loadCallback);
            }
        }, request);

    }

    private void loadCustom(final DynamicForm editor, String pid, String batchId,
            MetaModelRecord model, final BooleanCallback loadCallback) {

        metadata = null;
        Criteria pidCriteria = new Criteria(ModsCustomDataSource.FIELD_PID, pid);
        Criteria criteria = new Criteria(MetaModelDataSource.FIELD_EDITOR, model.getEditorId());
        criteria.addCriteria(pidCriteria);
        if (batchId != null) {
            criteria.addCriteria(ModsCustomDataSource.FIELD_BATCHID, batchId);
        }
        ClientUtils.fine(LOG, "loadCustom pid: %s, batchId: %s, editor: %s", pid, batchId, model.getEditorId());
        DSRequest request = new DSRequest();
        if (showFetchPrompt != null) {
            request.setShowPrompt(showFetchPrompt);
        }
        ModsCustomDataSource.getInstance().fetchData(criteria, new DSCallback() {

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
                DescriptionMetadata dm = new DescriptionMetadata(customRecord);
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

}
