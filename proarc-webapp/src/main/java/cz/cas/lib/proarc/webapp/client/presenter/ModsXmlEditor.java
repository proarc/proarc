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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource.DescriptionMetadata;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource.DescriptionSaveHandler;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.TextDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowMaterialDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.widget.CodeMirror;
import cz.cas.lib.proarc.webapp.client.widget.DatastreamEditor;
import java.util.logging.Logger;

/**
 * Edits MODS data in XML format.
 *
 * @author Jan Pokorsky
 */
final class ModsXmlEditor implements DatastreamEditor, Refreshable {

    private static final Logger LOG = Logger.getLogger(ModsXmlEditor.class.getName());
    private final CodeMirror sourceForm;
    private DigitalObject digitalObject;
    private String xml;
    private Long timestamp;
    private final ClientMessages i18n;

    public ModsXmlEditor(ClientMessages i18n) {
        sourceForm = new CodeMirror();
        this.i18n = i18n;
    }

    @Override
    public void edit(DigitalObject digitalObject) {
        this.digitalObject = digitalObject;
        this.sourceForm.clearHistory();
        refresh(true);
    }

    @Override
    public void focus() {
        sourceForm.getUI().focus();
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
        return sourceForm.getUI();
    }

    @Override
    public void refresh() {
        refresh(false);
    }

    private void refresh(final boolean cleanHistory) {
        if (digitalObject != null) {
            if (digitalObject.getPid() != null) {
                Criteria pidCriteria = new Criteria(ModsCustomDataSource.FIELD_PID, digitalObject.getPid());
                if (digitalObject.getBatchId() != null) {
                    pidCriteria.addCriteria(ModsCustomDataSource.FIELD_BATCHID, digitalObject.getBatchId());
                }
                TextDataSource.getMods().fetchData(pidCriteria, new DSCallback() {
                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        handleFetchResponse(response, TextDataSource.FIELD_CONTENT, cleanHistory);
                    }
                });
            }

            if (digitalObject.getWorkflowJobId() != null) {
                Criteria criteria = new Criteria(WorkflowModelConsts.MATERIALFILTER_JOBID, digitalObject.getWorkflowJobId().toString());
                criteria.addCriteria(new Criteria(WorkflowModelConsts.MATERIAL_TYPE, MaterialType.PHYSICAL_DOCUMENT.name()));
                WorkflowMaterialDataSource.getInstance().fetchData(criteria, new DSCallback() {
                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        handleFetchResponse(response, WorkflowModelConsts.MATERIAL_METADATA, cleanHistory);
                    }
                });
            }
        }
    }

    private void handleFetchResponse(DSResponse response, String property, boolean cleanHistory) {
        xml = "";
        if (RestConfig.isStatusOk(response)) {
            Record[] data = response.getData();
            if (data != null && data.length == 1) {
                xml = data[0].getAttribute(property);
                timestamp = data[0].getAttributeAsLong(TextDataSource.FIELD_TIMESTAMP);
            }
        }
        sourceForm.setContent(xml);
        if (cleanHistory) {
            sourceForm.clearHistory();
        }
    }

    /**
     * Stores editor content.
     *
     * @param callback notifies whether the save was successful
     * @param ask ask user before the save
     * @param strategy validation strategy
     * @see SaveAction#saveTask
     */
    public void save(final BooleanCallback callback, boolean ask, SaveAction.SaveValidation strategy) {
        String sx = sourceForm.getContent();
        final String newXml = sx == null || sx.trim().isEmpty() ? null : sx;
        String oldXml = xml == null || xml.isEmpty() ? null : xml;

        if (oldXml != null && oldXml.equals(newXml)) {
            callback.execute(Boolean.FALSE);
            return ;
        }
        SaveAction.saveTask(new SaveAction.Savable() {

            @Override
            public void save(BooleanCallback result) {
                saveImpl(result, newXml);
            }

            @Override
            public void validate(BooleanCallback result) {
                result.execute(true);
            }
        }, callback, ask, strategy, i18n);
    }

    private void saveImpl(final BooleanCallback callback, String newXml) {
        if (digitalObject != null && digitalObject.getWorkflowJobId() != null) {
            WorkflowModsCustomDataSource instance = WorkflowModsCustomDataSource.getInstance();
            instance.saveXmlDescription(digitalObject, newXml, timestamp, new DescriptionSaveHandler() {

                @Override
                protected void onSave(DescriptionMetadata dm) {
                    super.onSave(dm);
                    refresh(false);
                    callback.execute(Boolean.TRUE);
                }

                @Override
                protected void onError() {
                    super.onError();
                    callback.execute(Boolean.FALSE);
                }

                @Override
                protected void onValidationError() {
                    // Do not ignore XML validation!
                    String msg = i18n.SaveAction_IgnoreRemoteInvalid_Msg(getValidationMessage());

                    SC.ask(i18n.SaveAction_Title(), msg, value -> {
                        // save again
                        if (value != null && value) {
                            WorkflowModsCustomDataSource.getInstance().saveXmlDescription(digitalObject, newXml, timestamp, this, true);
                        }
                    });
                }
            });

        } else {
            ModsCustomDataSource.getInstance().saveXmlDescription(digitalObject, newXml, timestamp, new DescriptionSaveHandler() {

                @Override
                protected void onSave(DescriptionMetadata dm) {
                    super.onSave(dm);
                    refresh(false);
                    callback.execute(Boolean.TRUE);
                }

                @Override
                protected void onError() {
                    super.onError();
                    callback.execute(Boolean.FALSE);
                }

                @Override
                protected void onValidationError() {
                    // Do not ignore XML validation!
                    String msg = i18n.SaveAction_IgnoreRemoteInvalid_Msg(getValidationMessage());

                    SC.ask(i18n.SaveAction_Title(), msg, value -> {
                        // save again
                        if (value != null && value) {
                            ModsCustomDataSource.getInstance().saveXmlDescription(digitalObject, newXml, timestamp, this, true);
                        }
                    });
                }
            });
        }
    }

}
