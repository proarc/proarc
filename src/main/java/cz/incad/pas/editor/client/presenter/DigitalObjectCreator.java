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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.client.presenter;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.BibliographyQueryDataSource;
import cz.incad.pas.editor.client.ds.DigitalObjectDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.widget.DCEditor;
import cz.incad.pas.editor.client.widget.ImportParentChooser;
import cz.incad.pas.editor.client.widget.ImportParentChooser.ImportParentHandler;
import cz.incad.pas.editor.client.widget.NewDigObject;
import cz.incad.pas.editor.client.widget.Wizard;
import cz.incad.pas.editor.client.widget.Wizard.StepKind;
import cz.incad.pas.editor.client.widget.Wizard.WizardStep;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Creates new digital object.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectCreator {

    private static final Logger LOG = Logger.getLogger(DigitalObjectCreator.class.getName());

    private final NewDigObjectStep newDigObjectStep;
    private final NewModsStep newModsStep;
//    private final NewDcStep newDcStep;
    private final SelectParentStep selectParentStep;
    private final FinishedStep finishStep;
    private final Wizard wizard;
    private WizardContext wc;
    private final PasEditorMessages i18nPas;

    public DigitalObjectCreator(PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
        newDigObjectStep = new NewDigObjectStep();
        newModsStep = new NewModsStep();
//        newDcStep = new NewDcStep();
        selectParentStep = new SelectParentStep();
        finishStep = new FinishedStep();
        wizard = new Wizard(i18nPas, newDigObjectStep, newModsStep,
                selectParentStep, finishStep, Wizard.emptyStep());
    }

    public void newObject() {
        // bind object editor ui
        wizard.moveAt(newDigObjectStep);
    }

    public Canvas getUI() {
        return wizard;
    }
    
    private void initContext() {
        this.wc = new WizardContext();
    }
    
    private WizardContext getContext() {
        return wc;
    }

    private static final class WizardContext {
        private String pid;
        private MetaModelRecord model;
        private String parentPid;
        private boolean modsInitialized;
        private boolean dcInitialized;

        public boolean isDcInitialized() {
            return dcInitialized;
        }

        public void setDcInitialized(boolean dcInitialized) {
            this.dcInitialized = dcInitialized;
        }

        public MetaModelRecord getModel() {
            return model;
        }

        public void setModel(MetaModelRecord model) {
            this.model = model;
        }

        public boolean isModsInitialized() {
            return modsInitialized;
        }

        public void setModsInitialized(boolean modsInitialized) {
            this.modsInitialized = modsInitialized;
        }

        public String getParentPid() {
            return parentPid;
        }

        public void setParentPid(String parentPid) {
            this.parentPid = parentPid;
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
        }

    }

    private final class NewDigObjectStep implements WizardStep {

        private NewDigObject newDigObject;
        private Wizard wizard;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            initContext();
            wizard.setBackButton(false, null);
//            wizard.setForwardButton(true, "Resume");
            wizard.setWizardLabel(i18nPas.DigitalObjectCreator_DescriptionPrefix_Title(),
                    i18nPas.DigitalObjectCreator_NewDigObjectStep_Description_Title());
            newDigObject.bind(null);
//            newDigObject.bind(new AdvancedCriteria("issn", OperatorId.ICONTAINS, "my issn"));
        }

        @Override
        public void onHide(Wizard wizard) {
            this.wizard = null;
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            if (step == StepKind.FORWARD) {
                if (!newDigObject.validate()) {
                    return false;
                }
                WizardContext wc = getContext();
                MetaModelRecord model = newDigObject.getModel();
                wc.setModel(model);
                String mods = newDigObject.getMods();
                String newPid = newDigObject.getNewPid();
                ClientUtils.info(LOG, "NewDigObjectStep.onStepAction.FORWARD: model: %s pid: %s",
                        model.getId(), newPid);
                saveNewDigitalObject(model.getId(), newPid, mods);
                return false;

            }
            return true;
        }

        private void saveNewDigitalObject(String modelId, String pid, String mods) {
            Record r = new Record();
            DigitalObjectDataSource ds = DigitalObjectDataSource.getInstance();
            r.setAttribute(DigitalObjectDataSource.FIELD_MODEL, modelId);
            if (mods != null) {
                r.setAttribute(BibliographyQueryDataSource.FIELD_MODS, mods);
            }
            if (pid != null && !pid.isEmpty()) {
                r.setAttribute(DigitalObjectDataSource.FIELD_PID, pid);
            }
            DSRequest dsRequest = new DSRequest();
            dsRequest.setWillHandleError(true);
            ds.addData(r, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    if (response.getStatus() == RPCResponse.STATUS_VALIDATION_ERROR) {
                        Map errors = response.getErrors();
                        newDigObject.setValidationErrors(errors);
                        request.setWillHandleError(true);
                        return ;
                    }
                    if (response.getHttpResponseCode() >= 400) {
                        // failure
                        SC.warn("Failed to create digital object!");
                    } else {
                        Record[] data = response.getData();
                        if (data != null && data.length > 0) {
                            String pid = data[0].getAttribute(DigitalObjectDataSource.FIELD_PID);
                            wc.setPid(pid);
                            wizard.moveAt(newModsStep);
                        } else {
                            SC.warn("Failed to create digital object!");
                        }
                    }
                }
            }, dsRequest);
        }

        @Override
        public Canvas asWidget() {
            if (newDigObject == null) {
                newDigObject = new NewDigObject(i18nPas);
            }
//            if (true) {
//                RepeatableForm dfl = new RepeatableForm("Identifiers");
//                return dfl;
//            }
            return newDigObject;
        }
    }

    private final class NewModsStep implements WizardStep {

        private Wizard wizard;
        private ModsFullEditor modsFullEditor;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            wizard.setBackButton(false, null);
//            wizard.setForwardButton(true, "Resume");
            wizard.setWizardLabel(i18nPas.DigitalObjectCreator_DescriptionPrefix_Title(),
                    i18nPas.DigitalObjectCreator_NewModsStep_Description_Title());

            WizardContext wc = getContext();
            if (!wc.isModsInitialized()) {
                modsFullEditor.loadData(wc.getPid(), wc.getModel());
                wc.setModsInitialized(true);
            }
        }

        @Override
        public void onHide(Wizard wizard) {
            wizard = null;
        }

        @Override
        public boolean onStepAction(Wizard w, StepKind step) {
            if (step == StepKind.FORWARD) {
                modsFullEditor.save(new Runnable() {

                    @Override
                    public void run() {
                        wizard.moveAt(selectParentStep);
                    }
                });
            }
            return false;
        }

        @Override
        public Canvas asWidget() {
            if (modsFullEditor == null) {
                modsFullEditor = new ModsFullEditor(i18nPas);
            }
            return modsFullEditor.getUI();
        }

    }

    private final class NewDcStep implements WizardStep {

        private Wizard wizard;
        private DCEditor dcEditor;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
//            wizard.setBackButton(false, null);
//            wizard.setForwardButton(true, "Resume");
            wizard.setWizardLabel(i18nPas.DigitalObjectCreator_DescriptionPrefix_Title(),
                    i18nPas.DigitalObjectCreator_NewDcStep_Description_Title());

            WizardContext wc = getContext();
            if (!wc.isDcInitialized()) {
//                modsFullEditor.newData();
                wc.setDcInitialized(true);
            }
        }

        @Override
        public void onHide(Wizard wizard) {
            wizard = null;
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            return true;
        }

        @Override
        public Canvas asWidget() {
            if (dcEditor == null) {
                dcEditor = new DCEditor(i18nPas);
                dcEditor.setOverflow(Overflow.AUTO);
            }
            return dcEditor;
        }
    }

    private final class SelectParentStep implements WizardStep, ImportParentHandler {

        private Wizard wizard;
        private ImportParentChooser editor;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            wizard.setBackButton(false, null);
//            wizard.setForwardButton(true, "Resume");
            wizard.setWizardLabel(i18nPas.DigitalObjectCreator_DescriptionPrefix_Title(),
                    i18nPas.DigitalObjectCreator_SelectParentStep_Description_Title());

            editor.setHandler(this);
            editor.setDataSource(getContext().getParentPid());
            onParentSelectionUpdated();
        }

        @Override
        public void onHide(Wizard wizard) {
            wizard = null;
        }

        @Override
        public boolean onStepAction(Wizard w, StepKind step) {
            Record selectedParent = editor.getSelectedParent();
            String parentPid = null;
            WizardContext wc = getContext();
            if (selectedParent != null) {
                parentPid = selectedParent.getAttribute(RelationDataSource.FIELD_PID);
            }
            if (step == StepKind.FORWARD) {
                if (parentPid != null) {
                    addMember(wc.getPid(), parentPid, new BooleanCallback() {

                        @Override
                        public void execute(Boolean value) {
                            if (value != null && value) {
                                wizard.moveAt(finishStep);
                            }
                        }
                    });
                }
                return false;
            } else {
                wc.setParentPid(parentPid);
            }
            return true;
        }

        private void addMember(String memberPid, final String parentPid, final BooleanCallback call) {
            RelationDataSource dsBatch = RelationDataSource.getInstance();
            DSRequest dsRequest = new DSRequest();
            Record update = new Record();
            update.setAttribute(RelationDataSource.FIELD_PID, memberPid);
            update.setAttribute(RelationDataSource.FIELD_PARENT, parentPid);
            dsBatch.addData(update, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    if (response.getStatus() != RPCResponse.STATUS_SUCCESS) {
                        call.execute(false);
                        return;
                    }
                    call.execute(true);
                }
            }, dsRequest);
        }

        @Override
        public Canvas asWidget() {
            if (editor == null) {
                editor = new ImportParentChooser(i18nPas);
//                editor = new VLayout();
//                editor.setContents("ImportParentChooser");
            }
            return editor;
        }

        @Override
        public void onParentSelectionUpdated() {
            Record selectedParent = editor.getSelectedParent();
            String pid = (selectedParent != null)
                    ? selectedParent.getAttribute(RelationDataSource.FIELD_PID)
                    : null;
            boolean valid = pid != null && !pid.equals(getContext().getPid());
            wizard.setCanStepForward(valid);
        }
    }

    /**
     * This should summerize created objects and error logs.
     */
    private final class FinishedStep implements WizardStep {

        private final Canvas widget;

        public FinishedStep() {
            widget = new Canvas();
            widget.setContents("Object created!");
        }

        @Override
        public void onShow(Wizard wizard) {
            wizard.setBackButton(true, i18nPas.DigitalObjectCreator_FinishedStep_CreateNewObjectButton_Title());
            wizard.setForwardButton(true, i18nPas.DigitalObjectCreator_FinishedStep_OpenInEditorButton_Title());
            wizard.setWizardLabel(i18nPas.DigitalObjectCreator_DescriptionPrefix_Title(),
                    i18nPas.DigitalObjectCreator_FinishedStep_Description_Title());
        }

        @Override
        public void onHide(Wizard wizard) {
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            if (step == StepKind.BACK) {
                newObject();
            } else {
                // XXX fire place change
            }
            return false;
        }

        @Override
        public Canvas asWidget() {
            return widget;
        }

    }

}
