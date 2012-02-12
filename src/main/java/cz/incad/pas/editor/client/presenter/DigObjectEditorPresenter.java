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

import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.widget.DCEditor;
import cz.incad.pas.editor.client.widget.ImportParentChooser;
import cz.incad.pas.editor.client.widget.ImportParentChooser.ImportParentHandler;
import cz.incad.pas.editor.client.widget.NewDigObject;
import cz.incad.pas.editor.client.widget.Wizard;
import cz.incad.pas.editor.client.widget.Wizard.StepKind;
import cz.incad.pas.editor.client.widget.Wizard.WizardStep;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public final class DigObjectEditorPresenter {

    private static final Logger LOG = Logger.getLogger(DigObjectEditorPresenter.class.getName());

    private final NewDigObjectStep newDigObjectStep;
    private final NewModsStep newModsStep;
    private final NewDcStep newDcStep;
    private final Wizard wizard;
    private WizardContext wc;
    private final PasEditorMessages i18nPas;

    public DigObjectEditorPresenter(PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
        newDigObjectStep = new NewDigObjectStep();
        newModsStep = new NewModsStep();
        newDcStep = new NewDcStep();
        wizard = new Wizard(i18nPas, newDigObjectStep, newModsStep, newDcStep,
                new SelectParentStep(), new FinishedStep(), Wizard.emptyStep());
    }

    public void newObject() {
        // bind object editor ui
        wizard.moveAt(newDigObjectStep);
        // select dig object type
//        rootPanel.setMembers(modsFullEditor.getUI());
    }

    public void edit() {

    }
    
    public void search() {
        // select digital object to edit
        // edit digital object
        //
//        rootPanel.setMembers(modsFullEditor.getUI());
//        modsFullEditor.loadData("id:sample");
    }

    public Canvas getUI() {
        return wizard;
//        return rootPanel;
    }
    
    private void initContext() {
        this.wc = new WizardContext();
    }
    
    private WizardContext getContext() {
        return wc;
    }

    private static final class WizardContext {
        private String pid;
        private Record model;
        private String parentPid;
        private boolean modsInitialized;
        private boolean dcInitialized;

        public boolean isDcInitialized() {
            return dcInitialized;
        }

        public void setDcInitialized(boolean dcInitialized) {
            this.dcInitialized = dcInitialized;
        }

        public Record getModel() {
            return model;
        }

        public void setModel(Record model) {
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
            wizard.setWizardLabel(i18nPas.NewDigObjectWizard_DescriptionPrefix_Title(),
                    i18nPas.NewDigObjectWizard_NewDcStep_Description_Title());
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
                WizardContext wc = getContext();
                wc.setModel(newDigObject.getModel());
                wc.setPid("id:sample");
                ClientUtils.info(LOG, "NewDigObjectStep.onStepAction.FORWARD: model: %s pid: %s",
                        wc.getModel().toMap(), wc.getPid());
            }
            return true;
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
            wizard.setWizardLabel(i18nPas.NewDigObjectWizard_DescriptionPrefix_Title(),
                    i18nPas.NewDigObjectWizard_NewModsStep_Description_Title());

            WizardContext wc = getContext();
            if (!wc.isModsInitialized()) {
//                modsFullEditor.newData();
//                modsFullEditor.loadData("id:sample", "model:page");
//                String model = wc.getModel().getAttribute(MetaModelDataSource.FIELD_PID);
                modsFullEditor.loadData(wc.getPid(), new MetaModelRecord(wc.getModel()));
                wc.setModsInitialized(true);
            }
        }

        @Override
        public void onHide(Wizard wizard) {
            wizard = null;
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            // XXX persist MODS
            return true;
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
            wizard.setWizardLabel(i18nPas.NewDigObjectWizard_DescriptionPrefix_Title(),
                    i18nPas.NewDigObjectWizard_NewDcStep_Description_Title());

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
//            wizard.setBackButton(false, null);
//            wizard.setForwardButton(true, "Resume");
            wizard.setWizardLabel(i18nPas.NewDigObjectWizard_DescriptionPrefix_Title(),
                    i18nPas.NewDigObjectWizard_SelectParentStep_Description_Title());

            editor.setHandler(this);
            if (getContext().getParentPid() == null) {
                editor.setDataSource();
            }
            onParentSelectionUpdated();
        }

        @Override
        public void onHide(Wizard wizard) {
            wizard = null;
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            if (step == StepKind.FORWARD) {
                Record selectedParent = editor.getSelectedParent();
                WizardContext wc = getContext();
                if (selectedParent != null) {
                    wc.setParentPid(selectedParent.getAttribute(RelationDataSource.FIELD_PID));
                } else {
                    return false;
                }
            }
            return true;
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
            boolean valid = pid != null;
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
            wizard.setBackButton(true, i18nPas.NewDigObjectWizard_FinishedStep_CreateNewObjectButton_Title());
            wizard.setForwardButton(true, i18nPas.NewDigObjectWizard_FinishedStep_OpenInEditorButton_Title());
            wizard.setWizardLabel(i18nPas.NewDigObjectWizard_DescriptionPrefix_Title(),
                    i18nPas.NewDigObjectWizard_FinishedStep_Description_Title());
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
