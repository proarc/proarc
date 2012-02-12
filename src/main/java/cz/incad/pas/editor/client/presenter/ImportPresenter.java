/*
 * Copyright (C) 2011 Jan Pokorsky
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
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.ImportBatchDataSource;
import cz.incad.pas.editor.client.ds.ImportBatchDataSource.BatchRecord;
import cz.incad.pas.editor.client.ds.ImportBatchItemDataSource;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.widget.ImportBatchChooser;
import cz.incad.pas.editor.client.widget.ImportBatchChooser.ImportBatchChooserHandler;
import cz.incad.pas.editor.client.widget.ImportBatchItemEditor;
import cz.incad.pas.editor.client.widget.ImportParentChooser;
import cz.incad.pas.editor.client.widget.ImportParentChooser.ImportParentHandler;
import cz.incad.pas.editor.client.widget.ImportSourceChooser;
import cz.incad.pas.editor.client.widget.ImportSourceChooser.ImportSourceChooserHandler;
import cz.incad.pas.editor.client.widget.ImportTreeRestDataSource;
import cz.incad.pas.editor.client.widget.ImportTreeRestDataSource.ImportRecord;
import cz.incad.pas.editor.client.widget.Wizard;
import cz.incad.pas.editor.client.widget.Wizard.StepKind;
import cz.incad.pas.editor.client.widget.Wizard.WizardStep;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportPresenter {

    private static final Logger LOG = Logger.getLogger(ImportPresenter.class.getName());

    private final Wizard wizard;
    private final SelectFolderStep selectFolderStep;
    private final SelectBatchStep selectBatchStep;
    private final UpdateItemsStep updateItemsStep;
    private final SelectParentStep selectParentStep;
    private final FinishedStep finishedStep;
    private ImportContext importContext;
    private final PasEditorMessages i18nPas;

    public ImportPresenter(PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
        selectFolderStep = new SelectFolderStep();
        selectBatchStep = new SelectBatchStep();
        selectParentStep = new SelectParentStep();
        updateItemsStep = new UpdateItemsStep();
        finishedStep = new FinishedStep();
        wizard = new Wizard(i18nPas, selectFolderStep, selectBatchStep,
                selectParentStep, updateItemsStep, finishedStep);
    }

    public void bind() {
        importContext = new ImportContext();
//        importFolder();
//        wizard.stepInit();
//        wizard.moveAt(importSourceStep);
//        wizard.moveAt(updateImportStep);
//        wizard.moveAt(selectParentStep);
    }

    public void unbind() {

    }

    public Canvas getUI() {
        return wizard;
    }

    public ImportContext getImportContext() {
        return importContext;
    }

    public void importFolder() {
        wizard.moveAt(selectFolderStep);
    }

    public void updateImportedObjects() {
        wizard.moveAt(updateItemsStep);
    }

    public void selectParent() {
        wizard.moveAt(selectParentStep);
    }

    public void selectBatchFromHistory() {
        wizard.moveAt(selectBatchStep);
    }

    public void finishImport() {
        wizard.moveAt(finishedStep);
    }

    private final class SelectBatchStep implements WizardStep, ImportBatchChooserHandler {

        private ImportBatchChooser widget;
        private Wizard wizard;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            wizard.setBackButton(false, null); // XXX this could be "Import New Folder"
            wizard.setForwardButton(true, i18nPas.ImportWizard_SelectBatchStep_ForwardButton_Title());
            wizard.setWizardLabel(i18nPas.ImportWizard_DescriptionPrefix_Title(),
                    i18nPas.ImportWizard_SelectBatchStep_Description_Title());
            widget.bind();
        }

        @Override
        public void onHide(Wizard wizard) {
            this.wizard = null;
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            if (step == StepKind.FORWARD) {
                
            }
            return true;
        }

        @Override
        public Canvas asWidget() {
            if (widget == null) {
                widget = new ImportBatchChooser(i18nPas);
            }
            return widget;
        }

        @Override
        public void itemSelected() {
            // XXX check selected item for imported/not yet imported/selected
            wizard.setCanStepForward(true);
        }

    }
    private final class SelectFolderStep implements WizardStep, ImportSourceChooserHandler {

        private ImportSourceChooser importSourceChooser;
        private Wizard wizard;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            wizard.setBackButton(false, null);
            wizard.setForwardButton(true, i18nPas.ImportWizard_SelectFolderStep_ForwardButton_Title());
            wizard.setWizardLabel(i18nPas.ImportWizard_DescriptionPrefix_Title(),
                    i18nPas.ImportWizard_SelectFolderStep_Description_Title());

            ImportPresenter.this.importContext = new ImportContext();
            importSourceChooser.setViewHandler(this);
            importSourceChooser.setDigitalObjectModelDataSource(null);
            importSourceChooser.setFolderDataSource(null);
            sourceSelected();
        }

        @Override
        public void onHide(Wizard wizard) {
            importSourceChooser.setViewHandler(null);
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            if (step == StepKind.FORWARD) {
                handleImportSource();
                return false;
            }
            return true;
        }

        @Override
        public Canvas asWidget() {
            if (importSourceChooser == null) {
                importSourceChooser = new ImportSourceChooser(i18nPas);
            }
            return importSourceChooser;
        }

        @Override
        public void sourceSelected() {
            Record record = importSourceChooser.getImportSource();
            ImportRecord importRecord = record == null ? null : new ImportRecord(record);
            if (importRecord != null && importRecord.isNew()) {
                wizard.setCanStepForward(true);
            } else {
                wizard.setCanStepForward(false);
            }
        }

        private void handleImportSource() {
            Record record = importSourceChooser.getImportSource();
            ImportRecord importRecord = record == null ? null : new ImportRecord(record);
            ImportTreeRestDataSource ds = ImportTreeRestDataSource.getInstance();

            String model = importSourceChooser.getImportAsType();

    //        // XXX do import
    //        importSourceChooser.update();
            if (importRecord != null && importRecord.isNew() && model != null) {
                ImportBatchDataSource dsBatch = ImportBatchDataSource.getInstance();
                dsBatch.addData(dsBatch.newBatch(importRecord.getPath(), model), new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        Record[] data = response.getData();
                        if (data != null && data.length > 0) {
                            BatchRecord newBatch = new BatchRecord(data[0]);
                            ImportPresenter.this.getImportContext().setBatch(newBatch);
                            // XXX propoj spravne kroky
//                            presenter.updateImportedObjects();
                            ImportPresenter.this.selectParent();
                        } else {
                            // XXX show warning something is wrong
                        }
                    }
                });
//                ds.updateData(ImportTreeRestDataSource.createUpdateRecord(record, model));
            } else {
                throw new IllegalStateException();
            }
        }

        private void handleImportSourceSelection() {
        }
    }

    private final class UpdateItemsStep implements WizardStep {

        private ImportBatchItemEditor widget;

        @Override
        public void onShow(Wizard wizard) {
            // back (reselect parent), forward (import)
//            wizard.setBackButton(true, "Import Next Folder");
            wizard.setBackButton(true, null);
            wizard.setForwardButton(true, i18nPas.ImportWizard_UpdateItemsStep_ForwardButton_Title());
            wizard.setWizardLabel(i18nPas.ImportWizard_DescriptionPrefix_Title(),
                    i18nPas.ImportWizard_UpdateItemsStep_Description_Title());
            // XXX presenter.getImportContext().getBatch();
            widget.setBatchItems(ImportBatchItemDataSource.getInstance());
        }

        @Override
        public void onHide(Wizard wizard) {
            // XXX check
        }

        @Override
        public boolean onStepAction(final Wizard wizard, final StepKind step) {
            widget.onHide(new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    if (step == StepKind.BACK) {
//                        presenter.importFolder();
                        ImportPresenter.this.selectParent();
                    } else {
                        // XXX import
//                        presenter.finishImport();
                        SC.say("Imported.", new BooleanCallback() {

                            @Override
                            public void execute(Boolean value) {
                                ImportPresenter.this.importFolder();
                            }
                        });
                    }
                }
            });
            return false;
        }

        @Override
        public Canvas asWidget() {
            if (widget == null) {
                widget = new ImportBatchItemEditor(i18nPas);
            }
            return widget;
        }

    }

    private final class SelectParentStep implements WizardStep, ImportParentHandler {

        private ImportParentChooser widget;
        private Wizard wizard;

        @Override
        public void onShow(Wizard wizard) {
            wizard.setBackButton(true, i18nPas.ImportWizard_SelectParentStep_BackButton_Title());
            wizard.setForwardButton(true, null);
            wizard.setWizardLabel(i18nPas.ImportWizard_DescriptionPrefix_Title(),
                    i18nPas.ImportWizard_SelectParentStep_Description_Title());
            this.wizard = wizard;
            widget.setHandler(this);
            if (ImportPresenter.this.getImportContext().getParentPid() == null) {
                widget.setDataSource();
            }
            onParentSelectionUpdated();
        }

        @Override
        public void onHide(Wizard wizard) {
            widget.setHandler(null);
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            if (step == StepKind.FORWARD) {
                Record selectedParent = widget.getSelectedParent();
                ImportContext importContext = ImportPresenter.this.getImportContext();
                if (selectedParent != null) {
                    importContext.setParentPid(selectedParent.getAttribute(RelationDataSource.FIELD_PID));
                } else {
//                    importContext.setParentPid(null);
                    return false;
                }
            } else {
                ImportPresenter.this.importFolder();
                return false;
            }
            return true;
        }

        @Override
        public Canvas asWidget() {
            if (widget == null) {
                widget = new ImportParentChooser(i18nPas);
            }
            return widget;
        }

        @Override
        public void onParentSelectionUpdated() {
            Record selectedParent = widget.getSelectedParent();
            String pid = (selectedParent != null)
                    ? selectedParent.getAttribute(RelationDataSource.FIELD_PID)
                    : null;
            boolean valid = pid != null;
            wizard.setCanStepForward(valid);
        }
    }

    /**
     * This should summerize imported objects and error logs.
     */
    private final class FinishedStep implements WizardStep {

        private final Canvas widget;

        public FinishedStep() {
            widget = new Canvas();
            widget.setContents("Imported!");
        }

        @Override
        public void onShow(Wizard wizard) {
            wizard.setBackButton(true, "Import New Folder");
            wizard.setForwardButton(false, null);
            wizard.setWizardLabel(i18nPas.ImportWizard_DescriptionPrefix_Title(), "done");
        }

        @Override
        public void onHide(Wizard wizard) {
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            ImportPresenter.this.importFolder();
            return false;
        }

        @Override
        public Canvas asWidget() {
            return widget;
        }

    }

    public static final class ImportContext {

        private BatchRecord batch;
        private String parentPid;

        public BatchRecord getBatch() {
            return batch;
        }

        public void setBatch(BatchRecord batch) {
            this.batch = batch;
        }

        public String getParentPid() {
            return parentPid;
        }

        public void setParentPid(String parentPid) {
            this.parentPid = parentPid;
        }

    }

}
