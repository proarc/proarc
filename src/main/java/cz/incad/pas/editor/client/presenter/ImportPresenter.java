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

    public ImportPresenter() {
        selectFolderStep = new SelectFolderStep(this);
        selectBatchStep = new SelectBatchStep();
        selectParentStep = new SelectParentStep(this);
        updateItemsStep = new UpdateItemsStep(this);
        finishedStep = new FinishedStep(this);
        wizard = new Wizard(selectFolderStep, selectBatchStep, selectParentStep, updateItemsStep, finishedStep);
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

    private static final class SelectBatchStep implements WizardStep, ImportBatchChooserHandler {

        private ImportBatchChooser widget;
        private Wizard wizard;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            wizard.setBackButton(false, null); // XXX this could be "Import New Folder"
            wizard.setForwardButton(true, "Resume");
            wizard.setWizardLabel("select import batch to resume import");
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
                widget = new ImportBatchChooser();
            }
            return widget;
        }

        @Override
        public void itemSelected() {
            // XXX check selected item for imported/not yet imported/selected
            wizard.setCanStepForward(true);
        }

    }
    private static final class SelectFolderStep implements WizardStep, ImportSourceChooserHandler {

        private ImportSourceChooser importSourceChooser;
        private Wizard wizard;
        private final ImportPresenter presenter;

        public SelectFolderStep(ImportPresenter presenter) {
            this.presenter = presenter;
        }

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            wizard.setBackButton(false, null);
            wizard.setForwardButton(true, "Import");
            wizard.setWizardLabel("select folder to import");

            presenter.importContext = new ImportContext();
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
                importSourceChooser = new ImportSourceChooser();
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
                            presenter.getImportContext().setBatch(newBatch);
                            // XXX propoj spravne kroky
//                            presenter.updateImportedObjects();
                            presenter.selectParent();
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

    private static final class UpdateItemsStep implements WizardStep {

        private ImportBatchItemEditor widget;
        private final ImportPresenter presenter;

        public UpdateItemsStep(ImportPresenter presenter) {
            this.presenter = presenter;
        }

        @Override
        public void onShow(Wizard wizard) {
            // back (reselect parent), forward (import)
//            wizard.setBackButton(true, "Import Next Folder");
            wizard.setBackButton(true, null);
            wizard.setForwardButton(true, "Import");
            wizard.setWizardLabel("prepare metadata to import");
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
                        presenter.selectParent();
                    } else {
                        // XXX import
//                        presenter.finishImport();
                        SC.say("Imported.", new BooleanCallback() {

                            @Override
                            public void execute(Boolean value) {
                                presenter.importFolder();
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
                widget = new ImportBatchItemEditor();
            }
            return widget;
        }

    }

    private static final class SelectParentStep implements WizardStep, ImportParentHandler {

        private ImportParentChooser widget;
        private final ImportPresenter presenter;
        private Wizard wizard;

        public SelectParentStep(ImportPresenter presenter) {
            this.presenter = presenter;
        }

        @Override
        public void onShow(Wizard wizard) {
            wizard.setBackButton(true, "Import Next Folder");
            wizard.setForwardButton(true, null);
            wizard.setWizardLabel("select parent digital object that will reference imported objects.");
            this.wizard = wizard;
            widget.setHandler(this);
            if (presenter.getImportContext().getParentPid() == null) {
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
                ImportContext importContext = presenter.getImportContext();
                if (selectedParent != null) {
                    importContext.setParentPid(selectedParent.getAttribute(RelationDataSource.FIELD_PID));
                } else {
//                    importContext.setParentPid(null);
                    return false;
                }
            } else {
                presenter.importFolder();
                return false;
            }
            return true;
        }

        @Override
        public Canvas asWidget() {
            if (widget == null) {
                widget = new ImportParentChooser();
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
    private static final class FinishedStep implements WizardStep {

        private final Canvas widget;
        private final ImportPresenter presenter;

        public FinishedStep(ImportPresenter presenter) {
            widget = new Canvas();
            widget.setContents("Imported!");
            this.presenter = presenter;
        }

        @Override
        public void onShow(Wizard wizard) {
            wizard.setBackButton(true, "Import New Folder");
            wizard.setForwardButton(false, null);
            wizard.setWizardLabel("done");
        }

        @Override
        public void onHide(Wizard wizard) {
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            presenter.importFolder();
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
