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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.PromptStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.ImportBatchDataSource;
import cz.incad.pas.editor.client.ds.ImportBatchDataSource.BatchRecord;
import cz.incad.pas.editor.client.ds.ImportBatchItemDataSource;
import cz.incad.pas.editor.client.ds.ImportTreeDataSource.ImportRecord;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.widget.ImportBatchChooser;
import cz.incad.pas.editor.client.widget.ImportBatchChooser.ImportBatchChooserHandler;
import cz.incad.pas.editor.client.widget.ImportBatchItemEditor;
import cz.incad.pas.editor.client.widget.ImportParentChooser;
import cz.incad.pas.editor.client.widget.ImportParentChooser.ImportParentHandler;
import cz.incad.pas.editor.client.widget.ImportSourceChooser;
import cz.incad.pas.editor.client.widget.ImportSourceChooser.ImportSourceChooserHandler;
import cz.incad.pas.editor.client.widget.ProgressTracker;
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
                updateItemsStep, selectParentStep, finishedStep);
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

    private void ingest(String batchId, String parentId, final BooleanCallback call) {
        ImportBatchDataSource dsBatch = ImportBatchDataSource.getInstance();
        DSRequest dsRequest = new DSRequest();
        dsRequest.setPromptStyle(PromptStyle.DIALOG);
        dsRequest.setPrompt(i18nPas.ImportWizard_UpdateItemsStep_Ingesting_Title());
        Record update = new Record();
        update.setAttribute(ImportBatchDataSource.FIELD_ID, batchId);
        update.setAttribute(ImportBatchDataSource.FIELD_PARENT, parentId);
        update.setAttribute(ImportBatchDataSource.FIELD_STATE, ImportBatchDataSource.State.INGESTING.name());
        dsBatch.updateData(update, new DSCallback() {

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

    private void updateBatchParent(String batchId, final String parentPid, final BooleanCallback call) {
        ImportBatchDataSource dsBatch = ImportBatchDataSource.getInstance();
        DSRequest dsRequest = new DSRequest();
        Record update = new Record();
        update.setAttribute(ImportBatchDataSource.FIELD_ID, batchId);
        update.setAttribute(ImportBatchDataSource.FIELD_PARENT, parentPid);
        dsBatch.updateData(update, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (response.getStatus() != RPCResponse.STATUS_SUCCESS) {
                    call.execute(false);
                    return;
                }
                Record[] data = response.getData();
                if (data != null && data.length > 0) {
                    importContext.setParentPid(parentPid);
                    call.execute(true);
                } else {
                    // XXX show warning something is wrong
                    call.execute(false);
                }
            }
        }, dsRequest);
    }

    private final class SelectBatchStep implements WizardStep, ImportBatchChooserHandler {

        private ImportBatchChooser widget;
        private Wizard wizard;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            wizard.setBackButton(false, null); // XXX this could be "Import New Folder"
            wizard.setForwardButton(true, i18nPas.ImportWizard_ButtonResume_Title());
            wizard.setWizardLabel(i18nPas.ImportWizard_DescriptionPrefix_Title(),
                    i18nPas.ImportWizard_SelectBatchStep_Description_Title());
            wizard.setCanStepForward(false);
            widget.setHandler(this);
            widget.bind();
        }

        @Override
        public void onHide(Wizard wizard) {
            this.wizard = null;
            widget.setHandler(null);
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            if (step == StepKind.FORWARD) {
                Record record = widget.getSelectedBatch();
                BatchRecord batch = record == null ? null : new BatchRecord(record);
                getImportContext().setBatch(batch);
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
            Record record = widget.getSelectedBatch();
            if (record != null) {
                boolean canForward = new BatchRecord(record).getState() == ImportBatchDataSource.State.LOADED;
                wizard.setCanStepForward(canForward);
            } else {
                wizard.setCanStepForward(false);
            }
        }

    }
    private final class SelectFolderStep implements WizardStep, ImportSourceChooserHandler {

        private ImportSourceChooser importSourceChooser;
        private Wizard wizard;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            wizard.setBackButton(false, null);
            wizard.setForwardButton(true, i18nPas.ImportWizard_ButtonLoadFolder_Title());
            wizard.setWizardLabel(i18nPas.ImportWizard_DescriptionPrefix_Title(),
                    i18nPas.ImportWizard_SelectFolderStep_Description_Title());
            wizard.setCanStepForward(false);

            ImportPresenter.this.importContext = new ImportContext();
            importSourceChooser.setViewHandler(this);
            importSourceChooser.setDigitalObjectModelDataSource(null);
            importSourceChooser.setFolderDataSource(null);
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

            if (importRecord != null && importRecord.isNew()) {
                ImportBatchDataSource dsBatch = ImportBatchDataSource.getInstance();
                DSRequest dsRequest = new DSRequest();
                dsRequest.setPromptStyle(PromptStyle.DIALOG);
                dsRequest.setPrompt(i18nPas.ImportWizard_SelectFolderStep_Wait_Title());
                Record newBatch = dsBatch.newBatch(importRecord.getPath(),
                        importSourceChooser.getImportAsType(),
                        importSourceChooser.getDevice(),
                        importSourceChooser.getGenerateIndices());
                dsBatch.addData(newBatch, new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        if (response.getStatus() != RPCResponse.STATUS_SUCCESS) {
                            response.setInvalidateCache(true);
                            return;
                        }
                        Record[] data = response.getData();
                        if (data != null && data.length > 0) {
                            BatchRecord newBatch = new BatchRecord(data[0]);
                            showProgress(newBatch);
                        } else {
                            response.setInvalidateCache(true);
                            SC.warn(i18nPas.ImportWizard_SelectFolderStep_NothingToImport_Msg());
                        }
                    }
                }, dsRequest);
            } else {
                throw new IllegalStateException();
            }
        }

        private void showProgress(final BatchRecord batch) {
            final Criteria criteria = new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, batch.getId());
            ImportBatchItemDataSource ds = ImportBatchItemDataSource.getInstance();
            ProgressTracker progress = new ProgressTracker(i18nPas);
            progress.setDataSource(ds, criteria);
            progress.setInit();
            progress.setProgressPrefix(i18nPas.ImportWizard_SelectFolderStep_ImportProgress_Prefix_Title());
            progress.showInWindow(new Runnable() {

                @Override
                public void run() {
                    checkBatchState(batch);
                }
            }, i18nPas.ImportWizard_SelectFolderStep_ImportProgress_Title());
        }

        private void checkBatchState(BatchRecord batch) {
            final Criteria criteria = new Criteria(ImportBatchDataSource.FIELD_ID, batch.getId());
            ImportBatchDataSource.getInstance().fetchData(criteria, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    Record[] data = response.getData();
                    if (data != null && data.length > 0) {
                        BatchRecord batch = new BatchRecord(data[0]);
                        if (batch.getState() == ImportBatchDataSource.State.LOADED) {
                            ImportPresenter.this.getImportContext().setBatch(batch);
                            ImportPresenter.this.updateImportedObjects();
                            return ;
                        }
                    }
                    importSourceChooser.refresh();
                }
            });
        }

    }

    private final class UpdateItemsStep implements WizardStep {

        private ImportBatchItemEditor widget;

        @Override
        public void onShow(Wizard wizard) {
            wizard.setBackButton(true, i18nPas.ImportWizard_ButtonLoadNextFolder_Title());
            wizard.setForwardButton(true, null);
            wizard.setWizardLabel(i18nPas.ImportWizard_DescriptionPrefix_Title(),
                    i18nPas.ImportWizard_UpdateItemsStep_Description_Title());
            BatchRecord batch = getImportContext().getBatch();
            widget.onShow(batch);
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
                        ImportPresenter.this.importFolder();
                    } else {
                        ImportPresenter.this.selectParent();
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
            wizard.setBackButton(true, null);
            wizard.setForwardButton(true, i18nPas.ImportWizard_ButtonImport_Title());
            wizard.setWizardLabel(i18nPas.ImportWizard_DescriptionPrefix_Title(),
                    i18nPas.ImportWizard_SelectParentStep_Description_Title());
            this.wizard = wizard;
            widget.setHandler(this);
            widget.setDataSource(ImportPresenter.this.getImportContext().getParentPid());
            onParentSelectionUpdated();
        }

        @Override
        public void onHide(Wizard wizard) {
            widget.setHandler(null);
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            final ImportContext importContext = ImportPresenter.this.getImportContext();
            final String parentPid = importContext.getParentPid();
            String batchId = importContext.getBatch().getId();
            if (step == StepKind.FORWARD) {
                if (parentPid != null) {
                    ingest(batchId, parentPid);
                }
                return false;
            } else {
                if (parentPid != null) {
                    updateBatchParent(batchId, parentPid);
                    return false;
                }
                return true;
            }
        }

        private void ingest(String batchId, String parentPid) {
            ImportPresenter.this.ingest(batchId, parentPid, new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    if (value != null && value) {
                        ImportPresenter.this.importFolder();
                    }
                }
            });
        }

        private void updateBatchParent(String batchId, final String parentPid) {
            ImportPresenter.this.updateBatchParent(batchId, parentPid, new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    if (value != null && value) {
                        ImportPresenter.this.updateImportedObjects();
                    } else {
                        // show some warning
                    }
                }
            });
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
            importContext.setParentPid(pid);
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

        public BatchRecord getBatch() {
            return batch;
        }

        public void setBatch(BatchRecord batch) {
            this.batch = batch;
        }

        public String getParentPid() {
            return batch.getParentPid();
        }

        public void setParentPid(String pid) {
            batch.setParentPid(pid);
        }

    }

}
