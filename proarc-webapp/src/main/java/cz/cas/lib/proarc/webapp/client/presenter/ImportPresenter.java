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
package cz.cas.lib.proarc.webapp.client.presenter;

import com.google.gwt.place.shared.PlaceController;
import com.google.gwt.safehtml.shared.SafeHtmlUtils;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.PromptStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.ErrorHandler;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectFormValidateAction;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectFormValidateAction.Validatable;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchDataSource.BatchRecord;
import cz.cas.lib.proarc.webapp.client.ds.ImportBatchItemDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ImportTreeDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ImportTreeDataSource.ImportRecord;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectManaging.DigitalObjectManagerPlace;
import cz.cas.lib.proarc.webapp.client.presenter.Importing.ImportPlace;
import cz.cas.lib.proarc.webapp.client.presenter.Importing.ImportPlace.Type;
import cz.cas.lib.proarc.webapp.client.widget.ImportBatchChooser;
import cz.cas.lib.proarc.webapp.client.widget.ImportBatchChooser.ImportBatchChooserHandler;
import cz.cas.lib.proarc.webapp.client.widget.ImportBatchItemEditor;
import cz.cas.lib.proarc.webapp.client.widget.ImportParentChooser;
import cz.cas.lib.proarc.webapp.client.widget.ImportParentChooser.ImportParentHandler;
import cz.cas.lib.proarc.webapp.client.widget.ImportSourceChooser;
import cz.cas.lib.proarc.webapp.client.widget.ImportSourceChooser.ImportSourceChooserHandler;
import cz.cas.lib.proarc.webapp.client.widget.ProgressTracker;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import cz.cas.lib.proarc.webapp.client.widget.Wizard;
import cz.cas.lib.proarc.webapp.client.widget.Wizard.StepKind;
import cz.cas.lib.proarc.webapp.client.widget.Wizard.WizardStep;
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
    private final ClientMessages i18n;
    private final PlaceController placeController;

    public ImportPresenter(ClientMessages i18n, PlaceController placeController) {
        this.i18n = i18n;
        selectFolderStep = new SelectFolderStep();
        selectBatchStep = new SelectBatchStep();
        selectParentStep = new SelectParentStep();
        updateItemsStep = new UpdateItemsStep();
        finishedStep = new FinishedStep();
        wizard = new Wizard(i18n, selectFolderStep, selectBatchStep,
                updateItemsStep, selectParentStep, finishedStep);
        this.placeController = placeController;
    }

    public void bind() {
        importContext = new ImportContext();
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
        wizard.setShowButtons(false);
    }

    public void updateImportedObjects() {
        wizard.moveAt(updateItemsStep);
        wizard.setShowButtons(true);
    }
    
    public void updateImportedObjects(String batchId) {
        BatchRecord batchRecord = new BatchRecord(new Record());
        batchRecord.setId(batchId);
        getImportContext().setBatch(batchRecord);
        wizard.moveAt(updateItemsStep);
        wizard.setShowButtons(false);
    }

    public void selectParent() {
        wizard.moveAt(selectParentStep);
        wizard.setShowButtons(false);
    }

    public void selectParent(String batchId) {
        BatchRecord batchRecord = new BatchRecord(new Record());
        batchRecord.setId(batchId);
        getImportContext().setBatch(batchRecord);
        wizard.moveAt(selectParentStep);
        wizard.setShowButtons(false);
    }

    public void selectBatchFromHistory() {
        wizard.moveAt(selectBatchStep);
        wizard.setShowButtons(false);
    }

    public void finishImport() {
        wizard.moveAt(finishedStep);
        wizard.setShowButtons(true);
    }

    private void loadBatch(final String batchId, final Runnable callback) {
        Criteria criteria = new Criteria(ImportBatchDataSource.FIELD_ID, batchId);
        ImportBatchDataSource.getInstance().fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                BatchRecord batchRecord = null;
                if (RestConfig.isStatusOk(response)) {
                    Record[] records = response.getData();
                    if (records.length > 0) {
                        batchRecord = new BatchRecord(records[0]);
                    } else {
                        SC.warn("Batch not found! " + batchId);
                    }
                }
                getImportContext().setBatch(batchRecord);
                callback.run();
            }
        });
    }

    private void ingest(String batchId, String parentId, final BooleanCallback call) {
        ImportBatchDataSource dsBatch = ImportBatchDataSource.getInstance();
        DSRequest dsRequest = new DSRequest();
        dsRequest.setPromptStyle(PromptStyle.DIALOG);
        dsRequest.setPrompt(i18n.ImportWizard_UpdateItemsStep_Ingesting_Title());
        Record update = new Record();
        update.setAttribute(ImportBatchDataSource.FIELD_ID, batchId);
        update.setAttribute(ImportBatchDataSource.FIELD_PARENT, parentId);
        update.setAttribute(ImportBatchDataSource.FIELD_STATE, ImportBatchDataSource.State.INGESTING.name());
        dsBatch.updateData(update, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] records = response.getData();
                    if (records != null && records.length > 0) {
                        importContext.setBatch(new BatchRecord(records[0]));
                        call.execute(true);
                        return;
                    }
                }
                call.execute(false);
            }
        }, dsRequest);
    }

    /**
     * Resets batch import.
     * Supports {@code LOADING_FAILED, LOADED, INGESTING_FAILED} for now.
     */
    private void reset(BatchRecord batch, String newProfileId, final BooleanCallback call) {
        ImportBatchDataSource.State state = batch.getState();
        String stateAttr;
        if (state == ImportBatchDataSource.State.LOADING_FAILED
                || state == ImportBatchDataSource.State.LOADED
                || state == ImportBatchDataSource.State.STOPPED) {

            stateAttr = ImportBatchDataSource.State.LOADING_FAILED.name();
        } else if (state == ImportBatchDataSource.State.INGESTING_FAILED) {
            stateAttr = ImportBatchDataSource.State.INGESTING.name();
        } else {
            throw new IllegalStateException("Unsupported state: " + batch.getState());
        }
        ImportBatchDataSource dsBatch = ImportBatchDataSource.getInstance();
        DSRequest dsRequest = new DSRequest();
        Record update = new Record();
        update.setAttribute(ImportBatchDataSource.FIELD_ID, batch.getId());
        update.setAttribute(ImportBatchDataSource.FIELD_STATE, stateAttr);
        if (newProfileId != null) {
            update.setAttribute(ImportBatchDataSource.FIELD_PROFILE_ID, newProfileId);
        }
        dsBatch.updateData(update, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] records = response.getData();
                    if (records != null && records.length > 0) {
                        importContext.setBatch(new BatchRecord(records[0]));
                        call.execute(true);
                        return;
                    }
                }
                call.execute(false);
            }
        }, dsRequest);
    }

    private void updateBatchParent(String batchId, final String parentPid, final BooleanCallback call) {
        ImportBatchDataSource dsBatch = ImportBatchDataSource.getInstance();
        DSRequest dsRequest = new DSRequest();
        Record update = new Record();
        update.setAttribute(ImportBatchDataSource.FIELD_ID, batchId);
        update.setAttribute(ImportBatchDataSource.FIELD_PARENT, parentPid != null ? parentPid : "");
        dsBatch.updateData(update, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (!RestConfig.isStatusOk(response)) {
                    importContext.setParentPid(null);
                    call.execute(false);
                    return;
                }
                Record[] data = response.getData();
                if (data != null && data.length > 0) {
                    importContext.setBatch(new BatchRecord(data[0]));
                    call.execute(true);
                } else {
                    // XXX show warning something is wrong
                    importContext.setBatch(null);
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
            wizard.setWizardLabel(i18n.ImportWizard_DescriptionPrefix_Title(),
                    i18n.ImportWizard_SelectBatchStep_Description_Title());
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
            return true;
        }

        @Override
        public Canvas asWidget() {
            if (widget == null) {
                widget = new ImportBatchChooser(i18n);
            }
            return widget;
        }

        @Override
        public void itemSelected() {
            BatchRecord batch = widget.getSelectedBatch();
            getImportContext().setBatch(batch);
            placeController.goTo(new ImportPlace(Type.EDIT_ITEMS, batch.getId()));
        }

        @Override
        public void itemReset() {
            BatchRecord batch = widget.getSelectedBatch();
            String profile = widget.getSelectedProfile();
            reset(batch, profile, ClientUtils.EMPTY_BOOLEAN_CALLBACK);
        }

    }
    private final class SelectFolderStep implements WizardStep, ImportSourceChooserHandler {

        private ImportSourceChooser importSourceChooser;
        private Wizard wizard;

        @Override
        public void onShow(Wizard wizard) {
            this.wizard = wizard;
            wizard.setWizardLabel(i18n.ImportWizard_DescriptionPrefix_Title(),
                    i18n.ImportWizard_SelectFolderStep_Description_Title());

            ImportPresenter.this.importContext = new ImportContext();
            importSourceChooser.setViewHandler(this);
            importSourceChooser.edit();
        }

        @Override
        public void onHide(Wizard wizard) {
            importSourceChooser.setViewHandler(null);
        }

        @Override
        public boolean onStepAction(Wizard wizard, StepKind step) {
            return true;
        }

        @Override
        public Canvas asWidget() {
            if (importSourceChooser == null) {
                importSourceChooser = new ImportSourceChooser(i18n);
            }
            return importSourceChooser;
        }

        @Override
        public void sourceSelected() {
            handleImportSource();
        }

        private void handleImportSource() {
            if (!importSourceChooser.validateOptions()) {
                return ;
            }
            Record record = importSourceChooser.getImportSource();
            ImportRecord importRecord = record == null ? null : new ImportRecord(record);

            if (importRecord != null && importRecord.isNew()) {
                final ImportBatchDataSource dsBatch = ImportBatchDataSource.getInstance();
                DSRequest dsRequest = new DSRequest();
                dsRequest.setPromptStyle(PromptStyle.DIALOG);
                dsRequest.setPrompt(i18n.ImportWizard_SelectFolderStep_Wait_Title());
                Record newBatch = dsBatch.newBatch(importRecord.getPath(),
                        importSourceChooser.getImportProfile(),
                        importSourceChooser.getDevice(),
                        importSourceChooser.getGenerateIndices());
                dsBatch.addData(newBatch, new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        if (!RestConfig.isStatusOk(response)) {
                            response.setInvalidateCache(true);
                            return;
                        }
                        Record[] data = response.getData();
                        if (data != null && data.length > 0) {
                            BatchRecord newBatch = new BatchRecord(data[0]);
                            if (newBatch.isArchiveOrKramerius()) {
                                SC.say(i18n.ImportWizard_SelectFolderStep_ImportScheduled_Title());
                                response.setInvalidateCache(true);
                                dsBatch.updateCaches(response, request);
                            } else {
                                showProgress(newBatch);
                            }
                        } else {
                            response.setInvalidateCache(true);
                            SC.warn(i18n.ImportWizard_SelectFolderStep_NothingToImport_Msg());
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
            ProgressTracker progress = new ProgressTracker(i18n);
            progress.setCloseButton(i18n.ProgressTracker_Continue_Title(),
                    i18n.ProgressTracker_Continue_Hint());
            progress.setDataSource(ds, criteria);
            progress.setInit();
            progress.setProgressPrefix(i18n.ImportWizard_SelectFolderStep_ImportProgress_Prefix_Title());
            progress.showInWindow(new Runnable() {

                @Override
                public void run() {
                    checkBatchState(batch);
                }
            }, i18n.ImportWizard_SelectFolderStep_ImportProgress_Title());
        }

        private void checkBatchState(BatchRecord batch) {
            loadBatch(batch.getId(), new Runnable() {

                @Override
                public void run() {
                    BatchRecord batch = getImportContext().getBatch();
                    if (batch != null && batch.getState() == ImportBatchDataSource.State.LOADED) {
                        ImportPresenter.this.getImportContext().setBatch(batch);
                        importSourceChooser.updateCache(ImportTreeDataSource.FolderState.IMPORTED);
                        placeController.goTo(new ImportPlace(Type.EDIT_ITEMS, batch.getId()));
                    } else if (batch.getState() == ImportBatchDataSource.State.LOADING) {
                        // The import batch is still loading! An user chose to skip the progress.
                        importSourceChooser.refreshSelectedNode();
                    } else {
                        String error = batch.getLog();
                        if (error != null && !error.isEmpty()) {
                            error = SafeHtmlUtils.htmlEscape(error);
                        } else {
                            error = "The import batch loading failed!";
                        }
                        ErrorHandler.warn(error);
                        importSourceChooser.refreshSelectedNode();
                    }
                }
            });
        }

    }

    private final class UpdateItemsStep implements WizardStep, ImportBatchItemEditor.Handler {

        private ImportBatchItemEditor widget;

        @Override
        public void onShow(Wizard wizard) {
            wizard.setWizardLabel(i18n.ImportWizard_DescriptionPrefix_Title(),
                    i18n.ImportWizard_UpdateItemsStep_Description_Title());
            BatchRecord batch = getImportContext().getBatch();
            widget.setHandler(this);
            widget.onShow(batch);
        }

        @Override
        public void onHide(Wizard wizard) {
            // XXX check
        }

        @Override
        public boolean onStepAction(final Wizard wizard, final StepKind step) {
            return true;
        }

        @Override
        public Canvas asWidget() {
            if (widget == null) {
                widget = new ImportBatchItemEditor(i18n);
            }
            return widget;
        }

        @Override
        public void handleNextAction() {
            BatchRecord batch = getImportContext().getBatch();
            ImportPlace place = new ImportPlace(Type.EDIT_PARENT, batch.getId());
            placeController.goTo(place);
        }

    }

    private final class SelectParentStep implements WizardStep, ImportParentHandler {

        private ImportParentChooser widget;
        private Wizard wizard;
        private VLayout container;
        private final ActionSource actionSource = new ActionSource(this);

        @Override
        public void onShow(Wizard wizard) {
            wizard.setWizardLabel(i18n.ImportWizard_DescriptionPrefix_Title(),
                    i18n.ImportWizard_SelectParentStep_Description_Title());
            this.wizard = wizard;
            BatchRecord batch = ImportPresenter.this.getImportContext().getBatch();
            widget.getUI().hide();
            loadBatch(batch.getId(), new Runnable() {

                @Override
                public void run() {
                    bindData();
                }
            });

        }

        private void bindData() {
            BatchRecord batch = ImportPresenter.this.getImportContext().getBatch();
            if (batch != null && batch.getId() != null) {
                widget.setHandler(this);
                widget.setImport(batch.getId());
            }
            canStep();
        }

        private void canStep() {
            BatchRecord batch = getImportContext().getBatch();
            actionSource.fireEvent();
            widget.getUI().setVisible(batch != null);
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
                    fetchItems(batchId, parentPid);
                }
                return false;
            } else {
                placeController.goTo(new ImportPlace(Type.EDIT_ITEMS, batchId));
                return false;
            }
        }

        private void fetchItems(final String batchId, final String parentPid) {
            final Criteria criteria = new Criteria(ImportBatchItemDataSource.FIELD_BATCHID, batchId);
            ImportBatchItemDataSource ds = ImportBatchItemDataSource.getInstance();
            ds.fetchData(criteria, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    if (RestConfig.isStatusOk(response)) {
                        validateItems(batchId, parentPid, response.getData());
                    }
                }
            });
        }

        private void validateItems(final String batchId, final String parentPid, final Record[] items) {
            final DigitalObjectFormValidateAction action = DigitalObjectFormValidateAction.getInstance(i18n);
            action.validate(new Validatable() {

                private boolean valid;

                @Override
                public void clearErrors(Record r) {
                    // no-op
                }

                @Override
                public void init() {
                    valid = true;
                }

                @Override
                public void setErrors(Record r, String errors) {
                    valid = false;
                }

                @Override
                public Record[] getSelection() {
                    return items;
                }

                @Override
                public void onFinish(boolean canceled) {
                    if (!canceled) {
                        if (valid) {
                            action.closeWindow();
                            ingest(batchId, parentPid);
                        }
                    }
                }
            });
        }

        private void ingest(String batchId, String parentPid) {
            ImportPresenter.this.ingest(batchId, parentPid, new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    BooleanCallback bc = new BooleanCallback() {

                        @Override
                        public void execute(Boolean value) {
                            placeController.goTo(new DigitalObjectManagerPlace());
                        }
                    };
                    // XXX implement status page listing items and their states
                    if (value != null && value) {
                        SC.say(i18n.ImportWizard_Ingest_Done_Msg(), bc);
                    } else {
                        SC.warn(i18n.ImportWizard_Ingest_Failed_Msg(), bc);
                    }
                }
            });
        }

        private void updateBatchParent(String batchId, final String parentPid) {
            ImportPresenter.this.updateBatchParent(batchId, parentPid, new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    StatusView.getInstance().show(i18n.ImportParentChooser_SaveAction_Done_Msg());
                    canStep();
                }
            });
        }

        @Override
        public Canvas asWidget() {
            if (widget == null) {
                widget = new ImportParentChooser(i18n);
                widget.setParentOwnerCheck(true);
                widget.getUI().setMargin(4);

                container = new VLayout();
                container.setMembers(buildToolbar(), widget.getUI());
            }
            return container;
        }

        private Canvas buildToolbar() {
            Action ingestAction = new AbstractAction(i18n.ImportWizard_ButtonImport_Title(),
                    "[SKIN]/actions/save.png", i18n.ImportWizard_ButtonImport_Hint()) {

                @Override
                public void performAction(ActionEvent event) {
                    onStepAction(wizard, StepKind.FORWARD);
                }

                @Override
                public boolean accept(ActionEvent event) {
                    BatchRecord batch = getImportContext().getBatch();
                    String batchId = batch != null ? batch.getId() : null;
                    String parentPid = batch != null ? batch.getParentPid() : null;
                    return batchId != null && parentPid != null;
                }
            };

            Action backAction = new AbstractAction(i18n.ImportWizard_UpdateItemsStepAction_Title(),
                    "[SKIN]/actions/prev.png", i18n.ImportWizard_UpdateItemsStepAction_Hint()) {

                @Override
                public void performAction(ActionEvent event) {
                    onStepAction(wizard, StepKind.BACK);
                }
            };
            ToolStrip toolbar = Actions.createToolStrip();
            toolbar.addMember(Actions.asIconButton(new RefreshAction(i18n), this));
            toolbar.addMember(Actions.asIconButton(backAction, this));
            toolbar.addMember(Actions.asIconButton(ingestAction, actionSource));
            return toolbar;
        }

        @Override
        public void onParentSelectionUpdated() {
            Record selectedParent = widget.getSelectedParent();
            String pid = (selectedParent != null)
                    ? selectedParent.getAttribute(RelationDataSource.FIELD_PID)
                    : null;
            String batchId = getImportContext().getBatch().getId();
            updateBatchParent(batchId, pid);
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
            wizard.setWizardLabel(i18n.ImportWizard_DescriptionPrefix_Title(), "done");
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
            return batch != null ? batch.getParentPid() : null;
        }

        public void setParentPid(String pid) {
            batch.setParentPid(pid);
        }

    }

}
