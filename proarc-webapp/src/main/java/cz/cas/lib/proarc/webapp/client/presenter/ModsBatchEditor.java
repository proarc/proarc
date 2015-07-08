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
package cz.cas.lib.proarc.webapp.client.presenter;

import com.google.gwt.core.client.Callback;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.DigitalObjectCopyMetadataAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource.DescriptionMetadata;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource.DescriptionSaveHandler;
import cz.cas.lib.proarc.webapp.client.widget.AbstractDatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.BatchDatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.CopyPageMetadataWidget;
import cz.cas.lib.proarc.webapp.client.widget.PageMetadataEditor;
import cz.cas.lib.proarc.webapp.client.widget.ProgressTracker;
import java.util.Iterator;

/**
 * Support for batch edits of MODS of digital objects.
 *
 * <p>For now accepts selection of pages.
 *
 * @author Jan Pokorsky
 */
public final class ModsBatchEditor extends AbstractDatastreamEditor implements BatchDatastreamEditor, Refreshable {

    private BatchJob editor;
    private DigitalObject[] digitalObjects;
    private final ProgressTracker progress;
    private final ClientMessages i18n;
    private final BatchJob metadataGenerator;
    private final BatchJob metadataDuplicator;

    public ModsBatchEditor(ClientMessages i18n) {
        this.i18n = i18n;
        this.metadataGenerator = new GenerateJob(this);
        this.metadataDuplicator = new CopyJob(this);
        this.progress = new ProgressTracker(i18n);
        switchEditor();
    }

    @Override
    public void edit(DigitalObject[] items) {
        this.digitalObjects = items;
        switchEditor();
    }

    private void switchEditor() {
        Record[] selection = DigitalObjectCopyMetadataAction.getSelection();
        if (selection != null) {
            editor = metadataDuplicator;
        } else {
            editor = metadataGenerator;
        }
    }

    @Override
    public void edit(DigitalObject digitalObject) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void focus() {
        editor.focus();
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
        return new Canvas[0];
    }

    @Override
    public Canvas getUI() {
        switchEditor();
        return editor.getFormPanel();
    }

    @Override
    public void refresh() {
        editor.refreshPanel();
    }

    void save(BooleanCallback callback) {
        if (editor.validatePanel()) {
            editor.execute(callback);
        }
    }

    private static abstract class BatchJob {

        private int index = 0;
        private int length = -1;
        private boolean stop = false;
        private String errorMsg;
        private final ModsBatchEditor editor;
        private BooleanCallback taskDoneCallback;
        protected final ClientMessages i18n;

        public BatchJob(ModsBatchEditor editor) {
            this.editor = editor;
            this.i18n = editor.i18n;
        }

        public void execute(BooleanCallback taskDoneCallback) {
            this.taskDoneCallback = taskDoneCallback;
            index = 0;
            length = -1;
            stop = false;
            execute();
        }

        protected void execute() {
            if (length < 0) {
                init();
            }
            if (!stop && index < length) {
                processStep();
            } else {
                close();
            }
        }

        public ProgressTracker getProgress() {
            return editor.progress;
        }

        public int getCurrentIndex() {
            return index;
        }

        public DigitalObject getCurrent() {
            return editor.digitalObjects[getCurrentIndex()];
        }

        public DigitalObject[] getSelection() {
            return editor.digitalObjects;
        }

        public void next() {
            ++index;
            getProgress().setProgress(index, length);
            execute();
        }

        public void stop(String reason) {
            stop = true;
            errorMsg = reason;
            execute();
        }

        public void focus() {
            getFormPanel().focus();
        }

        public abstract Canvas getFormPanel();
        public abstract void refreshPanel();
        public abstract boolean validatePanel();

        protected abstract void processStep();

        protected void init() {
            length = getSelection().length;
            getProgress().setInit();
            getProgress().showInWindow(new Runnable() {

                @Override
                public void run() {
                    stop = true;
                }
            });
        }

        private void close() {
            if (errorMsg != null) {
                getProgress().stop();
                SC.warn(errorMsg);
            } else {
                getProgress().stop();
            }
            if (taskDoneCallback != null) {
                taskDoneCallback.execute(errorMsg == null);
            }
        }

        void saveMods(DescriptionMetadata description) {
            ModsCustomDataSource.getInstance().saveDescription(
                    description,
                    new DescriptionSaveHandler() {

                        @Override
                        protected void onSave(DescriptionMetadata dm) {
                            super.onSave(dm);
                            next();
                        }

                        @Override
                        protected void onConcurrencyError() {
                            stop("Update failed!");
                        }

                        @Override
                        protected void onError() {
                            super.onError();
                            stop("Update failed!");
                        }

                        @Override
                        protected void onValidationError() {
                            stop(getValidationMessage());
                        }

                    },
                    false);
        }

    }

    /**
     * Generates metadata for selected digital objects.
     */
    private static class GenerateJob extends BatchJob {

        private final PageMetadataEditor editor;
        /** An increment of the job item index used to process only required pages. */
        private int batchApplyTo;
        private Integer batchIndexStart;
        private Iterator<String> batchSequence;
        private String batchNumberFormat;
        private Canvas panel;

        public GenerateJob(final ModsBatchEditor editor) {
            super(editor);
            this.editor = new PageMetadataEditor();
            this.editor.setSubmitHandler(new SubmitValuesHandler() {

                @Override
                public void onSubmitValues(SubmitValuesEvent event) {
                    editor.fireEvent(event);
                }
            });
        }

        @Override
        public Canvas getFormPanel() {
            if (panel == null) {
                panel = editor.getFormPanel();
                panel.setWidth100();
                panel.setHeight100();
                panel.setOverflow(Overflow.AUTO);
            }
            return panel;
        }

        @Override
        public void refreshPanel() {
            editor.initAll();
        }

        @Override
        public boolean validatePanel() {
            editor.setMaxApplyTo(getSelection().length);
            return editor.validate();
        }

        @Override
        protected void init() {
            super.init();
            batchIndexStart = null;
            batchSequence = null;
            batchNumberFormat = "%s";
            if (editor.getAllowPageIndexes()) {
                batchIndexStart = editor.getIndexStart();
            }
            if (editor.getAllowPageNumbers()) {
                batchSequence = editor.getSequence();
                String prefix = editor.getPrefix();
                String suffix = editor.getSuffix();
                if (prefix != null) {
                    batchNumberFormat = prefix + batchNumberFormat;
                }
                if (suffix != null) {
                    batchNumberFormat += suffix;
                }
            }
            batchApplyTo = editor.getApplyTo();
            if (batchApplyTo > getSelection().length) {
                stop(i18n.PageMetadataEditor_ApplyToErrOutOfBounds_Msg(String.valueOf(batchApplyTo)));
            }
        }

        @Override
        protected void processStep() {
            if ((getCurrentIndex() + 1) % batchApplyTo == 0) {
                fetchMods(getCurrent());
            } else {
                next();
            }
        }

        private void fetchMods(final DigitalObject dobj) {
            ModsCustomDataSource.getInstance().fetchDescription(dobj, new Callback<DescriptionMetadata, String>() {

                @Override
                public void onFailure(String reason) {
                    stop(reason);
                }

                @Override
                public void onSuccess(DescriptionMetadata result) {
                    updatePage(result);
                }
            }, false);
        }

        private void updatePage(DescriptionMetadata description) {
            Record customModsRecord = description.getDescription();
            // fill data
//            RPCManager.startQueue();
            if (editor.getAllowPageIndexes()) {
                String old = customModsRecord.getAttributeAsString(ModsCustomDataSource.FIELD_PAGE_INDEX);
                String newVal = batchIndexStart == null ? null : String.valueOf(batchIndexStart++);
                newVal = (old != null && newVal == null) ? "" : newVal;
                customModsRecord.setAttribute(ModsCustomDataSource.FIELD_PAGE_INDEX, newVal);
            }
            if (editor.getAllowPageNumbers()) {
                String old = customModsRecord.getAttributeAsString(ModsCustomDataSource.FIELD_PAGE_NUMBER);
                String newVal = batchSequence != null
                        ? ClientUtils.format(batchNumberFormat, batchSequence.next())
                        : ClientUtils.format(batchNumberFormat, "");
                newVal = newVal.isEmpty() ? null : newVal;
                newVal = (old != null && newVal == null) ? "" : newVal;
                customModsRecord.setAttribute(ModsCustomDataSource.FIELD_PAGE_NUMBER, newVal);
            }
            if (editor.getAllowPageTypes()) {
                String pageType = editor.getPageType();
                customModsRecord.setAttribute(ModsCustomDataSource.FIELD_PAGE_TYPE, pageType);
            }
            ClientUtils.removeNulls(customModsRecord);
//            RPCManager.sendQueue();
            saveMods(description);
        }

    }

    /**
     * Copies metadata to selected digital objects.
     * It takes {@link DigitalObjectCopyMetadataAction#getSelection() }
     * as a source.
     */
    private static class CopyJob extends BatchJob {

        private Record[] templateRecords;
        private DescriptionMetadata[] templateDescriptions;
        private Canvas panel;
        private final CopyPageMetadataWidget widget;

        public CopyJob(ModsBatchEditor editor) {
            super(editor);
            widget = new CopyPageMetadataWidget();
            widget.initAll();
        }

        @Override
        public Canvas getFormPanel() {
            if (panel == null) {
                panel = widget.getPanel();
                panel.setWidth100();
                panel.setHeight100();
                panel.setOverflow(Overflow.AUTO);
            }
            return panel;
        }

        @Override
        protected void init() {
            templateRecords = DigitalObjectCopyMetadataAction.getSelection();
            templateDescriptions = new DescriptionMetadata[templateRecords.length];
            super.init();
        }

        @Override
        public void refreshPanel() {
            widget.initAll();
        }

        @Override
        public boolean validatePanel() {
            return widget.validate();
        }

        @Override
        protected void processStep() {
            updateDescription();
        }

        private void updateDescription() {
            int templateIndex = getTemplateIndex();
            Record templateRecord = templateRecords[templateIndex];
            prepareTemplate(DigitalObject.create(templateRecord), templateDescriptions[templateIndex]);
        }

        private void updatePage(DescriptionMetadata descCurrent, DescriptionMetadata descTemplate) {
            Record current = descCurrent.getDescription();
            Record template = descTemplate.getDescription();
            copyAttribute(widget.getAllowPageIndexes(), template, current, ModsCustomDataSource.FIELD_PAGE_INDEX);
            copyAttribute(widget.getAllowPageNumbers(), template, current, ModsCustomDataSource.FIELD_PAGE_NUMBER);
            copyAttribute(widget.getAllowPageTypes(), template, current, ModsCustomDataSource.FIELD_PAGE_TYPE);
            saveMods(descCurrent);
        }

        private void prepareDescription(DigitalObject dobj, final DescriptionMetadata templateDesc) {
            ModsCustomDataSource.getInstance().fetchDescription(dobj, new Callback<DescriptionMetadata, String>() {

                @Override
                public void onFailure(String reason) {
                    stop(reason);
                }

                @Override
                public void onSuccess(DescriptionMetadata result) {
                    updatePage(result, templateDesc);
                }
            }, false);
        }

        private void prepareTemplate(DigitalObject templateObj, DescriptionMetadata templateDesc) {
            if (templateDesc != null) {
                prepareDescription(getCurrent(), templateDesc);
                return ;
            }
            ModsCustomDataSource.getInstance().fetchDescription(templateObj, new Callback<DescriptionMetadata, String>() {

                @Override
                public void onFailure(String reason) {
                    stop(reason);
                }

                @Override
                public void onSuccess(DescriptionMetadata result) {
                    templateDescriptions[getTemplateIndex()] = result;
                    prepareDescription(getCurrent(), result);
                }
            }, false);
        }

        private void copyAttribute(boolean enabled, Record src, Record dst, String attrName) {
            if (enabled) {
                String value = src.getAttribute(attrName);
                if (value != null) {
                    dst.setAttribute(attrName, value);
                }
            }
        }

        private int getTemplateIndex() {
            int tmplIndex = getCurrentIndex() % templateRecords.length;
            return tmplIndex;
        }

    }

}
