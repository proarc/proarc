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
package cz.incad.pas.editor.client.presenter;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import cz.incad.pas.editor.client.widget.BatchDatastreamEditor;
import cz.incad.pas.editor.client.widget.PageMetadataEditor;
import cz.incad.pas.editor.client.widget.ProgressTracker;
import java.util.Iterator;

/**
 * Support for batch edits of MODS of digital objects.
 *
 * <p>For now accepts selection of pages.
 *
 * @author Jan Pokorsky
 */
public final class ModsBatchEditor implements BatchDatastreamEditor, Refreshable {

    private final PageMetadataEditor editor;
    private DigitalObject[] digitalObjects;
    private String batchId;
    private final ProgressTracker progress;
    private final ClientMessages i18n;

    public ModsBatchEditor(ClientMessages i18n) {
        this.i18n = i18n;
        this.editor = new PageMetadataEditor();
        this.progress = new ProgressTracker(i18n);
    }

    @Override
    public void edit(Record[] items, String batchId) {
        throw new UnsupportedOperationException();
    }
    
    public void edit(DigitalObject[] items, String batchId) {
        this.digitalObjects = items;
        this.batchId = batchId;
    }

    @Override
    public void edit(String pid, String batchId, MetaModelRecord model) {
        throw new UnsupportedOperationException();
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
        return editor.getFormPanel();
    }

    @Override
    public void refresh() {
        editor.initAll();
    }

    void save(BooleanCallback callback) {
        if (editor.validate()) {
            new SaveTask(callback).execute();
        }
    }

    private class SaveTask {

        private int index = 0;
        private int length = -1;
        private boolean stop = false;
        private String errorMsg;

        private Integer batchIndexStart;
        private Iterator<String> batchSequence;
        private String batchNumberFormat;
        private final BooleanCallback taskDoneCallback;

        private SaveTask(BooleanCallback callback) {
            this.taskDoneCallback = callback;
        }

        public void execute() {
            if (length < 0) {
                initTask();
            }
            if (!stop && index < length) {
                processStep();
            } else {
                closeTask();
            }
        }

        private void initTask() {
            length = digitalObjects.length;
            progress.setInit();
            progress.showInWindow(new Runnable() {

                @Override
                public void run() {
                    stop = true;
                }
            });

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

        }

        private void processStep() {
            fetchMods(digitalObjects[index]);
        }

        private void nextStep() {
            execute();
        }

        private void fetchMods(final DigitalObject dobj) {
            MetaModelRecord model = dobj.getModel();
            Criteria criteria = new Criteria(MetaModelDataSource.FIELD_EDITOR, model.getEditorId());
            criteria.addCriteria(ModsCustomDataSource.FIELD_PID, dobj.getPid());
            String bi = dobj.getBatchId() == null ? batchId : dobj.getBatchId();
            if (bi != null) {
                criteria.addCriteria(ModsCustomDataSource.FIELD_BATCHID, bi);
            }
            DSRequest request = new DSRequest();
            request.setShowPrompt(false);
            ModsCustomDataSource.getInstance().fetchData(criteria, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    if (RestConfig.isStatusOk(response)) {
                        Record[] data = response.getData();
                        if (data != null && data.length == 1) {
                            Record customRecord = data[0];
                            Record customModsRecord = customRecord.getAttributeAsRecord(ModsCustomDataSource.FIELD_DATA);
                            if (customModsRecord != null) {
                                updatePage(customRecord, customModsRecord);
                                return ;
                            }
                        } else {
                            errorMsg = "No record found! " + dobj;
                        }
                    } else {
                        errorMsg = "Fetch failed! " + dobj;
                    }
                    stop = true;
                    nextStep();
                }
            }, request);
        }

        private void updatePage(Record customRecord, Record customModsRecord) {
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
            saveMods(customRecord);
        }

        private void saveMods(Record customRecord) {
            DSRequest request = new DSRequest();
            request.setShowPrompt(false);
            ModsCustomDataSource.getInstance().updateData(customRecord, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    if (RestConfig.isStatusOk(response)) {
                        ++index;
                        progress.setProgress(index, length);
                    } else {
                        errorMsg = "Update failed!";
                        stop = true;
                    }
                    nextStep();
                }
            }, request);
        }

        private void closeTask() {
            if (errorMsg != null) {
                progress.stop();
                SC.warn(errorMsg);
            } else {
                progress.stop();
            }
            if (taskDoneCallback != null) {
                taskDoneCallback.execute(errorMsg == null);
            }
        }

    }

}
