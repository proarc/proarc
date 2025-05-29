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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.Offline;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.ColorPicker;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ColorSelectedEvent;
import com.smartgwt.client.widgets.form.events.ColorSelectedHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.DataArrivedEvent;
import com.smartgwt.client.widgets.form.fields.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.MediaDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.StreamProfileDataSource;
import cz.cas.lib.proarc.webapp.client.ds.StreamProfileDataSource.StreamProfile;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.ArrayList;

/**
 * Edits data streams containing digitized multimedia content.
 *
 * @author Jan Pokorsky
 */
public final class MediaEditor implements DatastreamEditor, Refreshable {

    public static final String MEDIA_EDITOR_LAST_SELECTION = "mediaEditorLastSelection";

    public static final String SOURCE_DIGITAL_OBJECT_EDITOR = "DigitalObjectEditor";
    public static final String SOURCE_IMPORT_BATCH_ITEM_EDITOR = "ImportBatchItemEditor";

    public static final String RAW_ID = "RAW";

    public static final String SOURCE_IDENTIFIER = "source";

    private static String REFRESH;

    private final ClientMessages i18n;
    private final DigitalObjectPreview doPreview;
    private String imgParams;
    private final ArrayList<Canvas> backgroundListeners = new ArrayList<Canvas>();
    private Action fullAction;
    private AbstractAction backgroundAction;
    private AbstractAction uploadAction;
    private AbstractAction removeAction;
    private DigitalObject digitalObject;
    private SelectItem streamMenu;
    private boolean showRefreshButton;
    private final ActionSource actionSource;
    private String source;

    public MediaEditor(ClientMessages i18n, String source) {
        this.i18n = i18n;
        this.actionSource = new ActionSource(this);
        this.source = source;
        doPreview = new DigitalObjectPreview(i18n);
        initActions(i18n);
    }

    @Override
    public void edit(DigitalObject digitalObject) {
        if (digitalObject == null) {
            throw new NullPointerException();
        }
        if (this.digitalObject != null && this.digitalObject.getPid().equals(digitalObject.getPid())) {
            return ;
        }
        this.digitalObject = digitalObject;
        updateStreamMenu(digitalObject);
        actionSource.fireEvent();
    }

    @Override
    public void focus() {
        // no-op
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
        Canvas zoomer = doPreview.getPreviewZoomer();
        zoomer.setWidth(100);
        ArrayList<Canvas> toolbar = new ArrayList<Canvas>();
        if (showRefreshButton) {
            RefreshAction refreshAction = new RefreshAction(i18n);
            refreshAction.setTitle(null);
            toolbar.add(Actions.asIconButton(refreshAction, this));
        }
        toolbar.add(Actions.asIconButton(fullAction, this));
        toolbar.add(Actions.asIconButton(uploadAction, actionSource));
        toolbar.add(Actions.asIconButton(removeAction, actionSource));
        toolbar.add(Actions.asIconButton(backgroundAction, this));
        toolbar.add(zoomer);
        toolbar.add(createStreamMenu());
        return toolbar.toArray(new Canvas[toolbar.size()]);
    }

    @Override
    public Canvas getUI() {
        return doPreview.asCanvas();
    }

    @Override
    public void refresh() {
        DigitalObject refresh = this.digitalObject;
        this.digitalObject = null;
        REFRESH = String.valueOf(System.currentTimeMillis());
        edit(refresh);
    }

    public void addBackgroundColorListeners(Canvas c) {
        backgroundListeners.add(c);
    }

    public void setBackgroundColor(String color) {
        doPreview.setBackgroundColor(color);
        for (Canvas canvas : backgroundListeners) {
            canvas.setBackgroundColor(color);
        }
    }

    public boolean isShowRefreshButton() {
        return showRefreshButton;
    }

    public void setShowRefreshButton(boolean showRefreshButton) {
        this.showRefreshButton = showRefreshButton;
    }

    private void initActions(final ClientMessages i18n) {
        fullAction = new AbstractAction(
                i18n.DigitalObjectPreview_ViewFullAction_Title(),
                "[SKIN]/actions/view.png",
                i18n.DigitalObjectPreview_ViewFullAction_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                doPreview.showInWindow(getObjectTitle());
            }
        };

        backgroundAction = new AbstractAction(
                i18n.DigitalObjectPreview_ColorChooserAction_Title(),
                "[SKIN]/actions/color_swatch.png",
                i18n.DigitalObjectPreview_ColorChooserAction_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                ColorPicker picker = new ColorPicker();
                picker.addColorSelectedHandler(new ColorSelectedHandler() {

                    @Override
                    public void onColorSelected(ColorSelectedEvent event) {
                        setBackgroundColor(event.getColor());
                    }
                });
                picker.setDefaultColor(DigitalObjectPreview.BACKGROUND_COLOR);
                picker.setKeepInParentRect(true);
                picker.show();
            }
        };

        uploadAction = new AbstractAction(
                i18n.DigitalObjectEditor_MediaEditor_UploaderAction_Title(),
                "[SKIN]/MultiUploadItem/icon_add_files.png",
                i18n.DigitalObjectEditor_MediaEditor_UploaderAction_Hint()) {

            @Override
            public boolean accept(ActionEvent event) {
                return acceptModel(event);
            }

            @Override
            public void performAction(ActionEvent event) {
                UploadFile uploadFile = new UploadFile(i18n);
                uploadFile.showWindow(digitalObject, new BooleanCallback() {

                    @Override
                    public void execute(Boolean value) {
                        if (value != null && value) {
                            RelationDataSource.getInstance().fireRelationChange(digitalObject.getPid());
                            refresh();
                        }
                    }
                });
            }
        };

        removeAction = new AbstractAction(
                i18n.DigitalObjectEditor_MediaEditor_RemoverAction_Title(),
                "[SKIN]/MultiUploadItem/icon_remove_files.png",
                i18n.DigitalObjectEditor_MediaEditor_RemoverAction_Hint()) {

            @Override
            public boolean accept(ActionEvent event) {
                return acceptModel(event);
            }

            @Override
            public void performAction(ActionEvent event) {

                DynamicForm optionsForm = createExpandOptionsForm();

                optionsForm.clearValues();
                final Dialog d = new Dialog(i18n.RemoveDSAction_Window_Title());
                d.getDialogLabelContainer().setContents(i18n.RemoveDSAction_Window_Msg());
                d.getDialogContentContainer().setMembers(optionsForm);
                d.addYesButton((ClickEvent eventX) -> {
                    Record options = optionsForm.getValuesAsRecord();
                    d.destroy();

                    removeDS();
                    refresh();
                });
                d.addNoButton(new Dialog.DialogCloseHandler() {
                    @Override
                    public void onClose() {
                        d.destroy();
                    }
                });
                d.setWidth(400);
                d.show();
            }
        };
    }

    private boolean acceptModel(ActionEvent event) {
        String modelId = digitalObject.getModelId();
        return modelId != null && (modelId.startsWith("model:bdm") || "model:derFile".equals(modelId) || "model:desFile".equals(modelId)
                || NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME.equals(modelId) || NdkEbornPlugin.MODEL_EMONOGRAPHUNIT.equals(modelId) || NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(modelId)
                || NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT.equals(modelId) || NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT.equals(modelId)
                || NdkEbornPlugin.MODEL_EARTICLE.equals(modelId) || NdkEbornPlugin.MODEL_ECHAPTER.equals(modelId));
    }

    private static DynamicForm createExpandOptionsForm() {
        DynamicForm f = new DynamicForm();
        f.setAutoHeight();
        return f;
    }

    private void removeDS() {
        if (digitalObject == null) {
            throw new IllegalArgumentException("uuid cannot be null");
        }

        String pid = digitalObject.getPid();
        String batchId = digitalObject.getBatchId();

        Record query = new Record();
        query.setAttribute(MediaDataSource.OBJECT_PID, pid);

        if (batchId != null) {
            query.setAttribute(MediaDataSource.BATCH_ID, batchId);
        }

        query.setAttribute(MediaDataSource.DATASTREAM_ID, "RAW");

        BooleanCallback call = new BooleanCallback() {
            @Override
            public void execute(Boolean value) {
            }
        };

        DSRequest dsRequest = new DSRequest();
        dsRequest.setData(query); // prevents removeData to drop other than primary key attributes

        MediaDataSource.getInstance().removeData(query, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (!RestConfig.isStatusOk(response)) {
                    call.execute(false);
                    return;
                }
                call.execute(true);
            }
        }, dsRequest);
    }

    private DynamicForm createStreamMenu() {
        streamMenu = new SelectItem();
        streamMenu.setShowTitle(Boolean.FALSE);
        streamMenu.setWidth(100);
        streamMenu.setPickListWidth(150);
        streamMenu.addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                showStream();
            }
        });
        streamMenu.setAutoFetchData(Boolean.FALSE);
        streamMenu.setOptionDataSource(StreamProfileDataSource.getInstance());
        SortSpecifier sort = new SortSpecifier(StreamProfileDataSource.FIELD_ORDER, SortDirection.ASCENDING);
        streamMenu.setPickListSort(new SortSpecifier[] {sort});
        streamMenu.setDisplayField(StreamProfileDataSource.FIELD_LABEL);
        streamMenu.setValueField(StreamProfileDataSource.FIELD_ID);
        streamMenu.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                updateStreamMenu(event.getData(), streamMenu);
            }
        });

        DynamicForm form = new DynamicForm();
        form.setFields(streamMenu);
        form.setLayoutAlign(Alignment.CENTER);
        return form;
    }

    private void updateStreamMenu(DigitalObject dobj) {
        Criteria streamMenuFilter = dobj.toCriteria();
        streamMenu.setPickListCriteria(streamMenuFilter);

        DSRequest dsRequest = new DSRequest();
        dsRequest.setAttribute(SOURCE_IDENTIFIER, getLastSelectionId(dobj, source));

        streamMenu.fetchData((dsResponse, o, dsRequest1) -> {}, dsRequest);
    }

    private void updateStreamMenu(ResultSet data, SelectItem view) {
        ListGridRecord lastViewSelection = view.getSelectedRecord();
        Boolean contains = lastViewSelection == null ? false : data.contains(lastViewSelection);
        if (!contains) {
            String dsId = data.isEmpty() ? null : data.get(0).getAttribute(StreamProfileDataSource.FIELD_ID);
            view.setValue(dsId);
        }
        showStream();
    }

    private void showStream() {
        StreamProfile stream = StreamProfile.get(streamMenu.getSelectedRecord());

        if (stream != null) {
            Offline.put(getLastSelectionId(digitalObject, source), stream.getId());

            StringBuilder sb = new StringBuilder();
            sb.append(DigitalObjectResourceApi.DIGITALOBJECT_PID).append('=')
                    .append(digitalObject.getPid())
                    .append('&').append(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM)
                    .append('=').append(stream.getId());
            String batchId = digitalObject.getBatchId();
            if (batchId != null) {
                sb.append('&').append(DigitalObjectResourceApi.BATCHID_PARAM).append('=').append(batchId);
            }
            if (REFRESH != null) {
                sb.append('&').append(REFRESH);
            }
            imgParams = sb.toString();
            String previewUrl = buildResourceUrl(RestConfig.URL_DIGOBJECT_DISSEMINATION, imgParams);
            if (stream.getMime().equals("image/tiff") || stream.getMime().equals("image/jp2")) {
                doPreview.show(previewUrl, "image/jpeg");
            } else {
                doPreview.show(previewUrl, stream.getMime());
            }
        } else {
            doPreview.show(null);
        }
    }

    private String getObjectTitle() {
        String label = digitalObject.getRecord().getAttribute(DigitalObjectResourceApi.MEMBERS_ITEM_LABEL);
        MetaModelRecord model = digitalObject.getModel();
        String modelName = null;
        if (model != null) {
            modelName = model.getDisplayName();
        }
        StringBuilder sb = new StringBuilder();
        sb.append(i18n.DigitalObjectPreview_Window_Title());
        if (modelName != null) {
            sb.append(" - ").append(modelName);
        }
        if (label != null) {
            sb.append(": ").append(label);
        }
        return sb.toString();
    }

    /**
     *
     * @param datastreamUrl data stream URL
     * @param objectParams PID and batch ID as URL parameters
     * @return URL
     */
    private static String buildResourceUrl(String datastreamUrl, String objectParams) {
        String url = ClientUtils.format("%s?%s", datastreamUrl, objectParams);
        return url;
    }

    private static String getLastSelectionId(DigitalObject digitalObject, String source) {
        return MEDIA_EDITOR_LAST_SELECTION + "_" + source + "_" + digitalObject.getModelId();
    }
}
