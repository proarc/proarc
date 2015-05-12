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
package cz.cas.lib.proarc.webapp.client.widget;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.json.client.JSONBoolean;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.user.client.ui.Widget;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Encoding;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Progressbar;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.ErrorHandler;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.HashMap;
import java.util.logging.Logger;
import org.moxieapps.gwt.uploader.client.File;
import org.moxieapps.gwt.uploader.client.Uploader;
import org.moxieapps.gwt.uploader.client.Uploader.ButtonAction;
import org.moxieapps.gwt.uploader.client.Uploader.Cursor;
import org.moxieapps.gwt.uploader.client.events.FileQueueErrorEvent;
import org.moxieapps.gwt.uploader.client.events.FileQueueErrorHandler;
import org.moxieapps.gwt.uploader.client.events.FileQueuedEvent;
import org.moxieapps.gwt.uploader.client.events.FileQueuedHandler;
import org.moxieapps.gwt.uploader.client.events.UploadCompleteEvent;
import org.moxieapps.gwt.uploader.client.events.UploadCompleteHandler;
import org.moxieapps.gwt.uploader.client.events.UploadErrorEvent;
import org.moxieapps.gwt.uploader.client.events.UploadErrorHandler;
import org.moxieapps.gwt.uploader.client.events.UploadProgressEvent;
import org.moxieapps.gwt.uploader.client.events.UploadProgressHandler;
import org.moxieapps.gwt.uploader.client.events.UploadSuccessEvent;
import org.moxieapps.gwt.uploader.client.events.UploadSuccessHandler;

/**
 * The widget to browse and send a file.
 *
 * <p>It uses HTML5 based widget {@link Uploader} to send the file. The fallback is Flash.
 *
 * @author Jan Pokorsky
 */
public final class UploadFile {

    private static final String FIELD_FILE = DigitalObjectResourceApi.DISSEMINATION_FILE;
    private static final String FIELD_MIMETYPE = DigitalObjectResourceApi.DISSEMINATION_MIME;
    private static final String FIELD_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    private static final String FIELD_BATCHID = DigitalObjectResourceApi.BATCHID_PARAM;

    private static final Logger LOG = Logger.getLogger(UploadFile.class.getName());

    private final ClientMessages i18n;
    private final SmartGwtMessages i18nSgwt;
    private final DynamicForm form;
    private Window window;
    private BooleanCallback windowCallback;
    private Uploader uploader;
    private HStack btnLayout;
    private final Progressbar progressbar;
    private IButton btnOk;
    private IButton btnCancel;
    private DigitalObject dobj;

    public UploadFile(ClientMessages i18n) {
        this.i18n = i18n;
        i18nSgwt = ClientUtils.createSmartGwtMessages();
        progressbar = new Progressbar();
        form = createForm();
    }

    public void showWindow(DigitalObject dobj, BooleanCallback callback) {
        this.dobj = dobj;
        this.windowCallback = callback;
        form.setAutoHeight();

        window = new Window();
        window.setAutoSize(true);
        window.setAutoCenter(true);
        window.setIsModal(true);
        window.setTitle(i18n.DigitalObjectEditor_MediaEditor_Uploader_Title());
        window.setShowMinimizeButton(false);
        window.setShowModalMask(true);
        window.addCloseClickHandler(new CloseClickHandler() {

            @Override
            public void onCloseClick(CloseClickEvent event) {
                closeWindow();
            }
        });

        VLayout winContent = new VLayout(4);
        winContent.setWidth(400);
        winContent.setPadding(5);
        winContent.addMember(createBrowseCanvas());
        winContent.addMember(form);
        winContent.addMember(createProgressBar());
        winContent.addMember(createButtons());
        window.addItem(winContent);

        window.show();
    }

    private void uploadFile() {
        JSONObject post = new JSONObject();
        String mime = form.getValueAsString(FIELD_MIMETYPE);
        if (mime != null && !mime.trim().isEmpty()) {
            post.put(FIELD_MIMETYPE, new JSONString(mime));
        }
        post.put(FIELD_PID, new JSONString(dobj.getPid()));
        String batchId = dobj.getBatchId();
        if (batchId != null) {
            post.put(FIELD_BATCHID, new JSONString(batchId));
        }
        post.put(DigitalObjectResourceApi.DISSEMINATION_ERROR, JSONBoolean.getInstance(true));
        uploader.setPostParams(post);
        uploader.startUpload();
        showUploading(true);
    }

    private void onUploadClick() {
        form.clearErrors(true);
        progressbar.setPercentDone(0);
        boolean valid = form.validate();
        if (valid) {
            uploadFile();
        }
    }

    private void closeWindow() {
        window.hide();
        window.destroy();
        boolean uploaded = uploader.getStats().getSuccessfulUploads() > 0;
        windowCallback.execute(uploaded);
    }

    private void showUploading(boolean uploading) {
        btnLayout.setDisabled(uploading);
    }

    private DynamicForm createForm() {
        DynamicForm form = new DynamicForm();
        form.setEncoding(Encoding.MULTIPART);
        form.setBrowserSpellCheck(false);
        form.setNumCols(2);
        form.setTitleOrientation(TitleOrientation.TOP);
        form.setCanSubmit(true);

        TextItem mimeItem = new TextItem(FIELD_MIMETYPE,
                i18n.DigitalObjectEditor_MediaEditor_Uploader_Mimetype_Title());
        mimeItem.setWidth(400);
        mimeItem.setColSpan(2);

        TextItem filenameItem = new TextItem(FIELD_FILE,
                i18n.DigitalObjectEditor_MediaEditor_Uploader_Filename_Title());
        filenameItem.setWidth(400);
        filenameItem.setColSpan(2);
        filenameItem.setRequired(Boolean.TRUE);

        HiddenItem pidItem = new HiddenItem(FIELD_PID);
        HiddenItem batchIdItem = new HiddenItem(FIELD_BATCHID);
        form.setFields(filenameItem, mimeItem, pidItem, batchIdItem);
        return form;
    }

    private Canvas createBrowseCanvas() {
        UploadHandler uploadHandler = new UploadHandler();
        uploader = new Uploader() {

            @Override
            protected void onLoad() {
                super.onLoad();
                openFileDialog(uploader);
            }

        };
        uploader.setUploadURL(RestConfig.URL_DIGOBJECT_DISSEMINATION)
                .setButtonImageURL(Page.getSkinImgDir() + "MultiUploadItem/icon_add_files.png")
                .setButtonWidth(16)
                .setButtonHeight(16)
                .setButtonCursor(Cursor.HAND)
                .setButtonAction(ButtonAction.SELECT_FILE)
                .setFileSizeLimit("1 GB")
                .setFilePostName(FIELD_FILE)
                .setFileUploadLimit(0)
                .setFileQueuedHandler(uploadHandler)
                .setFileQueueErrorHandler(uploadHandler)
                .setUploadCompleteHandler(uploadHandler)
                .setUploadErrorHandler(uploadHandler)
                .setUploadProgressHandler(uploadHandler)
                .setUploadSuccessHandler(uploadHandler)
                ;
        HStack hStack = new HStack(4);
        Label label = new Label(i18n.DigitalObjectEditor_MediaEditor_Uploader_Browse_Title() + ": ");
        label.setAutoFit(true);
        label.setWrap(false);
        hStack.addMember(label);
        hStack.addMember(uploader);
        return hStack;
    }

    private void openFileDialog(Uploader uploader) {
        // workaround to call Uploader.openFileDialog
        for (int i = uploader.getWidgetCount() - 1; i >= 0; i--) {
            Widget widget = uploader.getWidget(i);
            if (widget instanceof com.google.gwt.user.client.ui.Label) {
                final com.google.gwt.user.client.ui.Label btn = (com.google.gwt.user.client.ui.Label) widget;
                btn.fireEvent(new ClickEvent() {});
                break;
            }
        }
    }

    private class UploadHandler implements UploadCompleteHandler, UploadErrorHandler,
            UploadProgressHandler, UploadSuccessHandler, FileQueuedHandler, FileQueueErrorHandler {

        @Override
        public boolean onUploadComplete(UploadCompleteEvent uploadCompleteEvent) {
            showUploading(false);
            return true;
        }

        @Override
        public boolean onUploadError(UploadErrorEvent uploadErrorEvent) {
            form.clearValues();
            showFormError(uploadErrorEvent.getMessage());
            return true;
        }

        private void showFormError(String message) {
            HashMap<String, String> errors = new HashMap<String, String>();
            errors.put(FIELD_FILE, message);
            form.setErrors(errors, true);
        }

        @Override
        public boolean onUploadProgress(UploadProgressEvent uploadProgressEvent) {
//            uploadProgressEvent.getBytesComplete();
            int percentDone = (int) uploadProgressEvent.getFile().getPercentUploaded();
            progressbar.setPercentDone(percentDone);
            return true;
        }

        @Override
        public boolean onUploadSuccess(UploadSuccessEvent uploadSuccessEvent) {
            String response = uploadSuccessEvent.getServerData();
            DSResponse dsResponse = ErrorHandler.getDsResponse(response);
            if (dsResponse.getStatus() == RPCResponse.STATUS_SUCCESS) {
                btnOk.hide();
                uploader.setButtonDisabled(true);
                btnCancel.setTitle(i18nSgwt.dialog_DoneButtonTitle());
                closeWindow();
            } else {
                // upload error
                uploader.cancelUpload(false);
                // clear the chosen file name to force user to enqueue the cancelled file again
                // as there is no uploader API to do it
                form.clearValues();
                progressbar.setPercentDone(0);
                showError(dsResponse);
            }
            return true;
        }


        private void showError(DSResponse response) {
//            ClientUtils.info(LOG, "dsResponse: %s, %s", response.getStatus(), response.getDataAsString());
            switch(response.getStatus()) {
                case RPCResponse.STATUS_SUCCESS:
                    break;
                case RPCResponse.STATUS_VALIDATION_ERROR:
                    form.setErrors(response.getErrors(), true);
                    break;
                case RPCResponse.STATUS_LOGIN_REQUIRED:
                case RPCResponse.STATUS_LOGIN_INCORRECT:
                    // ignore, it is handled as HTTP 403 (onUploadError)
                case RPCResponse.STATUS_FAILURE:
                default:
                    ErrorHandler.warn(formatStackTrace(response.getDataAsString()));
            }
        }

        private String formatStackTrace(String errMsg) {
            return errMsg == null ? "" : errMsg
                    .replaceAll("\n", "<br>")
                    .replaceAll("\t", "&nbsp;&nbsp;&nbsp;&nbsp;")
                    ;
        }

        @Override
        public boolean onFileQueued(FileQueuedEvent fileQueuedEvent) {
            form.clearErrors(true);
            File file = fileQueuedEvent.getFile();
            form.setValue(FIELD_MIMETYPE, file.getType());
            form.setValue(FIELD_FILE, file.getName());
            return true;
        }

        @Override
        public boolean onFileQueueError(FileQueueErrorEvent fileQueueErrorEvent) {
            form.clearValues();
            showFormError(fileQueueErrorEvent.getMessage());
            return true;
        }

    }

    private Canvas createProgressBar() {
        progressbar.setVertical(false);
        progressbar.setWidth100();
        progressbar.setHeight(24);
        progressbar.setBreadth(1);
        VLayout vLayout = new VLayout();
        vLayout.addMember(progressbar);
        return vLayout;
    }

    private Canvas createButtons() {
        btnOk = new IButton(i18nSgwt.dialog_OkButtonTitle(), new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                onUploadClick();
            }
        });
        btnCancel = new IButton(i18nSgwt.dialog_CancelButtonTitle(), new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                closeWindow();
            }
        });

        btnLayout = new HStack(5);
        btnLayout.setAutoHeight();
        btnLayout.setLayoutTopMargin(20);
        btnLayout.setLayoutAlign(Alignment.CENTER);
        btnLayout.setMembers(btnOk, btnCancel);
        return btnLayout;
    }

}
