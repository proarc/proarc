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
package cz.incad.pas.editor.client;

import com.google.gwt.core.client.GWT;
import com.google.gwt.safehtml.shared.SafeHtmlUtils;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.rpc.HandleErrorCallback;
import com.smartgwt.client.rpc.HandleTransportErrorCallback;
import com.smartgwt.client.rpc.RPCManager;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Global Smart GWT RPC error handler.
 *
 * <p>Replaces default {@link HandleTransportErrorCallback} and {@link HandleErrorCallback}
 * to provide more detailed notifications of RPC errors and to make available
 * {@link #getTransportError() HTTP response text} in case of HTTP error.
 *
 * <p>Each {@link DataSource} can override global handler with custom one with {@link DataSource#addHandleErrorHandler }.
 * Then it is still necessary to cancel the {@link com.smartgwt.client.data.events.ErrorEvent event}.
 *
 * @author Jan Pokorsky
 */
public final class ErrorHandler {

    private TransportError transportError;

    /**
     * Gets last transport error or {@code null}.
     */
    public TransportError getTransportError() {
        return transportError;
    }

    /**
     * Registers custom error handler.
     */
    void initTransportErrorHandler() {
        RPCManager.setHandleTransportErrorCallback(new HandleTransportErrorCallback() {

            @Override
            public void handleTransportError(int transactionNum, int status, int httpResponseCode, String httpResponseText) {
                transportError = new TransportError(transactionNum, status, httpResponseCode, httpResponseText);
            }
        });

        RPCManager.setHandleErrorCallback(new HandleErrorCallback() {

            @Override
            public void handleError(DSResponse response, DSRequest request) {
                TransportError te = transportError;
                String requestDump = ClientUtils.dump(request.getData());
                // message from original error handler; contains URL
                Object smartGwtMsg = response.getAttributeAsObject("data");
                if (te != null && response.getTransactionNum() == te.getTransactionNum()) {
//                    boolean clientError = te.getHttpResponseCode() >= 400 && te.getHttpResponseCode() < 500;
                    String clientMsg = getClientMessage(te, response, request);
                    String debugInfo = ClientUtils.format("%s\nStatus: %s\nQuery: %s",
                            smartGwtMsg,
                            te.getStatus(),
                            requestDump
                            );
                    String htmlDebugInfo = ClientUtils.format("%s<br/>Status: %s<br/>Query: %s",
                            smartGwtMsg,
                            te.getStatus(),
                            SafeHtmlUtils.htmlEscape(requestDump)
                            );
                    warn(clientMsg, te.getHttpResponseText(), htmlDebugInfo);
                    SC.logWarn(debugInfo);
                } else {
                    String debugInfo = ClientUtils.format("Invalid transaction numbers %s != %s"
                            +"\n%s\nStatus: %s\nQuery: %s",
                            response.getTransactionNum(),
                            te == null ? null : te.getTransactionNum(),
                            smartGwtMsg,
                            te.getStatus(),
                            requestDump
                            );
                    throw new IllegalStateException(debugInfo);
                }

            }
        });
    }

    /**
     * Gets simple message from error response for user.
     */
    private String getClientMessage(TransportError te, DSResponse response, DSRequest request) {
        String contentType = String.valueOf(response.getHttpHeaders().get("Content-Type"));
        String result = null;
        if (contentType.contains("text/plain")) {
            result = te.getHttpResponseText();
        }
        if (result == null || result.isEmpty() || result.length() > 1000) {
            // message from original error handler; contains URL
            return String.valueOf(response.getAttributeAsObject("data"));
        } else {
            return result;
        }
    }

    /**
     * Notifies user about error.
     * @param msg simple client message
     * @param detailMsg detail response message; can be HTML
     * @param debugInfo request details, URL, ...
     */
    private void warn(String msg, String detailMsg, String debugInfo) {
        SmartGwtMessages sgi18n = GWT.create(SmartGwtMessages.class);
        boolean allowDetail = !msg.equals(detailMsg);
        final Dialog d = new Dialog();
        d.setTitle(sgi18n.dialog_WarnTitle());
        d.setIsModal(true);
        d.setAutoSize(Boolean.FALSE);
        d.setMessage(msg);
        d.setIcon("[SKIN]warn.png");
        d.setCanDragResize(true);
        d.setCanDragReposition(Boolean.TRUE);
        d.setKeepInParentRect(Boolean.TRUE);
//        d.setShowMaximizeButton(Boolean.TRUE);
        d.setMinMemberSize(50);
        Button details = new Button("Details");
        details.setVisible(allowDetail);
        d.setButtons(Dialog.OK, details);
        if (allowDetail) {
            Canvas errorPane = new Canvas();
            errorPane.setOverflow(Overflow.AUTO);
            errorPane.setWidth100();
            errorPane.setHeight100();
            errorPane.setContents(detailMsg);
            Canvas debugInfoPane = new Canvas();
            debugInfoPane.setWidth100();
            debugInfoPane.setAutoHeight();
            debugInfoPane.setContents(debugInfo);
            final VLayout detailPane = new VLayout(4);
            detailPane.setLayoutMargin(4);
            detailPane.setGroupTitle("Details");
            detailPane.setIsGroup(true);
            detailPane.setVisible(false);
            detailPane.addMember(errorPane);
            detailPane.addMember(debugInfoPane);
            detailPane.setWidth100();
            detailPane.setHeight100();
            d.addItem(detailPane);
            details.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
                @Override
                public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                    if (detailPane.isVisible()) {
                        d.restore();
                    } else {
                        d.maximize();
                    }
                    detailPane.setVisible(!detailPane.isVisible());
                }
            });
        }
        d.show();
    }

    /**
     * Holds parameters from last {@link HandleTransportErrorCallback#handleTransportError
     * transport error} that are not accessible from {@link DSResponse}.
     * Particularly {@code httpResponseText}.
     */
    public static final class TransportError {
        
        private int status;
        private int httpResponseCode;
        private int transactionNum;
        private String httpResponseText;

        public TransportError(int transactionNum, int status, int httpResponseCode, String httpResponseText) {
            this.transactionNum = transactionNum;
            this.status = status;
            this.httpResponseCode = httpResponseCode;
            this.httpResponseText = httpResponseText;
        }

        public int getStatus() {
            return status;
        }

        public int getHttpResponseCode() {
            return httpResponseCode;
        }

        public int getTransactionNum() {
            return transactionNum;
        }

        public String getHttpResponseText() {
            return httpResponseText;
        }

        @Override
        public String toString() {
            return "TransportError{" + "status=" + status
                    + ", httpResponseCode=" + httpResponseCode
                    + ", transactionNum=" + transactionNum
                    + ", httpResponseText=" + httpResponseText
                    + '}';
        }

    }

}
