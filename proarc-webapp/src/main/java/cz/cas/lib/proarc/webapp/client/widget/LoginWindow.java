/*
 * Copyright (C) 2014 Jan Pokorsky
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

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.rpc.LoginRequiredCallback;
import com.smartgwt.client.rpc.RPCCallback;
import com.smartgwt.client.rpc.RPCManager;
import com.smartgwt.client.rpc.RPCRequest;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ReadOnlyDisplayAppearance;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import java.util.HashMap;
import java.util.Map;

/**
 * The login window to get user credentials and authenticate the user.
 * It registers own relogin callback to {@link RPCManager}.
 *
 * @author Jan Pokorsky
 */
public final class LoginWindow {

    private static LoginWindow INSTANCE;
    private static final String USERNAME = "j_username";
    private static final String PASSWORD = "j_password";
    private static final String PRODUCER_CODE = "j_code";
    private static final String ERROR = "error";
    private final DynamicForm form;
    private final ClientMessages i18n;
    private final SmartGwtMessages i18nSgwt;
    private Window window;

    public static LoginWindow login() {
        if (INSTANCE == null) {
            INSTANCE = new LoginWindow();
        }
        return INSTANCE;
    }

    private LoginWindow() {
        i18n = GWT.create(ClientMessages.class);
        i18nSgwt = ClientUtils.createSmartGwtMessages();
        form = createForm();
        reloginCallback();
    }

    private void showWindow() {
        if (window == null) {
            VLayout container = new VLayout();
            container.setMembers(form, createButtons());
            container.setMargin(15);
            window = new Window();
            window.setAutoCenter(true);
            window.setAutoSize(true);
            window.setIsModal(true);
            window.addItem(container);
            window.setTitle(i18nSgwt.dialog_LoginTitle());
            window.setShowCloseButton(false);
            window.setShowMinimizeButton(false);
            window.setKeepInParentRect(true);
            window.setShowModalMask(true);
            window.setCanDragReposition(false);
            form.focusInItem(USERNAME);
        }
        window.show();
        form.clearErrors(true);
        form.focus();
        if (form.getValueAsString(USERNAME) != null) {
            // relogin
            form.getField(USERNAME).setCanEdit(false);
            form.getField(USERNAME).setCanFocus(false);
            form.getField(PRODUCER_CODE).setCanEdit(false);
            form.getField(PRODUCER_CODE).setCanFocus(false);
            form.clearValue(PASSWORD);
            form.focusInItem(PASSWORD);
        }
    }

    private DynamicForm createForm() {
        DynamicForm f = new DynamicForm();
        f.setWidth(400);
        f.setBrowserSpellCheck(false);
        f.setNumCols(1);
        f.setTitleOrientation(TitleOrientation.TOP);
        f.setSaveOnEnter(true);

        TextItem user = new TextItem(USERNAME, i18nSgwt.dialog_UserNameTitle());
        user.setRequired(true);
        user.setWidth("*");
        user.setReadOnlyDisplay(ReadOnlyDisplayAppearance.STATIC);

        PasswordItem passwd = new PasswordItem(PASSWORD, i18nSgwt.dialog_PasswordTitle());
        passwd.setRequired(true);
        passwd.setWidth("*");

        TextItem producerCode = new TextItem(PRODUCER_CODE, i18n.LoginWindow_ProducerCode());
        producerCode.setWidth("*");
        producerCode.setReadOnlyDisplay(ReadOnlyDisplayAppearance.STATIC);

        StaticTextItem error = new StaticTextItem(ERROR);
        error.setShowTitle(false);
        error.setShowErrorText(true);

        f.setItems(user, producerCode, passwd, error);
        f.addSubmitValuesHandler(new SubmitValuesHandler() {

            @Override
            public void onSubmitValues(SubmitValuesEvent event) {
                submitCredentials();
            }
        });
        return f;
    }

    private void finish() {
        window.hide();
    }

    private Canvas createButtons() {
        IButton btnSubmit = new IButton(i18nSgwt.dialog_LoginButtonTitle(), new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                submitCredentials();
            }
        });

        HStack btnLayout = new HStack(5);
        btnLayout.setAutoHeight();
        btnLayout.setLayoutTopMargin(20);
        btnLayout.setLayoutAlign(Alignment.CENTER);
        btnLayout.setMembers(btnSubmit);
        return btnLayout;
    }

    private Map<?,?> getCredentials() {
        Map<?,?> values = form.getValues();
        values.remove(ERROR);
        if (values.get(PRODUCER_CODE) == null) {
            values.remove(PRODUCER_CODE);
        }
        return values;
    }

    private void showErrors() {
        i18nSgwt.dialog_LoginErrorMessage();
        HashMap<String, String> errors = new HashMap<String, String>();
        errors.put(ERROR, i18nSgwt.dialog_LoginErrorMessage());
        form.setErrors(errors, true);
    }

    private void submitCredentials() {
        RPCRequest request = new RPCRequest();
        request.setContainsCredentials(true);
        request.setActionURL(RestConfig.URL_LOGIN_SERVLET);
        request.setUseSimpleHttp(true);
        request.setShowPrompt(false);
        request.setParams(getCredentials());
        RPCManager.sendRequest(request, new RPCCallback() {

            @Override
            public void execute(RPCResponse response, Object rawData, RPCRequest request) {
                response.getHttpResponseCode();
                if (response.getHttpResponseCode() == 200) {
                    finish();
                    RPCManager.resendTransaction();
                } else {
                    showErrors();
                }
            }
        });
    }
    /**
     * Registers relogin callback.
     *
     * @see <a href="http://www.smartclient.com/smartgwt/javadoc/com/smartgwt/client/docs/Relogin.html">SmartGWT Relogin</a>
     */
    private void reloginCallback() {
        RPCManager.setLoginRequiredCallback(new LoginRequiredCallback() {

                @Override
                public void loginRequired(int transactionNum,
                        com.smartgwt.client.rpc.RPCRequest rpcRequest,
                        RPCResponse rpcResponse) {

                    showWindow();
                }
        });
    }
}
