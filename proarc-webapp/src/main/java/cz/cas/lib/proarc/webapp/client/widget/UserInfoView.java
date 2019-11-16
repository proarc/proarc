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

import com.google.gwt.user.client.Window;
import com.smartgwt.client.core.Function;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.rpc.RPCCallback;
import com.smartgwt.client.rpc.RPCManager;
import com.smartgwt.client.rpc.RPCRequest;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.ClickMaskMode;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;

/**
 * Shows details about the active user.
 *
 * @author Jan Pokorsky
 */
public class UserInfoView {

    private final Canvas widget;
    private final DynamicForm form;

    public UserInfoView(ClientMessages i18n) {
        VLayout vLayout = new VLayout();
        this.widget = vLayout;
        widget.setStyleName("pickListMenuBody");
        widget.setShowShadow(true);
        form = UsersView.createUserEditor(false, false, i18n);
        form.addSubmitValuesHandler(new SubmitValuesHandler() {

            @Override
            public void onSubmitValues(SubmitValuesEvent event) {
                form.saveData(new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        if (RestConfig.isStatusOk(response)) {
                            hide();
                        }
                    }
                });
            }
        });
        FormItem cancel = form.getField("cancel");
        cancel.setTitle(i18n.UserInfoView_UserForm_Logout_Title());
        cancel.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                logout();
            }
        });
        vLayout.addMember(form);
    }

    public void show(Canvas otherWidget, String side, boolean canOcclude) {
        form.clearErrors(true);
        // canOcclude not fully supported in SmartGWT 4.0; introduce by patch 4.0-p; do not use yet
//        widget.showNextTo(otherWidget, side, canOcclude);
        widget.showNextTo(otherWidget, side);
        widget.showClickMask(new Function() {

            @Override
            public void execute() {
                hide();
            }
        }, ClickMaskMode.SOFT, new Canvas[] {widget});
        form.focus();
        form.focusInItem(0);
        form.fetchData(new Criteria(UserDataSource.FIELD_WHOAMI, "true"));
    }

    private void hide() {
        widget.hide();
    }

    private void logout() {
        RPCRequest request = new RPCRequest();
        request.setActionURL(RestConfig.URL_LOGIN_SERVLET);
        request.setUseSimpleHttp(true);
        request.setHttpMethod("DELETE");
        request.setShowPrompt(true);
        RPCManager.sendRequest(request, new RPCCallback() {

            @Override
            public void execute(RPCResponse response, Object rawData, RPCRequest request) {
                Window.Location.reload();
            }
        });
    }

}
