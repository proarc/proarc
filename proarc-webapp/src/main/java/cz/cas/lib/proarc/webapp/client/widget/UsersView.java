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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.types.ListGridComponent;
import com.smartgwt.client.types.ReadOnlyDisplayAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IconButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.CancelItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.RowSpacerItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.LengthRangeValidator;
import com.smartgwt.client.widgets.form.validator.RegExpValidator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;
import java.util.logging.Logger;

/**
 * Manages list of users and their profiles.
 *
 * @author Jan Pokorsky
 */
public final class UsersView implements RefreshAction.Refreshable {

    private static final Logger LOG = Logger.getLogger(UsersView.class.getName());
    private final ClientMessages i18n;
    private final Canvas widget;
    private final ListGrid userGrid;
    private DynamicForm newEditor;
    private DynamicForm rowEditor;
    private Window window;

    public UsersView(ClientMessages i18n) {
        this.i18n = i18n;

        ToolStrip toolbar = createToolbar(i18n);
        userGrid = createUserList(toolbar);

        VLayout main = new VLayout();
        main.setWidth100();
        main.setHeight100();
        main.addMember(userGrid);
        this.widget = main;
    }

    public Canvas asWidget() {
        return widget;
    }

    public void onShow() {
        refresh();
    }

    private ToolStrip createToolbar(ClientMessages i18n) {
        ToolStrip gridEditControls = Actions.createToolStrip();

        IconButton newButton = Actions.asIconButton(new AbstractAction(
                i18n.UsersView_Button_New_Title(),
                "[SKIN]/actions/add.png",
                i18n.UsersView_Button_New_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                showNewWindow();
            }
        }, this);

        IconButton editButton = Actions.asIconButton(new AbstractAction(
                i18n.UsersView_Button_Edit_Title(),
                "[SKIN]/actions/edit.png",
                i18n.UsersView_Button_Edit_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                ListGridRecord selection = userGrid.getSelectedRecord();
                if (selection != null) {
                    userGrid.expandRecord(selection);
                }
            }
        }, this);

        gridEditControls.setMembers(Actions.asIconButton(new RefreshAction(i18n), this), newButton, editButton);
        return gridEditControls;
    }

    private ListGrid createUserList(ToolStrip gridEditControls) {
        final ListGrid grid = new ListGrid() {

            @Override
            protected Canvas getExpansionComponent(ListGridRecord record) {
                return getRowProfileEditor(record);
            }

        };
        grid.setDataSource(UserDataSource.getInstance());
        grid.setUseAllDataSourceFields(true);
        grid.setSelectionType(SelectionStyle.SINGLE);
        grid.setCanExpandRecords(true);
        grid.setCanExpandMultipleRecords(false);
        grid.setGridComponents(gridEditControls, ListGridComponent.HEADER, ListGridComponent.BODY);
        // Since SmartGWT 4.0; disable auto-save to post updates of nested forms just on the submit actions.
        grid.setAutoSaveEdits(false);
        grid.setShowClippedValuesOnHover(true);
        ListGridPersistance gridPersistence = new ListGridPersistance("UsersView.userList", grid);
        grid.setViewState(gridPersistence.getViewState());
        return grid;
    }

    private DynamicForm getNewProfileEditor() {
        if (newEditor == null) {
            newEditor = getProfileEditor(null);
        }
        newEditor.clearErrors(true);
        newEditor.editNewRecord();
        return newEditor;
    }

    private DynamicForm getRowProfileEditor(ListGridRecord record) {
        boolean isNew = record == null;
        if (rowEditor == null) {
            rowEditor = getProfileEditor(record);
        }
        rowEditor.clearErrors(true);
        if (isNew) {
            rowEditor.editNewRecord();
        } else {
            rowEditor.editRecord(record);
        }
        return rowEditor;
    }

    private DynamicForm getProfileEditor(ListGridRecord record) {
        final DynamicForm form = createUserEditor(record == null, i18n);

        FormItem cancel = form.getField("cancel");
        cancel.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                form.clearErrors(true);
                closeEditor();
            }
        });

        form.addSubmitValuesHandler(new SubmitValuesHandler() {

            @Override
            public void onSubmitValues(SubmitValuesEvent event) {
                form.saveData(new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        if (RestConfig.isStatusOk(response)) {
                            closeEditor();
                        }
                    }
                });
            }
        });
        return form;
    }

    static DynamicForm createUserEditor(final boolean isNewUser, ClientMessages i18n) {
        final DynamicForm form = new DynamicForm();
        form.setMargin(15);
        form.setDataSource(UserDataSource.getInstance());
        form.setAutoWidth();
        form.setNumCols(4);
        form.setAutoFocus(true);
        form.setBrowserSpellCheck(false);
        TextItem username = new TextItem(UserDataSource.FIELD_USERNAME);
        if (isNewUser) {
            username.setValidators(new RegExpValidator("[a-z][a-z0-9]{4,}"));
            username.setTooltip(i18n.UsersView_UserForm_Username_Hint());
        }
        username.setCanEdit(isNewUser);
        username.setEndRow(true);
        final PasswordItem password = new PasswordItem(UserDataSource.FIELD_PASSWORD);
        password.setRequired(isNewUser);
        password.setPrompt(i18n.UsersView_UserForm_Password_Hint());
        LengthRangeValidator pswdLengthValidator = new LengthRangeValidator();
        pswdLengthValidator.setMin(6);
        pswdLengthValidator.setMax(20);
        password.setValidators(pswdLengthValidator);
        TextItem surname = new TextItem(UserResourceApi.USER_SURNAME);
        TextItem forename = new TextItem(UserResourceApi.USER_FORENAME);
        forename.setStartRow(true);
        TextItem email = new TextItem(UserResourceApi.USER_EMAIL);
        email.setColSpan("*");
        email.setWidth(300);
        email.setEndRow(true);
        TextItem remoteName = new TextItem(UserResourceApi.USER_REMOTENAME);
        remoteName.setReadOnlyDisplay(ReadOnlyDisplayAppearance.STATIC);
        remoteName.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                boolean isLocal = value == null;
                password.setVisible(isLocal);
                if (form.getFocusItem() == password) {
                    form.focusInNextTabElement();
                }
                password.setCanFocus(isLocal);
                return !isLocal;
            }
        });
        TextItem remoteType = new TextItem(UserResourceApi.USER_REMOTETYPE);
        remoteType.setReadOnlyDisplay(ReadOnlyDisplayAppearance.STATIC);
        remoteType.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return value != null;
            }
        });
        TextItem home = new TextItem(UserResourceApi.USER_HOME);
        home.setColSpan("*");
        home.setWidth(300);
        home.setEndRow(true);
        form.setCanSubmit(false);
        SubmitItem submit = new SubmitItem();
        submit.setTitle(i18n.UsersView_UserForm_Submit_Title());
        submit.setEndRow(false);
        submit.setStartRow(false);
        CancelItem cancel = new CancelItem("cancel", i18n.UsersView_UserForm_Cancel_Title());
        cancel.setStartRow(false);

        form.setFields(username, password, forename, surname, email,
                remoteName, remoteType, home,
                new RowSpacerItem(), submit, cancel);

        return form;
    }

    private void showNewWindow() {
        closeEditor();
        DynamicForm form = getNewProfileEditor();
        if (window == null) {
            window = new Window();
            window.setAutoSize(true);
            window.setAutoCenter(true);
            window.setDismissOnEscape(true);
            window.setShowMaximizeButton(false);
            window.setShowMinimizeButton(false);
            window.setIsModal(true);
            window.setModalMaskOpacity(10);
            window.setShowModalMask(true);
            window.setTitle(i18n.UsersView_NewUser_Window_Title());

            window.addItem(form);
        }

        window.show();
        form.focusInItem(0);
    }

    private void closeEditor() {
        ListGridRecord selected = userGrid.getSelectedRecord();
        if (selected != null) {
            userGrid.collapseRecord(selected);
        }
        if (window != null && window.isVisible()) {
            window.hide();
        }
    }

    @Override
    public void refresh() {
        userGrid.invalidateCache();
        userGrid.fetchData(null, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response) && response.getData().length > 0) {
                    userGrid.focus();
                    userGrid.selectSingleRecord(0);
                }
            }
        });
    }
}
