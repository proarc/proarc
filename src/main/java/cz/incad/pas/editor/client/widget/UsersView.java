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
package cz.incad.pas.editor.client.widget;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.types.ListGridComponent;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.CancelItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.RowSpacerItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.LengthRangeValidator;
import com.smartgwt.client.widgets.form.validator.RegExpValidator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.UserDataSource;
import java.util.logging.Logger;

/**
 * Manages list of users and their profiles.
 *
 * @author Jan Pokorsky
 */
public final class UsersView {

    private static final Logger LOG = Logger.getLogger(UsersView.class.getName());
    private final PasEditorMessages i18nPas;
    private final Canvas widget;
    private final ListGrid userGrid;
    private DynamicForm newEditor;
    private DynamicForm rowEditor;
    private Window window;

    public UsersView(PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
        userGrid = new ListGrid() {

            @Override
            protected Canvas getExpansionComponent(ListGridRecord record) {
                return getRowProfileEditor(record);
            }

        };
        userGrid.setDataSource(UserDataSource.getInstance());
        userGrid.setUseAllDataSourceFields(true);
        userGrid.setEditEvent(ListGridEditEvent.DOUBLECLICK);
        userGrid.setCanExpandRecords(true);
        userGrid.setCanExpandMultipleRecords(false);

        ToolStrip gridEditControls = new ToolStrip();
        gridEditControls.setWidth100();

        ToolStripButton newButton = new ToolStripButton();
        newButton.setIcon("[SKIN]/actions/add.png");
        newButton.setPrompt(i18nPas.UsersView_Button_New_Hint());
        newButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                showNewWindow();
            }
        });

        ToolStripButton editButton = new ToolStripButton();
        editButton.setIcon("[SKIN]/actions/edit.png");
        editButton.setPrompt(i18nPas.UsersView_Button_Edit_Hint());
        editButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                ListGridRecord selection = userGrid.getSelectedRecord();
                if (selection != null) {
                    userGrid.expandRecord(selection);
                }
            }
        });

        LayoutSpacer indentButtons = new LayoutSpacer();
        indentButtons.setWidth100();
        gridEditControls.setMembers(indentButtons, newButton, editButton);
        gridEditControls.addSpacer(2);

        userGrid.setGridComponents(ListGridComponent.HEADER, ListGridComponent.BODY, gridEditControls);

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
        userGrid.invalidateCache();
        userGrid.fetchData();
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
        final boolean isNew = record == null;
        final DynamicForm form = new DynamicForm();
        form.setMargin(15);
        form.setDataSource(userGrid.getDataSource());
        form.setAutoWidth();
        form.setNumCols(4);
        form.setAutoFocus(true);
        TextItem username = new TextItem(UserDataSource.FIELD_USERNAME);
        username.setValidators(new RegExpValidator("[a-z][a-z0-9]{4,}"));
        username.setTooltip(i18nPas.UsersView_UserForm_Username_Hint());
        PasswordItem password = new PasswordItem(UserDataSource.FIELD_PASSWORD);
        password.setRequired(isNew);
        password.setPrompt(i18nPas.UsersView_UserForm_Password_Hint());
        LengthRangeValidator pswdLengthValidator = new LengthRangeValidator();
        pswdLengthValidator.setMin(6);
        pswdLengthValidator.setMax(20);
        password.setValidators(pswdLengthValidator);
        TextItem surname = new TextItem(UserDataSource.FIELD_SURNAME);
        TextItem forename = new TextItem(UserDataSource.FIELD_FORENAME);
        TextItem email = new TextItem(UserDataSource.FIELD_EMAIL);
        email.setColSpan("*");
        email.setWidth(300);
        email.setEndRow(true);
        TextItem home = new TextItem(UserDataSource.FIELD_HOME);
        home.setColSpan("*");
        home.setWidth(300);
        home.setEndRow(true);
        form.setCanSubmit(false);
        SubmitItem submit = new SubmitItem();
        submit.setEndRow(false);
        submit.setStartRow(false);
        CancelItem cancel = new CancelItem();
        cancel.setStartRow(false);
        cancel.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                form.clearErrors(true);
                closeEditor();
            }
        });

        form.setFields(username, password, forename, surname, email, home, new RowSpacerItem(), submit, cancel);

        form.addSubmitValuesHandler(new SubmitValuesHandler() {

            @Override
            public void onSubmitValues(SubmitValuesEvent event) {
                form.saveData(new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        if (response.getStatus() == DSResponse.STATUS_SUCCESS) {
                            closeEditor();
                        }
                    }
                });
            }
        });
        
        form.getField(UserDataSource.FIELD_USERNAME).setCanEdit(isNew);
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
            window.setTitle(i18nPas.UsersView_NewUser_Window_Title());

            window.addItem(form);
        }

        window.show();
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
}
