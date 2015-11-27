/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget.workflow;

import com.google.gwt.user.client.ui.Widget;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.ReadOnlyDisplayAppearance;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemValueFormatter;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.DataArrivedEvent;
import com.smartgwt.client.widgets.form.fields.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.HoverCustomizer;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowProfileDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.WorkflowNewJobEditor;
import cz.cas.lib.proarc.webapp.client.widget.CatalogBrowser;
import cz.cas.lib.proarc.webapp.shared.rest.WorkflowResourceApi;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowNewJobView {

    private final ClientMessages i18n;
    private DynamicForm optionForm;
    private CatalogBrowser catalogBrowser;
    private final Canvas widget;
    private WorkflowNewJobEditor handler;

    public WorkflowNewJobView(ClientMessages i18n) {
        this.i18n = i18n;
        this.widget = createMainLayout();
    }

    public Canvas getWidget() {
        return widget;
    }

    public DynamicForm getOptions() {
        return optionForm;
    }

    public void init() {
        optionForm.clearValues();
        optionForm.getField(WorkflowResourceApi.NEWJOB_METADATA).setVisible(false);
        catalogBrowser.bind();
    }

    public void setHandler(WorkflowNewJobEditor handler) {
        this.handler = handler;
    }

    private Canvas createMainLayout() {
        VLayout main = new VLayout();
        main.addMember(createPanelLabel());
        main.addMember(createToolbar());
        main.addMember(createOptionForm());
        main.addMember(createCatalogBrowser());
        return main;
    }

    private Label createPanelLabel() {
        Label lblHeader = new Label();
        String title = ClientUtils.format("<b>%s</b>", "Nový záměr");//i18n.DigitalObjectManager_Title());
        lblHeader.setContents(title);
        lblHeader.setAutoHeight();
        lblHeader.setPadding(4);
        lblHeader.setStyleName(Editor.CSS_PANEL_DESCRIPTION_TITLE);
        return lblHeader;
    }

    private ToolStrip createToolbar() {
        Action actionNew = new AbstractAction(
                "Vytvořit nový záměr",
//                i18n.DigitalObjectCreator_FinishedStep_CreateNewObjectButton_Title(),
                "[SKIN]/actions/save.png",
                null) {

                    @Override
                    public void performAction(ActionEvent event) {
                        fireOnCreateNew();
                    }
                };
        ToolStrip toolbar = Actions.createToolStrip();
        toolbar.addMember(Actions.asIconButton(actionNew, this));
        return toolbar;
    }

    private void fireOnCreateNew() {
        if (handler != null) {
            String mods = catalogBrowser.getMods();
            optionForm.setValue(WorkflowResourceApi.NEWJOB_METADATA, mods);
            optionForm.getField(WorkflowResourceApi.NEWJOB_METADATA).setVisible(mods == null);
            optionForm.setValue(WorkflowResourceApi.NEWJOB_CATALOGID, catalogBrowser.getCatalogId());
            boolean valid = optionForm.validate();
            if (valid) {
                handler.onCreateNew(optionForm.getValuesAsRecord());
            }
        }
    }

    private Widget createOptionForm() {
        final SelectItem profile = createProfileSelector();

        TextItem metadata = new TextItem(WorkflowResourceApi.NEWJOB_METADATA, "Metadata");
        metadata.setRequired(true);
        metadata.setShowTitle(false);
        metadata.setCanEdit(false);
        metadata.setStartRow(true);
        metadata.setReadOnlyDisplay(ReadOnlyDisplayAppearance.STATIC);
        metadata.setRequiredMessage("Nejsou vybrána žádná metadata!");
        // show empty value instead of XML
        metadata.setEditorValueFormatter(new FormItemValueFormatter() {

            @Override
            public String formatValue(Object value, Record record, DynamicForm form, FormItem item) {
                return "";
            }
        });

//        HiddenItem catalog = new HiddenItem(WorkflowResourceApi.NEWJOB_CATALOGID);
//        catalog.setRequired(true);
//        catalog.setRequiredMessage("Nejsou vybrána žádná metadata!");

        optionForm = new DynamicForm();
        optionForm.setAutoWidth();
        optionForm.setWrapItemTitles(false);
        optionForm.setTitleOrientation(TitleOrientation.TOP);
        optionForm.setItems(profile, metadata);
        // XXX HiddenValidationErrorsHandler does not work in smartgwt 4.0
//        optionForm.addHiddenValidationErrorsHandler(new HiddenValidationErrorsHandler() {
//
//            @Override
//            public void onHiddenValidationErrors(HiddenValidationErrorsEvent event) {
//                event.cancel();
//                String[] profileErrors = optionForm.getFieldErrors(profile.getName());
//                ArrayList<String> result = new ArrayList<String>();
//                result.addAll(Arrays.asList(profileErrors));
//                Map<?,?> errors = event.getErrors();
//                for (Entry<? extends Object, ? extends Object> entrySet : errors.entrySet()) {
//                    StringBuilder sb = new StringBuilder();
//                    Object key = entrySet.getKey();
//                    Object value = entrySet.getValue();
//                    result.add(String.valueOf(key) + " - " + String.valueOf(value));
//                }
//                System.out.println("## onHiddenValidationErrors. origErr: " + profileErrors +
//                        "\n event.err: " + errors + "\n result: " + result);
//                optionForm.setFieldErrors(profile.getName(), result.toArray(new String[result.size()]), true);
//            }
//        });
        return optionForm;
    }

    private SelectItem createProfileSelector() {
        final SelectItem profile = new SelectItem(WorkflowResourceApi.NEWJOB_PROFILE, "Vybrat profil");
        profile.setOptionDataSource(WorkflowProfileDataSource.getInstance());
        profile.setValueField(WorkflowProfileDataSource.FIELD_ID);
        profile.setDisplayField(WorkflowProfileDataSource.FIELD_LABEL);
        profile.setOptionCriteria(new Criteria(WorkflowProfileDataSource.FIELD_DISABLED, Boolean.FALSE.toString()));
        profile.setFilterLocally(true);
        profile.setRequired(true);
        profile.setWidth(300);
        profile.setAllowEmptyValue(true);
        ListGrid profilePickListProperties = new ListGrid();
        profilePickListProperties.setCanHover(true);
        profilePickListProperties.setShowHover(true);
        profilePickListProperties.setHoverWidth(300);
        profilePickListProperties.setHoverCustomizer(new HoverCustomizer() {

            @Override
            public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                String hint = record.getAttribute(WorkflowProfileDataSource.FIELD_HINT);
                return hint;
            }
        });
        profile.setPickListProperties(profilePickListProperties);
        profile.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                if (event.getStartRow() == 0) {
                    ResultSet data = event.getData();
                    int length = data.getLength();
                    if (length == 1) {
                        Record r = data.get(0);
                        String profileId = r.getAttribute(WorkflowProfileDataSource.FIELD_ID);
                        profile.setValue(profileId);
                        profile.setDefaultValue(profileId);
                    }
                }
            }
        });
        return profile;
    }

    private Widget createCatalogBrowser() {
        catalogBrowser = new CatalogBrowser(i18n);
        return catalogBrowser.getUI();
    }

}
