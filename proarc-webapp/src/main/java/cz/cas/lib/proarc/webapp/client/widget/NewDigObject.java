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

import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import com.smartgwt.client.widgets.form.validator.RegExpValidator;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Widget to select type of a newly created digital object.
 *
 * @author Jan Pokorsky
 */
public final class NewDigObject extends VLayout {

    private static final Logger LOG = Logger.getLogger(NewDigObject.class.getName());

    private final SectionStack sections;
    private final DynamicForm optionsForm;
    private final CatalogBrowser catalogBrowser;
    private final ClientMessages i18n;
    private Handler handler;

    public NewDigObject(ClientMessages i18n) {
        this.i18n = i18n;
        this.catalogBrowser = new CatalogBrowser(i18n);
        setHeight100();
        setWidth100();

        ToolStrip toolbar = createToolbar();

        optionsForm = createOptionsForm();

        SectionStackSection sectionMain = new SectionStackSection(
                i18n.NewDigObject_SectionOptions_Title());
        sectionMain.setExpanded(true);
        sectionMain.setCanCollapse(false);
        sectionMain.setItems(optionsForm);

        SectionStackSection sectionAdvanced = new SectionStackSection(
                i18n.NewDigObject_SectionAdvancedOptions_Title());
        sectionAdvanced.setItems(catalogBrowser.getUI());

        sections = new SectionStack();
        sections.setVisibilityMode(VisibilityMode.MULTIPLE);
        sections.setSections(sectionMain, sectionAdvanced);

        setMembers(toolbar, sections);
    }

    public void bind(String model, AdvancedCriteria criteria) {
        optionsForm.clearErrors(true);
        optionsForm.editNewRecord();
        if (model != null) {
            optionsForm.setValue(DigitalObjectDataSource.FIELD_MODEL, model);
        }
        catalogBrowser.bind();
        if (criteria == null) {
//            sections.collapseSection(1);
            sections.expandSection(1);
        } else {
            sections.expandSection(1);
        }

    }

    public void setHandler(Handler handler) {
        this.handler = handler;
    }

    public MetaModelRecord getModel() {
        FormItem field = optionsForm.getField(DigitalObjectDataSource.FIELD_MODEL);
        ListGridRecord selectedRecord = field.getSelectedRecord();
//        Map<?, ?> values = selectedRecord.toMap();
//        ClientUtils.info(LOG, "getModel: %s", values);
        return MetaModelRecord.get(selectedRecord);
    }

    public String getMods() {
        return catalogBrowser.getMods();
    }

    public String getNewPid() {
        String newPid = optionsForm.getValueAsString(DigitalObjectDataSource.FIELD_PID);
        return newPid;
    }

    public boolean validate() {
        return optionsForm.validate();
    }

    public void setValidationErrors(Map<?,?> errors) {
        optionsForm.setErrors(errors, true);
    }

    private ToolStrip createToolbar() {
        Action actionNewObject = new AbstractAction(
                i18n.DigitalObjectCreator_FinishedStep_CreateNewObjectButton_Title(),
                "[SKIN]/actions/save.png",
                null) {

            @Override
            public void performAction(ActionEvent event) {
                if (handler != null) {
                    handler.onCreateObject();
                }
            }
        };
        ToolStrip t = Actions.createToolStrip();
        t.addMember(Actions.asIconButton(actionNewObject, this));
        return t;
    }

    private DynamicForm createOptionsForm() {
        SelectItem selectModel = new SelectItem(DigitalObjectDataSource.FIELD_MODEL,
                i18n.NewDigObject_OptionModel_Title());
        selectModel.setRequired(true);
        selectModel.setWidth(300);
        // issue 46: always start with empty model
        selectModel.setAllowEmptyValue(true);
        selectModel.setEmptyDisplayValue(ClientUtils.format("<i>&lt;%s&gt;</i>", i18n.NewDigObject_OptionModel_EmptyValue_Title()));
        selectModel.setOptionDataSource(MetaModelDataSource.getInstance());
//        selectModel.setShowOptionsFromDataSource(true);
        selectModel.setValueField(MetaModelDataSource.FIELD_PID);
        selectModel.setDisplayField(MetaModelDataSource.FIELD_DISPLAY_NAME);
        selectModel.setAutoFetchData(true);
        selectModel.setValidators(new CustomValidator() {

            @Override
            protected boolean condition(Object value) {
                boolean valid = getFormItem().getSelectedRecord() != null;
                return valid;
            }
        });

        TextItem newPid = new TextItem(DigitalObjectDataSource.FIELD_PID);
        newPid.setTitle(i18n.NewDigObject_OptionPid_Title());
        newPid.setTooltip(i18n.NewDigObject_OptionPid_Hint());
        newPid.setLength(36 + 5);
        newPid.setWidth((36 + 5) * 8);
        newPid.setValidators(new RegExpValidator(
                "uuid:[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{12}"));
        DynamicForm form = new DynamicForm();
        form.setWrapItemTitles(false);
        form.setAutoFocus(true);
        form.setNumCols(4);
        form.setBrowserSpellCheck(false);
        form.setFields(selectModel, newPid);
        form.setAutoWidth();
        form.setMargin(4);
        return form;
    }

    public interface Handler {
        void onCreateObject();
    }

}
