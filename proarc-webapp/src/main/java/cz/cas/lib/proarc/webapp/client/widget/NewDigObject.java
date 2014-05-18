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
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import com.smartgwt.client.widgets.form.validator.RegExpValidator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
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
import cz.cas.lib.proarc.webapp.client.ds.BibliographyDataSource;
import cz.cas.lib.proarc.webapp.client.ds.BibliographyQueryDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.shared.rest.BibliographicCatalogResourceApi;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Widget to select type of a newly created digital object.
 *
 * @author Jan Pokorsky
 */
public final class NewDigObject extends VLayout {

    private static final Logger LOG = Logger.getLogger(NewDigObject.class.getName());

    /**
     * Titles of common field types. Used when the server config does not declare
     * any field title. It ensures backward compatibility.
     */
    private static final HashMap<String, String> FIELD_TYPE_TITLES = new HashMap<String, String>();

    private final SectionStack sections;
    private final DynamicForm optionsForm;
    private DynamicForm formCatalog;
    private final ClientMessages i18n;
    private ListGrid lgResult;
    private Handler handler;

    public NewDigObject(ClientMessages i18n) {
        this.i18n = i18n;
        initFields(i18n);
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
        sectionAdvanced.setItems(createAdvancedOptions());

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
        formCatalog.clearErrors(true);
        fixExpandedListGrid();
        lgResult.setData(new Record[0]);
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
        ListGridRecord r = lgResult.getSelectedRecord();
        String mods = (r == null) ? null : r.getAttribute(BibliographyQueryDataSource.FIELD_MODS);
        ClientUtils.info(LOG, "getMods: %s", mods);
        return mods;
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

    private Canvas createAdvancedOptions() {
        formCatalog = createCatalogForm();

        lgResult = new ListGrid();
        lgResult.setDataSource(BibliographyQueryDataSource.getInstance());
//        lgResult.setUseAllDataSourceFields(true);
        ListGridField preview = new ListGridField(BibliographyQueryDataSource.FIELD_PREVIEW,
                i18n.NewDigObject_CatalogHeaderPreview_Title());
        ListGridField title = new ListGridField(BibliographyQueryDataSource.FIELD_TITLE,
                i18n.NewDigObject_CatalogHeaderTitle_Title());
        lgResult.setDetailField(BibliographyQueryDataSource.FIELD_PREVIEW);
        lgResult.setFields(title, preview);
//        lgResult.setAutoFetchData(true);
        lgResult.setHeight100();
        lgResult.setWidth100();
        lgResult.setCanExpandRecords(true);
        lgResult.setCanExpandMultipleRecords(false);
        lgResult.setExpansionMode(ExpansionMode.DETAIL_FIELD);
        lgResult.setSelectionType(SelectionStyle.SINGLE);
//        lgResult.setSelectionAppearance(SelectionAppearance.CHECKBOX);
        lgResult.setAlternateRecordStyles(true);
        lgResult.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                if (event.getStartRow() == 0 && event.getEndRow() > 0) {
                    lgResult.focus();
                    lgResult.selectSingleRecord(0);
                }
            }
        });

        VLayout layout = new VLayout();
        layout.setMembers(formCatalog, lgResult);
        layout.setMargin(4);
        layout.setMembersMargin(4);
        return layout;
    }

    private void queryCatalog() {
        if (formCatalog.validate()) {
            Criteria plain = formCatalog.getValuesAsCriteria();
            lgResult.invalidateCache();
            lgResult.fetchData(plain);
        }
    }

    private DynamicForm createCatalogForm() {
        SelectItem selection = new SelectItem(BibliographicCatalogResourceApi.FIND_CATALOG_PARAM,
                i18n.NewDigObject_OptionCatalog_Title());
        selection.setRequired(true);
        selection.setOptionDataSource(BibliographyDataSource.getInstance());
//        selectModel.setShowOptionsFromDataSource(true);
        selection.setValueField(BibliographicCatalogResourceApi.CATALOG_ID);
        selection.setDisplayField(BibliographicCatalogResourceApi.CATALOG_NAME);
        selection.setAutoFetchData(true);
        selection.setDefaultToFirstOption(true);
        selection.setWidth(250);

        SelectItem selectField = new SelectItem(BibliographicCatalogResourceApi.FIND_FIELDNAME_PARAM, "Pole");
        selectField.setRequired(true);
        selectField.setAllowEmptyValue(false);
        selectField.setDefaultToFirstOption(true);
        CatalogChangedHandler catalogHandler = new CatalogChangedHandler(selection, selectField);
        selection.addDataArrivedHandler(catalogHandler);
        selection.addChangedHandler(catalogHandler);

        TextItem value = new TextItem(BibliographicCatalogResourceApi.FIND_VALUE_PARAM, "Dotaz");
        value.setWidth(300);
        value.setRequired(true);

        ButtonItem findBtn = new ButtonItem("findBtn", i18n.NewDigObject_CatalogFind_Title());
        findBtn.setStartRow(false);
        findBtn.setVAlign(VerticalAlignment.BOTTOM);
        findBtn.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                queryCatalog();
            }
        });

        DynamicForm form = new DynamicForm();
        form.setFields(selection, selectField, value, findBtn);
        form.setBrowserSpellCheck(false);
        form.setAutoWidth();
        form.setWrapItemTitles(false);
        form.setTitleOrientation(TitleOrientation.TOP);
        form.setNumCols(4);
        form.setSaveOnEnter(true);
        form.addSubmitValuesHandler(new SubmitValuesHandler() {

            @Override
            public void onSubmitValues(SubmitValuesEvent event) {
                queryCatalog();
            }
        });
        return form;
    }

    /**
     * Switching panels containing ListGrid with an expanded record results
     * to NPE in GWT javascript code.
     * Steps to reproduce:
     * 1. New Object
     * 2. find catalog records
     * 3. expand some record
     * 4. select whatever item in main menu
     * 5. select New Object again
     * 6. panel content not shown
     * The workaround is to collapse the selected record before rendering.
     */
    private void fixExpandedListGrid() {
        ListGridRecord selectedResult = lgResult.getSelectedRecord();
        if (selectedResult != null) {
            lgResult.collapseRecord(selectedResult);
        }
    }

    private static void initFields(ClientMessages i18n) {
        if (FIELD_TYPE_TITLES.isEmpty()) {
            FIELD_TYPE_TITLES.put("issn", i18n.NewDigObject_CatalogFieldIssn_Title());
            FIELD_TYPE_TITLES.put("isbn", i18n.NewDigObject_CatalogFieldIsbn_Title());
            FIELD_TYPE_TITLES.put("ccnb", i18n.NewDigObject_CatalogFieldCcnb_Title());
            FIELD_TYPE_TITLES.put("barcode", i18n.NewDigObject_CatalogFieldBarcode_Title());
            FIELD_TYPE_TITLES.put("signature", i18n.NewDigObject_CatalogFieldSignature_Title());
            FIELD_TYPE_TITLES.put("title", i18n.NewDigObject_CatalogHeaderTitle_Title());
        }
    }

    public interface Handler {
        void onCreateObject();
    }

    private static class CatalogChangedHandler implements ChangedHandler,
            com.smartgwt.client.widgets.form.fields.events.DataArrivedHandler {

        private final SelectItem selectCatalog;
        private final SelectItem selectField;

        public CatalogChangedHandler(SelectItem selection, SelectItem selectField) {
            this.selectCatalog = selection;
            this.selectField = selectField;
        }

        @Override
        public void onChanged(ChangedEvent event) {
            ListGridRecord r = selectCatalog.getSelectedRecord();
            String lastFieldSelection = selectField.getValueAsString();
            Record[] fields = r.getAttributeAsRecordArray(BibliographicCatalogResourceApi.CATALOG_FIELDS);
            LinkedHashMap<String, String> fieldMap = new LinkedHashMap<String, String>();
            for (Record field : fields) {
                String fId = field.getAttribute(BibliographicCatalogResourceApi.CATALOG_FIELD_ID);
                String fTitle = field.getAttribute(BibliographicCatalogResourceApi.CATALOG_FIELD_TITLE);
                fTitle = fTitle == null || fId.equals(fTitle) ? FIELD_TYPE_TITLES.get(fId) : fTitle;
                fTitle = fTitle == null ? fId : fTitle;
                fieldMap.put(fId, fTitle);
            }
            if (!fieldMap.containsKey(lastFieldSelection)) {
                if (fieldMap.isEmpty()) {
                    lastFieldSelection = null;
                } else {
                    lastFieldSelection = fieldMap.keySet().iterator().next();
                }
            }
            selectField.setValueMap(fieldMap);
            selectField.setValue(lastFieldSelection);
        }

        @Override
        public void onDataArrived(com.smartgwt.client.widgets.form.fields.events.DataArrivedEvent event) {
            onChanged(null);
        }
    }
    
}
