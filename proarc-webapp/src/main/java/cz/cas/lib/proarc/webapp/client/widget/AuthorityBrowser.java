/*
 * Copyright (C) 2018 Martin Rumanek
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
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.AuthorityDataSource;
import cz.cas.lib.proarc.webapp.client.ds.AuthorityQueryDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource;
import cz.cas.lib.proarc.webapp.shared.rest.AuthorityCatalogResourceApi;
import java.util.HashMap;
import java.util.LinkedHashMap;

public class AuthorityBrowser implements DatastreamEditor, Refreshable {

    /**
     * Titles of common field types. Used when the server config does not declare
     * any field title. It ensures backward compatibility.
     */
    private static final HashMap<String, String> FIELD_TYPE_TITLES = new HashMap<String, String>();

    private final ClientMessages i18n;
    private Canvas widget;
    private DynamicForm formCatalog;
    private ListGrid lgResult;
    private boolean compactUi;
    private long timestamp;

    public AuthorityBrowser(ClientMessages i18n) {
        this.i18n = i18n;
        initFields(i18n);
    }

    /**
     * Gets the selected metadata or {@code null}.
     */
    public String getMods() {
        ListGridRecord r = lgResult.getSelectedRecord();
        String mods = (r == null) ? null : r.getAttribute(AuthorityQueryDataSource.FIELD_MODS);

//        ClientUtils.info(LOG, "getMods: %s", mods);
        return mods;
    }

    private ListGrid createLgResult() {
        ListGrid lgResult = new ListGrid();
        lgResult.setDataSource(AuthorityQueryDataSource.getInstance());
//        lgResult.setUseAllDataSourceFields(true);
        ListGridField preview = new ListGridField(AuthorityQueryDataSource.FIELD_PREVIEW,
                i18n.CatalogBrowser_HeaderPreview_Title());
        ListGridField title = new ListGridField(AuthorityQueryDataSource.FIELD_TITLE,
                i18n.CatalogBrowser_HeaderTitle_Title());
        lgResult.setDetailField(AuthorityQueryDataSource.FIELD_PREVIEW);
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

        return lgResult;
    }

    protected DynamicForm getFormCatalog() {
        return formCatalog;
    }

    public void bind() {
        formCatalog.clearErrors(true);
        fixExpandedListGrid();
        lgResult.setData(new Record[0]);
    }

    public boolean isCompactUi() {
        return compactUi;
    }

    public void setCompactUi(boolean compactUi) {
        this.compactUi = compactUi;
    }

    @Override
    public void edit(DigitalObjectDataSource.DigitalObject digitalObject) {
        bind();
    }

    @Override
    public void focus() {
        if (formCatalog != null) {
            formCatalog.focus();
            formCatalog.clearValues();
        }
    }

    @Override
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        return null;
    }

    @Override
    public Canvas getUI() {
        if (widget == null) {
            widget = createAdvancedOptions();
        }
        return widget;
    }


    private Canvas createAdvancedOptions() {
        formCatalog = createCatalogForm();
        lgResult = createLgResult();

        VLayout layout = new VLayout();
        layout.setMembers(formCatalog, lgResult);
        layout.setMargin(4);
        layout.setMembersMargin(4);
        layout.setOverflow(Overflow.AUTO);
        return layout;
    }

    private void queryCatalog() {
        if (formCatalog.validate()) {
            Criteria plain = formCatalog.getValuesAsCriteria();
            lgResult.invalidateCache();
            lgResult.fetchData(plain);
        }
    }

    protected DynamicForm createCatalogForm() {
        SelectItem selection = new SelectItem(AuthorityCatalogResourceApi.FIND_CATALOG_PARAM,
                i18n.CatalogBrowser_CriteriaCatalog_Title());
        selection.setRequired(true);
        selection.setOptionDataSource(AuthorityDataSource.getInstance());
//        selectModel.setShowOptionsFromDataSource(true);
        selection.setValueField(AuthorityCatalogResourceApi.CATALOG_ID);
        selection.setDisplayField(AuthorityCatalogResourceApi.CATALOG_NAME);
        selection.setAutoFetchData(true);
        selection.setDefaultToFirstOption(true);
        selection.setWidth(250);

        SelectItem selectField = new SelectItem(AuthorityCatalogResourceApi.FIND_FIELDNAME_PARAM,
                i18n.CatalogBrowser_CriteriaField_Title());
        selectField.setRequired(true);
        selectField.setAllowEmptyValue(false);
        selectField.setDefaultToFirstOption(true);
        AuthorityChangeHandler handler = new AuthorityChangeHandler(selection, selectField);
        selection.addDataArrivedHandler(handler);
        selection.addChangedHandler(handler);

        TextItem value = new TextItem(AuthorityCatalogResourceApi.FIND_VALUE_PARAM,
                i18n.CatalogBrowser_CriteriaQuery_Title());
        value.setWidth(300);
        value.setRequired(true);

        SelectItem type = new SelectItem(AuthorityCatalogResourceApi.FIELD_TYPE, i18n.CatalogBrowser_CriterieType_Title());
        type.setRequired(true);
        type.setDefaultToFirstOption(true);
        type.setWidth(250);
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put(AuthorityDataSource.Type.ALL.name(), i18n.CatalogBrowser_CriterieType_All_Title());
        map.put(AuthorityDataSource.Type.NAME.name(), i18n.CatalogBrowser_CriterieType_Name_Title());
        map.put(AuthorityDataSource.Type.SUBJECT.name(), i18n.CatalogBrowser_CriterieType_Subject_Title());
        type.setValueMap(map);

        ButtonItem findBtn = new ButtonItem("findBtn", i18n.CatalogBrowser_CriteriaFindBtn_Title());
        findBtn.setStartRow(false);
        findBtn.setVAlign(VerticalAlignment.BOTTOM);
        findBtn.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                queryCatalog();
            }
        });

        DynamicForm form = new DynamicForm();
        form.setFields(selection, selectField, value, type, findBtn);
        form.setBrowserSpellCheck(false);
        form.setAutoWidth();
        form.setWrapItemTitles(false);
        if (isCompactUi()) {
            form.setTitleOrientation(TitleOrientation.LEFT);
            form.setNumCols(1);
        } else {
            form.setTitleOrientation(TitleOrientation.TOP);
            form.setNumCols(4);
        }
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
            FIELD_TYPE_TITLES.put("issn", i18n.CatalogBrowser_CriteriaFieldIssn_Title());
            FIELD_TYPE_TITLES.put("isbn", i18n.CatalogBrowser_CriteriaFieldIsbn_Title());
            FIELD_TYPE_TITLES.put("ccnb", i18n.CatalogBrowser_CriteriaFieldCcnb_Title());
            FIELD_TYPE_TITLES.put("barcode", i18n.CatalogBrowser_CriteriaFieldBarcode_Title());
            FIELD_TYPE_TITLES.put("signature", i18n.CatalogBrowser_CriteriaFieldSignature_Title());
            FIELD_TYPE_TITLES.put("title", i18n.CatalogBrowser_HeaderTitle_Title());
        }
    }

    @Override
    public void refresh() {
        this.formCatalog.clearValues();
    }

    private static class AuthorityChangeHandler implements ChangedHandler, com.smartgwt.client.widgets.form.fields.events.DataArrivedHandler {

        private final SelectItem selectCatalog;
        private final SelectItem selectField;

        public AuthorityChangeHandler(SelectItem selection, SelectItem selectField) {
            this.selectCatalog = selection;
            this.selectField = selectField;
        }

        @Override
        public void onChanged(ChangedEvent event) {
            ListGridRecord r = selectCatalog.getSelectedRecord();
            String lastFieldSelection = selectField.getValueAsString();
            Record[] fields = r.getAttributeAsRecordArray(AuthorityCatalogResourceApi.CATALOG_FIELDS);
            LinkedHashMap<String, String> fieldMap = new LinkedHashMap<String, String>();
            for (Record field : fields) {
                String fId = field.getAttribute(AuthorityCatalogResourceApi.CATALOG_FIELD_ID);
                String fTitle = field.getAttribute(AuthorityCatalogResourceApi.CATALOG_FIELD_TITLE);
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
        public void onDataArrived(com.smartgwt.client.widgets.form.fields.events.DataArrivedEvent dataArrivedEvent) {
            onChanged(null);
        }
    }

}
