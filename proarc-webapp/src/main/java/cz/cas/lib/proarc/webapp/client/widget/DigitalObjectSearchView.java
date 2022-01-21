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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SelectionType;
import com.smartgwt.client.util.Offline;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IconButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.LengthRangeValidator;
import com.smartgwt.client.widgets.form.validator.RequiredIfFunction;
import com.smartgwt.client.widgets.form.validator.RequiredIfValidator;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.Editor;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.action.SelectionCache;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
import cz.cas.lib.proarc.webapp.client.ds.UserDataSource;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi.SearchType;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.logging.Logger;

/**
 * Shows list of digital objects.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectSearchView implements Selectable<Record>, RefreshAction.Refreshable {
    
    private static final Logger LOG = Logger.getLogger(DigitalObjectSearchView.class.getName());
    private static final String FILTER_LAST_CREATED = SearchType.LAST_CREATED.toString();
    private static final String FILTER_LAST_MODIFIED = SearchType.LAST_MODIFIED.toString();
    private static final String FILTER_PHRASE = SearchType.PHRASE.toString();
    private static final String FILTER_QUERY = SearchType.QUERY.toString();
    private static final String FILTER_DELETED = SearchType.DELETED.toString();
    private static final String FILTER_ALPHABETICAL = SearchType.ALPHABETICAL.toString();
    private static final String FILTER_ADVANCED = SearchType.ADVANCED.toString();

    private final DynamicForm filters;
    private final Canvas rootWidget;
    private final ListGrid foundGrid;
    private final SelectionCache<ListGridRecord> selectionCache;
    private final ClientMessages i18n;
    private final SmartGwtMessages i18nSmartGwt;
    private final ToolStrip toolbar;
    private final String sourceName;

    public DigitalObjectSearchView(ClientMessages i18n, String sourceName) {
        this.i18n = i18n;
        this.i18nSmartGwt = ClientUtils.createSmartGwtMessages();

        foundGrid = createList();
        selectionCache = SelectionCache.selector(foundGrid);
        
        filters = createFilter();

        VLayout vLayout = new VLayout();
        vLayout.addMember(filters);
        toolbar = createToolbar();
        vLayout.addMember(toolbar);
        vLayout.addMember(foundGrid);
        rootWidget = vLayout;
        this.sourceName = sourceName;
    }

    private ListGrid createList() {
        final ListGrid grid = new ListGrid();
        grid.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                int startRow = event.getStartRow();
                int endRow = event.getEndRow();
                if (startRow == 0 && endRow >= 0) {
                    grid.focus();
                    grid.selectSingleRecord(0);
                } else if (endRow < 0) {
                    grid.deselectAllRecords();
                }
            }
        });
        grid.setSelectionType(SelectionStyle.SINGLE);
        grid.setCanSort(false);
        grid.setCanGroupBy(false);
        grid.setDataSource(SearchDataSource.getInstance());
        ListGridPersistance gridPersistence = new ListGridPersistance("DigitalObjectSearchView.searchList", grid);
        grid.setViewState(gridPersistence.getViewState());

        ListGridField label = new ListGridField(SearchDataSource.FIELD_LABEL,
                i18n.DigitalObjectSearchView_ListHeaderLabel_Title());
        ListGridField model = new ListGridField(SearchDataSource.FIELD_MODEL,
                i18n.DigitalObjectSearchView_ListHeaderModel_Title(), 150);
        model.setAlign(Alignment.CENTER);
        ListGridField pid = new ListGridField(SearchDataSource.FIELD_PID,
                i18n.DigitalObjectSearchView_ListHeaderPid_Title(), 100);
        pid.setAlign(Alignment.CENTER);
        ListGridField created = new ListGridField(SearchDataSource.FIELD_CREATED,
                i18n.DigitalObjectSearchView_ListHeaderCreated_Title(), 100);
        created.setAlign(Alignment.CENTER);
        ListGridField modified = new ListGridField(SearchDataSource.FIELD_MODIFIED,
                i18n.DigitalObjectSearchView_ListHeaderModified_Title(), 100);
        modified.setAlign(Alignment.CENTER);
        ListGridField owner = new ListGridField(SearchDataSource.FIELD_OWNER,
                i18n.DigitalObjectSearchView_ListHeaderOwner_Title(), 100);
        ListGridField isLocked = new ListGridField(SearchDataSource.FIELD_IS_LOCKED,
                "locked", 100);
        isLocked.setHidden(false);
        isLocked.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        ListGridField state = new ListGridField(SearchDataSource.FIELD_STATE,
                i18n.DigitalObjectSearchView_ListHeaderState_Title(), 100);
        state.setHidden(true);
        ListGridField organization = new ListGridField(SearchDataSource.FIELD_ORGANIZATION,
                i18n.DigitalObjectSearchView_ListHeaderOrganization_Title(), 100);
        organization.setHidden(true);
        ListGridField user = new ListGridField(SearchDataSource.FIELD_USER,
                i18n.UsersView_ListHeader_Proccesor_Title(), 100);
        user.setHidden(true);
        user.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord listGridRecord, int i, int i1) {
                return value == null || "all".equals(value)
                        ? i18n.UsersView_ListHeader_Proccesors_All_Title()
                        : value.toString();
            }
        });
        ListGridField status = new ListGridField(SearchDataSource.FIELD_STATUS,
                i18n.DigitalObjectEditor_AdministrationEditor_Status_Title(), 100);
        status.setValueMap(DigitalObjectState.getMap(i18n));
        status.setHidden(true);
        ListGridField export = new ListGridField(SearchDataSource.FIELD_EXPORT,
                i18n.DigitalObjectSearchView_ListHeaderExport_Title(), 100);
        export.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        ListGridField ndkExport = new ListGridField(SearchDataSource.FIELD_NDK_EXPORT,
                i18n.DigitalObjectEditor_AdministrationEditor_Ndk_Export_Title(), 100);
        ndkExport.setHidden(true);
        ndkExport.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        ListGridField archiveExport = new ListGridField(SearchDataSource.FIELD_ARCHIVE_EXPORT,
                i18n.DigitalObjectEditor_AdministrationEditor_Archive_Export_Title(), 100);
        archiveExport.setHidden(true);
        archiveExport.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        ListGridField krameriusExport = new ListGridField(SearchDataSource.FIELD_KRAMERIUS_EXPORT,
                i18n.DigitalObjectEditor_AdministrationEditor_Kramerius_Export_Title(), 100);
        krameriusExport.setHidden(true);
        krameriusExport.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        ListGridField crossrefExport = new ListGridField(SearchDataSource.FIELD_CROSSREF_EXPORT,
                i18n.DigitalObjectEditor_AdministrationEditor_Crossref_Export_Title(), 100);
        crossrefExport.setHidden(true);
        crossrefExport.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        grid.setFields(label, model, isLocked, pid, created, modified, owner, state, organization, user, status, export, ndkExport, krameriusExport, archiveExport,crossrefExport);
        grid.setContextMenu(Actions.createMenu());
        grid.addSelectionUpdatedHandler((event) -> {
            selectionCache.setSelection();
        });
        return grid;
    }

    private ToolStrip createToolbar() {
        ToolStrip toolbar = Actions.createToolStrip();
        
        IconButton btnFilter = new IconButton();
        btnFilter.setActionType(SelectionType.CHECKBOX);
        btnFilter.setIcon("[SKIN]/actions/filter.png");
        btnFilter.setTitle(i18n.DigitalObjectSearchView_FilterButton_Title());
        btnFilter.setTooltip(i18n.DigitalObjectSearchView_FilterButton_Hint());
        btnFilter.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                if (filters.isVisible()) {
                    filters.hide();
                } else {
                    filters.show();
                }
            }
        });
        btnFilter.setSelected(filters.isVisible());

        toolbar.addMember(btnFilter);
        return toolbar;
    }

    private DynamicForm createFilter() {
        DynamicForm form = new DynamicForm();
        form.setBrowserSpellCheck(false);
        form.setValidateOnExit(true);
        form.setSaveOnEnter(true);
        form.setAutoHeight();
        form.setWidth100();
        form.setNumCols(6);

        final RadioGroupItem filterType = new RadioGroupItem(DigitalObjectResourceApi.SEARCH_TYPE_PARAM);
        filterType.setVertical(false);
        filterType.setShowTitle(false);
        filterType.setWrap(false);
        // setRedrawOnChange(true) enforces evaluation of other FormItem.setShowIfCondition
        filterType.setRedrawOnChange(true);
        filterType.setColSpan(5);
        final LinkedHashMap<String, String> filterMap = new LinkedHashMap<String, String>();
        filterMap.put(FILTER_ALPHABETICAL, i18n.DigitalObjectSearchView_FilterGroupAlphabetical_Title());
        filterMap.put(FILTER_LAST_CREATED, i18n.DigitalObjectSearchView_FilterGroupLastCreated_Title());
        filterMap.put(FILTER_LAST_MODIFIED, i18n.DigitalObjectSearchView_FilterGroupLastModified_Title());
        filterMap.put(FILTER_PHRASE, i18n.DigitalObjectSearchView_FilterGroupPhrase_Title());
        filterMap.put(FILTER_QUERY, i18n.DigitalObjectSearchView_FilterGroupAdvanced_Title());
        if (Editor.getInstance().hasPermission("proarc.permission.admin") || Editor.getInstance().hasPermission(UserRole.ROLE_SUPERADMIN)) {
            filterMap.put(FILTER_DELETED, i18n.DigitalObjectSearchView_FilterGroupDeleted_Title());
        }
        filterMap.put(FILTER_ADVANCED, i18n.DigitalObjectSearchView_FilterGroupAdvancedSearch_Title());
        filterType.setValueMap(filterMap);
        filterType.setValue(FILTER_LAST_CREATED);

        FormItemIfFunction showIfQuery = new StringMatchFunction(filterType, FILTER_QUERY, FILTER_DELETED);
        FormItemIfFunction showIfPhrase = new StringMatchFunction(filterType, FILTER_PHRASE);
        FormItemIfFunction showIfCreatedModifiedQuery = new StringMatchFunction(filterType, FILTER_LAST_CREATED, FILTER_LAST_MODIFIED, FILTER_QUERY, FILTER_DELETED, FILTER_ALPHABETICAL, FILTER_ADVANCED);
        FormItemIfFunction showIfAplhabetical = new StringMatchFunction(filterType, FILTER_LAST_CREATED, FILTER_LAST_MODIFIED, FILTER_ALPHABETICAL, FILTER_ADVANCED);
        FormItemIfFunction showIfAdvanced = new StringMatchFunction(filterType, FILTER_ADVANCED);
        FormItemIfFunction showIfQueryOrAdvanced = new StringMatchFunction(filterType, FILTER_QUERY, FILTER_DELETED, FILTER_ADVANCED);

        final TextItem phrase = createAdvancedItem(DigitalObjectResourceApi.SEARCH_PHRASE_PARAM,
                i18n.DigitalObjectSearchView_FilterPhrase_Title(), showIfPhrase);
        phrase.setValidators(new RequiredIfValidator(new RequiredIfFunction() {

            @Override
            public boolean execute(FormItem formItem, Object value) {
                return FILTER_PHRASE.equals(filterType.getValueAsString());
            }
        }), new LengthRangeValidator() {{
            setMax(1000);
        }});

        SubmitItem submit = new SubmitItem("search", i18n.DigitalObjectSearchView_FilterSearchButton_Title());

        form.setFields(filterType, createSpacerItem("100%", null),
                phrase,
                createAdvancedItem(DigitalObjectResourceApi.SEARCH_QUERY_TITLE_PARAM,
                        i18n.DigitalObjectSearchView_FilterAdvancedTitle_Title(), showIfQuery), createSpacerItem("100%", showIfQuery),
                createAdvancedItem(DigitalObjectResourceApi.SEARCH_QUERY_IDENTIFIER_PARAM,
                        i18n.DigitalObjectSearchView_FilterAdvancedIdentifier_Title(), showIfQueryOrAdvanced), createSpacerItem("100%", showIfQueryOrAdvanced),
                //createOwnerItem(i18n.DigitalObjectSearchView_FilterAdvancedCreator_Title(), showIfQueryOrAdvanced), createSpacerItem("100%", showIfQueryOrAdvanced),
                createAdvancedItem(DigitalObjectResourceApi.SEARCH_QUERY_CREATOR_PARAM, i18n.DigitalObjectSearchView_FilterAdvancedCreator_Title(), showIfQueryOrAdvanced), createSpacerItem("100%", showIfQuery),
                createAdvancedItem(DigitalObjectResourceApi.SEARCH_QUERY_LABEL_PARAM,
                        i18n.DigitalObjectSearchView_FilterAdvancedLabel_Title(), showIfQueryOrAdvanced), createSpacerItem("100%", showIfQueryOrAdvanced),
                createOwnerItem(i18n.DigitalObjectSearchView_FilterAdvancedOwner_Title(), showIfQueryOrAdvanced), createSpacerItem("100%", showIfQuery),
                //createAdvancedItem(DigitalObjectResourceApi.SEARCH_OWNER_PARAM, i18n.DigitalObjectSearchView_FilterAdvancedOwner_Title(), showIfQueryOrAdvanced), createSpacerItem("100%", showIfQuery),
                createStatusItem(i18n.DigitalObjectSearchView_FilterGroupStatus_Title(), showIfAdvanced), createSpacerItem("100%", showIfAdvanced),
                createOrganizationItem(i18n.DigitalObjectSearchView_ListHeaderOrganization_Title(), showIfAdvanced), createSpacerItem("100%", showIfAdvanced),
                createProcessorItem(i18n.UsersView_ListHeader_Proccesor_Title(), showIfAdvanced), createSpacerItem("100%", showIfAdvanced),
                createModelItem(i18n.DigitalObjectSearchView_FilterAdvancedModel_Title(), showIfCreatedModifiedQuery),
                createRememberModelItem(i18n.DigitalObjectSearchView_FilterAdvancedModel_Remember_Title(), showIfCreatedModifiedQuery),
                createSortFieldItem(i18n.DigitalObjectSearchView_FilterSort_Title(), showIfAdvanced),
                createSortItem(i18n.DigitalObjectSearchView_FilterSort_Title(), showIfAplhabetical),
                //, createSpacerItem("100%", showIfCreatedModifiedQuery),
                submit);
        
        form.addSubmitValuesHandler(new SubmitValuesHandler() {

            @Override
            public void onSubmitValues(SubmitValuesEvent event) {
                if (filters.validate(false)) {
                    filter();
                }
            }
        });
        return form;
    }

    private FormItem createOwnerItem(String title, FormItemIfFunction showIf) {
        SelectItem item = new SelectItem(DigitalObjectResourceApi.SEARCH_OWNER_PARAM, title);
        item.setWidth("100%");
        item.setColSpan(3);
        item.setAllowEmptyValue(true);
        item.setOptionDataSource(UserDataSource.getInstance());
        item.setValueField(UserDataSource.FIELD_USERNAME);
        item.setDisplayField(UserDataSource.FIELD_USERNAME);
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        return item;
    }

    private FormItem createSortFieldItem(String title, FormItemIfFunction showIf) {
        SelectItem item = new SelectItem(DigitalObjectResourceApi.SEARCH_SORT_FIELD_PARAM, title);
        item.setWidth(200);
        item.setAllowEmptyValue(true);
        LinkedHashMap<String, String> valueMap = new LinkedHashMap<>();
        valueMap.put("pid", i18n.DigitalObjectSearchView_ListHeaderPid_Title());
        valueMap.put("model", i18n.DigitalObjectSearchView_ListHeaderModel_Title());
        valueMap.put("owner", i18n.DigitalObjectSearchView_ListHeaderOwner_Title());
        valueMap.put("label", i18n.DigitalObjectSearchView_ListHeaderLabel_Title());
        valueMap.put("state", i18n.DigitalObjectSearchView_ListHeaderState_Title());
        valueMap.put("created", i18n.DigitalObjectSearchView_ListHeaderCreated_Title());
        valueMap.put("modified", i18n.DigitalObjectSearchView_ListHeaderModified_Title());
        valueMap.put("organization", i18n.DigitalObjectSearchView_ListHeaderOrganization_Title());
        valueMap.put("user", i18n.UsersView_ListHeader_Proccesor_Title());
        valueMap.put("status", i18n.DigitalObjectSearchView_FilterGroupStatus_Title());
        item.setValueMap(valueMap);
        item.setDefaultValue("label");
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        return item;
    }

    private FormItem createSortItem(String title, FormItemIfFunction showIf) {
        SelectItem item = new SelectItem(DigitalObjectResourceApi.SEARCH_SORT_PARAM, title);
        item.setWidth(200);
        item.setAllowEmptyValue(false);
        LinkedHashMap<String, String> valueMap = new LinkedHashMap();
        valueMap.put("asc", i18n.DigitalObjectSearchView_FilterAsc_Title());
        valueMap.put("desc", i18n.DigitalObjectSearchView_FilterDesc_Title());
        item.setDefaultValue("asc");
        item.setValueMap(valueMap);
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        return item;
    }

    private FormItem createStatusItem(String title, FormItemIfFunction showIf) {
        SelectItem item = new SelectItem(DigitalObjectResourceApi.SEARCH_STATUS_PARAM, title);
        item.setWidth("100%");
        item.setColSpan(3);
        item.setAllowEmptyValue(true);
        item.setValueMap(DigitalObjectState.getMap(i18n));
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        return item;
    }

    private FormItem createProcessorItem(String title, FormItemIfFunction showIf) {
        SelectItem item = new SelectItem(DigitalObjectResourceApi.SEARCH_PROCESSOR_PARAM, title);
        item.setOptionDataSource(UserDataSource.getInstance());
        item.setValueField(UserDataSource.FIELD_USERNAME);
        item.setDisplayField(UserDataSource.FIELD_USERNAME);
        item.setWidth("100%");
        item.setColSpan(3);
        item.setAllowEmptyValue(true);
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        return item;
    }

    private FormItem createOrganizationItem(String title, FormItemIfFunction showIf) {
        SelectItem item = new SelectItem(DigitalObjectResourceApi.SEACH_ORGANIZATION_PARAM, title);
        item.setWidth("100%");
        item.setColSpan(3);
        item.setAllowEmptyValue(true);
        item.setValueMap(Organization.getMap());
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        return item;
    }

    private static TextItem createAdvancedItem(String name, String title, FormItemIfFunction showIf) {
        TextItem item = new TextItem(name, title);
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        item.setWidth("100%");
        item.setColSpan(3);
        item.setValidators(new LengthRangeValidator() {{ setMax(1000); }});
        return item;
    }

    private SelectItem createModelItem(String title, FormItemIfFunction showIf) {
        SelectItem item = new SelectItem(DigitalObjectResourceApi.SEARCH_QUERY_MODEL_PARAM, title);
        item.setWidth(200);
        item.setAllowEmptyValue(true);
        item.setEmptyDisplayValue("<i>" + i18nSmartGwt.filterBuilder_matchAllTitle() + "</i>");
        // see setFilterModel
//        item.setValue("model:periodical");
//        item.setDefaultToFirstOption(true);
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        return item;
    }

    private CheckboxItem createRememberModelItem(String title, FormItemIfFunction showIf) {
        CheckboxItem item = new CheckboxItem(DigitalObjectResourceApi.SEARCH_MODEL_PARAM_REMEMBER, title);
        item.setColSpan(2);
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        return item;
    }

    private SpacerItem createSpacerItem(String width, FormItemIfFunction showIf) {
        SpacerItem item = new SpacerItem();
        if (width != null) {
            item.setWidth(width);
        }
        if (showIf != null) {
            item.setShowIfCondition(showIf);
        }
        return item;
    }

    public Canvas asWidget() {
        return rootWidget;
    }

    public ListGrid getGrid() {
        return foundGrid;
    }

    public ToolStrip getToolbar() {
        return toolbar;
    }

    public void onShow() {
        // fetch values after fetching models (setFilterModel, setModels).
    }

    public void setFilterModel(Object modelId) {
        FormItem filterModel = filters.getField(DigitalObjectResourceApi.SEARCH_QUERY_MODEL_PARAM);
        filterModel.setValue(modelId);
    }

    public void setSort(Object sort) {
        FormItem sortField = filters.getField(DigitalObjectResourceApi.SEARCH_SORT_PARAM);
        sortField.setValue(sort);
    }

    public void setModels(LinkedHashMap<?, ?> valueMap) {
        ListGridField field = foundGrid.getField(SearchDataSource.FIELD_MODEL);
        field.setValueMap(valueMap);
        FormItem filterModel = filters.getField(DigitalObjectResourceApi.SEARCH_QUERY_MODEL_PARAM);
        filterModel.setValueMap(valueMap);
    }

    @Override
    public Record[] getSelection() {
        return selectionCache.getSelection();
    }

    @Override
    public void refresh() {
        foundGrid.invalidateCache();
        filter();
    }

    private void filter() {
        CheckboxItem checkbox = ((CheckboxItem)filters.getField(DigitalObjectResourceApi.SEARCH_MODEL_PARAM_REMEMBER));
        if (checkbox.getValueAsBoolean()) {
            checkbox.setValue(false);
            Offline.put(sourceName, filters.getField(DigitalObjectResourceApi.SEARCH_QUERY_MODEL_PARAM).getValue());
            Offline.put(sourceName + "_sort", filters.getField(DigitalObjectResourceApi.SEARCH_SORT_PARAM).getValue());
        }
        Criteria valuesAsCriteria = filters.getValuesAsCriteria();
        foundGrid.deselectAllRecords();
        foundGrid.fetchData(valuesAsCriteria);
    }

    private static final class StringMatchFunction implements FormItemIfFunction {

        private final HashSet<String> patterns;
        private final FormItem item2query;

        public StringMatchFunction(FormItem item2query, String... patterns) {
            this.patterns = new HashSet<String>(Arrays.asList(patterns));
            this.item2query = item2query;
        }

        @Override
        public boolean execute(FormItem item, Object value, DynamicForm form) {
            Object itemValue = item2query.getValue();
            return itemValue != null && patterns.contains(itemValue.toString());
        }

    }

}
