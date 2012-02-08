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

import com.smartgwt.client.core.DataClass;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TopOperatorAppearance;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FilterBuilder;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.ClientUtils.DataSourceFieldBuilder;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;

/**
 * Widget to select type of a newly created digital object.
 *
 * @author Jan Pokorsky
 */
public final class NewDigObject extends VLayout {

    private FilterBuilder filter;
    private final SectionStack sections;

    public NewDigObject() {
        setHeight100();
        setWidth100();

        DynamicForm optionsForm = createOptionsForm();

        SectionStackSection sectionMain = new SectionStackSection("Options");
        sectionMain.setExpanded(true);
        sectionMain.setCanCollapse(false);
        sectionMain.setItems(optionsForm);

        SectionStackSection sectionAdvanced = new SectionStackSection("Advanced Options");
        VLayout advancedWidget = new VLayout();
        advancedWidget.setContents("Advanced");
        sectionAdvanced.setItems(createAdvancedOptions());

        sections = new SectionStack();
        sections.setVisibilityMode(VisibilityMode.MULTIPLE);
        sections.setSections(sectionMain, sectionAdvanced);

        setMembers(sections);
//        setMembers(optionsForm);
    }

    public void bind(AdvancedCriteria criteria) {
        if (criteria == null) {
            sections.collapseSection(1);
            filter.setCriteria(new AdvancedCriteria());
        } else {
            sections.expandSection(1);
            filter.setCriteria(criteria);
        }

    }

    private DynamicForm createOptionsForm() {
        SelectItem selectModel = new SelectItem("model", "Select type");
        selectModel.setRequired(true);
        selectModel.setDefaultToFirstOption(true);
        selectModel.setOptionDataSource(MetaModelDataSource.getInstance());
//        selectModel.setShowOptionsFromDataSource(true);
        selectModel.setValueField(MetaModelDataSource.FIELD_PID);
        selectModel.setDisplayField(MetaModelDataSource.FIELD_DISPLAY_NAME);
        selectModel.setAutoFetchData(true);
        DynamicForm optionsForm = new DynamicForm();
        optionsForm.setFields(selectModel);
        return optionsForm;
    }

    private Canvas createAdvancedOptions() {
        DynamicForm formCatalog = createCatalogForm();
        DataSource ds = new DataSource();
        ds.setFields(
                new DataSourceFieldBuilder(new DataSourceTextField("id", "ID"))
                        .hidden().build(),
                new DataSourceFieldBuilder(new DataSourceTextField("mods", "Preview"))
                        .filter(false).build(),
                new DataSourceFieldBuilder(new DataSourceTextField("modsDetail", "MODS"))
                        .filter(false).build(),
                new DataSourceFieldBuilder(new DataSourceTextField("issn", "ISSN"))
                        .validOperators(DataSourceFieldBuilder.TEXT_OPERATIONS).build(),
                new DataSourceFieldBuilder(new DataSourceTextField("isbn", "ISBN"))
                        .validOperators(DataSourceFieldBuilder.TEXT_OPERATIONS).build(),
                new DataSourceFieldBuilder(new DataSourceTextField("ccnb", "čČNB"))
                        .validOperators(DataSourceFieldBuilder.TEXT_OPERATIONS).build()
                );
        
        ds.setClientOnly(true);
        ds.setTestData(new Record[] {
            new Record() {{
                setAttribute("id", "ID");
                setAttribute("mods", "Preview of MODS");
                setAttribute("modsDetail", "Full MODS");
                setAttribute("issn", "ISSN");
            }}
        });

        filter = new FilterBuilder();
        filter.setDataSource(ds);
//        filter.setTopOperatorAppearance(TopOperatorAppearance.INLINE);
        filter.setTopOperatorAppearance(TopOperatorAppearance.RADIO);

        ListGrid lgResult = new ListGrid();
        lgResult.setDataSource(ds);
        lgResult.setAutoFetchData(true);

        VLayout layout = new VLayout(2);
        layout.setMembers(formCatalog, filter, lgResult);
        return layout;
    }

    private DynamicForm createCatalogForm() {
        DataSource ds = new DataSource();
        ds.setFields(
                new DataSourceTextField("id", "ID"),
                new DataSourceTextField("catalog", "Catalog")
                );
        ds.setClientOnly(true);
        ds.setTestData(new Record[] {
            new Record() {{
                setAttribute("id", "id:registrdigitalizace.cz");
                setAttribute("catalog", "registrdigitalizace.cz");
            }}
        });

        SelectItem selection = new SelectItem("catalog", "Select catalog");
        selection.setRequired(true);
        selection.setOptionDataSource(ds);
//        selectModel.setShowOptionsFromDataSource(true);
        selection.setValueField("id");
        selection.setDisplayField("catalog");
        selection.setAutoFetchData(true);
        selection.setDefaultToFirstOption(true);

        DynamicForm form = new DynamicForm();
        form.setFields(selection);
        return form;
    }
}
