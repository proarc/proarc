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
package cz.incad.pas.editor.client.widget.mods;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.ds.mods.IdentifierDataSource;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Simple form to edit MODS page.
 *
 * @author Jan Pokorsky
 */
public final class PageForm extends DynamicForm {

    private static final Logger LOG = Logger.getLogger(PageForm.class.getName());

    public PageForm(ClientMessages i18n) {
        setWidth100();
        setHeight100();
        setTitleOrientation(TitleOrientation.TOP);
        setNumCols(4);
        setColWidths(20, 20, 20);
        SelectItem pageType = new SelectItem(ModsCustomDataSource.FIELD_PAGE_TYPE, i18n.PageForm_PageType_Title());
        pageType.setValueMap(ModsCustomDataSource.getPageTypes());
        pageType.setDefaultValue(ModsCustomDataSource.getDefaultPageType());

        IntegerItem pageIndex = new IntegerItem(ModsCustomDataSource.FIELD_PAGE_INDEX);
        pageIndex.setTitle(i18n.PageForm_PageIndex_Title());

        TextItem pageNumber = new TextItem(ModsCustomDataSource.FIELD_PAGE_NUMBER);
        pageNumber.setTitle(i18n.PageForm_PageNumber_Title());
        pageNumber.setEndRow(true);
//        pageNumber.setLength(5);

        final RepeatableFormItem identifiers = new RepeatableFormItem(ModsCustomDataSource.FIELD_IDENTIFIERS,
                i18n.PageForm_Identifiers_Title());
        identifiers.setDataSource(IdentifierDataSource.getInstance());
        DynamicForm identifierForm = new DynamicForm();
        identifierForm.setUseAllDataSourceFields(true);
        identifierForm.setNumCols(4);
        identifiers.setFormPrototype(identifierForm);
        identifiers.setEndRow(true);
        identifiers.setColSpan("3");

//        TextAreaItem note = new AutoFitTextAreaItem(ModsCustomDataSource.FIELD_NOTE, "Note");
        TextAreaItem note = new TextAreaItem(ModsCustomDataSource.FIELD_NOTE, i18n.PageForm_Note_Title());
        note.setWidth("*");
        note.setHeight("*");
        note.setColSpan("*");

        ButtonItem btnTest = new ButtonItem("test");
        btnTest.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                Object idFieldVal = identifiers.getValue();
                Map values = getValues();
                Map changedValues = getChangedValues();
                Map oldValues = getOldValues();
                Boolean valuesHaveChanged = valuesHaveChanged();
                LOG.info("ModsPage.valuesHaveChanged: " + valuesHaveChanged);
                LOG.info("identifiers.getFieldValue: " + idFieldVal);
                Object idVal = values.get(ModsCustomDataSource.FIELD_IDENTIFIERS);
                Object idOldVal = oldValues.get(ModsCustomDataSource.FIELD_IDENTIFIERS);
                LOG.info("identifiers.getValue: " + idVal);
                LOG.info("identifiers.getOldValue: " + idOldVal);
                LOG.info("identifiers.getChangedValues: " + changedValues.get(ModsCustomDataSource.FIELD_IDENTIFIERS));
                LOG.info("identifiers.getChangedValues: " + ClientUtils.dump(changedValues, "", "  ", new StringBuilder()).toString());
//                LOG.info("identifiers.getValue: " + ClientUtils.dump(values, "", "  ", new StringBuilder()).toString());
            }
        });
        ButtonItem btnSave = new ButtonItem("save");
        btnSave.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                saveData(new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        // XXX reload data?
                        // XXX notify errors
                    }
                });
            }
        });
//        setFields(btnTest, btnSave, pageType, pageIndex, pageNumber, identifiers, note);
        setFields(pageType, pageIndex, pageNumber, identifiers, note);

        IntegerRangeValidator integerRangeValidator = new IntegerRangeValidator();
        integerRangeValidator.setMin(0);
        integerRangeValidator.setMax(Integer.MAX_VALUE);

        pageIndex.setValidators(integerRangeValidator);

        addItemChangedHandler(new ItemChangedHandler() {

            @Override
            public void onItemChanged(ItemChangedEvent event) {
                LOG.info("## ModsPage.onItemChanged.name: " + event.getItem().getName());
                LOG.info("## ModsPage.onItemChanged.newValue: " + event.getNewValue());
            }
        });

        LOG.info("ModsPage.valuesHaveChanged.beforFetch: " + valuesHaveChanged());

//        fetchData(null, new DSCallback() {
//
//            @Override
//            public void execute(DSResponse response, Object rawData, DSRequest request) {
//                Record record = response.getData()[0];
//                editRecord(record);
//                rememberValues();
//            }
//        });
    }

//    void dumpChangedValues(String point) {
//        Boolean valuesHaveChanged = valuesHaveChanged();
//        Map changedValues = getChangedValues();
//        Object ref = changedValues.get("__ref");
//        String refContent = null;
//        if (ref instanceof Record) {
//            Record rec = (Record) ref;
//            refContent = "\n" + ClientUtils.dump(rec.getJsObj());
//        }
//        LOG.info(ClientUtils.format("PageForm.valuesHaveChanged.%s, changed: %s, values: %s, ref: %s, refContent: %s",
//                point, valuesHaveChanged,Arrays.toString(changedValues.keySet().toArray()), ref, refContent));
//    }
}
