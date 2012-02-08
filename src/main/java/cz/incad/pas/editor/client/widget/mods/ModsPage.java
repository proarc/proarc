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
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.ds.mods.IdentifierDataSource;
import java.util.Map;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public final class ModsPage extends DynamicForm {

    private static final Logger LOG = Logger.getLogger(ModsPage.class.getName());

    public ModsPage() {
        setWidth100();
        setHeight100();
        setTitleOrientation(TitleOrientation.TOP);
        setNumCols(4);
        setColWidths(20, 20, 20);
        SelectItem pageType = new SelectItem(PageDataSource.FIELD_PAGE_TYPE, "Page type");
//        radioGroupItem.setTooltip("podle ANL by tu mohlo byt mnohem vic typu. Viz http://digit.nkp.cz/DigitizedPeriodicals/DTD/2.10/Periodical.xsd/PeriodicalPage[@Type]");
        pageType.setValueMap("ListOfIllustrations", "TableOfContents", "Index",
                "Table", "TitlePage", "ListOfMaps", "NormalPage", "Blank", "ListOfTables", "Advertisement");
        pageType.setDefaultValue("NormalPage");

        IntegerItem pageIndex = new IntegerItem(PageDataSource.FIELD_PAGE_INDEX);
        pageIndex.setTitle("Page Index");

        TextItem pageNumber = new TextItem(PageDataSource.FIELD_PAGE_NUMBER);
        pageNumber.setTitle("Page Number");
        pageNumber.setEndRow(true);
//        pageNumber.setLength(5);

        final RepeatableFormItem identifiers = new RepeatableFormItem(PageDataSource.FIELD_IDENTIFIERS, "Identifiers");
        identifiers.setDataSource(IdentifierDataSource.getInstance());
        DynamicForm identifierForm = new DynamicForm();
        identifierForm.setUseAllDataSourceFields(true);
        identifierForm.setNumCols(4);
        identifiers.setFormPrototype(identifierForm);
        identifiers.setEndRow(true);
        identifiers.setColSpan("3");

//        TextAreaItem note = new AutoFitTextAreaItem(PageDataSource.FIELD_NOTE, "Note");
        TextAreaItem note = new TextAreaItem(PageDataSource.FIELD_NOTE, "Note");
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
                Object idVal = values.get(PageDataSource.FIELD_IDENTIFIERS);
                Object idOldVal = oldValues.get(PageDataSource.FIELD_IDENTIFIERS);
                LOG.info("identifiers.getValue: " + idVal);
                LOG.info("identifiers.getOldValue: " + idOldVal);
                LOG.info("identifiers.getChangedValues: " + changedValues.get(PageDataSource.FIELD_IDENTIFIERS));
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
//        setFields(btnTest, btnSave, pageType, pageIndex, pageNumber, note);
        setFields(btnTest, btnSave, pageType, pageIndex, pageNumber, identifiers, note);
        // TODO DS should be universal for all types or there will be DS per type?
        setDataSource(PageDataSource.getInstance());

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
//        LOG.info(ClientUtils.format("ModsPage.valuesHaveChanged.%s, changed: %s, values: %s, ref: %s, refContent: %s",
//                point, valuesHaveChanged,Arrays.toString(changedValues.keySet().toArray()), ref, refContent));
//    }
}
