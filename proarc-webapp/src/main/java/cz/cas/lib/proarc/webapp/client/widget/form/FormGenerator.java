/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget.form;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.ReadOnlyDisplayAppearance;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.JSOHelper;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemValueFormatter;
import com.smartgwt.client.widgets.form.FormItemValueParser;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.AutoFitTextAreaItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import cz.cas.lib.proarc.webapp.client.ds.ValueMapDataSource;
import cz.cas.lib.proarc.webapp.client.widget.mods.AbstractModelForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.CustomFormFactory;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.FormWidget;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.FormWidgetFactory;
import cz.cas.lib.proarc.webapp.client.widget.mods.StringFormFactory;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Generates the form hierarchy from {@link Form} declaration.
 *
 * @author Jan Pokorsky
 */
public class FormGenerator {

    private static final Logger LOG = Logger.getLogger(FormGenerator.class.getName());
    private final Form formDeclaration;
    private final String activeLocale;
    private int defaultTextLength = 1000;
    private String defaultWidth = "400";

    public FormGenerator(Form f, String activeLocale) {
        this.formDeclaration = f;
        this.activeLocale = activeLocale;
        if (f.getItemWidth() != null) {
            defaultWidth = f.getItemWidth();
        }
    }

    /**
     * Builds the SmartGWT form.
     * @return the form
     */
    public final DynamicForm generateForm() {
        List<Field> fields = formDeclaration.getFields();
        ArrayList<FormItem> formItems = new ArrayList<FormItem>(fields.size());
        for (Field child : fields) {
            FormItem formItem = createItem(child, activeLocale);
            if (formItem != null) {
                formItems.add(formItem);
            }
        }
        DynamicForm df = createDefaultForm();
        addFormItems(df, formItems);
        return df;
    }

    // enum is not extensible :-(
    protected enum ItemType {
        /** array of simple type values */
        ARRAY,
        /** simple type value */
        PLAIN,
        /** nested form */
        FORM,
        /** nested form that will be customized with {@link FormGenerator#customizeNestedForm } */
        CUSTOM_FORM
    }

    public FormItem createItem(final Field f, final String lang) {
        ItemType itemType = getType(f);
        FormItem formItem = null;
        switch (itemType) {
            case PLAIN:
                formItem = getFormItem(f, lang);
                break;
            case ARRAY:
                // XXX replace StringFormFactory with a generic type solution
                formItem = new RepeatableFormItem(f,
                        new StringFormFactory(f.getName(), f.getTitle(lang), false));
                break;
            case FORM:
                formItem = createNestedFormItem(f, lang);
                break;
            case CUSTOM_FORM:
                formItem = createNestedCustomFormItem(f, lang);
                break;
        }
//        formItem.setWidth("*");
        return formItem;
    }

    protected ItemType getType(Field f) {
        ItemType itemType;
        // fType != null -> simple field
        // fType maxOccurencies > 1 -> repeatable simple field
        String fType = f.getType();
        List<Field> fields = f.getFields();
        if (fType != null) {
            itemType = (f.getMaxOccurrences() > 1) ? ItemType.ARRAY : ItemType.PLAIN;
            if (Field.CUSTOM_FORM.equals(fType)) {
                itemType = ItemType.CUSTOM_FORM;
            }
        } else if (!fields.isEmpty()) {
            itemType = ItemType.FORM;
        } else {
            throw new UnsupportedOperationException(String.valueOf(f));
        }
        return itemType;
    }

    public DynamicForm createNestedForm(Field f, String lang) {
        List<Field> fields = f.getFields();
        ArrayList<FormItem> formItems = new ArrayList<FormItem>(fields.size());
        for (Field child : fields) {
            FormItem formItem = createItem(child, lang);
            if (formItem != null) {
                formItems.add(formItem);
            }
        }
        DynamicForm df = createDefaultForm();
        addFormItems(df, formItems);
        return df;
    }

    private RepeatableFormItem createNestedCustomFormItem(final Field f, final String lang) {
        RepeatableFormItem rfi = new RepeatableFormItem(f, new FormWidgetFactory() {

            @Override
            public DynamicForm create() {
                throw new UnsupportedOperationException();
            }

            @Override
            public FormWidget createFormWidget(Field formField) {
                DynamicForm df = createNestedForm(f, lang);
                ValuesManager vm = new ValuesManager();
                vm.addMember(df);
                return customizeNestedForm(new FormWidget(df, vm), f);
            }
        });
        oneRow(rfi);
        return rfi;
    }

    /** Implement to attach special stuff to the custom form. */
    protected FormWidget customizeNestedForm(final FormWidget fw, Field f) {
        return fw;
    }

    public TextItem getTextFormItem(Field f, String lang) {
        TextItem item = new TextItem(f.getName(), f.getTitle(activeLocale));
        item.setLength(f.getLength() != null ? f.getLength() : defaultTextLength);
        item.setWidth(defaultWidth);
        item.setReadOnlyDisplay(ReadOnlyDisplayAppearance.STATIC);
        return item;
    }

    public AutoFitTextAreaItem getTextAreaFormItem(Field f, String lang) {
        AutoFitTextAreaItem item = new AutoFitTextAreaItem(f.getName(), f.getTitle(activeLocale));
        item.setLength(f.getLength());
        item.setWidth(defaultWidth);
        return item;
    }

    public DateItem getDateFormItem(Field f, String lang) {
        DateItem item = new DateItem(f.getName(), f.getTitle(activeLocale));
        item.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATE);
        item.setUseTextField(true);
//        item.setEnforceDate(true);
        return item;
    }

    public TextItem getDateYearFormItem(Field f, String lang) {
        TextItem item = getTextFormItem(f, lang);
        item.setWidth("150");
        DateEditorValue yearEditorValue = DateEditorValue.gYear();
        item.setEditorValueFormatter(yearEditorValue);
        item.setEditorValueParser(yearEditorValue);
        return item;
    }

    public ComboBoxItem getComboBoxItem(Field f, String lang) {
        ComboBoxItem item = new ComboBoxItem(f.getName(), f.getTitle(lang));
        if (f.getOptionDataSource() != null) {
            setOptionDataSource(item, f, lang);
        } else {
            item.setValueMap(f.getValueMap());
        }
        return item;
    }

    public SelectItem getSelectItem(Field f, String lang) {
        SelectItem item = new SelectItem(f.getName(), f.getTitle(lang));
        if (f.getOptionDataSource() != null) {
            setOptionDataSource(item, f, lang);
        } else {
            item.setValueMap(f.getValueMap());
        }
        return item;
    }

    private void setOptionDataSource(FormItem item, Field f, String lang) {
        Field optionField = f.getOptionDataSource();
        DataSource ds = ValueMapDataSource.getInstance().getOptionDataSource(optionField.getName());
        item.setValueField(f.getOptionValueField()[0]);
        item.setOptionDataSource(ds);

        setPickListValueMapping(item, f);

        Integer pickListWidth = getWidthAsInteger(optionField.getWidth());
        if (item instanceof SelectItem) {
            SelectItem selectItem = (SelectItem) item;
            selectItem.setPickListFields(getPickListFields(optionField, lang));
            if (pickListWidth != null) {
                selectItem.setPickListWidth(pickListWidth);
            }
        } else if (item instanceof ComboBoxItem) {
            ComboBoxItem cbi = (ComboBoxItem) item;
            cbi.setPickListFields(getPickListFields(optionField, lang));
            if (pickListWidth != null) {
                cbi.setPickListWidth(pickListWidth);
            }
        }
    }

    private static ListGridField[] getPickListFields(Field optionField, String lang) {
        List<Field> columns = optionField.getFields();
        ListGridField[] listFields = new ListGridField[columns.size()];
        int i = 0;
        for (Field field : optionField.getFields()) {
            listFields[i++] = new ListGridField(field.getName(), field.getTitle(lang));
        }
        return listFields;
    }

    /**
     * Listens to item value changes and and propagate selection to sibling fields
     * according to Field.getOptionValueFieldMap and Field.getOptionValueField.
     * @param item select or combo
     * @param field field with option data source
     */
    private void setPickListValueMapping(FormItem item, Field field) {
        final Map<String, String> nameMap = field.getOptionValueFieldMap();
        final String[] valueFields = field.getOptionValueField();
        if (nameMap == null && valueFields.length == 1) {
            return ;
        }
        item.addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                FormItem item = event.getItem();
                ListGridRecord selectedRecord = null;
                if (item instanceof SelectItem) {
                    SelectItem selectItem = (SelectItem) item;
                    selectedRecord = selectItem.getSelectedRecord();
                } else if (item instanceof ComboBoxItem) {
                    ComboBoxItem cbi = (ComboBoxItem) item;
                    selectedRecord = cbi.getSelectedRecord();
                }
                if (selectedRecord != null) {
                    if (nameMap != null) {
                        for (Entry<String, String> entry : nameMap.entrySet()) {
                            String pickName = entry.getKey();
                            String fieldName = entry.getValue();
                            if (item.getName().equals(fieldName)) {
                                continue;
                            }
                            String value = selectedRecord.getAttribute(pickName);
                            ValuesManager vm = item.getForm().getValuesManager();
                            setFieldValue(vm, fieldName, value);
                        }
                    } else {
                        for (String fieldName : valueFields) {
                            if (item.getName().equals(fieldName)) {
                                continue;
                            }
                            String value = selectedRecord.getAttribute(fieldName);
                            ValuesManager vm = item.getForm().getValuesManager();
                            vm.setValue(fieldName, value);
                        }
                    }
                }
            }
        });
    }

    /**
     * Sets a value to field of the values manager.
     * @param vm values manager
     * @param fieldName field name or relative path to nested field; '/' as a separator
     * @param value value to set
     */
    private void setFieldValue(ValuesManager vm, String fieldName, String value) {
        String[] path = fieldName.split("/");
//        ClientUtils.severe(LOG, "#setValue: %s, %s, %s", fieldName, path.length, value);
        if (path.length <= 1) {
            vm.setValue(fieldName, value);
        } else {
            int index = 0;
            Object vmValue = vm.getValue(path[index]);
            Record vmValueRecord = fillValueForPath(path, 0, vmValue, value);
            if (vmValueRecord != null) {
//                ClientUtils.warning(LOG, "## vm.setValue %s", path[index]);
                vm.setValue(path[index], vmValueRecord);
            }
        }
    }

    /**
     * Fills a new value of the nested field. It traverses the path of field names
     * to the leaf recursively.
     *
     * @param path path to field; '/' as a separator
     * @param pathIndex field name index in path to update
     * @param pathValue set of attributes of selected field; see pathIndex
     * @param value value to set
     * @return the record with updated attributes or {@code null}
     */
    private Record fillValueForPath(String[] path, int pathIndex, Object pathValue, String value) {
        Record pathRecord = null;
//        ClientUtils.warning(LOG, "##setValue: %s, %s\n%s",
//                path[pathIndex], ClientUtils.safeGetClass(pathValue), ClientUtils.dump(pathValue));

        // Record[], RecordList, Object[]??
        if (pathValue instanceof JavaScriptObject) {
            JavaScriptObject jso = (JavaScriptObject) pathValue;
//            ClientUtils.warning(LOG, "## isArray %s", JSOHelper.isArray(jso));
            if (JSOHelper.isArray(jso)) {
                // RecordList ??
            } else {
                pathRecord = new Record(jso);
//                ClientUtils.warning(LOG, "## vmValueRecord.setAttribute %s", pathIndex + 2 == path.length);
                // is .../field1/field2 leaf?
                if (pathIndex + 2 == path.length) {
//                    ClientUtils.warning(LOG, "## vm.setValue %s", path[pathIndex + 1]);
                    pathRecord.setAttribute(path[pathIndex + 1], value);
                } else {
                    Object nextValueInPath = pathRecord.getAttributeAsObject(path[pathIndex + 1]);
                    // recursion
                    Record newValueForPath = fillValueForPath(path, pathIndex + 1, nextValueInPath, value);
                    if (newValueForPath != null) {
                        pathRecord.setAttribute(path[pathIndex + 1], newValueForPath);
                    }
                }
            }
        } else if (pathValue == null) {
            // no value yet; let's create new values for remaining path
            pathRecord = new Record();
            // is .../field1/field2 leaf?
            if (pathIndex + 2 == path.length) {
//                ClientUtils.warning(LOG, "## vm.setValue %s", path[pathIndex + 1]);
                pathRecord.setAttribute(path[pathIndex + 1], value);
            } else {
                Record newValueForPath = fillValueForPath(path, pathIndex + 1, null, value);
                if (newValueForPath != null) {
                    pathRecord.setAttribute(path[pathIndex + 1], newValueForPath);
                } else {
                    // unknown new value, do not create anything
                    pathRecord = null;
                }
            }
        }
        return pathRecord;
    }

    protected static Integer getWidthAsInteger(String width) {
        if (width != null) {
            try {
                return Integer.parseInt(width);
            } catch (NumberFormatException ex) {
                // no-op
            }
        }
        return null;
    }

    public RadioGroupItem getRadioGroupItem(Field f, String lang) {
        RadioGroupItem item = new RadioGroupItem(f.getName(), f.getTitle(lang));
        item.setVertical(false);
        item.setValueMap(f.getValueMap());
        item.setWrap(false);
        item.setWrapTitle(false);
        return item;
    }

    /**
     * Adds common properties to simple {@link #getFormItem items}.
     */
    public FormItem customizeFormItem(FormItem item, Field f) {
        if (item.getTitle() == null) {
            item.setShowTitle(false);
        }
        item.setRequired(f.getRequired());
        item.setPrompt(f.getHint(activeLocale));
        if (f.getHidden() != null && f.getHidden()) {
            item.setVisible(false);
        }
        String width = f.getWidth();
        if (width != null) {
            item.setWidth(width);
        }
        if (f.getReadOnly() != null && f.getReadOnly()) {
            item.setCanEdit(!f.getReadOnly());
        }
        return item;
    }

    public FormItem getFormItem(Field f, String lang) {
        FormItem formItem;
        String type = f.getType();
        if (Field.TEXT.equals(type)) {
            formItem = getTextFormItem(f, lang);
        } else if (Field.TEXTAREA.equals(type)) {
            formItem = getTextAreaFormItem(f, lang);
        } else if (Field.G_YEAR.equals(type)) {
            formItem = getDateYearFormItem(f, lang);
        } else if (Field.DATE.equals(type)) {
            formItem = getDateFormItem(f, lang);
        } else if (Field.COMBO.equals(type)) {
            formItem = getComboBoxItem(f, lang);
        } else if (Field.SELECT.equals(type)) {
            formItem = getSelectItem(f, lang);
        } else if (Field.RADIOGROUP.equals(type)) {
            formItem = getRadioGroupItem(f, lang);
        } else { // fallback
            formItem = getTextFormItem(f, lang);
        }
        return customizeFormItem(formItem, f);
    }

    public void oneRow(FormItem fi) {
        fi.setEndRow(true);
        fi.setStartRow(true);
        fi.setColSpan("*");
    }

    public RepeatableFormItem createNestedFormItem(final Field f, final String lang) {
        RepeatableFormItem rfi = new RepeatableFormItem(f, new CustomFormFactory() {

            @Override
            public DynamicForm create() {
                return createNestedForm(f, lang);
            }
        });
        oneRow(rfi);
        return rfi;
    }

    private RepeatableFormItem createFormItem(Field f, final DynamicForm df) {
        RepeatableFormItem rfi = new RepeatableFormItem(f, new CustomFormFactory() {

            @Override
            public DynamicForm create() {
                return df;
            }
        });
        oneRow(rfi);
        return rfi;
    }

    public DynamicForm createDefaultForm() {
        AbstractModelForm df = new AbstractModelForm() {};
        df.setBrowserSpellCheck(false);
        df.setTitleOrientation(TitleOrientation.TOP);
        df.setNumCols(1);
        df.setWrapItemTitles(false);
        df.setWidth100();
        df.setHoverWrap(false);
//        df.setHoverWidth(200);
        return df;
    }

    public static void addFormItems(DynamicForm df, List<? extends FormItem> t) {
        FormItem[] items = t.toArray(new FormItem[t.size()]);
        df.setFields(items);
    }

    public static class DateEditorValue implements FormItemValueFormatter, FormItemValueParser {

        private static final DateEditorValue GYEAR = new DateEditorValue(PredefinedFormat.YEAR);
        private static final DateEditorValue DATE_VALUE = new DateEditorValue(PredefinedFormat.DATE_SHORT);

        public static DateEditorValue date() {
            return DATE_VALUE;
        }

        public static DateEditorValue gYear() {
            return GYEAR;
        }

        private static final DateTimeFormat ISO_FORMAT = DateTimeFormat.getFormat(PredefinedFormat.ISO_8601);
        private final DateTimeFormat displayFormat;

        public DateEditorValue(PredefinedFormat displayFormat) {
            this.displayFormat = DateTimeFormat.getFormat(displayFormat);
        }

        @Override
        public String formatValue(Object value, Record record, DynamicForm form, FormItem item) {
//            ClientUtils.severe(LOG, "format: class: %s, value: %s", ClientUtils.safeGetClass(value), value);
            if (value == null) {
                return null;
            }
            try {
                Date date = value instanceof Date
                        ? (Date) value : ISO_FORMAT.parse((String) value);
                return displayFormat.format(date);
            } catch (IllegalArgumentException ex) {
                String toString = String.valueOf(value);
                LOG.log(Level.WARNING, toString, ex);
                return toString;
            }
        }

        @Override
        public Object parseValue(String value, DynamicForm form, FormItem item) {
//            ClientUtils.severe(LOG, "parse: value: %s", value);
            Object result = null;
            if (value != null && !value.isEmpty()) {
                try {
                    Date date = displayFormat.parse(value);
                    result = ISO_FORMAT.format(date);
                } catch (IllegalArgumentException ex) {
                    String toString = String.valueOf(value);
                    LOG.log(Level.WARNING, toString, ex);
                    result = null;
                }
            }
            return result;
        }

    }

}
