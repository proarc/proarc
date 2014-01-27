/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget.dc;

import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.widget.mods.AbstractModelForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.FormWidget;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.FormWidgetFactory;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Edits Dublin Core metadata. It provides {@link DynamicForm forms} that can
 * be customized declaratively with {@link Form}.
 *
 * @author Jan Pokorsky
 */
// XXX cleanup required
public class DcEditor {

    private final ClientMessages i18n;
    private DynamicForm form;
    private final MetaModelRecord model;
    private final String activeLocale;

    public DcEditor(ClientMessages i18n, MetaModelRecord model) {
        this.i18n = i18n;
        this.model = model;
        activeLocale = LanguagesDataSource.activeLocale();
    }

    public DynamicForm getForm() {
        if (form != null) {
            return form;
        }
        if (model != null) {
            form = new DerForms().getForm(model, activeLocale);
            if (form == null) {
                form = new DesForms().getForm(model, activeLocale);
            }
            if (form != null) {
                return form;
            }
        }
        form = createFullForm();
        return form;
    }

    private DynamicForm createFullForm() {
        DynamicForm f = new DynamicForm();
        f.setWidth100();
        f.setNumCols(2);
        f.setBrowserSpellCheck(false);
        f.setWrapItemTitles(false);
        f.setTitleOrientation(TitleOrientation.TOP);
        ArrayList<FormItem> items = new ArrayList<FormItem>();
        addElement(items, DcConstants.CONTRIBUTOR, null, true);
        addElement(items, DcConstants.COVERAGE, null, true);
        addElement(items, DcConstants.CREATOR, null, true);
        addElement(items, DcConstants.DATE, null, true);
        addElement(items, DcConstants.DESCRIPTION, null, true);
        addElement(items, DcConstants.FORMAT, null, true);
        addElement(items, DcConstants.IDENTIFIER, null, true);
        addElement(items, DcConstants.LANGUAGE, null, true);
        addElement(items, DcConstants.PUBLISHER, null, true);
        addElement(items, DcConstants.RELATION, null, true);
        addElement(items, DcConstants.RIGHTS, null, true);
        addElement(items, DcConstants.SOURCE, null, true);
        addElement(items, DcConstants.SUBJECT, null, true);
        addElement(items, DcConstants.TITLE, null, true);
        addElement(items, DcConstants.TYPE, null, true);
        f.setFields(items.toArray(new FormItem[items.size()]));
        return f;
    }

    private void addElement(ArrayList<FormItem> items, String name, String title, boolean visible) {
        TextItem val = new TextItem(name);
        TextItem lang = new TextItem(name + "lang", "Lang");
        items.add(val);
        items.add(lang);
    }
    private DynamicForm createForm(Form formProfile) {
        DynamicForm f = new AbstractModelForm() {};
        f.setWidth100();
        f.setNumCols(1);
        // XXX add all possible DC fields
        // XXX profiles will show/hide and set cardinality
        LinkedHashMap<String, FormItem> itemMap = creatFormItems();
        List<Field> fields = formProfile.getFields();
        ArrayList<FormItem> items = new ArrayList<FormItem>(itemMap.size());
        for (Field field : fields) {
            FormItem item = itemMap.remove(field.getName());
            if (item != null) {
                item.setAttribute(RepeatableFormItem.ATTR_PROFILE, field);
                items.add(item);
            } else {
//                throw new IllegalStateException("Unknown Field: " + field.getName());
            }
        }
        for (FormItem item : itemMap.values()) {
            item.setVisible(false);
            items.add(item);
        }
//        RepeatableFormItem titles = new RepeatableFormItem("title", "Titles",
//                new ElementFormFactory());
////                new StringFormFactory(DcConstants.VALUE, null, false, 600));
////        RepeatableFormItem titles = new RepeatableFormItem("title", "Titles", new StringFormFactory(DcConstants.VALUE, null, false, 600));
//        RepeatableFormItem identifiers = new RepeatableFormItem("identifier", "Identifier", new StringFormFactory(DcConstants.VALUE, null, false, 600));
//        RepeatableFormItem types = new RepeatableFormItem("type", "Type", new StringFormFactory(DcConstants.VALUE, null, false, 400));
//        RepeatableFormItem subjects = new RepeatableFormItem("subject", "Subject", new StringFormFactory(DcConstants.VALUE, null, false, 400));
////        identifiers.setWidth(400);
//        f.setFields(titles, identifiers, types, subjects);
        f.setFields(items.toArray(new FormItem[items.size()]));
        return f;
    }

    private LinkedHashMap<String, FormItem> creatFormItems() {
        return new FormItemBuilder().addItem(DcConstants.TITLE, i18n.DCEditor_Titles_Title())
                .addItem(DcConstants.IDENTIFIER, i18n.DCEditor_Identifiers_Title())
                .addItem(DcConstants.CREATOR, i18n.DCEditor_Creators_Title())
                .addItem(DcConstants.CONTRIBUTOR, i18n.DCEditor_Contributors_Title())
                .addItem(DcConstants.COVERAGE, i18n.DCEditor_Coverage_Title())
                .addItem(DcConstants.DATE, i18n.DCEditor_Dates_Title())
                .addItem(DcConstants.FORMAT, i18n.DCEditor_Formats_Title())
                .addItem(DcConstants.LANGUAGE, i18n.DCEditor_Language_Title())
                .addItem(DcConstants.PUBLISHER, i18n.DCEditor_Publishers_Title())
                .addItem(DcConstants.RELATION, i18n.DCEditor_Relations_Title())
                .addItem(DcConstants.RIGHTS, i18n.DCEditor_Rights_Title())
                .addItem(DcConstants.SOURCE, i18n.DCEditor_Sources_Title())
                .addItem(DcConstants.SUBJECT, i18n.DCEditor_Subjects_Title())
                .addItem(DcConstants.TYPE, i18n.DCEditor_Types_Title())
                .addItem(DcConstants.DESCRIPTION, i18n.DCEditor_Descriptions_Title())
                .getItems();
    }

    static class FormItemBuilder {

        private final LinkedHashMap<String, FormItem> items = new LinkedHashMap<String, FormItem>();

        public FormItemBuilder addItem(String name, String title) {
            FormItem item;
            item = new RepeatableFormItem(name, title, new ElementFormFactory());
            if (items.put(name, item) != null) {
                throw new IllegalStateException("duplicate: " + name);
            }
            return this;
        }

        public LinkedHashMap<String, FormItem> getItems() {
            return items;
        }

    }

    static class ElementFormFactory implements FormWidgetFactory {

        public ElementFormFactory() {
        }

        @Override
        public DynamicForm create() {
            throw new UnsupportedOperationException();
        }

        @Override
        public FormWidget createFormWidget(Field formField) {
//            Field valueField = formField == null ? null : formField.getMember(DcConstants.VALUE);
            ValuesManager valuesManager = new ValuesManager();
            DynamicForm fVal = new DynamicForm();
            fVal.setHoverWidth(200);
            fVal.setBrowserSpellCheck(false);
            fVal.setWrapItemTitles(false);
            fVal.setTitleOrientation(TitleOrientation.TOP);
            fVal.setWidth100();
            fVal.setNumCols(1);
//            fVal.setTitleWidth(0);
            Field valField = getProfile(formField, DcConstants.VALUE);
//            System.out.println("createFormWidget.field: " + valField);
            FormItem val;
            if (valField != null && valField.getType() != null) {
                String type = valField.getType();
                if ("select".equals(type)) {
                    val = new SelectItem(DcConstants.VALUE);
                } else if ("textArea".equals(type)) {
                    val = new TextAreaItem(DcConstants.VALUE);
                } else {
                    val = new TextItem(DcConstants.VALUE);
                }
            } else {
                val = new TextItem(DcConstants.VALUE);
            }
//            System.out.println("createFormWidget.type: " + val.getType());
//            val.setShowTitle(false);
//            val.setWidth("*");
            updateProfile(fVal, val, valField);
            fVal.setFields(val);

            DynamicForm fLang = new DynamicForm();
            fLang.setBrowserSpellCheck(false);
            fLang.setWrapItemTitles(false);
            fLang.setNumCols(1);
            fLang.setTitleOrientation(TitleOrientation.TOP);
            TextItem lang = new TextItem(DcConstants.LANG);
//            lang.setShowTitle(row == 0);
//            lang.setVisible(false);
            updateProfile(fLang, lang, getProfile(formField, DcConstants.LANG));
            fLang.setFields(lang);

            valuesManager.addMember(fVal);
            valuesManager.addMember(fLang);
            HLayout hLayout = new HLayout();
            hLayout.setMembers(fVal, fLang);
            hLayout.setWidth100();
            return new FormWidget(hLayout, valuesManager);
        }

        private static Field getProfile(Field formField, String name) {
            return  formField == null ? null : formField.getMember(name);
        }

        private void updateProfile(DynamicForm form, FormItem item, Field profile) {
            if (profile != null) {
                Boolean hidden = profile.getHidden();
                form.setVisible(hidden == null || !hidden);
                String title = profile.getTitle(null);
                if (title != null) {
                    if (title.isEmpty()) {
                        item.setShowTitle(false);
                    }
                    item.setTitle(title);
                }
                String hint = profile.getHint(null);
                if (hint != null) {
                    item.setPrompt(hint);
                }
                item.setRequired(profile.getRequired());
                String width = profile.getWidth();
                if (width != null) {
                    item.setWidth(width);
                }
                Integer length = profile.getLength();
                if (length != null && item instanceof TextItem) {
                    ((TextItem) item).setLength(length);
                }
                LinkedHashMap<String, String> valueMap = profile.getValueMap();
                if (valueMap != null) {
//                    System.out.println("DCE.updateProfile.valueMap: " + valueMap);
                    item.setValueMap(valueMap);
                }
            }
        }

    }

}
