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

import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import cz.incad.pas.editor.client.ds.mods.IdentifierDataSource;
import cz.incad.pas.editor.client.ds.mods.PageDataSource;
import cz.incad.pas.editor.client.widget.mods.RepeatableFormItem.CustomFormFactory;
import java.util.LinkedHashMap;
import java.util.logging.Logger;

/**
 * Simple form to edit MODS of periodical object.
 *
 * @author Jan Pokorsky
 */
public final class PeriodicalForm extends DynamicForm {

    private static final Logger LOG = Logger.getLogger(PeriodicalForm.class.getName());

    public PeriodicalForm() {
        setWidth100();
        setHeight100();
        setTitleOrientation(TitleOrientation.TOP);
        setNumCols(1);

        // identifiers
        final RepeatableFormItem identifiers = new RepeatableFormItem(PageDataSource.FIELD_IDENTIFIERS, "Identifiers");
        identifiers.setDataSource(IdentifierDataSource.getInstance());
        DynamicForm identifierForm = new DynamicForm();
        identifierForm.setUseAllDataSourceFields(true);
        identifierForm.setNumCols(4);
        identifiers.setFormPrototype(identifierForm);
        identifiers.setEndRow(true);
        identifiers.setColSpan("2");

        TextItem sigla = new TextItem(PageDataSource.FIELD_SIGLA, "Physical location");

        RepeatableFormItem shelfLocators = new RepeatableFormItem(
                PageDataSource.FIELD_SHELF_LOCATORS, "Shelf Locators",
                new StringFormFactory("value", null, false));
//        shelfLocators.setRowSpan(2);

        RepeatableFormItem periodicity = new RepeatableFormItem(
                PageDataSource.FIELD_PERIODICITY, "Periodicities",
                new StringFormFactory(PageDataSource.FIELD_PERIODICITY_VALUE, null, false));
        
        RepeatableFormItem titles = new RepeatableFormItem(
                PageDataSource.FIELD_TITLES, "Titles",
                new StringFormFactory("value", null, false, 600));
        oneRow(titles);

        RepeatableFormItem subtitles = new RepeatableFormItem(
                PageDataSource.FIELD_SUBTITLES, "Subtitles",
                new StringFormFactory("value", null, false, 600));
        oneRow(subtitles);

        RepeatableFormItem alternativeTitles = new RepeatableFormItem(
                PageDataSource.FIELD_ALTERNATIVE_TITLES, "Alternative titles",
                new StringFormFactory("value", null, false, 600));
        oneRow(alternativeTitles);

        RepeatableFormItem keyTitles = new RepeatableFormItem(
                PageDataSource.FIELD_KEY_TITLES, "Key titles",
                new StringFormFactory("value", null, false, 400));
        oneRow(keyTitles);

        // author
        RepeatableFormItem authors = new RepeatableFormItem(PageDataSource.FIELD_AUTHORS, "Authors", new PersonFormFactory());
        oneRow(authors);
        RepeatableFormItem contribs = new RepeatableFormItem(PageDataSource.FIELD_CONTRIBUTORS, "Contributors", new PersonFormFactory());
        oneRow(contribs);
        RepeatableFormItem printers = new RepeatableFormItem(PageDataSource.FIELD_PRINTERS, "Printers", new PrinterPublisherFormFactory(false));
        oneRow(printers);
        RepeatableFormItem publishers = new RepeatableFormItem(PageDataSource.FIELD_PUBLISHERS, "Publishers", new PrinterPublisherFormFactory(true));
        oneRow(publishers);

        RepeatableFormItem languages = new RepeatableFormItem(PageDataSource.FIELD_LANGUAGES, "Languages", new CustomFormFactory() {

            @Override
            public DynamicForm create() {
                DynamicForm form = new DynamicForm();
//                form.setNumCols(6);
                ComboBoxItem language = new ComboBoxItem("languageCode", "Code");
                LinkedHashMap<String, String> valueMap = new LinkedHashMap<String, String>();
                valueMap.put("cze", "Czech - cze");
                valueMap.put("eng", "English - eng");
                valueMap.put("ger", "German - ger ");
                language.setValueMap(valueMap);
                form.setFields(language);
                return form;
            }
        });

        RepeatableFormItem subjects = new RepeatableFormItem(PageDataSource.FIELD_CLASSIFICATIONS, "Subjects", new CustomFormFactory() {

            @Override
            public DynamicForm create() {
                DynamicForm form = new DynamicForm();
                form.setNumCols(4);
                TextItem udc = new TextItem(PageDataSource.FIELD_CLASSIFICATION_UDC, "UDC"); // MDT in czech
                TextItem ddc = new TextItem(PageDataSource.FIELD_CLASSIFICATION_DDC, "DDC"); // DDT in czech
                form.setFields(udc, ddc);
                return form;
            }
        });
        oneRow(subjects);

        RepeatableFormItem keywords = new RepeatableFormItem("keywords", "keywords",
                new StringFormFactory("value", null, false));

        RepeatableFormItem physicalDescriptions = new RepeatableFormItem(
                PageDataSource.FIELD_PHYSICAL_DESCRIPTIONS, "Physical Descriptions",
                new CustomFormFactory() {

            @Override
            public DynamicForm create() {
                DynamicForm form = new DynamicForm();
                form.setNumCols(4);
                TextItem extent = new TextItem(PageDataSource.FIELD_PHYSICAL_DESCRIPTIONS_EXTENT, "Extent"); // rozsah
                TextItem size = new TextItem(PageDataSource.FIELD_PHYSICAL_DESCRIPTIONS_SIZE, "Size"); // Rozmery
                form.setFields(extent, size);
                return form;
            }
        });
        oneRow(physicalDescriptions);

        TextItem recordOrigin = new TextItem("recordOrigin");

        TextAreaItem note = new TextAreaItem(PageDataSource.FIELD_NOTE, "Note");
        note.setWidth("*");
        note.setHeight("*");
        note.setColSpan("*");
        oneRow(note);
        note.setMinHeight(50);

        setFields(identifiers, sigla, shelfLocators, periodicity, titles, subtitles,
                alternativeTitles, keyTitles, authors, contribs, printers, publishers,
                languages, keywords, subjects, physicalDescriptions, recordOrigin, note
                );
        setDataSource(PageDataSource.getInstance());
    }

    private static void oneRow(FormItem fi) {
        fi.setEndRow(true);
        fi.setStartRow(true);
        fi.setColSpan("*");
    }

    private static final class StringFormFactory implements CustomFormFactory {
        private final String title;
        private final boolean showTitle;
        private final String name;
        private final int width;

        public StringFormFactory(String name, String title, boolean showTitle) {
            this(name, title, showTitle, Integer.MIN_VALUE);
        }
        public StringFormFactory(String name, String title, boolean showTitle, int width) {
            this.title = title;
            this.showTitle = showTitle;
            this.name = name;
            this.width = width;
        }

        @Override
        public DynamicForm create() {
            DynamicForm form = new DynamicForm();
            TextItem value = new TextItem(name, title);
            value.setShowTitle(showTitle);
            if (width == Integer.MAX_VALUE) {
                value.setWidth("100%");
                form.setWidth100();
            } else if (width > 0) {
                value.setWidth(width);
            }
            form.setFields(value);
            return form;
        }
    }

    private static final class PersonFormFactory implements CustomFormFactory {

        @Override
        public DynamicForm create() {
            DynamicForm form = new DynamicForm();
            form.setNumCols(4);
            TextItem family = new TextItem("family", "Family");
            family.setPrompt("Used for the surname used to identify members of the same family.");
            TextItem given = new TextItem("given", "Given");
            given.setPrompt("Used for a forename or first name.");
            form.setFields(family, given);
            return form;
        }
    }

    private static final class PrinterPublisherFormFactory implements CustomFormFactory {

        private final boolean publisher;

        public PrinterPublisherFormFactory(boolean publisher) {
            this.publisher = publisher;
        }

        @Override
        public DynamicForm create() {
            DynamicForm form = new DynamicForm();
            form.setNumCols(6);
            TextItem place = new TextItem(PageDataSource.FIELD_PRINTER_PUBLISHER_PLACE, "Place");
            place.setPrompt("Place of publication.");
            TextItem name = new TextItem(PageDataSource.FIELD_PRINTER_PUBLISHER_NAME, "Name");
            name.setPrompt(publisher ? "Publisher name." : "Printer name.");
            DateItem date = new DateItem(PageDataSource.FIELD_PRINTER_PUBLISHER_DATE, "Date");
            date.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATE);
            date.setUseTextField(true);
            form.setFields(name, place, date);
            return form;
        }
    }



}
