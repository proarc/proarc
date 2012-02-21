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

import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.mods.IdentifierDataSource;
import cz.incad.pas.editor.client.ds.mods.PageDataSource;
import java.util.LinkedHashMap;
import java.util.logging.Logger;

/**
 * Simple form to edit MODS of monograph object.
 *
 * @author Jan Pokorsky
 */
public final class MonographForm extends DynamicForm {

    private static final Logger LOG = Logger.getLogger(MonographForm.class.getName());

    public MonographForm(final PasEditorMessages i18nPas) {
        setWidth100();
        setHeight100();
        setTitleOrientation(TitleOrientation.TOP);
        setNumCols(2);

        // identifiers
        final RepeatableFormItem identifiers = new RepeatableFormItem(PageDataSource.FIELD_IDENTIFIERS,
                i18nPas.PeriodicalForm_Identifiers_Title());
        identifiers.setDataSource(IdentifierDataSource.getInstance());
        DynamicForm identifierForm = new DynamicForm();
        identifierForm.setUseAllDataSourceFields(true);
        identifierForm.setNumCols(4);
        identifiers.setFormPrototype(identifierForm);
        identifiers.setEndRow(true);
        identifiers.setColSpan("2");

        TextItem sigla = new TextItem(PageDataSource.FIELD_SIGLA, i18nPas.PeriodicalForm_Sigla_Title());

        RepeatableFormItem shelfLocators = new RepeatableFormItem(
                PageDataSource.FIELD_SHELF_LOCATORS, i18nPas.PeriodicalForm_ShelfLocators_Title(),
                new StringFormFactory(PageDataSource.FIELD_STRING_VALUE, null, false));
//        shelfLocators.setRowSpan(2);

        // titles
        RepeatableFormItem titles = new RepeatableFormItem(
                PageDataSource.FIELD_TITLES, i18nPas.PeriodicalForm_Titles_Title(),
                new StringFormFactory(PageDataSource.FIELD_STRING_VALUE, null, false, 600));
        oneRow(titles);

        RepeatableFormItem subtitles = new RepeatableFormItem(
                PageDataSource.FIELD_SUBTITLES, i18nPas.PeriodicalForm_Subtitles_Title(),
                new StringFormFactory(PageDataSource.FIELD_STRING_VALUE, null, false, 600));
        oneRow(subtitles);

        RepeatableFormItem alternativeTitles = new RepeatableFormItem(
                PageDataSource.FIELD_ALTERNATIVE_TITLES, i18nPas.PeriodicalForm_AlternativeTitles_Title(),
                new StringFormFactory(PageDataSource.FIELD_STRING_VALUE, null, false, 600));
        oneRow(alternativeTitles);

        // authors
        RepeatableFormItem authors = new RepeatableFormItem(PageDataSource.FIELD_AUTHORS,
                i18nPas.PeriodicalForm_Authors_Title(), new PersonFormFactory(i18nPas));
        oneRow(authors);
        RepeatableFormItem contribs = new RepeatableFormItem(PageDataSource.FIELD_CONTRIBUTORS,
                i18nPas.PeriodicalForm_Contributors_Title(), new PersonFormFactory(i18nPas));
        oneRow(contribs);
        RepeatableFormItem printers = new RepeatableFormItem(PageDataSource.FIELD_PRINTERS,
                i18nPas.PeriodicalForm_Printers_Title(), new PrinterPublisherFormFactory(false, i18nPas));
        oneRow(printers);
        RepeatableFormItem publishers = new RepeatableFormItem(PageDataSource.FIELD_PUBLISHERS,
                i18nPas.PeriodicalForm_Publishers_Title(), new PrinterPublisherFormFactory(true, i18nPas));
        oneRow(publishers);

        RepeatableFormItem languages = new RepeatableFormItem(PageDataSource.FIELD_LANGUAGES,
                i18nPas.PeriodicalForm_Languages_Title(), new RepeatableFormItem.CustomFormFactory() {

            @Override
            public DynamicForm create() {
                DynamicForm form = new DynamicForm();
//                form.setNumCols(6);
                ComboBoxItem language = new ComboBoxItem("languageCode", i18nPas.PeriodicalForm_LanguageCode_Title());
                LinkedHashMap<String, String> valueMap = new LinkedHashMap<String, String>();
                valueMap.put("cze", "Czech - cze");
                valueMap.put("eng", "English - eng");
                valueMap.put("ger", "German - ger ");
                language.setValueMap(valueMap);
                form.setFields(language);
                return form;
            }
        });

        RepeatableFormItem subjects = new RepeatableFormItem(PageDataSource.FIELD_CLASSIFICATIONS,
                i18nPas.PeriodicalForm_Subjects_Title(), new RepeatableFormItem.CustomFormFactory() {

            @Override
            public DynamicForm create() {
                DynamicForm form = new DynamicForm();
                form.setNumCols(4);
                TextItem udc = new TextItem(PageDataSource.FIELD_CLASSIFICATION_UDC,
                        i18nPas.PeriodicalForm_SubjectsUdc_Title()); // MDT in czech
                TextItem ddc = new TextItem(PageDataSource.FIELD_CLASSIFICATION_DDC,
                        i18nPas.PeriodicalForm_SubjectsDdc_Title()); // DDT in czech
                form.setFields(udc, ddc);
                return form;
            }
        });
        oneRow(subjects);

        RepeatableFormItem keywords = new RepeatableFormItem(PageDataSource.FIELD_KEYWORDS,
                i18nPas.PeriodicalForm_Keywords_Title(),
                new StringFormFactory(PageDataSource.FIELD_STRING_VALUE, null, false));

        RepeatableFormItem physicalDescriptions = new RepeatableFormItem(
                PageDataSource.FIELD_PHYSICAL_DESCRIPTIONS,
                i18nPas.PeriodicalForm_PhysicalDescriptions_Title(),
                new RepeatableFormItem.CustomFormFactory() {

            @Override
            public DynamicForm create() {
                DynamicForm form = new DynamicForm();
                form.setNumCols(4);
                TextItem extent = new TextItem(PageDataSource.FIELD_PHYSICAL_DESCRIPTIONS_EXTENT,
                        i18nPas.PeriodicalForm_PhysicalDescriptionsExtent_Title()); // rozsah
                TextItem size = new TextItem(PageDataSource.FIELD_PHYSICAL_DESCRIPTIONS_SIZE,
                        i18nPas.PeriodicalForm_PhysicalDescriptionsSize_Title()); // Rozmery
                form.setFields(extent, size);
                return form;
            }
        });
        oneRow(physicalDescriptions);

        TextItem recordOrigin = new TextItem(PageDataSource.FIELD_RECORD_ORIGIN,
                i18nPas.PeriodicalForm_RecordOrigin_Title());
        recordOrigin.setWidth("*");
        oneRow(recordOrigin);

        TextAreaItem note = new TextAreaItem(PageDataSource.FIELD_NOTE,
                i18nPas.PeriodicalForm_Note_Title());
        note.setWidth("*");
        note.setHeight("*");
        note.setColSpan("*");
        oneRow(note);
        note.setMinHeight(50);

        setFields(identifiers, sigla, shelfLocators, titles, subtitles, alternativeTitles, authors,
                contribs, printers, publishers, languages, subjects, keywords,
                physicalDescriptions, recordOrigin, note);
        
        setDataSource(PageDataSource.getInstance());
    }

    private static void oneRow(FormItem fi) {
        fi.setEndRow(true);
        fi.setStartRow(true);
        fi.setColSpan("*");
    }

}
