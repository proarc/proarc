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
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.form.validator.IsIntegerValidator;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.ds.mods.IdentifierDataSource;
import cz.incad.pas.editor.client.widget.StringTrimValidator;
import java.util.Arrays;
import java.util.logging.Logger;

/**
 * Simple form to edit MODS page.
 *
 * @author Jan Pokorsky
 */
public final class PageForm extends AbstractModelForm {

    private static final Logger LOG = Logger.getLogger(PageForm.class.getName());

    public PageForm(ClientMessages i18n) {
        // save on Enter is supposed mainly for ImportBatchItemEditor
        // see submit handler in ModsMultiEditor
        setSaveOnEnter(true);
        setWidth100();
        setHeight100();
        setTitleOrientation(TitleOrientation.TOP);
        SelectItem pageType = new SelectItem(ModsCustomDataSource.FIELD_PAGE_TYPE, i18n.PageForm_PageType_Title());
        pageType.setValueMap(ModsCustomDataSource.getPageTypes());
        pageType.setDefaultValue(ModsCustomDataSource.getDefaultPageType());
        pageType.setWidth(200);
        pageType.setEndRow(true);

        IntegerItem pageIndex = new IntegerItem(ModsCustomDataSource.FIELD_PAGE_INDEX);
        pageIndex.setTitle(i18n.PageForm_PageIndex_Title());
        pageIndex.setValidators(new IsIntegerValidator());
        pageIndex.setRequired(true);
        pageIndex.setEndRow(true);

        TextItem pageNumber = new TextItem(ModsCustomDataSource.FIELD_PAGE_NUMBER);
        pageNumber.setTitle(i18n.PageForm_PageNumber_Title());
        pageNumber.setEndRow(true);
        pageNumber.setRequired(true);
        pageNumber.setValidators(new StringTrimValidator());
//        pageNumber.setLength(5);

        final RepeatableFormItem identifiers = new RepeatableFormItem(ModsCustomDataSource.FIELD_IDENTIFIERS,
                i18n.PageForm_Identifiers_Title());
        identifiers.setDataSource(IdentifierDataSource.getInstance());
        identifiers.setRequired(true);
        identifiers.setValidators(
                new IdentifiersValidator(i18n, Arrays.asList(IdentifierDataSource.TYPE_UUID)));
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

        setFields(pageType, pageIndex, pageNumber, identifiers, note);

        IntegerRangeValidator integerRangeValidator = new IntegerRangeValidator();
        integerRangeValidator.setMin(0);
        integerRangeValidator.setMax(Integer.MAX_VALUE);

        pageIndex.setValidators(integerRangeValidator);
    }

}
