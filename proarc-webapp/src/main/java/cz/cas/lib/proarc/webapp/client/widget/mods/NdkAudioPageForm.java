/*
 * Copyright (C) 2018 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.client.widget.mods;

import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.form.validator.IsIntegerValidator;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ValueMapDataSource;
import cz.cas.lib.proarc.webapp.client.ds.mods.IdentifierDataSource;
import cz.cas.lib.proarc.webapp.client.widget.StringTrimValidator;

import java.util.Arrays;
import java.util.logging.Logger;

public class NdkAudioPageForm extends AbstractModelForm{

    private static final Logger LOG = Logger.getLogger(NdkAudioPageForm.class.getName());

    public NdkAudioPageForm(ClientMessages i18n) {
        this(i18n, BundleName.MODS_AUDIO_PAGE_TYPES.getValueMapId());
    }

    public NdkAudioPageForm(ClientMessages i18n, BundleName typeBundle) {
        this(i18n, typeBundle.getValueMapId());
    }

    /**
     * Create a new form.
     * @param i18n I18N
     * @param typeValueMapId {@link ValueMapDataSource#getOptionDataSource}
     *          reference to bundle with page types.
     */
    public NdkAudioPageForm(ClientMessages i18n, String typeValueMapId) {
        // save on Enter is supposed mainly for ImportBatchItemEditor
        // see submit handler in ModsMultiEditor
        setSaveOnEnter(true);
        setWidth100();
        setHeight100();
        setTitleOrientation(TitleOrientation.TOP);

        TextItem pageType = new TextItem(ModsCustomDataSource.FIELD_PAGE_TYPE);
        pageType.setTitle(i18n.PageForm_PageType_Title());
        pageType.setEndRow(true);
        pageType.setRequired(true);
        pageType.setReadOnly();
        pageType.disable();

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
        //https://github.com/proarc/proarc/issues/645 GWT v.6.0-p20161023 form draw issue
        note.setHeight("*");
        note.setColSpan("*");

        setFields(pageType, pageIndex, pageNumber, identifiers, note);

        IntegerRangeValidator integerRangeValidator = new IntegerRangeValidator();
        integerRangeValidator.setMin(0);
        integerRangeValidator.setMax(Integer.MAX_VALUE);

        pageIndex.setValidators(integerRangeValidator);
    }

}
