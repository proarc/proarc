/*
 * Copyright (C) 2018 Sykora Lukas
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
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.form.validator.IsIntegerValidator;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.i18n.BundleValue;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ValueMapDataSource;
import cz.cas.lib.proarc.webapp.client.ds.mods.IdentifierDataSource;
import cz.cas.lib.proarc.webapp.client.widget.StringTrimValidator;
import java.util.Arrays;

/**
* Simple form to edit MODS Ndk page.
*
* @author Lukas Sykora
*/
public class NdkPageForm extends AbstractModelForm {

    public NdkPageForm(ClientMessages i18n) {
        this(i18n, BundleName.MODS_PAGE_TYPES.getValueMapId());
    }

    /**
     * Create a new ndk page form.
     * @param i18n I18N
     * @param typeValueMapId {@link ValueMapDataSource#getOptionDataSource}
     *          reference to bundle with page types.
     */
    public NdkPageForm(ClientMessages i18n, String typeValueMapId) {
        // save on Enter is supposed mainly for ImportBatchItemEditor
        // see submit handler in ModsMultiEditor
        setSaveOnEnter(true);
        setWidth100();
        setHeight100();
        setTitleOrientation(TitleOrientation.TOP);
        SelectItem pageType = new SelectItem(ModsCustomDataSource.FIELD_PAGE_TYPE, i18n.PageForm_PageType_Title());
        pageType.setOptionDataSource(ValueMapDataSource.getInstance().getOptionDataSource(typeValueMapId));
        pageType.setValueField(BundleValue.KEY);
        pageType.setDisplayField(BundleValue.VALUE);
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

        TextItem title = new TextItem(ModsCustomDataSource.FIELD_PAGE_TITLE);
        title.setTitle("Title - RA");
        title.setEndRow(true);
        title.setRequired(false);
     //   title.setHint("Obsahuje nadpis či název, uvedený na dané straně. Pokud jej strana neobsahuje, je možné element ponechat nevyplněný.");
        title.setWidth(300);

        TextItem subTitle = new TextItem(ModsCustomDataSource.FIELD_PAGE_SUBTITLE);
        subTitle.setTitle("SubTitle - RA");
        subTitle.setEndRow(true);
        subTitle.setRequired(false);
      //  subTitle.setHint("Podnázev strany, pakliže existuje.");
        subTitle.setWidth(300);

        IntegerItem extent = new IntegerItem(ModsCustomDataSource.FIELD_PAGE_EXTENT);
        extent.setTitle("Extent - Start - M");
        extent.setValidators(new IsIntegerValidator());
        extent.setEndRow(true);
        extent.setRequired(true);
        extent.setWidth(300);
        extent.setHint("číslo strany, označující pořadí v reprezentaci.");

        TextItem genre = new TextItem(ModsCustomDataSource.FIELD_PAGE_GENRE, "Genre - M");
        genre.setEndRow(true);
        genre.setRequired(true);
        genre.setWidth(300);
        genre.setDefaultValue("page");
        genre.setHint("page / reprePage");

        TextItem note = new TextItem(ModsCustomDataSource.FIELD_PAGE_NOTE, "Note - O");
        note.setEndRow(true);
        note.setRequired(false);
        note.setWidth(300);
        note.setHint("siglaPage / right / left");

        TextItem typeOfResource = new TextItem(ModsCustomDataSource.FIELD_PAGE_TYPEOFRESOURCE, "Type of Resource - MA");
        typeOfResource.setEndRow(true);
        typeOfResource.setRequired(false);
        typeOfResource.setWidth(300);
        typeOfResource.setDefaultValue("text");
        typeOfResource.setHint("text / image / notated music / cartographic");


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
        TextAreaItem notePhysicalDescription = new TextAreaItem(ModsCustomDataSource.FIELD_NOTE, "Physical Description - Note - O");
        notePhysicalDescription.setWidth("300");
        //https://github.com/proarc/proarc/issues/645 GWT v.6.0-p20161023 form draw issue
        notePhysicalDescription.setHeight("500");
        notePhysicalDescription.setColSpan("200");

        setFields(pageType, pageIndex, pageNumber, identifiers,  title, subTitle, extent, genre, note, typeOfResource, notePhysicalDescription);

        IntegerRangeValidator integerRangeValidator = new IntegerRangeValidator();
        integerRangeValidator.setMin(0);
        integerRangeValidator.setMax(Integer.MAX_VALUE);

        pageIndex.setValidators(integerRangeValidator);
    }
}
