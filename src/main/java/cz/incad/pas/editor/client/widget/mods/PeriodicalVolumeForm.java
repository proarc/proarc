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
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.ds.mods.IdentifierDataSource;

/**
 * Simple form to edit MODS of periodical volume object.
 *
 * @author Jan Pokorsky
 */
public final class PeriodicalVolumeForm extends DynamicForm {

    public PeriodicalVolumeForm(ClientMessages i18n) {
        setWidth100();
        setHeight100();
        setTitleOrientation(TitleOrientation.TOP);
        setNumCols(1);

        TextItem volumeNumber = new TextItem(ModsCustomDataSource.FIELD_PER_VOLUME_NUMBER,
                i18n.PeriodicalVolumeForm_Number_Title());
        TextItem date = new TextItem(ModsCustomDataSource.FIELD_PER_VOLUME_YEAR,
                i18n.PeriodicalVolumeForm_Date_Title());
//        DateItem date = new DateItem(ModsCustomDataSource.FIELD_PER_VOLUME_YEAR, "Date of Issue");
        // work arounds missing DateItemSelectorFormat.YEAR
//        date.setAttribute("selectorFormat", "Y");
//        date.setShowPickerIcon(false);
//        date.setUseTextField(true);
//        date.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATE);
//        date.setEnforceDate(false);
        date.setEndRow(true);

        // identifiers
        final RepeatableFormItem identifiers = new RepeatableFormItem(ModsCustomDataSource.FIELD_IDENTIFIERS,
                i18n.PeriodicalVolumeForm_Identifiers_Title());
        identifiers.setDataSource(IdentifierDataSource.getInstance());
        DynamicForm identifierForm = new DynamicForm();
        identifierForm.setUseAllDataSourceFields(true);
        identifierForm.setNumCols(4);
        identifiers.setFormPrototype(identifierForm);
        identifiers.setEndRow(true);
        identifiers.setColSpan("2");

        TextAreaItem note = new TextAreaItem(ModsCustomDataSource.FIELD_NOTE,
                i18n.PeriodicalVolumeForm_Note_Title());
        note.setWidth("*");
        note.setHeight("*");
        note.setColSpan("*");

        setFields(volumeNumber, date, identifiers, note);
    }

}
