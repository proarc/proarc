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
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.widget.StringTrimValidator;
import cz.incad.pas.editor.client.widget.mods.RepeatableFormItem.CustomFormFactory;

/**
 * Repeatable printer/publisher form.
 *
 * @author Jan Pokorsky
 */
final class PrinterPublisherFormFactory implements CustomFormFactory {
    private final boolean publisher;
    private final ClientMessages i18n;

    public PrinterPublisherFormFactory(boolean publisher, ClientMessages i18n) {
        this.publisher = publisher;
        this.i18n = i18n;
    }

    @Override
    public DynamicForm create() {
        DynamicForm form = new DynamicForm();
        form.setNumCols(6);
        TextItem place = new TextItem(ModsCustomDataSource.FIELD_PRINTER_PUBLISHER_PLACE,
                i18n.PeriodicalForm_PrinterPublisherPlace_Title());
        place.setPrompt(i18n.PeriodicalForm_PrinterPublisherPlace_Hint());
        place.setValidators(new StringTrimValidator());
        TextItem name = new TextItem(ModsCustomDataSource.FIELD_PRINTER_PUBLISHER_NAME,
                i18n.PeriodicalForm_PrinterPublisherName_Title());
        name.setPrompt(publisher
                ? i18n.PeriodicalForm_PublisherName_Hint()
                : i18n.PeriodicalForm_PrinterName_Hint());
        name.setValidators(new StringTrimValidator());
        DateItem date = new DateItem(ModsCustomDataSource.FIELD_PRINTER_PUBLISHER_DATE,
                i18n.PeriodicalForm_PrinterPublisherDate_Title());
        date.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATE);
        date.setUseTextField(true);
        form.setFields(name, place, date);
        return form;
    }

}
