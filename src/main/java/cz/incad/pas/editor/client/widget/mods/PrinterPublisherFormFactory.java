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
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.mods.PageDataSource;
import cz.incad.pas.editor.client.widget.mods.RepeatableFormItem.CustomFormFactory;

/**
 * Repeatable printer/publisher form.
 *
 * @author Jan Pokorsky
 */
final class PrinterPublisherFormFactory implements CustomFormFactory {
    private final boolean publisher;
    private final PasEditorMessages i18nPas;

    public PrinterPublisherFormFactory(boolean publisher, PasEditorMessages i18nPas) {
        this.publisher = publisher;
        this.i18nPas = i18nPas;
    }

    @Override
    public DynamicForm create() {
        DynamicForm form = new DynamicForm();
        form.setNumCols(6);
        TextItem place = new TextItem(PageDataSource.FIELD_PRINTER_PUBLISHER_PLACE,
                i18nPas.PeriodicalForm_PrinterPublisherPlace_Title());
        place.setPrompt(i18nPas.PeriodicalForm_PrinterPublisherPlace_Hint());
        TextItem name = new TextItem(PageDataSource.FIELD_PRINTER_PUBLISHER_NAME,
                i18nPas.PeriodicalForm_PrinterPublisherName_Title());
        name.setPrompt(publisher
                ? i18nPas.PeriodicalForm_PublisherName_Hint()
                : i18nPas.PeriodicalForm_PrinterName_Hint());
        DateItem date = new DateItem(PageDataSource.FIELD_PRINTER_PUBLISHER_DATE,
                i18nPas.PeriodicalForm_PrinterPublisherDate_Title());
        date.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATE);
        date.setUseTextField(true);
        form.setFields(name, place, date);
        return form;
    }

}
