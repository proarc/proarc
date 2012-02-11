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
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import cz.incad.pas.editor.client.ds.mods.IdentifierDataSource;
import cz.incad.pas.editor.client.ds.mods.PageDataSource;

/**
 * Simple form to edit MODS of periodical issue object.
 *
 * @author Jan Pokorsky
 */
public final class PeriodicalIssueForm extends DynamicForm {

    public PeriodicalIssueForm() {
        setWidth100();
        setHeight100();
        setTitleOrientation(TitleOrientation.TOP);
        setNumCols(1);

        TextItem issueNumber = new TextItem(PageDataSource.FIELD_PER_ISSUE_NUMBER, "Issue Number");

        TextItem issueSequenceNumber = new TextItem(PageDataSource.FIELD_PER_ISSUE_NUMBER_SORTING, "Issue Sequence Number");

        DateItem date = new DateItem(PageDataSource.FIELD_PER_ISSUE_DATE, "Date");
        date.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATE);
        date.setUseTextField(true);

        // identifiers
        final RepeatableFormItem identifiers = new RepeatableFormItem(PageDataSource.FIELD_IDENTIFIERS, "Identifiers");
        identifiers.setDataSource(IdentifierDataSource.getInstance());
        DynamicForm identifierForm = new DynamicForm();
        identifierForm.setUseAllDataSourceFields(true);
        identifierForm.setNumCols(4);
        identifiers.setFormPrototype(identifierForm);
        identifiers.setEndRow(true);
        identifiers.setColSpan("2");

        TextAreaItem note = new TextAreaItem(PageDataSource.FIELD_NOTE, "Note");
        note.setWidth("*");
        note.setHeight("*");
        note.setColSpan("*");

        setFields(issueNumber, issueSequenceNumber, date, identifiers, note);

        setDataSource(PageDataSource.getInstance());
    }

}
