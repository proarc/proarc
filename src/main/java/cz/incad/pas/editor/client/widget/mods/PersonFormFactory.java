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

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextItem;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.widget.mods.RepeatableFormItem.CustomFormFactory;

/**
 * Repeatable person form.
 *
 * @author Jan Pokorsky
 */
final class PersonFormFactory implements CustomFormFactory {
    private final PasEditorMessages i18nPas;

    public PersonFormFactory(PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
    }

    @Override
    public DynamicForm create() {
        DynamicForm form = new DynamicForm();
        form.setNumCols(4);
        TextItem family = new TextItem("family", i18nPas.PeriodicalForm_FamilyName_Title());
        family.setPrompt(i18nPas.PeriodicalForm_FamilyName_Hint());
        TextItem given = new TextItem("given", i18nPas.PeriodicalForm_GivenName_Title());
        given.setPrompt(i18nPas.PeriodicalForm_GivenName_Hint());
        form.setFields(family, given);
        return form;
    }

}
