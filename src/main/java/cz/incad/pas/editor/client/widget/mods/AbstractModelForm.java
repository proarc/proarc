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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.client.widget.mods;

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import java.util.Map;

/**
 * DynamicForm supporting {@link RepeatableFormItem}s.
 *
 * @author Jan Pokorsky
 */
abstract class AbstractModelForm extends DynamicForm {

    @Override
    public void clearErrors(boolean show) {
        RepeatableFormItem.clearErrors(this, show);
        super.clearErrors(show);
    }

    @Override
    public void showErrors() {
//        Map<?, ?> errors = getErrors();
//        boolean empty = errors.isEmpty();
//        RepeatableFormItem.showErrors(this, errors);
        super.showErrors();
    }

    protected void oneRow(FormItem fi) {
        fi.setEndRow(true);
        fi.setStartRow(true);
        fi.setColSpan("*");
    }

}
