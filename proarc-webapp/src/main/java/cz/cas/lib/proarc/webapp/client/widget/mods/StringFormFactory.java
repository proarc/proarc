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
package cz.cas.lib.proarc.webapp.client.widget.mods;

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.IsStringValidator;
import cz.cas.lib.proarc.webapp.client.widget.StringTrimValidator;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.CustomFormFactory;

/**
 * Repeatable form factory to present list of string values.
 *
 * @author Jan Pokorsky
 */
final class StringFormFactory implements CustomFormFactory {
    
    private final String title;
    private final boolean showTitle;
    private final String name;
    private final int width;
    private Boolean required;

    public StringFormFactory(String name, String title, boolean showTitle) {
        this(name, title, showTitle, Integer.MIN_VALUE);
    }

    public StringFormFactory(String name, String title, boolean showTitle, int width) {
        this.title = title;
        this.showTitle = showTitle;
        this.name = name;
        this.width = width;
    }

    public StringFormFactory required(boolean required) {
        this.required = required;
        return this;
    }

    @Override
    public DynamicForm create() {
        DynamicForm form = new DynamicForm();
        TextItem value = new TextItem(name, title);
        value.setShowTitle(showTitle);
        if (width == Integer.MAX_VALUE) {
            value.setWidth("100%");
            form.setWidth100();
        } else if (width > 0) {
            value.setWidth(width);
        }
        value.setRequired(required);
        value.setValidators(new StringTrimValidator());
        form.setFields(value);
        return form;
    }

}
