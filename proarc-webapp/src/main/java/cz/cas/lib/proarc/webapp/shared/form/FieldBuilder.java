/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.shared.form;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * The helper class to build form fields programmatically.
 */
public final class FieldBuilder {
    private String name;
    private String type;
    private List<Localized> title = new ArrayList<Localized>();
    private List<Localized> hint = new ArrayList<Localized>();
    private Integer maxOccurrences;
    private Boolean required;
    private Boolean hidden;
    private Boolean readOnly;
    private Integer length;
    private String width;
    private LinkedHashMap<String, String> valueMap = new LinkedHashMap<String, String>();
    private final List<Field> fields = new ArrayList<Field>();
    private Field optionDataSource;
    private String[] valueFieldNames;

    public FieldBuilder(String name) {
        this.name = name;
    }

    public FieldBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public FieldBuilder setType(String type) {
        this.type = type;
        return this;
    }

    public FieldBuilder setTitle(String title) {
        this.title.add(new Localized(null, title));
        return this;
    }

    public FieldBuilder setHint(String hint) {
        this.hint.add(new Localized(null, hint));
        return this;
    }

    public FieldBuilder setWidth(String width) {
        this.width = width;
        return this;
    }

    public FieldBuilder setMaxOccurrences(Integer maxOccurrences) {
        this.maxOccurrences = maxOccurrences;
        return this;
    }

    public FieldBuilder setRequired(Boolean required) {
        this.required = required;
        return this;
    }

    public FieldBuilder setHidden(Boolean hidden) {
        this.hidden = hidden;
        return this;
    }

    public FieldBuilder setReadOnly(Boolean readOnly) {
        this.readOnly = readOnly;
        return this;
    }

    public FieldBuilder setLength(Integer length) {
        this.length = length;
        return this;
    }

    public FieldBuilder addField(Field field) {
        this.fields.add(field);
        return this;
    }

    public FieldBuilder addMapValue(String key, String value) {
        this.valueMap.put(key, value);
        return this;
    }

    public FieldBuilder setOptionDataSource(Field fieldAsDataSource, String... valueFieldNames) {
        this.optionDataSource = fieldAsDataSource;
        this.valueFieldNames = valueFieldNames;
        return this;
    }

    public Field createField() {
        return new Field(name, type, title, hint, maxOccurrences,
                required, hidden, readOnly, length, width,
                valueMap, optionDataSource, valueFieldNames,
                fields);
    }

}
