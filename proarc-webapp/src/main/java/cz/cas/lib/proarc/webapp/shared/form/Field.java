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
 * Describes field of a form.
 */
public class Field {

    public static final String COMBO = "combo";
    public static final String DATE = "date";
    public static final String G_YEAR = "gYear";
    public static final String CUSTOM_FORM = "customform";
    public static final String RADIOGROUP = "radiogroup";
    public static final String SELECT = "select";
    public static final String TEXT = "text";

    private String name;
    private String type;
    private List<Localized> title;
    private List<Localized> hint;
    private Integer maxOccurrences;
    private Boolean required;
    private Boolean hidden;
    private Boolean readOnly;
    private String width;
    private Integer length;
    private LinkedHashMap<String, String> valueMap;
    private List<Field> fields;
    private transient Field parent;

    public Field(String name, String type, List<Localized> title,
            List<Localized> hint, Integer maxOccurrences,
            Boolean required, Boolean hidden, Boolean readOnly,
            Integer length, String width,
            LinkedHashMap<String, String> valueMap,
            List<Field> fields) {

        this.name = name;
        this.type = type;
        this.title = title;
        this.hint = hint;
        this.maxOccurrences = maxOccurrences;
        this.required = required;
        this.hidden = hidden;
        this.readOnly = readOnly;
        this.length = length;
        this.width = width;
        this.valueMap = valueMap;
        this.fields = fields != null ? fields : new ArrayList<Field>();
        for (Field child : this.fields) {
            child.parent = this;
        }
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }

    public String getTitle(String locale) {
        return Localized.getElement(getTitle(), locale);
    }

    public List<Localized> getTitle() {
        return title;
    }

    public String getHint(String locale) {
        return Localized.getElement(getHint(), locale);
    }

    public List<Localized> getHint() {
        return hint;
    }

    public Integer getMaxOccurrences() {
        return maxOccurrences;
    }

    public Boolean getRequired() {
        return required;
    }

    public Boolean getHidden() {
        return hidden;
    }

    public Boolean getReadOnly() {
        return readOnly;
    }

    public Integer getLength() {
        return length;
    }

    public String getWidth() {
        return width;
    }

    public LinkedHashMap<String, String> getValueMap() {
        return valueMap;
    }

    public List<Field> getFields() {
        return fields;
    }

    public Field getMember(String name) {
        if (fields != null && name != null) {
            for (Field field : fields) {
                if (name.equals(field.getName())) {
                    return field;
                }
            }
        }
        return null;
    }

    public Field getParent() {
        return parent;
    }

    @Override
    public String toString() {
        return "Field{" + "name=" + name + ", type=" + type + ", title=" + title
                + ", hint=" + hint + ", maxOccurrences=" + maxOccurrences
                + ", required=" + required + ", hidden=" + hidden + ", readOnly=" + readOnly
                + ", width=" + width + ", length=" + length
                + ", valueMap=" + valueMap + '}';
    }

}
