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
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

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
    public static final String TEXTAREA = "textArea";

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
    private final Field optionDataSource;
    private final String[] valueFieldNames;
    private Map<String, String> valueFieldMap;
    private List<Field> fields;
    private transient Field parent;

    public Field(String name, String type, List<Localized> title,
            List<Localized> hint, Integer maxOccurrences,
            Boolean required, Boolean hidden, Boolean readOnly,
            Integer length, String width,
            LinkedHashMap<String, String> valueMap,
            Field optionDataSource, String[] valueFieldNames, Map<String, String> valueFieldMap,
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
        this.optionDataSource = optionDataSource;
        this.valueFieldNames = valueFieldNames;
        this.valueFieldMap = valueFieldMap;
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

    /**
     * Gets field describing data source.
     * The name stands for {@link cz.cas.lib.proarc.webapp.client.ds.ValueMapDataSource}
     * mapId. Child fields describe columns of ListGrid.
     */
    public Field getOptionDataSource() {
        return optionDataSource;
    }

    /**
     * Gets options data source field name(s) holding selected value(s).
     * Gets {@code array.length > 1} in case the selected values should be copied to other form fields.
     * Use {@link #getOptionValueFieldMap() } in case of option names differs from form field names
     * and there is more then one option field to copy.
     */
    public String[] getOptionValueField() {
        return valueFieldNames;
    }

    /**
     * Gets mapping of options data source field names to sibling field names.
     * Used by select or combo pick lists to put selected values to matching fields
     * in case options data source field names differs from the target form field names.
     * <p>It supports target fields described as a path to nested field with {@code '/'} as separator.
     * E.g. map {@code "code" -> "nestedFormFieldName/internalCode"}.
     */
    public Map<String, String> getOptionValueFieldMap() {
        return valueFieldMap;
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
                + ", valueMap=" + valueMap
                + ", optionDataSource=" + optionDataSource + ", valueFields=" + Arrays.toString(valueFieldNames)
                + '}';
    }

}
