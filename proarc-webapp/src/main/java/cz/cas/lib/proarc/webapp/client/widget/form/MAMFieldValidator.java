/*
 * Copyright (C) 2018 Jakub Kremlacek
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

package cz.cas.lib.proarc.webapp.client.widget.form;

import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import java.util.List;
import java.util.Map;

/**
 * Validator for M field in MA form
 *
 * Requires a list of all other fields within the form for checking their emptiness
 *
 * @author Jakub Kremlacek
 */
public class MAMFieldValidator extends CustomValidator {

    List<FormItem> itemsToCheck;
    Map<String, String> ignoredTitles;
    RadioGroupItem rdaRadioItem;
    Field field;

    public MAMFieldValidator(List<FormItem> items, Map<String, String> ignoredTitles, RadioGroupItem rdaRadioItem, Field field) {
        if (items == null) {
            throw new IllegalArgumentException("items cannot be null");
        }

        this.itemsToCheck = items;
        this.ignoredTitles = ignoredTitles;
        this.rdaRadioItem = rdaRadioItem;
        this.field = field;
    }

    @Override
    protected boolean condition(Object o) {
        if (o == null || o.toString().equals("")) {
            //empty M field, check if other items are empty

            String ignoredRadioVal = ignoredTitles.get(field.getName());

            if (rdaRadioItem.getValueAsString().equals(ignoredRadioVal)) {
                return true;
            }

            for (FormItem item : itemsToCheck) {
                Object itemValue = item.getValue();

                if (itemValue != null && !"".equals(itemValue.toString())) {
                    return false;
                }
            }
        }

        return true;
    }
}
