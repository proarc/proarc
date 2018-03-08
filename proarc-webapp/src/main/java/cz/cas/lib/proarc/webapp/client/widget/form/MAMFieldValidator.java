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
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import java.util.List;

/**
 * Validator for M field in MA form
 *
 * Requires a list of all other fields within the form for checking their emptiness
 *
 * @author Jakub Kremlacek
 */
public class MAMFieldValidator extends CustomValidator {

    List<FormItem> itemsToCheck;

    public MAMFieldValidator(List<FormItem> items) {
        if (items == null) {
            throw new IllegalArgumentException("items cannot be null");
        }

        this.itemsToCheck = items;
    }

    @Override
    protected boolean condition(Object o) {
        if (o == null || o.toString().equals("")) {
            //empty M field, check if other items are empty

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
