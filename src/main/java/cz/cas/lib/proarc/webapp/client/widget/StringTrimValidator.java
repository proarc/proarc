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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.widgets.form.validator.CustomValidator;

/**
 * Validate string value. It trims leading and trailing white spaces
 * from the input value.
 *
 * @author Jan Pokorsky
 */
public final class StringTrimValidator extends CustomValidator {

    @Override
    protected boolean condition(Object value) {
        boolean valid = true;
        if (value instanceof String) {
            setResultingValue(((String) value).trim());
        } else {
            valid = value == null;
        }
        return valid;
    }

}
