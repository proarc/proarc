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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cas.lib.proarc.webapp.client.widget.form;

import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import java.util.ArrayList;
import java.util.List;

/**
 * UUID validator with custom messages thrown via SC.warn,
 * for other channel reporting override reportMessage(String message)
 *
 * @author Jakub Kremlacek
 */
public class CustomUUIDValidator extends CustomValidator {

    private final ClientMessages i18n;

    private final String INVALID_UUID_FORMAT;
    private final String INVALID_UUID_LENGTH_SHORT;
    private final String INVALID_UUID_LENGTH_LONG;
    private final String INVALID_UUID_MISSING_PREFIX;

    private static final int UUID_LENGTH = 32;
    private static final int UUID_PREFIX_LENGTH = 5;
    private static final int UUID_DASH_COUNT = 4;
    private static final int[] UUID_SEGMENT_LENGTHS = {8,4,4,4,12};

    private static final String UUID_PREFIX = "uuid:";
    private static final String UUID_REGEXP = "[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{12}";
    private static final String UUID_INVALID_CHAR_REGEXP = "((uuid:)[A-Fa-f0-9-]*[^A-Fa-f0-9-])";

    private final RegExp UUID_PATTERN = RegExp.compile(UUID_PREFIX + UUID_REGEXP);
    private final RegExp UUID_INVALID_CHAR_PATTERN = RegExp.compile(UUID_INVALID_CHAR_REGEXP);

    public CustomUUIDValidator(ClientMessages i18n) {
        this.i18n = i18n;

        INVALID_UUID_FORMAT = i18n.Validation_UUID_Invalid();
        INVALID_UUID_LENGTH_SHORT = i18n.Validation_UUID_Invalid_Length_Short();
        INVALID_UUID_LENGTH_LONG = i18n.Validation_UUID_Invalid_Length_Long();
        INVALID_UUID_MISSING_PREFIX = i18n.Validation_UUID_Invalid_Missing_Prefix();
    }

    @Override
    protected boolean condition(Object value) {
        //empty UUID is valid - will be created random new
        if (value == null) {
            return true;
        }

        String line = value.toString();

        //full check
        if (UUID_PATTERN.test(line)) {
            return true;
        }

        String errorMessage = INVALID_UUID_FORMAT;

        //enable empty UUID - will be randomly generated
        if (line.length() == 0) {
            return true;
        }

        //check prefix
        if (!line.startsWith(UUID_PREFIX)) {
            errorMessage += INVALID_UUID_MISSING_PREFIX;
            reportMessage(errorMessage);
            return false;
        }

        //check invalid characters
        MatchResult matcher = UUID_INVALID_CHAR_PATTERN.exec(line);

        if (matcher != null) {
            char invalidChar = matcher.getGroup(1).charAt(matcher.getGroup(1).length() - 1);

            errorMessage += i18n.Validation_UUID_Invalid_Character(String.valueOf(invalidChar));
        }

        //check length
        if (line.length() != UUID_LENGTH + UUID_PREFIX_LENGTH + UUID_DASH_COUNT) {
            if (line.length() < UUID_LENGTH + UUID_PREFIX_LENGTH + UUID_DASH_COUNT) {
                errorMessage += INVALID_UUID_LENGTH_SHORT;
            } else if (line.length() > UUID_LENGTH + UUID_PREFIX_LENGTH + UUID_DASH_COUNT) {
                errorMessage += INVALID_UUID_LENGTH_LONG;
            }
        }

        //check dash count
        List<Integer> pos = new ArrayList<>();
        for (int i = 0; i < line.length(); i++) {
            //note: char == char does not work
            if (line.charAt(i) == '-' ) {
                pos.add(i);
            }
        }

        if (pos.size() != UUID_DASH_COUNT) {
            errorMessage += i18n.Validation_UUID_Invalid_Dash_Count(Integer.toString(pos.size()), Integer.toString(UUID_DASH_COUNT));
        }

        //add EoL for last segment validation
        pos.add(line.length());

        //check segment sizes
        int lastPos = 5;

        for (int i = 0; i < UUID_DASH_COUNT + 1; i++) {
            if (i >= pos.size()) {
                reportMessage(errorMessage);
                return false;
            }

            int segmentLength = line.substring(lastPos, pos.get(i)).length();

            if (segmentLength != UUID_SEGMENT_LENGTHS[i]) {
                errorMessage += i18n.Validation_UUID_Invalid_Segment_Length(Integer.toString(i+1), Integer.toString(segmentLength), Integer.toString(UUID_SEGMENT_LENGTHS[i]));
            }

            lastPos = pos.get(i) + 1;
        }

        reportMessage(errorMessage);
        return false;
    }

    private void reportMessage(String message) {
        SC.warn(message);
    }
}
