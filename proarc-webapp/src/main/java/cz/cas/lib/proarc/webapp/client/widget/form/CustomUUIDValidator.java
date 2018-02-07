package cz.cas.lib.proarc.webapp.client.widget.form;

import com.google.gwt.regexp.shared.RegExp;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Jakub Kremlacek
 */
public class CustomUUIDValidator extends CustomValidator {

    private final ClientMessages i18n;

    private final String INVALID_UUID_FORMAT;
    private final String INVALID_UUID_LENGTH_SHORT;
    private final String INVALID_UUID_LENGTH_LONG;

    private static final int UUID_LENGTH = 32;
    private static final int UUID_PREFIX_LENGTH = 5;
    private static final int UUID_DASH_COUNT = 4;
    private static final int[] UUID_SEGMENT_LENGTHS = {8,4,4,4,12};

    private static final String UUID_PREFIX = "uuid:";
    private static final String UUID_REGEXP = "[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{12}";

    //note that uuid prefix is not required
    private final RegExp UUID_PATTERN = RegExp.compile(UUID_PREFIX + UUID_REGEXP);

    public CustomUUIDValidator(ClientMessages i18n) {
        this.i18n = i18n;

        INVALID_UUID_FORMAT = i18n.Validation_Invalid_UUID_Msg();
        INVALID_UUID_LENGTH_SHORT = i18n.Validation_UUID_Invalid_Length_Short();
        INVALID_UUID_LENGTH_LONG = i18n.Validation_UUID_Invalid_Length_Long();
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
            errorMessage += "Missing 'uuid:'";
            reportMessage(errorMessage);
            return false;
        }

        //check length
        if (line.length() != UUID_LENGTH + UUID_PREFIX_LENGTH) {
            if (line.length() < UUID_LENGTH + UUID_PREFIX_LENGTH) {
                errorMessage += INVALID_UUID_LENGTH_SHORT;
            } else if (line.length() > UUID_LENGTH + UUID_PREFIX_LENGTH) {
                errorMessage += INVALID_UUID_LENGTH_LONG;
            }
        }

        //check dashes

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
