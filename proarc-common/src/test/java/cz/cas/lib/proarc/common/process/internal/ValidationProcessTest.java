package cz.cas.lib.proarc.common.process.internal;

import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.ndk.ModsRules;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ValidationProcessTest {

    private static final String[] SUPPORTED_DATES = {
            "27.01.1998",
            "01.1998",
            "1998",
            "1998-1999",
            "01.-02.1998",
            "12.1998-01.1999",
            "31.01.-01.02.1998",
            "31.12.1998-01.01.1999",
            "02.-03.02.1998"
    };

    @Test
    public void acceptsAllSupportedDateIssuedFormats() {
        for (String date : SUPPORTED_DATES) {
            assertTrue(ModsRules.DatumValidator.isValid(date), date);
        }
    }

    @Test
    public void acceptsAllSupportedIssueAndSupplementDatesForVolumeYear() {
        for (String date : SUPPORTED_DATES) {
            assertTrue(ValidationProcess.isDateIssuedValid("1998", date), date);
            assertTrue(ModsRules.DatumValidator.isDateIssuedValid("1998", date), date);
        }
    }

    @Test
    public void acceptsAllSupportedSupplementDatesForMatchingIssue() {
        for (String date : SUPPORTED_DATES) {
            assertTrue(ModsRules.DatumValidator.isDateIssuedValid(date, date), date);
        }
    }

    @Test
    public void acceptsIssueYearFromParentYearRange() {
        assertTrue(ValidationProcess.isDateIssuedValid("1930-1931", "1930"));
        assertTrue(ValidationProcess.isDateIssuedValid("1930-1931", "1931"));
        assertTrue(ValidationProcess.isDateIssuedValid("1930-1931", "07.01.1931"));

        assertTrue(ValidationProcess.isDateIssuedValid("1930-1933", "1930"));
        assertTrue(ValidationProcess.isDateIssuedValid("1930-1933", "1931"));
        assertTrue(ValidationProcess.isDateIssuedValid("1930-1933", "1932"));
        assertTrue(ValidationProcess.isDateIssuedValid("1930-1933", "1933"));
    }

    @Test
    public void rejectsIssueYearOutsideParentYearRange() {
        assertFalse(ValidationProcess.isDateIssuedValid("1930-1933", "1929"));
        assertFalse(ValidationProcess.isDateIssuedValid("1930-1933", "1934"));
        assertFalse(ValidationProcess.isDateIssuedValid("1933-1930", "1931"));
        assertFalse(ValidationProcess.isDateIssuedValid("1998", "01.2000"));
        assertFalse(ModsRules.DatumValidator.isValid("1999-1998"));
    }

    @Test
    public void preservesExactDateIssuedComparison() {
        assertTrue(ValidationProcess.isDateIssuedValid("1930", "1930"));
        assertTrue(ValidationProcess.isDateIssuedValid("07.01.1992", "07.01.1992"));
        assertFalse(ValidationProcess.isDateIssuedValid("1930", "1931"));
        assertFalse(ValidationProcess.isDateIssuedValid("1930/1931", "1930"));
        assertFalse(ValidationProcess.isDateIssuedValid("07.01.1992", "08.01.1992"));
    }

    @Test
    void identifiesModelsThatCanContainPages() {
        assertTrue(ValidationProcess.canContainPage(NdkPlugin.MODEL_PERIODICALISSUE));
        assertFalse(ValidationProcess.canContainPage(NdkPlugin.MODEL_PAGE));
        assertFalse(ValidationProcess.canContainPage(null));
    }
}
