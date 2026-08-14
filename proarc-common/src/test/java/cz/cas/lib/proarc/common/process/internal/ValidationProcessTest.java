package cz.cas.lib.proarc.common.process.internal;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ValidationProcessTest {

    @Test
    public void acceptsIssueYearFromParentYearRange() {
        assertTrue(ValidationProcess.isDateIssuedValid("1930-1931", "1930"));
        assertTrue(ValidationProcess.isDateIssuedValid("1930-1931", "1931"));

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
    }

    @Test
    public void preservesExactDateIssuedComparison() {
        assertTrue(ValidationProcess.isDateIssuedValid("1930", "1930"));
        assertFalse(ValidationProcess.isDateIssuedValid("1930", "1931"));
        assertFalse(ValidationProcess.isDateIssuedValid("1930/1931", "1930"));
    }
}
