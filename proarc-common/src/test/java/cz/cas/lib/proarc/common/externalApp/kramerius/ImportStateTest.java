package cz.cas.lib.proarc.common.externalApp.kramerius;

import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ImportStateTest {

    @ParameterizedTest
    @CsvSource({
            "FINISHED, FINISHED, FINISHED",
            "FINISHED, BATCH_FINISHED, FINISHED",
            "WARNING, FINISHED, WARNING",
            "WARNING, BATCH_FINISHED, WARNING",
            "FINISHED, NO_BATCH, WARNING",
            "WARNING, NO_BATCH, WARNING",
            "WARNING, FAILED, FAILED",
            "WARNING, BATCH_FAILED, FAILED",
            "FINISHED, KILLED, FAILED",
            "FAILED, FINISHED, FAILED",
            "ERROR, BATCH_FINISHED, FAILED",
            "UNKNOWN, FINISHED, FAILED",
            "FINISHED, UNKNOWN, FAILED",
            "WARNING, RUNNING, FAILED"
    })
    void combinesImportAndIndexing(String process, String batch, String expected) {
        assertEquals(expected, new KUtils.ImportState(process, batch).getOutcome());
    }

    @Test
    void waitsForIndexingAfterWarningForBothVersions() {
        for (String batch : List.of("PLANNED", "RUNNING", "BATCH_STARTED")) {
            assertTrue(new KUtils.ImportState("WARNING", batch).isRunning());
            assertTrue(new KUtils.ImportState("FINISHED", batch).isRunning());
        }
        assertFalse(new KUtils.ImportState("WARNING", "FINISHED").isRunning());
        assertFalse(new KUtils.ImportState("FAILED", "RUNNING").isRunning());
    }

    @Test
    void missingStatesAreNotSuccess() {
        assertEquals("FAILED", new KUtils.ImportState(null, "FINISHED").getOutcome());
        assertEquals("FAILED", new KUtils.ImportState("FINISHED", null).getOutcome());
    }

    @Test
    void aggregateDoesNotDependOnResultOrder() {
        assertEquals("FAILED", KUtils.combineOutcomes(List.of("FAILED", "FINISHED", "WARNING")));
        assertEquals("FAILED", KUtils.combineOutcomes(List.of("WARNING", "FINISHED", "FAILED")));
        assertEquals("WARNING", KUtils.combineOutcomes(List.of("WARNING", "FINISHED")));
        assertEquals("WARNING", KUtils.combineOutcomes(List.of("FINISHED", "WARNING")));
        assertEquals("FINISHED", KUtils.combineOutcomes(List.of("FINISHED", "FINISHED")));
        assertEquals("FAILED", KUtils.combineOutcomes(Arrays.asList("FINISHED", null)));
        assertEquals("FAILED", KUtils.combineOutcomes(List.of()));
    }
}
