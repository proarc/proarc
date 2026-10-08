package cz.cas.lib.proarc.common.process.internal;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.InternalExternalProcess;
import java.util.Collections;
import java.util.Locale;
import java.util.logging.Level;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

class ValidationBatchTest {

    @Mocked BatchManager manager;
    @Mocked ValidationProcess validationProcess;

    @Test
    void validationWithoutIssuesCompletesSuccessfully() throws Exception {
        assertResult(Batch.State.INTERNAL_DONE);
    }

    @Test
    void validationWithOnlyWarningsCompletesWithWarning() throws Exception {
        assertResult(Batch.State.INTERNAL_WARNING, Level.WARNING, Level.WARNING);
    }

    @Test
    void validationWithErrorFails() throws Exception {
        assertResult(Batch.State.INTERNAL_FAILED, Level.SEVERE);
    }

    @Test
    void errorTakesPrecedenceOverWarningsInEitherOrder() throws Exception {
        assertResult(Batch.State.INTERNAL_FAILED, Level.WARNING, Level.SEVERE);
        assertResult(Batch.State.INTERNAL_FAILED, Level.SEVERE, Level.WARNING);
    }

    private void assertResult(Batch.State expectedState, Level... levels) throws Exception {
        ValidationProcess.Result validationResult = new ValidationProcess.Result();
        for (Level level : levels) {
            validationResult.getValidationResults().add(
                    new ValidationProcess.ValidationResult("uuid:test", level.getName(), level));
        }
        Batch batch = new Batch();
        batch.setProfileId(Batch.INTERNAL_VALIDATION);
        batch.setFolder("uuid:test");
        BatchParams params = new BatchParams();
        params.setPids(Collections.singletonList("uuid:test"));
        batch.setParamsFromObject(params);

        new Expectations() {{
            manager.update(batch); result = batch;
            validationProcess.validate(ValidationProcess.Type.VALIDATION); result = validationResult;
        }};

        InternalExternalProcess process = InternalExternalProcess.prepare(
                null, null, batch, manager, null, null, Locale.ROOT);
        assertSame(batch, process.start());
        assertEquals(expectedState, batch.getState());
        assertEquals("uuid:test", batch.getFolder());
        if (levels.length == 0) {
            assertNull(batch.getLog());
        } else {
            assertEquals(validationResult.getMessages(), batch.getLog());
        }
        new Verifications() {{ validationProcess.indexResult(batch); times = 1; }};
    }
}
