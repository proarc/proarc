package cz.cas.lib.proarc.common.dao;

import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import java.util.Collections;
import mockit.Mocked;
import mockit.Verifications;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BatchUtilsTest {

    @Test
    void exportWarningIsPersistedAsWarning(@Mocked BatchManager manager) {
        Batch batch = new Batch();
        BatchUtils.finishedExportWithWarning(manager, batch, "export", "warning");
        assertEquals(Batch.State.EXPORT_WARNING, batch.getState());
        assertEquals("warning", batch.getLog());
        new Verifications() {{ manager.update(batch); times = 1; }};
    }

    @Test
    void uploadWarningIsPersistedAsWarning(@Mocked BatchManager manager) {
        Batch batch = new Batch();
        BatchUtils.finishedUploadWithWarning(manager, batch, "kramerius", "warning");
        assertEquals(Batch.State.UPLOAD_WARNING, batch.getState());
        assertEquals("warning", batch.getLog());
        new Verifications() {{ manager.update(batch); times = 1; }};
    }

    @Test
    void allValidationMessagesAreRetainedAndErrorWinsInEitherOrder(@Mocked BatchManager manager) {
        MetsExportException issues = new MetsExportException();
        issues.addException("warning", true);
        issues.addException("error", false);
        for (int i = 0; i < 2; i++) {
            Batch batch = new Batch();
            BatchUtils.finishedExportWithWarning(manager, batch, "export", issues.getExceptions());
            assertEquals(Batch.State.EXPORT_FAILED, batch.getState());
            assertTrue(batch.getLog().contains("warning"));
            assertTrue(batch.getLog().contains("error"));
            Collections.reverse(issues.getExceptions());
        }
    }

    @Test
    void validationWarningsRemainWarnings(@Mocked BatchManager manager) {
        MetsExportException issues = new MetsExportException();
        issues.addException("first", true);
        issues.addException("second", true);
        Batch batch = new Batch();
        BatchUtils.finishedExportWithWarning(manager, batch, "export", issues.getExceptions());
        assertEquals(Batch.State.EXPORT_WARNING, batch.getState());
        assertEquals("first\nsecond", batch.getLog());
    }
}
