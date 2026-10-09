package cz.cas.lib.proarc.common.externalApp.kramerius;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.*;

class CatalogImportConfirmationTest {
    @Test
    void requiresConfirmedIndexingForBothSuccessfulAndWarningImports() {
        for (String process : new String[]{KRAMERIUS_PROCESS_FINISHED, KRAMERIUS_PROCESS_WARNING}) {
            assertTrue(new ImportState(process, KRAMERIUS_BATCH_FINISHED_V5).isImportConfirmed());
            assertTrue(new ImportState(process, KRAMERIUS_BATCH_FINISHED_V7).isImportConfirmed());
            for (String incomplete : new String[]{KRAMERIUS_BATCH_NO_BATCH_V5, KRAMERIUS_BATCH_RUNNING_V7,
                    KRAMERIUS_BATCH_FAILED_V5, KRAMERIUS_BATCH_FAILED_V7, "unknown"}) {
                assertFalse(new ImportState(process, incomplete).isImportConfirmed());
            }
        }
        assertFalse(new ImportState(KRAMERIUS_PROCESS_FAILED, KRAMERIUS_BATCH_FINISHED_V7).isImportConfirmed());
    }
}
