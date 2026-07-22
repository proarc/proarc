package cz.cas.lib.proarc.common.process.internal;

import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ValidationProcessTest {

    @Test
    void identifiesModelsThatCanContainPages() {
        assertTrue(ValidationProcess.canContainPage(NdkPlugin.MODEL_PERIODICALISSUE));
        assertFalse(ValidationProcess.canContainPage(NdkPlugin.MODEL_PAGE));
        assertFalse(ValidationProcess.canContainPage(null));
    }
}
