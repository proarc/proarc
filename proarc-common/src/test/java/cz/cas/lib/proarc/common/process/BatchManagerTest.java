package cz.cas.lib.proarc.common.process;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import java.util.Arrays;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BatchManagerTest {

    @Test
    void importantExportParamsIncludeCollectionsAndModsUpdate() {
        BatchParams params = new BatchParams();
        params.setCollections(Arrays.asList("collection:one", "collection:two"));
        params.setUpdateMods(true);

        String result = BatchManager.getImportantParams(params, Batch.EXPORT_KRAMERIUS);

        assertEquals(
                "Sbírky: [collection:one, collection:two]\n"
                        + "Aktualizace MODS: Ano\n",
                result
        );
    }

    @Test
    void importantExportParamsIncludeDisabledModsUpdate() {
        BatchParams params = new BatchParams();
        params.setUpdateMods(false);

        String result = BatchManager.getImportantParams(params, Batch.EXPORT_KRAMERIUS);

        assertEquals("Aktualizace MODS: Ne\n", result);
    }
}
