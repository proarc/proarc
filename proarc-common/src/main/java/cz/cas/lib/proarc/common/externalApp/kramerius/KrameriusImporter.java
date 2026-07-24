package cz.cas.lib.proarc.common.externalApp.kramerius;

import java.io.File;
import java.io.IOException;
import org.json.JSONException;

interface KrameriusImporter {

    KUtils.ImportState importToKramerius(
            File exportFolder,
            boolean updateExisting,
            String exportType,
            String policy,
            String license,
            boolean updateMods
    ) throws JSONException, IOException, InterruptedException;
}
