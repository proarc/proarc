package cz.cas.lib.proarc.common.externalApp.kramerius;

import java.io.File;
import java.io.IOException;
import java.util.List;
import org.json.JSONException;

interface KrameriusImporter {

    KUtils.ImportState importToKramerius(
            File exportFolder,
            boolean updateExisting,
            String exportType,
            String policy,
            String license,
            boolean updateMods,
            List<String> collections
    ) throws JSONException, IOException, InterruptedException;
}
