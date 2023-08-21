/*
 * Copyright (C) 2023 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;
import org.codehaus.jettison.json.JSONException;

/**
 * @author Lukáš Sýkora
 * na zaklade aplikace Merlin z MZK
 */
public class KImporter {

    private static final Logger LOG = Logger.getLogger(KImporter.class.getName());

    private final AppConfiguration appConfig;
    private final KrameriusOptions.KrameriusInstance instance;

    public KImporter(AppConfiguration appConfig, KrameriusOptions.KrameriusInstance instance) {
        this.appConfig = appConfig;
        this.instance = instance;
    }

    public KUtils.ImportState importToKramerius(File exportFolder, boolean updateExisting, String exportType, String policy) throws JSONException, IOException, InterruptedException {
        String krameriusVersion = instance.getVersion();
        if (krameriusVersion == null || krameriusVersion.isEmpty()) {
            LOG.severe("Kramerius have to set field \"version\".");
        }
        krameriusVersion = krameriusVersion.replaceAll("[^0-9]", "");
        if (krameriusVersion.startsWith("7")) {
            K7Importer k7Importer = new K7Importer(appConfig, instance);
            return k7Importer.importToKramerius(exportFolder, updateExisting, exportType, policy);
        } else if (krameriusVersion.startsWith("5")) {
            K5Importer k5Importer = new K5Importer(appConfig, instance);
            return k5Importer.importToKramerius(exportFolder, updateExisting, exportType, policy);
        } else {
            LOG.severe("Unknown kramerius version. Expected values are 5.x or 7.x");
            throw new IOException("Unknown kramerius version \"" + instance.getVersion() + "\". Expected values are 5.x or 7.x");
        }
    }
}
