/*
 * Copyright (C) 2020 Jakub Kremlacek
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
package cz.cas.lib.proarc.common.export;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.ExportKWISPostProcess;
import cz.cas.lib.proarc.common.process.ExternalProcess;

import java.io.IOException;

/**
 * @author Jakub Kremlacek
 */
public final class KWISExport {

    private final AppConfiguration config;

    private final String imagesPath;
    private final String k4Path;
    private final String exportPath;

    /**
     * \
     * Creates new KWISExport
     *
     * @param appConfig  application configuration - used for external process configuration
     * @param imagesPath absolute path to image export
     * @param k4Path     absolute path to kramerius export
     * @param exportPath absolute path to final package
     */
    public KWISExport(AppConfiguration appConfig, String imagesPath, String k4Path, String exportPath) {
        this.config = appConfig;
        this.imagesPath = imagesPath;
        this.k4Path = k4Path;
        this.exportPath = exportPath;
    }

    public void run() throws IOException {
        ExternalProcess process = new ExportKWISPostProcess(config.getExportPostProcessor(), imagesPath, k4Path, exportPath);

        if (process != null) {
            process.run();

            if (!process.isOk()) {
                throw new IOException("Processing K4 Export failed. \n " + process.getFullOutput());
            }
        }
    }
}