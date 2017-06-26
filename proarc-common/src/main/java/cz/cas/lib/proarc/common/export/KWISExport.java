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