package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.external.ExternalProcess;
import cz.cas.lib.proarc.common.process.external.OcrGenerator;
import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.common.process.external.OcrGenerator.getOcrFiles;


public class GeneratorAltoOcr implements ImportHandler {

    static Logger LOG = Logger.getLogger(GeneratorAltoOcr.class.getName());

    @Override
    public int estimateItemNumber(ImportProcess.ImportOptions importConfig) throws IOException {
        return 1;
    }

    @Override
    public boolean isImportable(File folder) {
        String[] fileNames = folder.list();
        for (String fileName : fileNames) {
            if (ImportFileScanner.IMPORT_STATE_FILENAME.equals(fileName)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
        File importFolder = importConfig.getImportFolder();
        processFolder(importFolder, importConfig);
    }


    private static void processFolder(File sourceFile, ImportProcess.ImportOptions importConfig) throws Exception {
        if (sourceFile == null) {
            throw new IOException("Source file is null.");
        } else if (!sourceFile.exists()) {
            throw new IOException("Source file doesnt exists: " + sourceFile.getAbsolutePath());
        } else if (sourceFile.isDirectory()) {
            for (File childFile : sourceFile.listFiles()) {
                if (childFile.isDirectory()) {
                    LOG.info("Doing file: " + childFile.getAbsolutePath());
                }
                processFolder(childFile, importConfig);
            }
            return;
        } else {
            if (sourceFile.getName().endsWith(".tif") || sourceFile.getName().endsWith(".jpg") || sourceFile.getName().endsWith(".jpeg")) {
                LOG.fine("Generating file for " + sourceFile.getAbsolutePath() + " starting.");
                generateTechnicalFiles(sourceFile, importConfig);
                return;
            } else {
                LOG.fine("Skipping file: " + sourceFile.getName());
            }

        }
    }

    private static void generateTechnicalFiles(File imageFile, ImportProcess.ImportOptions importConfig) throws Exception {
        if (imageFile.getName().endsWith(".tif") || imageFile.getName().endsWith(".jpg") || imageFile.getName().endsWith(".jpeg")) {
            ImportProfile config = importConfig.getConfig();
            File[] outputFiles = getOcrFiles(imageFile, config.getPlainOcrFileSuffix(), config.getAltoFileSuffix());

            File outputOcr = outputFiles[0];
            File outputAlto = outputFiles[1];
            if (outputOcr.exists() && outputAlto.exists()) {
                LOG.fine("Skipping file " + imageFile.getName() + " - files exists");
            } else {
                try {
                    generateFromExternalProcess(imageFile, config);
                } catch (IOException ex) {
                    Thread.sleep(30000);
                    if (!(outputOcr.exists() && outputAlto.exists())) {
                        generateFromExternalProcess(imageFile, config);
                    }
                }
            }
        } else {
            LOG.fine("Skipping file: " + imageFile.getName());
        }
    }

    private static void generateFromExternalProcess(File imageFile, ImportProfile config) throws IOException {
        ExternalProcess process = new OcrGenerator(config.getOcrGenProcessor(), imageFile, config.getPlainOcrFileSuffix(), config.getAltoFileSuffix());
        if (process != null) {
            process.run();
        }
        if (!process.isOk()) {
            throw new IOException("Generating OCR for " + imageFile.getName() + " failed. \n " + process.getFullOutput());
        }
    }
}
