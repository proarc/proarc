package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.external.PeroOcrProcessor;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Logger;
import org.json.JSONException;


public class GeneratorAltoOcr implements ImportHandler {

    static Logger LOG = Logger.getLogger(GeneratorAltoOcr.class.getName());

    @Override
    public int estimateItemNumber(ImportProcess.ImportOptions importConfig) throws IOException {
        File importFolder = importConfig.getImportFolder();
        int size = 0;
        for (File file : importFolder.listFiles()) {
            if (file.isFile() && file.getName().endsWith(".tif") || file.getName().endsWith(".jpg") || file.getName().endsWith(".jpeg")) {
                size++;
            }
        }
        return size;
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
            File[] sourceFiles = sourceFile.listFiles();
            Arrays.sort(sourceFiles);
            for (File childFile : sourceFiles) {
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
            generateOcrAndAlto(imageFile, imageFile, importConfig);
        } else {
            LOG.fine("Skipping file: " + imageFile.getName());
        }
    }

    public static File[] generateOcrAndAlto(File imageFile, File outputBaseFile,
                                            ImportProcess.ImportOptions importConfig) throws IOException {
        ImportProfile config = importConfig.getConfig();
        File[] outputFiles = PeroOcrProcessor.getOcrFiles(
                outputBaseFile, config.getPlainOcrFileSuffix(), config.getAltoFileSuffix());
        if (outputFiles[0].exists() && outputFiles[1].exists()) {
            LOG.fine("Skipping file " + imageFile.getName() + " - files exist");
            return outputFiles;
        }

        Integer peroOcrEngine = getPeroOcrEngine(importConfig);
        PeroOcrProcessor ocrProcessor = new PeroOcrProcessor(config.getOcrGenProcessor(), peroOcrEngine);
        try {
            boolean processed = ocrProcessor.process(
                    imageFile.getAbsolutePath(),
                    outputFiles[0].getAbsolutePath(),
                    outputFiles[1].getAbsolutePath());
            if (processed) {
                LOG.info("OCR GENERATED SUCCESSFULLY for " + imageFile.getAbsolutePath());
            }
        } catch (JSONException ex) {
            LOG.severe("Generating OCR for " + imageFile.getName() + " failed: " + ex.getMessage());
            throw new IOException(ex);
        }
        if (!outputFiles[0].exists() || !outputFiles[1].exists()) {
            throw new IOException("Generating OCR/ALTO failed for " + imageFile.getAbsolutePath());
        }
        return outputFiles;
    }

    private static Integer getPeroOcrEngine(ImportProcess.ImportOptions importConfig) {
        try {
            Integer peroOcrEngine = importConfig.getBatch().getParamsAsObject().getPeroOcrEngine();
            return peroOcrEngine == null || peroOcrEngine < 0 ? 1 : peroOcrEngine;
        } catch (NullPointerException ex) {
            return 1;
        }
    }
}
