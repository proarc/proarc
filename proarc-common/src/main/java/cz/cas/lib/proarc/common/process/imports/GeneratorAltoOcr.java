package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.external.PeroOcrProcessor;
import org.codehaus.jettison.json.JSONException;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Logger;


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
            ImportProfile config = importConfig.getConfig();
            Integer peroOcrEngine = null;
            try {
                peroOcrEngine = importConfig.getBatch().getParamsAsObject().getPeroOcrEngine();
            } catch (NullPointerException e) {
                peroOcrEngine = 1;
            }
            File[] outputFiles = PeroOcrProcessor.getOcrFiles(imageFile, config.getPlainOcrFileSuffix(), config.getAltoFileSuffix());

            File outputOcr = outputFiles[0];
            File outputAlto = outputFiles[1];
            if (outputOcr.exists() && outputAlto.exists()) {
                LOG.fine("Skipping file " + imageFile.getName() + " - files exists");
            } else {
                generateFromExternalProcess(imageFile, config, peroOcrEngine);
            }
        } else {
            LOG.fine("Skipping file: " + imageFile.getName());
        }
    }

    private static void generateFromExternalProcess(File imageFile, ImportProfile config, Integer peroOcrEngine) throws IOException {
        PeroOcrProcessor ocrProcessor = new PeroOcrProcessor(config.getOcrGenProcessor(), peroOcrEngine);
        try {
            boolean processed = ocrProcessor.generate(imageFile, ".txt", ".xml");
            if (processed) {
                LOG.info("OCR GENERATED SUCCESSFULLY for " +  imageFile.getAbsolutePath());
            }
        } catch (JSONException ex) {
            LOG.severe("Generating OCR for " + imageFile.getName() + " failed.");
            ex.printStackTrace();
            throw new IOException(ex);
        }
    }
}
