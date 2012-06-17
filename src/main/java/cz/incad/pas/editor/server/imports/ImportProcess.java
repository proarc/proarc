/*
 * Copyright (C) 2011 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.imports;

import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportBatch;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.user.UserProfile;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.FileNameMap;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Import process
 * {@link #prepare(java.io.File, java.lang.String, cz.incad.pas.editor.server.user.UserProfile, cz.incad.pas.editor.server.imports.ImportBatchManager, java.lang.String, java.lang.String, boolean) checks}
 * preconditions of the import,
 * {@link #start() runs} the import and if necessary {@link #resume resumes}
 * already prepared import.
 *
 * @author Jan Pokorsky
 */
public final class ImportProcess implements Runnable {

    private static final Logger LOG = Logger.getLogger(ImportProcess.class.getName());
    static final String TMP_DIR_NAME = "proarc_import";
    private ImportBatchManager batchManager;
    private static final List<TiffImporter> consumerRegistery = Collections.singletonList(new TiffImporter());
    private List<ImportItemFailure> failures = new ArrayList<ImportItemFailure>();
    private final ImportOptions importConfig;
    private ImportBatch batch;

    ImportProcess(ImportOptions importConfig, ImportBatchManager batchManager) {
        this(null, importConfig, batchManager);
    }

    ImportProcess(ImportBatch batch, ImportOptions importConfig, ImportBatchManager batchManager) {
        this.batch = batch;
        this.importConfig = importConfig;
        this.batchManager = batchManager;
    }

    /**
     * Prepares a new import process. Creates batch, locks import folder. The process is ready
     * to run with {@link #start} immediately or later with {@link ImportDispatcher}.
     */
    public static ImportProcess prepare(
            File importFolder, String description,
            UserProfile user, ImportBatchManager batchManager,
            String importAs, String device, boolean generateIndices) throws IOException {

        importAs = "model:page"; // for now use default
        ImportOptions config = new ImportOptions(importFolder, importAs, device, generateIndices, user.getUserName());
        ImportProcess process = new ImportProcess(config, batchManager);
        process.prepare(description, user);
        return process;
    }

    /**
     * Resumes a scheduled import process.
     * @see #prepare
     * @see ImportDispatcher
     */
    public static ImportProcess resume(ImportBatch batch, ImportBatchManager ibm) {
        ImportOptions config = batch.getOptions();
        // if necessary reset old computed batch items
        ImportProcess process = new ImportProcess(batch, config, ibm);
        process.removeCaches(config.getImportFolder());
        process.removeBatchItems(batch.getId());
        return process;
    }

    /**
     * Read and submits scheduled processes from last session.
     * This should be run when application starts.
     */
    public static void resumeAll(ImportBatchManager ibm, ImportDispatcher dispatcher) {
        List<ImportBatch> batches2schedule = ibm.find(null, null, ImportBatchManager.ImportBatch.State.LOADING);
        for (ImportBatch batch : batches2schedule) {
            ImportProcess resume = ImportProcess.resume(batch, ibm);
            dispatcher.addImport(resume);
        }
    }

    private void prepare(String description, UserProfile user) throws IOException {
        // validate import folder
        File importFolder = importConfig.getImportFolder();
        ImportFileScanner.validateImportFolder(importFolder);

        // check import state
        lockImportFolder(importFolder);
        boolean transactionFailed = true;
        try {
            if (getTargetFolder(importFolder).exists()) {
                throw new IOException("Folder already exists: " + getTargetFolder(importFolder));
            }
            ImportFileScanner scanner = new ImportFileScanner();
            List<File> files = scanner.findDigitalContent(importFolder);
            if (!canImport(files)) {
                // nothing to import
                return;
            }
            batch = new ImportBatch();
            batch.setFolderPath(importFolder);
            batch.setDescription(description);
            batch.setUser(user.getUserName());
            batch.setUserId(user.getId());
            batch.setState(ImportBatch.State.LOADING);
            batch.setEstimateFileCount(files.size());
            batch.setOptions(importConfig);
            batch = batchManager.add(batch);
            transactionFailed = false;
        } finally {
            if (transactionFailed) {
                ImportFileScanner.rollback(importFolder);
            }
        }
    }

    private ImportBatch logBatchFailure(ImportBatch batch, Throwable t) {
        LOG.log(Level.SEVERE, "batch: " + batch.getId(), t);
        StringWriter dump = new StringWriter();
        PrintWriter pw = new PrintWriter(dump);
        t.printStackTrace(pw);
        pw.close();
        batch.setState(ImportBatch.State.LOADING_FAILED);
        batch.setFailure(dump.toString());
        return batchManager.update(batch);
    }

    @Override
    public void run() {
        start();
    }

    /**
     * Starts the process.
     * @return the import batch
     */
    public ImportBatch start() {
        if (batch == null) {
            throw new IllegalStateException("Batch not found: ");
        }
        int batchId = batch.getId();

        // validate import folder
        File importFolder = importConfig.getImportFolder();
        try {
            ImportFileScanner.validateImportFolder(importFolder);
        } catch (Throwable t) {
            return logBatchFailure(batch, t);
        }

        try {
            File targetFolder = createTargetFolder(importFolder);
            importConfig.setTargetFolder(targetFolder);
            ImportFileScanner scanner = new ImportFileScanner();
            List<File> files = scanner.findDigitalContent(importFolder);
            consumeFiles(batchId, files, importConfig);
            batch.setState(ImportBatch.State.LOADED);
            batch = batchManager.update(batch);
            return batch;
        } catch (InterruptedException ex) {
            // rollback files on batch resume
            return null;
        } catch (Throwable t) {
            return logBatchFailure(batch, t);
        }
    }

    private void removeCaches(File importFoder) {
        deleteFolder(getTargetFolder(importFoder));
    }

    private void removeBatchItems(int batchId) {
        batchManager.removeItems(batchId);
    }

    public ImportOptions getImportConfig() {
        return importConfig;
    }

    public ImportBatch getBatch() {
        return batch;
    }

    static boolean canImport(File file) {
        for (TiffImporter consumer : consumerRegistery) {
            String mimeType = findMimeType(file);
            if (consumer.accept(file, mimeType)) {
                return true;
            }
        }
        return false;
    }

    static boolean canImport(List<File> files) {
        for (File file : files) {
            if (canImport(file)) {
                return true;
            }
        }
        return false;
    }

    private static File createTargetFolder(File importFolder) throws IOException {
        File folder = getTargetFolder(importFolder);
        if (!folder.mkdir()) {
            throw new IOException("Import folder already exists: " + folder);
        }
        return folder;
    }

    private static File getTargetFolder(File importFolder) {
        File folder = new File(importFolder, TMP_DIR_NAME);
        return folder;
    }

    private void lockImportFolder(File folder) throws IOException {
        File statusFile = new File(folder, ImportFileScanner.IMPORT_STATE_FILENAME);
        if (!statusFile.createNewFile()) {
            throw new IOException("Folder already imported: " + folder);
        }
    }

    private static void deleteFolder(File folder) {
        if (folder.exists()) {
            for (File f : folder.listFiles()) {
                if (f.isDirectory()) {
                    deleteFolder(f);
                } else {
                    f.delete();
                }
            }
            folder.delete();
        }
    }

    public List<ImportItemFailure> getFailures() {
        return this.failures;
    }

    private void consumeFiles(int batchId, List<File> files, ImportOptions ctx) throws InterruptedException {
        long start = System.currentTimeMillis();
        for (File file : files) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            try {
                ImportItem item = consumeFile(file, ctx);
                if (item != null) {
                    batchManager.addItem(batchId, item);
                } else {
                    // XXX implement failed items in ImportBatchManager
                    // ImportItem importItem = new ImportItem((String) null, file.toString(), null);
                    this.failures.add(new ImportItemFailure(file.getName(), "unsupported file"));
                }
            } catch (IOException ex) {
                StringWriter sw = new StringWriter();
                ex.printStackTrace(new PrintWriter(sw));
                this.failures.add(new ImportItemFailure(file.getName(), sw.toString()));
                LOG.log(Level.SEVERE, file.toString(), ex);
            }
        }
        LOG.log(Level.INFO, "Total time: {0} ms", System.currentTimeMillis() - start);
    }

    private ImportItem consumeFile(File f, ImportOptions ctx) throws IOException {
        long start = System.currentTimeMillis();
        String mimeType = findMimeType(f);
        List<TiffImporter> consumers = getConsumers();
        for (TiffImporter consumer : consumers) {
            ImportItem item = consumer.consume(f, mimeType, ctx);
            if (item != null) {
                LOG.log(Level.INFO, "time: {0} ms, {1}", new Object[] {System.currentTimeMillis() - start, f});
                ++ctx.consumedFileCounter;
                return item;
            }
        }

        return null;
    }

    private List<TiffImporter> getConsumers() {
        return consumerRegistery;
    }

    /**
     * Simplified version uses filename extension. For niftier alternatives see
     * http://www.rgagnon.com/javadetails/java-0487.html
     */
    public static String findMimeType(File f) {
        FileNameMap fileNameMap = URLConnection.getFileNameMap();
        return fileNameMap.getContentTypeFor(f.getName());
    }

    public static final class ImportOptions {

        /** Folder with origin scan. */
        private final File importFolder;
        /** Folder containing generated stuff. It is available when the import
         * starts, not in prepare state.
         */
        private File targetFolder;
        private String device;
        private boolean generateIndices;
        private int consumedFileCounter;
        private final String username;
        private String model;

        ImportOptions(File importFolder, String model, String device,
                boolean generateIndices, String username
                ) {
            this.device = device;
            this.generateIndices = generateIndices;
            this.username = username;
            this.importFolder = importFolder;
            this.model = model;
        }

        private File getImportFolder() {
            return importFolder;
        }

        public File getTargetFolder() {
            return targetFolder;
        }

        public void setTargetFolder(File targetFolder) {
            this.targetFolder = targetFolder;
        }

        public boolean isGenerateIndices() {
            return generateIndices;
        }

        public String getDevice() {
            return device;
        }

        public String getModel() {
            return model;
        }

        public int getConsumedFileCounter() {
            return consumedFileCounter;
        }

        public String getUsername() {
            return username;
        }

        public String asString() {
            Properties properties = new Properties();
            properties.setProperty("indicies", String.valueOf(isGenerateIndices()));
            properties.setProperty("device", getDevice());
            properties.setProperty("model", getModel());
            try {
                StringWriter dump = new StringWriter();
                properties.store(dump, null);
                String options = dump.toString();
                // remove comments
                return options.replaceAll("#.*\\n", "");
            } catch (IOException ex) {
                throw new IllegalStateException(ex);
            }
        }

        public static ImportOptions fromString(File importFolder, String username, String options) {
            ImportOptions config = new ImportOptions(importFolder, null, null, true, username);
            if (options != null && !options.isEmpty()) {
                Properties p = new Properties();
                try {
                    p.load(new StringReader(options));
                    String indicies = p.getProperty("indicies", "true");
                    config.generateIndices = Boolean.parseBoolean(indicies);
                    String device = p.getProperty("device");
                    config.device = device;
                    String model = p.getProperty("model");
                    config.model = model;
                } catch (IOException ex) {
                    throw new IllegalStateException(ex);
                }
            }
            return config;
        }

    }
    
    public static class ImportItemFailure {
        private String filename;
        private String reason;

        private ImportItemFailure() {
        }

        public ImportItemFailure(String filename, String reason) {
            this.filename = filename;
            this.reason = reason;
        }

        public String getFilename() {
            return filename;
        }

        public String getReason() {
            return reason;
        }
        
    }

}
