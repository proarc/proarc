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

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem.FileState;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.incad.pas.editor.server.imports.ImportBatchManager.BatchItemObject;
import cz.incad.pas.editor.server.user.UserManager;
import cz.incad.pas.editor.server.user.UserProfile;
import cz.incad.pas.editor.server.user.UserUtil;
import java.io.File;
import java.io.IOException;
import java.net.FileNameMap;
import java.net.URLConnection;
import java.util.Collections;
import java.util.List;
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
    private static List<TiffImporter> consumerRegistery;
    private final ImportOptions importConfig;
    /** can be null before calling {@link #prepare} */
    private Batch batch;

    ImportProcess(ImportOptions importConfig, ImportBatchManager batchManager) {
        this(null, importConfig, batchManager);
    }

    ImportProcess(Batch batch, ImportOptions importConfig, ImportBatchManager batchManager) {
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
    public static ImportProcess resume(Batch batch, ImportBatchManager ibm) {
        UserManager users = UserUtil.getDefaultManger();
        UserProfile user = users.find(batch.getUserId());
        File importFolder = ibm.resolveBatchFile(batch.getFolder());
        ImportOptions config = ImportOptions.fromBatch(
                batch, importFolder, user.getUserName());
        // if necessary reset old computed batch items
        ImportProcess process = new ImportProcess(batch, config, ibm);
        process.removeCaches(config.getImportFolder());
        process.removeBatchItems(batch);
        return process;
    }

    /**
     * Read and submits scheduled processes from last session.
     * This should be run when application starts.
     */
    public static void resumeAll(ImportBatchManager ibm, ImportDispatcher dispatcher) {
        List<Batch> batches2schedule = ibm.findLoadingBatches();
        for (Batch batch : batches2schedule) {
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
            List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
            if (!canImport(fileSets)) {
                // nothing to import
                return;
            }
            batch = batchManager.add(
                    importFolder,
                    description,
                    user,
                    fileSets.size(),
                    importConfig);
            importConfig.setBatch(batch);
            transactionFailed = false;
        } finally {
            if (transactionFailed) {
                ImportFileScanner.rollback(importFolder);
            }
        }
    }

    private Batch logBatchFailure(Batch batch, Throwable t) {
        LOG.log(Level.SEVERE, batch.toString(), t);
        batch.setState(Batch.State.LOADING_FAILED);
        batch.setLog(ImportBatchManager.toString(t));
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
    public Batch start() {
        if (batch == null) {
            throw new IllegalStateException("run prepare first!");
        }
        File importFolder = importConfig.getImportFolder();
        try {
            ImportFileScanner.validateImportFolder(importFolder);

            File targetFolder = createTargetFolder(importFolder);
            importConfig.setTargetFolder(targetFolder);
            ImportFileScanner scanner = new ImportFileScanner();
            List<File> files = scanner.findDigitalContent(importFolder);
            List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
            consumeFileSets(batch, fileSets, importConfig);
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

    private void removeBatchItems(Batch batch) {
        batchManager.resetBatch(batch);
    }

    public ImportOptions getImportConfig() {
        return importConfig;
    }

    public Batch getBatch() {
        return batch;
    }

    static boolean canImport(FileSet fileSet) {
        for (TiffImporter consumer : getConsumers()) {
            if (consumer.accept(fileSet)) {
                return true;
            }
        }
        return false;
    }

    static boolean canImport(List<FileSet> fileSets) {
        for (FileSet fileSet : fileSets) {
            if (canImport(fileSet)) {
                return true;
            }
        }
        return false;
    }

    static File createTargetFolder(File importFolder) throws IOException {
        File folder = getTargetFolder(importFolder);
        if (!folder.mkdir()) {
            throw new IOException("Import folder already exists: " + folder);
        }
        return folder;
    }

    static File getTargetFolder(File importFolder) {
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

    private void consumeFileSets(Batch batch, List<FileSet> fileSets, ImportOptions ctx) throws InterruptedException {
        long start = System.currentTimeMillis();
        for (FileSet fileSet : fileSets) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            BatchItemObject item = consumeFileSet(fileSet, ctx);
            String pid = item == null ? null : item.getPid();
            FileState state = item == null ? FileState.SKIPPED : FileState.OK;
            batchManager.addFileItem(batch.getId(), pid, state, fileSet.getFiles());
            if (item != null) {
                if (ObjectState.LOADING_FAILED == item.getState()) {
                    batch.setState(Batch.State.LOADING_FAILED);
                    batch.setLog(item.getLog());
                    return ;
                }
            }
        }
        LOG.log(Level.INFO, "Total time: {0} ms", System.currentTimeMillis() - start);
        batch.setState(Batch.State.LOADED);
    }

    private BatchItemObject consumeFileSet(FileSet fileSet, ImportOptions ctx) {
        long start = System.currentTimeMillis();
        List<TiffImporter> consumers = getConsumers();
        for (TiffImporter consumer : consumers) {
            BatchItemObject item = consumer.consume(fileSet, ctx);
            if (item != null) {
                LOG.log(Level.INFO, "time: {0} ms, {1}", new Object[] {System.currentTimeMillis() - start, fileSet});
                ++ctx.consumedFileCounter;
                return item;
            }
        }

        return null;
    }

    private static List<TiffImporter> getConsumers() {
        if (consumerRegistery == null) {
            consumerRegistery = Collections.singletonList(
                    new TiffImporter(ImportBatchManager.getInstance()));
        }
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
        private String ocrFilePattern = ".+\\.ocr\\.txt";
        private String ocrCharset = "UTF-8";
        private Batch batch;

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

        public String getOcrFilePattern() {
            return ocrFilePattern;
        }

        public String getOcrCharset() {
            return ocrCharset;
        }

        public Batch getBatch() {
            return batch;
        }

        public void setBatch(Batch batch) {
            this.batch = batch;
        }

        public static ImportOptions fromBatch(Batch batch, File importFolder, String username) {
            ImportOptions config = new ImportOptions(
                    importFolder, "model:page", batch.getDevice(), batch.isGenerateIndices(), username);
            config.setBatch(batch);
            return config;
        }

    }

}
