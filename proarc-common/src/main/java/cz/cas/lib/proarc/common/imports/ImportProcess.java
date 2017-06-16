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
package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.config.Profiles;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.io.File;
import java.io.IOException;
import java.net.FileNameMap;
import java.net.URLConnection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Import process
 * {@link #prepare(java.io.File, java.lang.String, cz.cas.lib.proarc.common.user.UserProfile, cz.cas.lib.proarc.common.imports.ImportBatchManager, java.lang.String, java.lang.String, boolean) checks}
 * preconditions of the import,
 * {@link #start() runs} the import and if necessary {@link #resume resumes}
 * already prepared import.
 * <p>The process delegates to {@link ImportHandler} that is bound to the profile.
 *
 * @author Jan Pokorsky
 */
public final class ImportProcess implements Runnable {

    private static final Logger LOG = Logger.getLogger(ImportProcess.class.getName());
    static final String TMP_DIR_NAME = "proarc_import";
    private final ImportBatchManager batchManager;
    private static List<ImageImporter> consumerRegistery;
    private final ImportOptions importConfig;

    ImportProcess(ImportOptions importConfig, ImportBatchManager batchManager) {
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
            String device, boolean generateIndices,
            ImportProfile profile
            ) throws IOException {

        ImportOptions options = new ImportOptions(importFolder, device,
                generateIndices, user, profile);
        ImportProcess process = new ImportProcess(options, batchManager);
        process.prepare(description, user);
        return process;
    }

    /**
     * Resumes a scheduled import process.
     * @see #prepare
     * @see ImportDispatcher
     */
    public static ImportProcess resume(Batch batch, ImportBatchManager ibm, ImportProfile profile) {
        UserManager users = UserUtil.getDefaultManger();
        UserProfile user = users.find(batch.getUserId());
        File importFolder = ibm.resolveBatchFile(batch.getFolder());
        ImportOptions options = ImportOptions.fromBatch(
                batch, importFolder, user, profile);
        // if necessary reset old computed batch items
        ImportProcess process = new ImportProcess(options, ibm);
        process.removeCaches(options.getImportFolder());
        process.removeBatchItems(batch);
        return process;
    }

    /**
     * Read and submits scheduled processes from last session.
     * This should be run when application starts.
     */
    public static void resumeAll(ImportBatchManager ibm, ImportDispatcher dispatcher,
            AppConfiguration config) {

        Profiles profiles = config.getProfiles();
        List<Batch> batches2schedule = ibm.findLoadingBatches();
        for (Batch batch : batches2schedule) {
            try {
                ConfigurationProfile profile = resolveProfile(batch, profiles);
                ImportProfile importCfg = config.getImportConfiguration(profile);
                ImportProcess resume = ImportProcess.resume(batch, ibm, importCfg);
                dispatcher.addImport(resume);
            } catch (Exception ex) {
                logBatchFailure(ibm, batch, ex);
            }
        }
    }

    private static ConfigurationProfile resolveProfile(Batch batch, Profiles profiles) {
        String profileId = batch.getProfileId();
        if (profileId == null) {
            profileId = ConfigurationProfile.DEFAULT;
        }
        ConfigurationProfile profile = profiles.getProfile(ImportProfile.PROFILES, profileId);
        String err = null;
        if (profile == null) {
            err = String.format("Cannot resume batch %s! Missing profile %s."
                    + " Check proarc.cfg and %s registrations.",
                    batch.getId(), profileId, ImportProfile.PROFILES);
        } else if (profile.getError() != null) {
            err = String.format("Cannot resume batch %s! Check proarc.cfg.\n%s\nProfile error: %s",
                    new Object[]{batch.getId(), profile, profile.getError()});
        }
        if (err != null) {
            throw new IllegalStateException(err);
        }
        return profile;
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
            int estimateItemNumber = importConfig.getImporter().estimateItemNumber(importConfig);
            if (estimateItemNumber == 0) {
                throw new IOException("Nothing to import " + importFolder);
            }
            Batch batch = batchManager.add(
                    importFolder,
                    description,
                    user,
                    estimateItemNumber,
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
        return logBatchFailure(batchManager, batch, t);
    }

    private static Batch logBatchFailure(ImportBatchManager batchManager, Batch batch, Throwable t) {
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
        Batch batch = importConfig.getBatch();
        if (batch == null) {
            throw new IllegalStateException("run prepare first!");
        }
        File importFolder = importConfig.getImportFolder();
        try {
            try {
                ImportFileScanner.validateImportFolder(importFolder);
                ImportFolderStatus folderStatus = batchManager.getFolderStatus(batch);
                if (folderStatus == null) {
                    batchManager.updateFolderStatus(batch);
                } else if (!batch.getId().equals(folderStatus.getBatchId())) {
                    throw new IllegalStateException(String.format(
                        "The folder tracked by another batch import!\nfolder: %s\nfound ID: %s",
                        importFolder, folderStatus.getBatchId()));
                }
            } catch (Throwable ex) {
                return logBatchFailure(batch, ex);
            }
            File targetFolder = createTargetFolder(importFolder);
            importConfig.setTargetFolder(targetFolder);
            importConfig.getImporter().start(importConfig);
            if (batch.getState() == Batch.State.LOADING) {
                batch.setState(Batch.State.LOADED);
            }
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
        return importConfig.getBatch();
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

    static List<ImageImporter> getConsumers() {
        if (consumerRegistery == null) {
            consumerRegistery = new LinkedList<ImageImporter>();

            TiffImporter importer = new TiffImporter(ImportBatchManager.getInstance());

            consumerRegistery.add(importer);
            consumerRegistery.add(new JpegImporter(importer));
            consumerRegistery.add(new Jp2Importer(importer));
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
        private final UserProfile user;
        private Batch batch;
        private final ImportProfile profile;
        private JhoveContext jhoveContext;
        private ImportHandler importer;

        ImportOptions(File importFolder, String device,
                boolean generateIndices, UserProfile username,
                ImportProfile profile
                ) {
            this.device = device;
            this.generateIndices = generateIndices;
            this.user = username;
            this.importFolder = importFolder;
            this.profile = profile;
        }

        public ImportHandler getImporter() {
            if (importer == null) {
                importer = profile.createImporter();
            }
            return importer;
        }

        public File getImportFolder() {
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
            return profile.getModelId();
        }

        public int getConsumedFileCounter() {
            return consumedFileCounter;
        }

        public void setConsumedFileCounter(int consumedFileCounter) {
            this.consumedFileCounter = consumedFileCounter;
        }

        public String getUsername() {
            return user != null ? user.getUserName() : null;
        }

        public UserProfile getUser() {
            return user;
        }

        public Batch getBatch() {
            return batch;
        }

        public void setBatch(Batch batch) {
            this.batch = batch;
        }

        public ImportProfile getConfig() {
            return profile;
        }

        public JhoveContext getJhoveContext() {
            return jhoveContext;
        }

        public void setJhoveContext(JhoveContext jhoveContext) {
            this.jhoveContext = jhoveContext;
        }

        public static ImportOptions fromBatch(Batch batch, File importFolder,
                UserProfile username, ImportProfile profile) {

            ImportOptions options = new ImportOptions(
                    importFolder, batch.getDevice(),
                    batch.isGenerateIndices(), username, profile);
            options.setBatch(batch);
            return options;
        }

    }

}
