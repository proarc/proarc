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
package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.config.Profiles;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.mets.JhoveContext;
import cz.cas.lib.proarc.common.process.imports.audio.WaveImporter;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.io.File;
import java.io.IOException;
import java.net.FileNameMap;
import java.net.URLConnection;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Import process
 * {@link #prepare(java.io.File, java.lang.String, cz.cas.lib.proarc.common.user.UserProfile, BatchManager, java.lang.String, java.lang.String, boolean) checks}
 * preconditions of the import,
 * {@link #start() runs} the import and if necessary {@link #resume resumes}
 * already prepared import.
 * <p>The process delegates to {@link ImportHandler} that is bound to the profile.
 *
 * @author Jan Pokorsky
 */
public final class ImportProcess implements Runnable {

    private static final Logger LOG = Logger.getLogger(ImportProcess.class.getName());
    public static final String TMP_DIR_NAME = "proarc_import";
    private final BatchManager batchManager;
    private static List<ImageImporter> consumerRegistery;
    private final ImportOptions importConfig;
    private final AppConfiguration config;

    private static Map<String, String> myMimeType= new HashMap<>();

    static {
        myMimeType.put("flac", "audio/flac");
        myMimeType.put("ogg", "audio/ogg");
        myMimeType.put("ogv", "audio/ogg");
        myMimeType.put("oga", "audio/ogg");
        myMimeType.put("ogx", "audio/ogg");
        myMimeType.put("ogm", "audio/ogg");
        myMimeType.put("jp2", "image/jp2");
    }

    ImportProcess(ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) {
        this.importConfig = importConfig;
        this.batchManager = batchManager;
        this.config = config;
    }

    /**
     * Prepares a new import process. Creates batch, locks import folder. The process is ready
     * to run with {@link #start} immediately or later with {@link ImportDispatcher}.
     */
    public static ImportProcess prepare(
            File importFolder, String description,
            UserProfile user, BatchManager batchManager,
            String device, String software, boolean generateIndices, String priority,
            boolean useNewMetadata, boolean useOriginalMetadata, Integer peroOcrEngine, Boolean isNightOnly,
            ImportProfile profile, AppConfiguration config
            ) throws IOException {
        return prepare(importFolder, description, user, batchManager, device, software, generateIndices, false, priority, useNewMetadata, useOriginalMetadata, peroOcrEngine, isNightOnly, profile, config);
    }

    /**
     * Prepares a new import process. Creates batch, locks import folder. The process is ready
     * to run with {@link #start} immediately or later with {@link ImportDispatcher}.
     */
    public static ImportProcess prepare(
            File importFolder, String description,
            UserProfile user, BatchManager batchManager,
            String device, String software, boolean generateIndices, boolean generatePageNumber, String priority,
            boolean useNewMetadata, boolean useOriginalMetadata, Integer peroOcrEngine, Boolean isNightOnly, ImportProfile profile, AppConfiguration config
            ) throws IOException {

        ImportOptions options = new ImportOptions(importFolder, device, software,
                generateIndices, generatePageNumber, user, profile, priority, useNewMetadata, useOriginalMetadata);
        ImportProcess process = new ImportProcess(options, batchManager, config);
        process.prepare(description, user, peroOcrEngine, isNightOnly);
        return process;
    }

    /**
     * Resumes a scheduled import process.
     * @see #prepare
     * @see ImportDispatcher
     */
    public static ImportProcess resume(Batch batch, boolean useNewMetadata, boolean useOriginalMetadata, BatchManager ibm, ImportProfile profile) {
        UserManager users = UserUtil.getDefaultManger();
        UserProfile user = users.find(batch.getUserId());
        File importFolder = ibm.resolveBatchFile(batch.getFolder());
        ImportOptions options = ImportOptions.fromBatch(
                batch, importFolder, useNewMetadata, useOriginalMetadata, user, profile);
        options.setOriginalBatchState(batch.getState());
        // if necessary reset old computed batch items
        ImportProcess process = new ImportProcess(options, ibm, ibm.getAppConfig());
        process.removeCaches(options.getImportFolder(), options);
        process.removeBatchItems(batch);
        return process;
    }

    /**
     * Read and submits scheduled processes from last session.
     * This should be run when application starts.
     */
    public static void resumeAll(BatchManager ibm, ImportDispatcher dispatcher,
                                 AppConfiguration config) {

        Profiles profiles = config.getProfiles();

        List<Batch> batches2schedule = new ArrayList<>();
        batches2schedule.addAll(ibm.findLoadingBatches());
        batches2schedule.addAll(ibm.findWaitingImportBatches());

        for (Batch batch : batches2schedule) {
            try {
                ConfigurationProfile profile = resolveProfile(batch, profiles);
                ImportProfile importCfg = config.getImportConfiguration(profile);
                ImportProcess resume = ImportProcess.resume(batch, false, false, ibm, importCfg);
                dispatcher.addImport(resume);
            } catch (Exception ex) {
                logBatchFailure(ibm, batch, ex);
            }
        }
    }

    public static void stopLoadingBatch(Batch batch, BatchManager ibm, AppConfiguration config) {
        ImportDispatcher importDispatcher = ImportDispatcher.getDefault();
        try {
            importDispatcher.stopNow();
        } catch (Exception ex) {
            LOG.log(Level.WARNING, ex.getMessage());
        }
        LOG.log(Level.INFO, batch.toString(), "has been stopped");
        batch.setState(Batch.State.STOPPED);
        batch.setUpdated(new Timestamp(System.currentTimeMillis()));
        ibm.update(batch);

        importDispatcher.restart();
        resumeAll(ibm, importDispatcher, config);
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

    private void prepare(String description, UserProfile user, Integer peroOcrEngine, Boolean isNightOnly) throws IOException {
        // validate import folder
        File importFolder = importConfig.getImportFolder();
        ImportFileScanner.validateImportFolder(importFolder);

        // check import state
        lockImportFolder(importFolder);
        boolean transactionFailed = true;
        try {
            if (getTargetFolder(importFolder, importConfig.getConfig()).exists()) {
                throw new IOException("Folder already exists: " + getTargetFolder(importFolder, importConfig.getConfig()));
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
                    peroOcrEngine,
                    isNightOnly,
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

    private static Batch logBatchFailure(BatchManager batchManager, Batch batch, Throwable t) {
        LOG.log(Level.SEVERE, batch.toString(), t);
        if (!Batch.State.LOADING_CONFLICT.equals(batch.getState())) {
            batch.setState(Batch.State.LOADING_FAILED);
            batch.setUpdated(new Timestamp(System.currentTimeMillis()));
        }
        batch.setLog(BatchManager.toString(t));
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
        batch.setState(Batch.State.LOADED);
        batch.setUpdated(new Timestamp(System.currentTimeMillis()));
        batch = batchManager.update(batch);
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
            File targetFolder = createTargetFolder(importFolder, importConfig.getConfig(), importConfig.getOriginalBatchState());
            importConfig.setTargetFolder(targetFolder);
            importConfig.getImporter().start(importConfig, batchManager, config);
            if (batch.getState() == Batch.State.LOADING) {
                batch.setState(Batch.State.LOADED);
                batch.setUpdated(new Timestamp(System.currentTimeMillis()));
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

    private void removeCaches(File importFoder, ImportOptions importOptions) {
        deleteFolder(getTargetFolder(importFoder, importOptions.getConfig()));
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

    public static File createTargetFolder(File importFolder, ImportProfile config, Batch.State originalBatchState) throws IOException {
        File folder = getTargetFolder(importFolder, config);
        if (!folder.mkdir()) {
            throw new IOException("Import folder already exists: " + folder);
        }
        return folder;

    }

    public static File getTargetFolder(File importFolder, ImportProfile config) {
        if (!config.getDefaultImportFolder()) {
            String path = config.getImportFolderPath();
            if (path == null || path.isEmpty()) {
                File folder = new File(importFolder, TMP_DIR_NAME);
                return folder;
            } else {
                String importFolderPath = path + importFolder.getPath().substring(importFolder.getPath().lastIndexOf("import")-1);
                File folder = new File(importFolderPath, TMP_DIR_NAME);
                return folder;
            }
        }
        File folder = new File(importFolder, TMP_DIR_NAME);
        return folder;
    }

    public static void lockImportFolder(File folder) throws IOException {
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

    /**
     * returns a list of available image consumers
     *
     * @return unmodifiable list of image consumers
     */
    public static List<ImageImporter> getConsumers() {
        if (consumerRegistery == null) {

            TiffImporter tiffImporter = new TiffImporter(BatchManager.getInstance());
            WaveImporter waveImporter = new WaveImporter(BatchManager.getInstance());

            ImageImporter[] importers = {
                    tiffImporter,
                    new TiffAsJpegImporter(tiffImporter),
                    new TiffAsJp2Importer(tiffImporter),
                    waveImporter
            };

            consumerRegistery = Collections.unmodifiableList(Arrays.asList(importers));
        }
        return consumerRegistery;
    }

    /**
     * Simplified version uses filename extension. For niftier alternatives see
     * http://www.rgagnon.com/javadetails/java-0487.html
     */
    public static String findMimeType(File f) {
        FileNameMap fileNameMap = URLConnection.getFileNameMap();
        String mimeType = fileNameMap.getContentTypeFor(f.getName());
        if (mimeType == null) {
            String extension = f.getName().substring(f.getName().lastIndexOf(".") + 1);
            mimeType = myMimeType.get(extension);
        }
        return mimeType;
    }

    public static final class ImportOptions {

        /** Folder with origin scan. */
        private final File importFolder;
        /** Folder containing generated stuff. It is available when the import
         * starts, not in prepare state.
         */
        private File targetFolder;
        private String device;
        private String software;
        private boolean generateIndices;
        private boolean generatePageNumber;
        private int consumedFileCounter;
        private final UserProfile user;
        private Batch batch;
        private Batch.State originalBatchState;
        private final ImportProfile profile;
        private JhoveContext jhoveContext;
        private ImportHandler importer;
        private String priority;
        private boolean useNewMetadata;
        private boolean useOriginalMetadata;

        // parametry kvuli auto migraci z k4 na ndk
        private List<String> pidsToUpdate;
        private boolean wasK4Model;

        public ImportOptions(File importFolder, String device, String software, boolean generateIndices, UserProfile username, ImportProfile profile, String priority) {
            this(importFolder, device, software, generateIndices, false, username, profile, priority, false, false);
        }

        public ImportOptions(File importFolder, String device, String software,
                boolean generateIndices, boolean gerenatePageNumber, UserProfile username,
                ImportProfile profile, String priority, boolean useNewMetadata, boolean useOriginalMetadata
                ) {
            this.device = device;
            this.software = software;
            this.generateIndices = generateIndices;
            this.generatePageNumber = gerenatePageNumber;
            this.user = username;
            this.importFolder = importFolder;
            this.profile = profile;
            this.priority = priority;
            this.useNewMetadata = useNewMetadata;
            this.useOriginalMetadata = useOriginalMetadata;

            this.pidsToUpdate = new ArrayList<>();
            this.wasK4Model = false;
        }

        public ImportHandler getImporter() {
            if (importer == null) {
                importer = profile.createImporter();
            }
            return importer;
        }

        public ImportProfile getProfile() {
            return profile;
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

        public boolean isGeneratePageNumber() {
            return generatePageNumber;
        }

        public String getDevice() {
            return device;
        }

        public String getSoftware() {
            return software;
        }

        public String getModel() {
            return profile.getModelId();
        }

        public String getAudioModel() {
            return profile.getAudioModelID();
        }

        public boolean isPagePath() {
            return profile.isPagePath();
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

        public String getOrganization() {
            return user != null ? user.getOrganization() : null;
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
                                              boolean useNewMetadata, boolean useOriginalMetadata, UserProfile username, ImportProfile profile) {

            ImportOptions options = new ImportOptions(
                    importFolder, batch.getDevice(), batch.getSoftware(),
                    batch.isGenerateIndices(), batch.isGeneratePageNumber(), username, profile, batch.getPriority(), useNewMetadata, useOriginalMetadata);
            options.setBatch(batch);
            return options;
        }

        public String getFoxmlImageServerPath() {
            return profile.getFoxmlImageServerPath();
        }

        public String getFoxmlFolderPath() {
            return profile.getFoxmlFolderPath();
        }

        public String getPriority() {
            return priority;
        }

        public void setPriority(String priority) {
            this.priority = priority;
        }

        public Batch.State getOriginalBatchState() {
            return originalBatchState;
        }

        public void setOriginalBatchState(Batch.State originalBatchState) {
            this.originalBatchState = originalBatchState;
        }

        public boolean isUseNewMetadata() {
            return useNewMetadata;
        }

        public void setUseNewMetadata(boolean useNewMetadata) {
            this.useNewMetadata = useNewMetadata;
        }

        public boolean isUseOriginalMetadata() {
            return useOriginalMetadata;
        }

        public void setUseOriginalMetadata(boolean useOriginalMetadata) {
            this.useOriginalMetadata = useOriginalMetadata;
        }

        public List<String> getPidsToUpdate() {
            return pidsToUpdate;
        }

        public void setPidsToUpdate(List<String> pidsToUpdate) {
            this.pidsToUpdate = pidsToUpdate;
        }

        public boolean wasK4Model() {
            return wasK4Model;
        }

        public void setWasK4Model(boolean wasK4Model) {
            this.wasK4Model = wasK4Model;
        }
    }

}
