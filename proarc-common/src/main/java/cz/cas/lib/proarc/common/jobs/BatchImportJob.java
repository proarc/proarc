/*
 * Copyright (C) 2017 Jakub Kremlacek
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
package cz.cas.lib.proarc.common.jobs;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportDispatcher;
import cz.cas.lib.proarc.common.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.imports.ImportHandler;
import cz.cas.lib.proarc.common.imports.ImportProcess;
import cz.cas.lib.proarc.common.imports.ImportProfile;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.SchedulerException;

/**
 * Quartz executable job for automatic batch importing at specified schedule
 *
 * @author Jakub Kremlacek
 */
public class BatchImportJob implements Job {
    private static final Logger LOG = Logger.getLogger(BatchImportJob.class.getName());

    @Override
    /**
     * jobExectutionContext must have job path and config set, see JobHandler
     */
    public void execute(JobExecutionContext jobExecutionContext) throws JobExecutionException {
        JobHandler handler;

        try {
            handler = JobHandler.getInstance();
        } catch (SchedulerException e) {
            throw new JobExecutionException(e.getMessage());
        }

        String path = (String) jobExecutionContext.getJobDetail().getJobDataMap().get(JobHandler.BATCH_IMPORT_JOB_PATH);

        LOG.log(Level.INFO, "Importing directory:" + path);

        AppConfiguration appConfig = (AppConfiguration) jobExecutionContext.getJobDetail().getJobDataMap().get(JobHandler.JOB_CONFIG);

        File directory = new File(path);

        if (!directory.exists() || !directory.isDirectory()) {
            LOG.log(Level.SEVERE, "Directory: " + directory.getAbsolutePath() + " does not exist.");
            return;
        }

        //expected hierarchy: devices - profiles - batches
        for (File deviceDir : directory.listFiles(File::isDirectory)) {

            String deviceUUID = handler.getDeviceUUID(deviceDir.getName());

            if (deviceUUID == null) {
                //skip undefined directories
                continue;
            }

            for (File profileDir : deviceDir.listFiles(File::isDirectory)) {

                if (!handler.isProfileDefined(profileDir.getName())) {
                    //skip undefined profiles
                    continue;
                }

                ConfigurationProfile profile = findImportProfile(appConfig, null, profileDir.getName());

                ImportFileScanner scanner = new ImportFileScanner();
                List<ImportFileScanner.Folder> subfolders;

                ImportHandler importer = appConfig.getImportConfiguration(profile).createImporter();

                try {
                    subfolders = scanner.findSubfolders(profileDir, importer);
                } catch (FileNotFoundException e) {
                    throw new JobExecutionException(e.getMessage());
                }

                for (ImportFileScanner.Folder folder : subfolders) {

                    if (folder.getStatus() != ImportFileScanner.State.NEW) {
                        //skip already imported folders
                        continue;
                    }

                    ImportProcess process;
                    try {
                        //TODO: replace with user
                        UserProfile user = new UserProfile();
                        user.setId(1);
                        user.setUserName("proarc");

                        process = ImportProcess.prepare(folder.getHandle(), folder.getHandle().getName(), user,
                                ImportBatchManager.getInstance(), deviceUUID, true, appConfig.getImportConfiguration(profile));

                        ImportDispatcher.getDefault().addImport(process);
                        Batch batch = process.getBatch();

                        LOG.log(Level.FINE, "Imported: " + directory + " with batchId "+ batch.getId());
                    } catch (IOException e) {
                        LOG.log(Level.SEVERE, "Prepraring directory: " + folder.getHandle() + " for import failed.");
                        e.printStackTrace();
                    }
                }
            }
        }
    }

    private ConfigurationProfile findImportProfile(AppConfiguration appConfig, Integer batchId, String profileId) {
        profileId = "profile." + profileId;

        ConfigurationProfile profile = appConfig.getProfiles().getProfile(ImportProfile.PROFILES, profileId);
        if (profile == null) {
            LOG.log(Level.SEVERE,"Batch {3}: Unknown profile: {0}! Check {1} in proarc.cfg",
                    new Object[]{ImportProfile.PROFILES, profileId, batchId});
            throw new IllegalArgumentException("Unknown profile: " + profileId);
        }
        return profile;
    }
}
