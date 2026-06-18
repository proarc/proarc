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
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.ImportDispatcher;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.process.imports.ImportHandler;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import cz.cas.lib.proarc.common.process.imports.ImportProfile;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration2.Configuration;
import org.quartz.Job;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;

import static cz.cas.lib.proarc.common.user.UserUtil.DEFAULT_ADMIN_USER;
import static org.quartz.CronScheduleBuilder.cronSchedule;
import static org.quartz.JobBuilder.newJob;
import static org.quartz.TriggerBuilder.newTrigger;

/**
 * Quartz executable job for automatic batch importing at specified schedule
 *
 * @author Jakub Kremlacek
 */
public class BatchImportJob implements Job, ProArcJob {

    public static final String BATCH_IMPORT_JOB_TYPE = "BatchImportJob";
    public static final String BATCH_IMPORT_JOB_PATH = "path";
    public static final String BATCH_IMPORT_JOB_PROFILES = "profiles";
    public static final String BATCH_IMPORT_JOB_DETAIL_SUFFIX = "_batchImportJob";
    public static final String BATCH_IMPORT_JOB_TRIGGER_SUFFIX = "_batchImportTrigger";
    public static final String BATCH_IMPORT_JOB_GROUP = "importGroup";

    private static final Logger LOG = Logger.getLogger(BatchImportJob.class.getName());

    @Override
    public String getType() {
        return BATCH_IMPORT_JOB_TYPE;
    }

    @Override
    public void initJob(Scheduler scheduler, String jobId, Configuration jobConfig) throws SchedulerException {
        if (jobId == null || jobId.isEmpty()) {
            throw new IllegalArgumentException("JobId must be set");
        }

        //job.name.schedule
        String schedule = jobConfig.getString(JobHandler.JOB_SCHEDULE);

        if (schedule == null || schedule.isEmpty()) {
            throw new IllegalArgumentException("Job: " + jobId + " is not set properly, schedule missing");
        }
        //job.name.path
        String importPath = jobConfig.getString(BATCH_IMPORT_JOB_PATH);

        if (importPath == null || importPath.isEmpty()) {
            throw new IllegalArgumentException("Job: " + jobId + " is not set properly, path missing");
        }
        //job.name.profiles
        List<String> jobProfiles = Arrays.asList(jobConfig.getStringArray(BATCH_IMPORT_JOB_PROFILES));

        if (jobProfiles.isEmpty() || jobProfiles.contains("")) {
            throw new IllegalArgumentException("Job: " + jobId + "is not set properly, profiles missing");
        }

        JobDetail importJobDetail = newJob(BatchImportJob.class)
                .withIdentity(jobId + BATCH_IMPORT_JOB_DETAIL_SUFFIX, BATCH_IMPORT_JOB_GROUP)
                .build();

        importJobDetail.getJobDataMap().put(BATCH_IMPORT_JOB_PATH, importPath);
        importJobDetail.getJobDataMap().put(BATCH_IMPORT_JOB_PROFILES, jobProfiles);

        Trigger importJobTrigger = newTrigger()
                .withIdentity(jobId + BATCH_IMPORT_JOB_TRIGGER_SUFFIX, BATCH_IMPORT_JOB_GROUP)
                .withSchedule(cronSchedule(schedule))
                .build();

        scheduler.scheduleJob(importJobDetail, importJobTrigger);
    }

    @Override
    public void execute(JobExecutionContext jobExecutionContext) throws JobExecutionException {
        JobHandler handler;

        try {
            handler = JobHandler.getInstance();
        } catch (SchedulerException e) {
            throw new JobExecutionException(e);
        }

        String path = (String) jobExecutionContext.getJobDetail().getJobDataMap().get(BATCH_IMPORT_JOB_PATH);
        List<String> profiles = (List<String>) jobExecutionContext.getJobDetail().getJobDataMap().get(BATCH_IMPORT_JOB_PROFILES);

        LOG.log(Level.INFO, "Importing directory:" + path);

        AppConfiguration appConfig = handler.getAppConfig();

        File directory = new File(path);

        if (!directory.exists() || !directory.isDirectory()) {
            LOG.log(Level.SEVERE, "Directory: " + directory.getAbsolutePath() + " does not exist.");
            return;
        }

        //expected hierarchy: devices - profiles - batches
        for (File deviceDir : directory.listFiles(File::isDirectory)) {

            String deviceUUID = handler.getDeviceUUID(deviceDir.getName());

            //skip undefined directories
            if (deviceUUID == null) {
                continue;
            }

            for (File profileDir : deviceDir.listFiles(File::isDirectory)) {

                //skip undefined profiles
                if (!profiles.contains(profileDir.getName())) {
                    continue;
                }

                importDirectory(appConfig, directory, deviceUUID, profileDir);
            }
        }
    }

    private void importDirectory(AppConfiguration appConfig, File directory, String deviceUUID, File profileDir) throws JobExecutionException {
        ConfigurationProfile profile = findImportProfile(appConfig, null, profileDir.getName());

        ImportHandler importer = appConfig.getImportConfiguration(profile).createImporter();
        ImportFileScanner scanner = new ImportFileScanner();

        List<ImportFileScanner.Folder> folders;

        try {
            folders = scanner.findSubfolders(profileDir, importer);
        } catch (FileNotFoundException e) {
            throw new JobExecutionException(e);
        }

        for (ImportFileScanner.Folder folder : folders) {

            if (folder.getStatus() != ImportFileScanner.State.NEW) {
                //skip already imported folders
                continue;
            }

            ImportProcess process;
            try {
                UserManager userManger = UserUtil.getDefaultManger();
                UserProfile user = userManger.find(DEFAULT_ADMIN_USER);

                process = ImportProcess.prepare(folder.getHandle(), folder.getHandle().getName(), user,
                        BatchManager.getInstance(), deviceUUID, null, true, Batch.PRIORITY_MEDIUM, false, false, null, false, appConfig.getImportConfiguration(profile), appConfig);

                ImportDispatcher.getDefault().addImport(process);
                Batch batch = process.getBatch();

                LOG.log(Level.FINE, "Imported: " + directory + " with batchId " + batch.getId());
            } catch (IOException e) {
                LOG.log(Level.SEVERE, "Prepraring directory: " + folder.getHandle() + " for import failed.");
                e.printStackTrace();
            }
        }
    }

    private ConfigurationProfile findImportProfile(AppConfiguration appConfig, Integer batchId, String profileId) {
        profileId = "profile." + profileId;

        ConfigurationProfile profile = appConfig.getProfiles().getProfile(ImportProfile.PROFILES, profileId);
        if (profile == null) {
            LOG.log(Level.SEVERE, "Batch {3}: Unknown profile: {0}! Check {1} in proarc.cfg",
                    new Object[]{ImportProfile.PROFILES, profileId, batchId});
            throw new IllegalArgumentException("Unknown profile: " + profileId);
        }
        return profile;
    }
}
