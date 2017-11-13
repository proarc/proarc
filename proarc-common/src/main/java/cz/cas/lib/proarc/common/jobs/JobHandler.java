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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.configuration.Configuration;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.quartz.impl.StdSchedulerFactory;

import static org.quartz.CronScheduleBuilder.cronSchedule;
import static org.quartz.JobBuilder.newJob;
import static org.quartz.TriggerBuilder.newTrigger;

/**
 * @author Jakub Kremlacek
 */
public class JobHandler {

    private static final String PROPERTY_JOBS = "jobs";

    private static final String JOBS_DEVICES = "devices";
    private static final String JOBS_DEVICE_PREFIX = "device";
    private static final String JOBS_DEVICE_DIR = "dirname";
    private static final String JOBS_DEVICE_UUID = "uuid";

    private static final String JOBS_LIST = "list";

    private static final String JOB_PREFIX = "job";
    private static final String JOB_TYPE = "type";
    private static final String JOB_SCHEDULE = "schedule";

    public static final String BATCH_IMPORT_JOB_TYPE = "BatchImportJob";
    public static final String BATCH_IMPORT_JOB_PATH = "path";
    public static final String BATCH_IMPORT_JOB_PROFILES = "jobProfiles";
    //public static final String BATCH_IMPORT_JOB_ALL_PROFILES = "all";
    //public static final String BATCH_IMPORT_JOB_ALL_DEVICES = "all";

    public static final String JOB_CONFIG = "config";

    private static JobHandler INSTANCE;
    private Scheduler scheduler;
    private AppConfiguration appconfig;
    private List<String> jobProfiles;

    //Directory name, deviceUUID
    private Map<String, String> devices;

    public static JobHandler getInstance() throws SchedulerException {
        if (INSTANCE == null) {
            INSTANCE = new JobHandler();
        }

        return INSTANCE;
    }

    private JobHandler() throws SchedulerException {
        initScheduler();
    }

    public void init(AppConfiguration appConfig) throws SchedulerException {
        this.appconfig = appConfig;
        devices = new HashMap<>();

        Configuration config = appConfig.getAuthenticators();

        scheduler.clear();

        //jobs.list
        for (String jobId : config.getStringArray(PROPERTY_JOBS + "." + JOBS_LIST)) {
            Configuration jobConfig = config.subset(JOB_PREFIX + '.' + jobId);

            //job.name.type
            if (jobConfig.getString(JOB_TYPE).equals(BATCH_IMPORT_JOB_TYPE)) {
                initBatchImportJob(jobId, jobConfig);
            }

            //other types...
        }

        //jobs.devices
        for (String device : config.getStringArray(PROPERTY_JOBS + "." + JOBS_DEVICES)) {
            //jobs.device.name
            Configuration deviceConfig = config.subset(PROPERTY_JOBS + "." + JOBS_DEVICE_PREFIX + "." + device);

            String dir = deviceConfig.getString(JOBS_DEVICE_DIR);

            if (dir == null || dir.isEmpty()) {
                throw new IllegalArgumentException("Job device: " + device + " config directory cannot be empty");
            }

            String uuid = deviceConfig.getString(JOBS_DEVICE_UUID);

            if (uuid == null || uuid.isEmpty()) {
                throw new IllegalArgumentException("Job device: " + device + " config uuid cannot be empty");
            }

            devices.put(dir, uuid);
        }
    }

    private void initScheduler() throws SchedulerException {
        if (scheduler == null) {
            scheduler = StdSchedulerFactory.getDefaultScheduler();
            scheduler.start();
        }
    }

    private void initBatchImportJob(String jobId, Configuration jobConfig) throws SchedulerException {
        JobDetail importJobDetail = newJob(BatchImportJob.class)
                .withIdentity(jobId + "_batchImportJob", "importGroup")
                .build();

        String importPath = jobConfig.getString(BATCH_IMPORT_JOB_PATH);

        if (importPath == null || importPath.isEmpty()) {
            throw new IllegalArgumentException("Job: "+jobId+" is not set properly, path missing");
        }

        jobProfiles = Arrays.asList(jobConfig.getStringArray(BATCH_IMPORT_JOB_PROFILES));

        JobDataMap jdm = importJobDetail.getJobDataMap();

        jdm.put(JOB_CONFIG, appconfig);
        jdm.put(BATCH_IMPORT_JOB_PATH, importPath);

        String schedule = jobConfig.getString(JOB_SCHEDULE);

        if (schedule == null || schedule.isEmpty()) {
            throw new IllegalArgumentException("Job: "+jobId+" is not set properly, schedule missing");
        }

        Trigger importJobTrigger = newTrigger()
                .withIdentity(jobId + "_batchImportTrigger", "importGroup")
                .withSchedule(cronSchedule(schedule))
                .build();

        scheduler.scheduleJob(importJobDetail, importJobTrigger);
    }

    public String getDeviceUUID(String directoryName) {
        return devices.get(directoryName);
    }

    public boolean isProfileDefined(String profile) {
        return jobProfiles.contains(profile);
    }
}
