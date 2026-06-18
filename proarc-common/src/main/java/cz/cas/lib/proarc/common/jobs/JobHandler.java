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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.configuration2.Configuration;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.impl.StdSchedulerFactory;

/**
 * Main handler of periodically executed jobs.
 * Jobs use JobHandler.getAppConfig() to access their configuration.
 * New jobs should be added into getJobs()
 *
 * @author Jakub Kremlacek
 */
public class JobHandler {

    public static final String PROPERTY_JOBS = "job";
    public static final String JOB_SCHEDULE = "schedule";

    private static final String JOB_DEVICES = "devices";
    private static final String JOB_DEVICE_PREFIX = "device";
    private static final String JOB_DEVICE_DIR = "dirname";
    private static final String JOB_DEVICE_UUID = "uuid";

    private static final String JOBS_LIST = "list";

    private static final String JOB_TYPE = "type";


    private static JobHandler INSTANCE;

    private Scheduler scheduler;
    private AppConfiguration appconfig;

    //Directory name, deviceUUID
    private Map<String, String> devices;

    public static JobHandler getInstance() throws SchedulerException {
        if (INSTANCE == null) {
            INSTANCE = new JobHandler();
        }

        return INSTANCE;
    }

    public String getDeviceUUID(String directoryName) {
        return devices.get(directoryName);
    }

    /**
     * Access point for config of other parts of application (e.g. import profiles)
     *
     * @return loaded Application configuration
     */
    public AppConfiguration getAppConfig() {
        return appconfig;
    }

    public void init(AppConfiguration appConfig) throws SchedulerException {
        this.appconfig = appConfig;

        Configuration jobsConfig = appConfig.getJobCofig();

        loadDevices(jobsConfig);

        scheduler.clear();

        //job.list
        for (String jobId : jobsConfig.getStringArray(JOBS_LIST)) {
            Configuration jobConfig = jobsConfig.subset(jobId);

            for (ProArcJob job : getJobs()) {
                //job.name.type
                if (job.getType().equals(jobConfig.getString(JOB_TYPE))) {
                    job.initJob(scheduler, jobId, jobConfig);
                }
            }
        }
    }

    private JobHandler() throws SchedulerException {
        initScheduler();
    }

    private void loadDevices(Configuration jobsConfig) {
        devices = new HashMap<>();

        //job.devices
        for (String device : jobsConfig.getStringArray(JOB_DEVICES)) {
            //job.device.name
            Configuration deviceConfig = jobsConfig.subset(JOB_DEVICE_PREFIX + "." + device);
            //job.device.name.dirname
            String dir = deviceConfig.getString(JOB_DEVICE_DIR);

            if (dir == null || dir.isEmpty()) {
                throw new IllegalArgumentException("Job device: " + device + " config directory cannot be empty");
            }
            //job.device.name.uuid
            String uuid = deviceConfig.getString(JOB_DEVICE_UUID);

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

    private List<ProArcJob> getJobs() {
        List<ProArcJob> jobs = new ArrayList<>();

        jobs.add(new BatchImportJob());

        return jobs;
    }
}
