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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.jobs;

import java.util.List;
import java.util.Map;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.quartz.JobKey;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.quartz.TriggerKey;
import org.quartz.impl.StdSchedulerFactory;

/**
 * @author Jakub Kremlacek
 */
public class BatchImportJobTest {

    Scheduler scheduler;

    @Test
    public void getTypeTest() {
        Assert.assertEquals(BatchImportJob.BATCH_IMPORT_JOB_TYPE, new BatchImportJob().getType());
    }

    @Before
    public void initScheduler() throws SchedulerException {
        scheduler = StdSchedulerFactory.getDefaultScheduler();
        scheduler.start();
    }

    @After
    public void shutDownScheduler() throws SchedulerException {
        scheduler = StdSchedulerFactory.getDefaultScheduler();
        scheduler.shutdown();
    }

    @Test
    public void initJobTest() throws Exception {
        String schedule = "0 0 12 * * ?";
        String jobName = "JobX";
        String path = "/testPath";
        String profiles = "default";

        Configuration cfg = getConfig(schedule, path, profiles);

        new BatchImportJob().initJob(scheduler, jobName, cfg);

        TriggerKey tk = new TriggerKey(jobName + BatchImportJob.BATCH_IMPORT_JOB_TRIGGER_SUFFIX, BatchImportJob.BATCH_IMPORT_JOB_GROUP);
        JobKey jk = new JobKey(jobName + BatchImportJob.BATCH_IMPORT_JOB_DETAIL_SUFFIX, BatchImportJob.BATCH_IMPORT_JOB_GROUP);

        Assert.assertTrue(scheduler.checkExists(tk));
        Assert.assertTrue(scheduler.checkExists(jk));

        Trigger t = scheduler.getTrigger(tk);

        Assert.assertTrue(t.getNextFireTime().toString().contains("12:00:00"));

        Map mp = scheduler.getJobDetail(jk).getJobDataMap();

        Assert.assertEquals(path, mp.get(BatchImportJob.BATCH_IMPORT_JOB_PATH));

        List<String> profileList = (List<String>) mp.get(BatchImportJob.BATCH_IMPORT_JOB_PROFILES);

        Assert.assertEquals(1, profileList.size());
        Assert.assertEquals(profiles, profileList.get(0));
    }

    @Test(expected = IllegalArgumentException.class)
    public void initJobEmptyScheduleTest() throws Exception {
        new BatchImportJob().initJob(
                scheduler,
                "JobX", getConfig(
                        "",
                        "/testPath",
                        "default"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void initJobEmptyPathTest() throws Exception {
        new BatchImportJob().initJob(
                scheduler,
                "JobX", getConfig(
                        "0 0 12 * * ?",
                        "",
                        "default"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void initJobEmptyJobIdTest() throws Exception {
        new BatchImportJob().initJob(
                scheduler,
                "", getConfig(
                        "0 0 12 * * ?",
                        "/testPath",
                        "default"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void initJobEmptyProfilesTest() throws Exception {
        new BatchImportJob().initJob(
                scheduler,
                "JobX", getConfig(
                        "0 0 12 * * ?",
                        "/testPath",
                        ""));
    }

    //Ignored due to faulty Quartz implementation of scheduler validation, which does not detect invalid symbol in schedule
    @Deprecated
    @Test(expected = SchedulerException.class)
    @Ignore
    public void initJobInvalidScheduleTest() throws Exception {
        new BatchImportJob().initJob(
                scheduler,
                "JobX", getConfig(
                        "0 0 12X * * ?",
                        "/testPath",
                        "default"));
    }

    private Configuration getConfig(String schedule, String path, String profiles) {
        return new BaseConfiguration() {{
            addProperty(BatchImportJob.BATCH_IMPORT_JOB_PATH, path);
            addProperty(JobHandler.JOB_SCHEDULE, schedule);
            addProperty(BatchImportJob.BATCH_IMPORT_JOB_PROFILES, profiles);
        }};
    }
}
