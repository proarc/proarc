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

import org.apache.commons.configuration2.Configuration;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;

/**
 * Interface for Quartz-based schedulable job
 *
 * @author Jakub Kremlacek
 */
public interface ProArcJob {

    /**
     * String identifier of a job type, should correspond to class name
     *
     * @return
     */
    String getType();

    /**
     * Initializes the job and sets itself into the supplied scheduler
     *
     * @param scheduler scheduler for the job to schedule into
     * @param jobId     job instance identifier from config (do not mix with job type)
     * @param jobConfig subset of app configuration with config specified by jobId
     * @throws SchedulerException when job scheduling fails
     */
    void initJob(Scheduler scheduler, String jobId, Configuration jobConfig) throws SchedulerException;
}
