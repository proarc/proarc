/*
 * Copyright (C) 2025 Lukas Sykora
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
package cz.cas.lib.proarc.common.process;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

public class WorkWindow {

    public static boolean isNotAllowed(Batch batch) {

        if (batch == null) {
            return true;
        }

        if (!batch.isNightOnly()) {
            return true;
        }

        BatchParams params = batch.getParamsAsObject();
        if (params == null) {
            return true;
        }

        if (params.getNotBefore() == null || params.getNotAfter() == null) {
            return true;
        }

        Timestamp notBefore = Timestamp.valueOf(params.getNotBefore());
        Timestamp notAfter = Timestamp.valueOf(params.getNotAfter());

        Timestamp now = new Timestamp(System.currentTimeMillis());

        return !now.before(notBefore) && !now.after(notAfter);
    }

    public static Timestamp nextWindowStart() {
        LocalTime windowStart = getWindowStart();
        LocalDateTime now = LocalDateTime.now();

        LocalDateTime candidate = now.toLocalDate().atTime(windowStart);
        if (now.isBefore(candidate)) {
            return Timestamp.valueOf(candidate);
        } else {
            return Timestamp.valueOf(candidate.plusDays(1));
        }
    }

    private static LocalTime getWindowStart() {
        try {
            AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
            String startTime = appConfig.getBatchSchedulerStartTime();

            if (startTime == null) {
                return parseTime("18:00");
            }

            return parseTime(startTime);
        } catch (AppConfigurationException e) {
            return parseTime("18:00");
        }
    }

    public static Timestamp nextWindowEnd(Timestamp start) {
        LocalTime windowEnd = getWindowEnd();
        LocalDateTime startWindow = start.toLocalDateTime();

        LocalDate candidate = startWindow.toLocalDate();

        if (!windowEnd.isAfter(startWindow.toLocalTime())) {
            candidate = candidate.plusDays(1);
        }

        return Timestamp.valueOf(candidate.atTime(windowEnd));
    }

    private static LocalTime getWindowEnd() {
        try {
            AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
            String endTime = appConfig.getBatchSchedulerEndTime();
            if (endTime == null) {
                return parseTime("06:00");
            }

            return parseTime(endTime);
        } catch (AppConfigurationException e) {
            return parseTime("06:00");
        }
    }

    private static LocalTime parseTime(String startTime) {
        DateTimeFormatter TIME_FMT = DateTimeFormatter.ofPattern("HH:mm");
        return LocalTime.parse(startTime, TIME_FMT);
    }

    public static Batch scheduleBatch(Batch batch) {
        Timestamp nextStart = WorkWindow.nextWindowStart();
        Timestamp nextEnd = WorkWindow.nextWindowEnd(nextStart);

        BatchParams params = batch.getParamsAsObject();
        if (params == null) {
            params = new BatchParams();
        }
        params.setNotBefore(nextStart.toString());
        params.setNotAfter(nextEnd.toString());

        batch.setParamsFromObject(params);

        BatchManager batchManager = BatchManager.getInstance();
        return batchManager.update(batch);
    }

    public static Batch reschedule(Batch batch) {
        return scheduleBatch(batch);
    }
}
