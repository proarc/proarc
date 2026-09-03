/*
 * Copyright (C) 2026 Lukas Sykora
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
package cz.cas.lib.proarc.common.process;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class WorkWindowTest {

    @Test
    void usesPragueTimeZone() {
        assertEquals(ZoneId.of("Europe/Prague"), WorkWindow.WORK_ZONE);
    }

    @Test
    void convertsInstantToPragueSummerTime() {
        Clock clock = Clock.fixed(Instant.parse("2026-09-01T16:00:00Z"), ZoneOffset.UTC);

        assertEquals(LocalDateTime.parse("2026-09-01T18:00:00"), WorkWindow.currentDateTime(clock));
    }

    @Test
    void convertsInstantToPragueWinterTime() {
        Clock clock = Clock.fixed(Instant.parse("2026-12-01T17:00:00Z"), ZoneOffset.UTC);

        assertEquals(LocalDateTime.parse("2026-12-01T18:00:00"), WorkWindow.currentDateTime(clock));
    }

    @Test
    void evaluatesStoredWindowAsPragueLocalTime() {
        Batch batch = scheduledBatch("2026-09-01 18:00:00.0", "2026-09-02 06:00:00.0");

        assertFalse(WorkWindow.isNotAllowed(batch, LocalDateTime.parse("2026-09-01T17:59:59")));
        assertTrue(WorkWindow.isNotAllowed(batch, LocalDateTime.parse("2026-09-01T18:00:00")));
        assertTrue(WorkWindow.isNotAllowed(batch, LocalDateTime.parse("2026-09-02T05:59:59")));
        assertFalse(WorkWindow.isNotAllowed(batch, LocalDateTime.parse("2026-09-02T06:00:01")));
    }

    private Batch scheduledBatch(String notBefore, String notAfter) {
        BatchParams params = new BatchParams();
        params.setNotBefore(notBefore);
        params.setNotAfter(notAfter);

        Batch batch = new Batch();
        batch.setNightOnly(true);
        batch.setParamsFromObject(params);
        return batch;
    }
}
