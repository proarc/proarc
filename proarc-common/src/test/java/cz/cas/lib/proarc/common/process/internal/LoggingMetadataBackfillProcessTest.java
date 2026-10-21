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
package cz.cas.lib.proarc.common.process.internal;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import java.io.IOException;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class LoggingMetadataBackfillProcessTest {
    @Mocked BatchManager manager;
    @Mocked AkubraConfiguration config;
    @Mocked AkubraStorage storage;

    private Batch batch(Batch.State state) {
        Batch batch = new Batch();
        batch.setId(42);
        batch.setState(state);
        batch.setProfileId(Batch.INTERNAL_BACKFILL_LOG_METADATA);
        return batch;
    }

    @Test
    void cancelledQueuedJobNeverAccessesStorageAndReleasesRunGuard() throws Exception {
        Batch stopped = batch(Batch.State.STOPPED);
        new Expectations() {{ manager.get(42); result = stopped; }};
        assertSame(stopped, LoggingMetadataBackfillProcess.run(config, manager, stopped));
        assertSame(stopped, LoggingMetadataBackfillProcess.run(config, manager, stopped));
        new Verifications() {{ AkubraStorage.getInstance(config); times = 0; manager.update((Batch) any); times = 0; }};
    }

    @Test
    void cancellationRemainsStoppedWhenStorageFailsAtTheSameTime() throws Exception {
        Batch planned = batch(Batch.State.INTERNAL_PLANNED);
        Batch stopped = batch(Batch.State.STOPPED);
        new Expectations() {{
            manager.get(42); returns(planned, stopped);
            manager.update(planned); result = planned;
            AkubraStorage.getInstance(config); result = new IOException("storage unavailable");
        }};
        assertSame(stopped, LoggingMetadataBackfillProcess.run(config, manager, planned));
        assertEquals(Batch.State.STOPPED, stopped.getState());
        new Verifications() {{ manager.update(planned); times = 1; }};
    }
}
