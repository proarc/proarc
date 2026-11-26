package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.info.ServerUsage;
import cz.cas.lib.proarc.common.info.StorageUsage;
import java.util.List;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class ServerUsageResponseTest {
    @Test void serializesCurrentUsageAndUnavailableMeasurements() throws Exception {
        var mapper = new JacksonProvider().getContext(ProArcResponse.class);
        var current = new ServerUsage.Current(new ServerUsage.Sample(1, "2026-11-25T00:00:00Z", "2026-11-25",
                List.of(new ServerUsage.Disk("archive", "Archiv", "/data", 9000000000L, 6000000000L, 3000000000L, null))),
                null, new StorageUsage.Snapshot(10L, 20L, null, "2026-11-25T00:00:00Z"));
        var data = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(current)))
                .path("response").path("data").get(0);
        assertEquals(6000000000L, data.path("actual").path("disks").get(0).path("usedBytes").asLong());
        assertEquals(10L, data.path("storageUsage").path("datastoreSize").asLong());
        var missing = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(
                new ServerUsage.History(List.of(), "missing")))).path("response").path("data").get(0);
        assertEquals("missing", missing.path("error").asText());
        assertTrue(missing.path("samples").isEmpty());
    }
}
