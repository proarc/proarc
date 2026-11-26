package cz.cas.lib.proarc.common.info;

import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.junit.jupiter.api.Assertions.*;

class ServerUsageTest {
    @TempDir Path dir;
    private static final String SAMPLE = """
            {"schemaVersion":1,"measuredAt":"2026-11-25T00:00:00+01:00","date":"2026-11-25",
             "disks":[{"id":"archive","name":"Archiv ProArc","path":"/data",
             "totalBytes":9000000000,"usedBytes":6000000000,"freeBytes":2000000000,"error":null},
             {"id":"offline","name":"Offline","path":"/offline",
             "totalBytes":null,"usedBytes":null,"freeBytes":null,"error":"Not mounted"}]}
            """;

    @Test void readsMultipleDisksAndRefreshesAtomicallyPublishedContents() throws Exception {
        Path file = dir.resolve("actual.json");
        Files.writeString(file, SAMPLE);
        var storage = new StorageUsage.Snapshot(10L, 20L, 30L, "2026-11-25T00:00:00Z");
        var result = ServerUsage.current(file, storage);
        assertNull(result.error());
        assertSame(storage, result.storageUsage());
        assertEquals(6000000000L, result.actual().disks().get(0).usedBytes());
        assertNull(result.actual().disks().get(1).usedBytes());
        assertEquals("Not mounted", result.actual().disks().get(1).error());
        Files.writeString(file, SAMPLE.replace("6000000000", "7000000000"));
        assertEquals(7000000000L, ServerUsage.current(file, storage).actual().disks().get(0).usedBytes());
    }

    @Test void missingOrMalformedDataIsUnavailableRatherThanZero() throws Exception {
        Path file = dir.resolve("actual.json");
        assertEquals("missing", ServerUsage.current(file, null).error());
        assertEquals("missing", ServerUsage.history(file).error());
        for (String json : new String[]{"{", "null", SAMPLE + SAMPLE,
                SAMPLE.replace("2026-11-25", "invalid"), SAMPLE.replace("6000000000", "6.2"),
                SAMPLE.replace("schemaVersion\":1", "schemaVersion\":2"),
                SAMPLE.replace("\"offline\"", "\"archive\"")}) {
            Files.writeString(file, json);
            var result = ServerUsage.current(file, null);
            assertEquals("invalid", result.error(), json);
            assertNull(result.actual());
        }
    }

    @Test void sortsHistoryAndRejectsDuplicateDates() throws Exception {
        Path file = dir.resolve("history.json");
        String earlier = SAMPLE.replace("2026-11-25", "2026-11-24");
        Files.writeString(file, "{\"schemaVersion\":1,\"samples\":[" + SAMPLE + "," + earlier + "]}");
        var result = ServerUsage.history(file);
        assertNull(result.error());
        assertEquals("2026-11-24", result.samples().get(0).date());
        Files.writeString(file, "{\"schemaVersion\":1,\"samples\":[" + SAMPLE + "," + SAMPLE + "]}");
        assertEquals("invalid", ServerUsage.history(file).error());
        assertTrue(ServerUsage.history(file).samples().isEmpty());
    }
}
