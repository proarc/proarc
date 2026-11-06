package cz.cas.lib.proarc.common.info;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;

class StorageUsageTest {

    @TempDir
    Path temp;

    @Test
    void measuresStoresSeparatelyAndKeepsCachedValuesUntilRefresh() throws Exception {
        Path data = Files.createDirectory(temp.resolve("datastore"));
        Path objects = Files.createDirectory(temp.resolve("objectstore"));
        Files.write(Files.createDirectories(data.resolve("a/b")).resolve("blob"), new byte[17]);
        Files.write(data.resolve("other"), new byte[5]);
        Files.write(objects.resolve("object"), new byte[8]);
        try (StorageUsage usage = new StorageUsage()) {
            assertNull(usage.getSnapshot().datastoreSize());
            assertNull(usage.getSnapshot().objectstoreSize());
            assertNull(usage.getSnapshot().calculatedAt());
            usage.refresh(data, objects);
            StorageUsage.Snapshot first = usage.getSnapshot();
            assertEquals(22L, first.datastoreSize());
            assertEquals(8L, first.objectstoreSize());
            assertNotNull(Instant.parse(first.calculatedAt()));
            Files.write(data.resolve("new"), new byte[3]);
            assertSame(first, usage.getSnapshot());
            usage.refresh(data, objects);
            assertEquals(25L, usage.getSnapshot().datastoreSize());
        }
    }

    @Test
    void emptyDirectoryIsZeroAndFailedScanPreservesWholeSnapshot() throws Exception {
        Path data = Files.createDirectory(temp.resolve("data"));
        Path objects = Files.createDirectory(temp.resolve("objects"));
        try (StorageUsage usage = new StorageUsage()) {
            usage.refresh(data, objects);
            StorageUsage.Snapshot first = usage.getSnapshot();
            assertEquals(0L, first.datastoreSize());
            assertEquals(0L, first.objectstoreSize());
            Files.write(data.resolve("new"), new byte[4]);
            assertThrows(IOException.class, () -> usage.refresh(data, temp.resolve("missing")));
            assertSame(first, usage.getSnapshot());
            assertThrows(IOException.class, () -> StorageUsage.directorySize(data.resolve("new")));
        }
    }

    @Test
    void startMeasuresInBackgroundWithoutAnInfoRequest() throws Exception {
        Path data = Files.createDirectory(temp.resolve("data"));
        Path objects = Files.createDirectory(temp.resolve("objects"));
        Files.write(data.resolve("blob"), new byte[7]);
        try (StorageUsage usage = new StorageUsage()) {
            usage.start(data, objects);
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
            while (usage.getSnapshot().calculatedAt() == null && System.nanoTime() < deadline) {
                Thread.sleep(10);
            }
            assertEquals(7L, usage.getSnapshot().datastoreSize());
            assertEquals(0L, usage.getSnapshot().objectstoreSize());
        }
    }

    @Test
    void schedulesLocalMidnightAcrossDaylightSavingChanges() {
        ZoneId zone = ZoneId.of("Europe/Prague");
        assertEquals(Duration.ofHours(23), StorageUsage.untilNextMidnight(
                ZonedDateTime.of(2026, 3, 29, 0, 0, 0, 0, zone)));
        assertEquals(Duration.ofHours(25), StorageUsage.untilNextMidnight(
                ZonedDateTime.of(2026, 10, 25, 0, 0, 0, 0, zone)));
        assertEquals(Duration.ofMinutes(1), StorageUsage.untilNextMidnight(
                ZonedDateTime.of(2026, 11, 6, 23, 59, 0, 0, zone)));
    }
}
