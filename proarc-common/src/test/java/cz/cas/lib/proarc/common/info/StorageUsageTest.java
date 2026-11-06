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
        Path users = Files.createDirectory(temp.resolve("users"));
        Files.write(Files.createDirectories(data.resolve("a/b")).resolve("blob"), new byte[17]);
        Files.write(data.resolve("other"), new byte[5]);
        Files.write(objects.resolve("object"), new byte[8]);
        Files.write(Files.createDirectories(users.resolve("alice/import/nested")).resolve("file"), new byte[11]);
        Files.write(Files.createDirectories(users.resolve("bob/export")).resolve("file"), new byte[13]);
        try (StorageUsage usage = new StorageUsage()) {
            assertNull(usage.getSnapshot().datastoreSize());
            assertNull(usage.getSnapshot().objectstoreSize());
            assertNull(usage.getSnapshot().usersSize());
            assertNull(usage.getSnapshot().calculatedAt());
            usage.refresh(data, objects, users);
            StorageUsage.Snapshot first = usage.getSnapshot();
            assertEquals(22L, first.datastoreSize());
            assertEquals(8L, first.objectstoreSize());
            assertEquals(24L, first.usersSize());
            assertNotNull(Instant.parse(first.calculatedAt()));
            Files.write(data.resolve("new"), new byte[3]);
            Files.write(users.resolve("alice/new"), new byte[9]);
            assertSame(first, usage.getSnapshot());
            usage.refresh(data, objects, users);
            assertEquals(25L, usage.getSnapshot().datastoreSize());
            assertEquals(33L, usage.getSnapshot().usersSize());
        }
    }

    @Test
    void emptyDirectoryIsZeroAndFailedScanPreservesWholeSnapshot() throws Exception {
        Path data = Files.createDirectory(temp.resolve("data"));
        Path objects = Files.createDirectory(temp.resolve("objects"));
        Path users = Files.createDirectory(temp.resolve("users"));
        try (StorageUsage usage = new StorageUsage()) {
            usage.refresh(data, objects, users);
            StorageUsage.Snapshot first = usage.getSnapshot();
            assertEquals(0L, first.datastoreSize());
            assertEquals(0L, first.objectstoreSize());
            assertEquals(0L, first.usersSize());
            Files.write(data.resolve("new"), new byte[4]);
            assertThrows(IOException.class, () -> usage.refresh(data, temp.resolve("missing"), users));
            assertSame(first, usage.getSnapshot());
            assertThrows(IOException.class, () -> usage.refresh(data, objects, temp.resolve("missing-users")));
            assertSame(first, usage.getSnapshot());
            assertThrows(IOException.class, () -> StorageUsage.directorySize(data.resolve("new")));
        }
    }

    @Test
    void startMeasuresInBackgroundWithoutAnInfoRequest() throws Exception {
        Path data = Files.createDirectory(temp.resolve("data"));
        Path objects = Files.createDirectory(temp.resolve("objects"));
        Path users = Files.createDirectory(temp.resolve("users"));
        Files.write(data.resolve("blob"), new byte[7]);
        Files.write(Files.createDirectories(users.resolve("alice/edit")).resolve("image"), new byte[19]);
        try (StorageUsage usage = new StorageUsage()) {
            usage.start(data, objects, users);
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
            while (usage.getSnapshot().calculatedAt() == null && System.nanoTime() < deadline) {
                Thread.sleep(10);
            }
            assertEquals(7L, usage.getSnapshot().datastoreSize());
            assertEquals(0L, usage.getSnapshot().objectstoreSize());
            assertEquals(19L, usage.getSnapshot().usersSize());
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
