package cz.cas.lib.proarc.common.info;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Duration;
import java.time.Instant;
import java.time.ZonedDateTime;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/** Cached logical file sizes of the local Akubra stores and user directories. */
public final class StorageUsage implements AutoCloseable {

    private static final Logger LOG = Logger.getLogger(StorageUsage.class.getName());
    private static final StorageUsage INSTANCE = new StorageUsage();

    private volatile Snapshot snapshot = new Snapshot(null, null, null, null);
    private ScheduledExecutorService executor;

    public static StorageUsage getInstance() {
        return INSTANCE;
    }

    public Snapshot getSnapshot() {
        return snapshot;
    }

    public synchronized void start(Path datastore, Path objectstore, Path users) {
        close();
        executor = Executors.newSingleThreadScheduledExecutor(task -> {
            Thread thread = new Thread(task, "proarc-storage-usage");
            thread.setDaemon(true);
            return thread;
        });
        ScheduledExecutorService scheduler = executor;
        scheduler.execute(() -> refreshAndSchedule(scheduler, datastore, objectstore, users));
    }

    private void refreshAndSchedule(ScheduledExecutorService scheduler, Path datastore, Path objectstore, Path users) {
        try {
            refresh(datastore, objectstore, users);
        } catch (IOException | RuntimeException ex) {
            LOG.log(Level.WARNING, "Cannot refresh storage usage; retaining the last successful measurement", ex);
        } finally {
            synchronized (this) {
                if (!scheduler.isShutdown()) {
                    // Recompute the next local midnight, including daylight saving changes.
                    long delay = untilNextMidnight(ZonedDateTime.now()).toMillis();
                    scheduler.schedule(() -> refreshAndSchedule(scheduler, datastore, objectstore, users),
                            Math.max(0, delay), TimeUnit.MILLISECONDS);
                }
            }
        }
    }

    static Duration untilNextMidnight(ZonedDateTime now) {
        return Duration.between(now, now.toLocalDate().plusDays(1).atStartOfDay(now.getZone()));
    }

    void refresh(Path datastore, Path objectstore, Path users) throws IOException {
        long datastoreSize = directorySize(datastore);
        long objectstoreSize = directorySize(objectstore);
        long usersSize = directorySize(users);
        snapshot = new Snapshot(datastoreSize, objectstoreSize, usersSize, Instant.now().toString());
    }

    static long directorySize(Path directory) throws IOException {
        directory = directory.toRealPath();
        if (!Files.isDirectory(directory)) {
            throw new IOException("Storage is not a directory: " + directory);
        }
        long[] size = {0};
        // Do not follow symbolic links into other stores or directory cycles.
        Files.walkFileTree(directory, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                checkInterrupted();
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                checkInterrupted();
                if (attrs.isRegularFile()) {
                    size[0] += attrs.size();
                }
                return FileVisitResult.CONTINUE;
            }

            private void checkInterrupted() throws InterruptedIOException {
                if (Thread.currentThread().isInterrupted()) {
                    throw new InterruptedIOException("Storage usage measurement interrupted");
                }
            }
        });
        return size[0];
    }

    @Override
    public synchronized void close() {
        if (executor != null) {
            executor.shutdownNow();
            executor = null;
        }
    }

    public record Snapshot(Long datastoreSize, Long objectstoreSize, Long usersSize, String calculatedAt) {
    }
}
