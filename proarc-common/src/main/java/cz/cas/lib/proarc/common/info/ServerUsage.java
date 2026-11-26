package cz.cas.lib.proarc.common.info;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.DateTimeException;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/** Reads the collector's atomically published files without scanning the disks. */
public final class ServerUsage {
    private static final Logger LOG = Logger.getLogger(ServerUsage.class.getName());
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private ServerUsage() { }

    public record Disk(String id, String name, String path, Long totalBytes, Long usedBytes,
                       Long freeBytes, String error) { }
    public record Sample(int schemaVersion, String measuredAt, String date, List<Disk> disks) { }
    public record Current(Sample actual, String error, StorageUsage.Snapshot storageUsage) { }
    public record History(List<Sample> samples, String error) { }

    public static Current current(Path file, StorageUsage.Snapshot storage) {
        try {
            return new Current(sample(read(file)), null, storage);
        } catch (NoSuchFileException ex) {
            return new Current(null, "missing", storage);
        } catch (IOException | IllegalArgumentException | DateTimeException ex) {
            LOG.log(Level.WARNING, "Cannot read server usage: " + file, ex);
            return new Current(null, "invalid", storage);
        }
    }

    public static History history(Path file) {
        try {
            JsonNode root = read(file);
            version(root);
            require(root.path("samples").isArray(), "Expected samples array");
            List<Sample> samples = new ArrayList<>();
            HashSet<String> dates = new HashSet<>();
            for (JsonNode node : root.path("samples")) {
                Sample sample = sample(node);
                require(dates.add(sample.date()), "Duplicate sample date");
                samples.add(sample);
            }
            samples.sort(Comparator.comparing(Sample::date));
            return new History(List.copyOf(samples), null);
        } catch (NoSuchFileException ex) {
            return new History(List.of(), "missing");
        } catch (IOException | IllegalArgumentException | DateTimeException ex) {
            LOG.log(Level.WARNING, "Cannot read server usage history: " + file, ex);
            return new History(List.of(), "invalid");
        }
    }

    private static JsonNode read(Path file) throws IOException {
        try (var input = Files.newInputStream(file)) {
            return MAPPER.readTree(input);
        }
    }

    private static void version(JsonNode root) {
        require(root != null && root.isObject() && root.path("schemaVersion").isIntegralNumber()
                && root.path("schemaVersion").asLong() == 1, "Unsupported schemaVersion");
    }

    private static Sample sample(JsonNode root) {
        version(root);
        String measuredAt = text(root, "measuredAt");
        String date = text(root, "date");
        OffsetDateTime.parse(measuredAt);
        LocalDate.parse(date);
        require(root.path("disks").isArray(), "Expected disks array");
        List<Disk> disks = new ArrayList<>();
        HashSet<String> ids = new HashSet<>();
        for (JsonNode disk : root.path("disks")) {
            String id = text(disk, "id");
            require(ids.add(id), "Duplicate disk id");
            String name = text(disk, "name");
            String path = text(disk, "path");
            JsonNode error = disk.path("error");
            if (error.isTextual() && !error.asText().isBlank()) {
                require(disk.path("totalBytes").isNull() && disk.path("usedBytes").isNull()
                        && disk.path("freeBytes").isNull(), "Failed disks must have null sizes");
                disks.add(new Disk(id, name, path, null, null, null, error.asText()));
            } else {
                require(error.isNull() || error.isMissingNode(), "Invalid disk error");
                long total = number(disk, "totalBytes");
                long used = number(disk, "usedBytes");
                long free = number(disk, "freeBytes");
                // Available space may be negative on Unix; reserved blocks also mean
                // used + free does not necessarily equal total.
                require(total >= 0 && used >= 0 && used <= total, "Invalid disk sizes");
                disks.add(new Disk(id, name, path, total, used, free, null));
            }
        }
        return new Sample(1, measuredAt, date, List.copyOf(disks));
    }

    private static String text(JsonNode root, String field) {
        JsonNode value = root.path(field);
        require(value.isTextual() && !value.asText().isBlank(), "Invalid " + field);
        return value.asText();
    }

    private static long number(JsonNode root, String field) {
        JsonNode value = root.path(field);
        require(value.isIntegralNumber() && value.canConvertToLong(), "Invalid " + field);
        return value.longValue();
    }

    private static void require(boolean valid, String message) {
        if (!valid) throw new IllegalArgumentException(message);
    }
}
