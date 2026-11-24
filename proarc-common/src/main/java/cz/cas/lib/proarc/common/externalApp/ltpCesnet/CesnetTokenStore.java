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
package cz.cas.lib.proarc.common.externalApp.ltpCesnet;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import java.io.IOException;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.apache.commons.configuration2.Configuration;

/** Process-local credentials, isolated by installation, API destination and ProArc user. */
public final class CesnetTokenStore {
    private static final CesnetTokenStore INSTANCE = new CesnetTokenStore(Clock.systemUTC());
    private static final Duration LIFETIME = Duration.ofHours(8);
    private final Clock clock;
    private final ConcurrentHashMap<Key, Entry> tokens = new ConcurrentHashMap<>();
    private final Object[] locks = new Object[64];
    private ScheduledExecutorService cleanup;
    private record Key(String installation, String endpoint, Integer userId) { }
    // Deliberately not a record: its generated toString must never reveal a token.
    private static final class Entry {
        final String token;
        final Instant expiresAt;
        Entry(String token, Instant expiresAt) { this.token = token; this.expiresAt = expiresAt; }
    }
    public record Status(boolean present, String expiresAt) { }

    CesnetTokenStore(Clock clock) {
        this.clock = clock;
        for (int i = 0; i < locks.length; i++) locks[i] = new Object();
    }
    private Object lock(Key key) { return locks[(key.hashCode() & Integer.MAX_VALUE) % locks.length]; }
    public static CesnetTokenStore getInstance() { return INSTANCE; }

    public synchronized void startCleanup() {
        if (cleanup != null) return;
        cleanup = Executors.newSingleThreadScheduledExecutor(task -> {
            Thread thread = new Thread(task, "cesnet-token-expiry");
            thread.setDaemon(true);
            return thread;
        });
        cleanup.scheduleWithFixedDelay(this::purgeExpired, 1, 1, TimeUnit.MINUTES);
    }

    void purgeExpired() {
        tokens.entrySet().removeIf(e -> !clock.instant().isBefore(e.getValue().expiresAt));
    }

    public synchronized void shutdown() {
        if (cleanup != null) cleanup.shutdownNow();
        cleanup = null;
        tokens.clear();
    }

    private Key key(AppConfiguration config, Integer userId, Configuration options) throws IOException {
        if (userId == null) throw new IOException("Chybí vlastník přístupových údajů CESNET");
        return new Key(config.getConfigHome().getAbsoluteFile().toPath().normalize().toString(),
                options.getString("apiUrl", "https://ltp.cesnet.cz/api").replaceAll("/+$", ""), userId);
    }

    private Entry require(Key key) throws CesnetTokenException {
        Entry entry = tokens.get(key);
        if (entry == null) throw new CesnetTokenException(CesnetTokenException.Reason.MISSING);
        if (!clock.instant().isBefore(entry.expiresAt)) {
            tokens.remove(key, entry);
            throw new CesnetTokenException(CesnetTokenException.Reason.EXPIRED);
        }
        return entry;
    }

    /** Validate before remembering a replacement. Reusing a token never extends its local lifetime. */
    public Status validate(AppConfiguration config, Integer userId, String supplied) throws IOException {
        Configuration options = config.getLtpCesnetConfiguration();
        Key key = key(config, userId, options);
        synchronized (lock(key)) {
            Entry previous = tokens.get(key);
            Entry candidate;
            if (supplied == null || supplied.isBlank()) {
                candidate = require(key);
            } else if (previous != null && previous.token.equals(supplied.strip())) {
                candidate = require(key);
            } else {
                candidate = new Entry(supplied.strip(), clock.instant().plus(LIFETIME));
            }
            try (LtpCesnetClient client = new LtpCesnetClient(options, () -> candidate.token)) {
                client.validateToken();
            } catch (CesnetTokenException ex) {
                tokens.remove(key, candidate);
                throw ex;
            }
            if (!clock.instant().isBefore(candidate.expiresAt)) {
                tokens.remove(key, candidate);
                throw new CesnetTokenException(CesnetTokenException.Reason.EXPIRED);
            }
            tokens.put(key, candidate);
            purgeExpired();
            return status(config, userId);
        }
    }

    public LtpCesnetClient open(AppConfiguration config, Integer userId, Configuration options) throws IOException {
        Key key = key(config, userId, options);
        require(key);
        return new LtpCesnetClient(options, new LtpCesnetClient.TokenProvider() {
            @Override public String get() throws IOException { return require(key).token; }
            @Override public void rejected(String token) {
                synchronized (lock(key)) {
                    tokens.computeIfPresent(key, (k, entry) -> entry.token.equals(token) ? null : entry);
                }
            }
        });
    }

    public Status status(AppConfiguration config, Integer userId) throws IOException {
        Key key = key(config, userId, config.getLtpCesnetConfiguration());
        try {
            Entry entry = require(key);
            return new Status(true, entry.expiresAt.toString());
        } catch (CesnetTokenException ex) { return new Status(false, null); }
    }

    public void forget(AppConfiguration config, Integer userId) throws IOException {
        Key key = key(config, userId, config.getLtpCesnetConfiguration());
        synchronized (lock(key)) { tokens.remove(key); }
    }
}
