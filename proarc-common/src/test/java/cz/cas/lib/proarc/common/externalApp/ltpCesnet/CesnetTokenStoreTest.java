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

import com.sun.net.httpserver.HttpServer;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.junit.jupiter.api.Assertions.*;

class CesnetTokenStoreTest {
    @TempDir Path temp;
    HttpServer server;
    AppConfiguration config;
    AtomicInteger status = new AtomicInteger(200);
    AtomicInteger calls = new AtomicInteger();
    MutableClock clock = new MutableClock();
    CesnetTokenStore store = new CesnetTokenStore(clock);

    @BeforeEach void setup() throws Exception {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/", exchange -> {
            calls.incrementAndGet();
            int response = exchange.getRequestURI().getPath().equals("/api/auth/account/")
                    && "Token secret".equals(exchange.getRequestHeaders().getFirst("Authorization")) ? status.get() : 401;
            byte[] body = "{\"detail\":\"remote-secret\"}".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(response, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.start();
        config = configuration(temp.resolve("first"));
    }

    AppConfiguration configuration(Path home) throws Exception {
        Files.createDirectories(home);
        Files.writeString(home.resolve("proarc.cfg"), "export.ltpCesnet.apiUrl=http://127.0.0.1:"
                + server.getAddress().getPort() + "/api\nexport.ltpCesnet.groupToken=group\n");
        return AppConfigurationFactory.getInstance().create(new HashMap<>(Map.of(AppConfiguration.PROPERTY_APP_HOME, home.toString())));
    }

    @AfterEach void stop() { server.stop(0); }

    @Test void reusesTokenWithoutExtendingEightHourLifetimeAndExpiresAtBoundary() throws Exception {
        var saved = store.validate(config, 7, "secret");
        assertTrue(saved.present());
        clock.now = clock.now.plusSeconds(7 * 3600);
        assertEquals(saved, store.validate(config, 7, null));
        assertEquals(saved, store.validate(config, 7, "secret"));
        clock.now = clock.now.plusSeconds(3600);
        var error = assertThrows(CesnetTokenException.class, () -> store.validate(config, 7, null));
        assertEquals(CesnetTokenException.Reason.EXPIRED, error.getReason());
        assertFalse(store.status(config, 7).present());
        assertEquals(3, calls.get(), "Expired credentials must not leave ProArc");
    }

    @Test void isolatesUsersAndInstallationsAndResetInvalidatesAlreadyOpenedClients() throws Exception {
        store.validate(config, 7, "secret");
        assertFalse(store.status(config, 8).present());
        assertFalse(store.status(configuration(temp.resolve("second")), 7).present());
        try (var client = store.open(config, 7, config.getLtpCesnetConfiguration())) {
            store.forget(config, 7);
            assertThrows(CesnetTokenException.class, client::validateToken);
        }
        assertEquals(1, calls.get());
    }

    @Test void rejectedCachedTokenIsRemovedButPermissionAndServerErrorsPreserveIt() throws Exception {
        store.validate(config, 7, "secret");
        for (int code : new int[]{403, 500}) {
            status.set(code);
            IOException error = assertThrows(IOException.class, () -> store.validate(config, 7, null));
            assertFalse(error instanceof CesnetTokenException);
            assertFalse(error.getMessage().contains("remote-secret"));
            assertTrue(store.status(config, 7).present());
        }
        status.set(401);
        var error = assertThrows(CesnetTokenException.class, () -> store.validate(config, 7, null));
        assertEquals(CesnetTokenException.Reason.REJECTED, error.getReason());
        assertFalse(store.status(config, 7).present());
    }

    @Test void invalidReplacementDoesNotDestroyValidSavedToken() throws Exception {
        var saved = store.validate(config, 7, "secret");
        assertThrows(CesnetTokenException.class, () -> store.validate(config, 7, "wrong"));
        assertEquals(saved, store.status(config, 7));
    }

    @Test void runtimeUnauthorizedResponseEvictsTokenAndNetworkFailureDoesNot() throws Exception {
        store.validate(config, 7, "secret");
        try (var client = store.open(config, 7, config.getLtpCesnetConfiguration())) {
            status.set(401);
            assertThrows(CesnetTokenException.class, client::validateToken);
        }
        assertFalse(store.status(config, 7).present());
        status.set(200);
        store.validate(config, 7, "secret");
        server.stop(0);
        assertThrows(IOException.class, () -> store.validate(config, 7, null));
        assertTrue(store.status(config, 7).present());
    }

    @Test void openedClientCannotUseExpiredToken() throws Exception {
        store.validate(config, 7, "secret");
        try (var client = store.open(config, 7, config.getLtpCesnetConfiguration())) {
            clock.now = clock.now.plusSeconds(8 * 3600);
            assertThrows(CesnetTokenException.class, client::validateToken);
        }
        assertEquals(1, calls.get());
    }

    @Test void cleanupAndShutdownDiscardCredentialsAndDestinationIsPartOfCacheKey() throws Exception {
        store.validate(config, 7, "secret");
        var otherDestination = new org.apache.commons.configuration2.BaseConfiguration();
        otherDestination.copy(config.getLtpCesnetConfiguration());
        otherDestination.setProperty("apiUrl", "http://127.0.0.1:1/api");
        assertThrows(CesnetTokenException.class, () -> store.open(config, 7, otherDestination));
        clock.now = clock.now.plusSeconds(8 * 3600);
        store.purgeExpired();
        assertFalse(store.status(config, 7).present());
        store.validate(config, 7, "secret");
        store.shutdown();
        assertFalse(store.status(config, 7).present());
    }

    private static class MutableClock extends Clock {
        Instant now = Instant.parse("2026-11-19T10:00:00Z");
        @Override public ZoneId getZone() { return ZoneOffset.UTC; }
        @Override public Clock withZone(ZoneId zone) { return this; }
        @Override public Instant instant() { return now; }
    }
}
