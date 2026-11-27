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

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import org.apache.commons.configuration2.BaseConfiguration;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.junit.jupiter.api.Assertions.*;

class LtpCesnetClientTest {
    @TempDir Path temp;
    HttpServer server;
    String base;
    BaseConfiguration config;
    JSONObject registered;
    int created;
    int updated;
    int finishStatus = 200;
    int accountStatus = 200;
    int accountCalls;
    boolean uploaded;
    AtomicBoolean checkpoint = new AtomicBoolean();
    List<byte[]> parts = new ArrayList<>();
    List<String> errors = new ArrayList<>();

    @BeforeEach void setup() throws Exception {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        base = "http://127.0.0.1:" + server.getAddress().getPort();
        config = new BaseConfiguration();
        config.setProperty("apiUrl", base + "/api"); config.setProperty("groupToken", "group");
        config.setProperty("token", "test-token");
        server.createContext("/", exchange -> {
            try { handle(exchange); }
            catch (Throwable error) { errors.add(error.toString()); respond(exchange, 500, "test server error"); }
        });
        server.start();
    }
    @AfterEach void stop() { server.stop(0); assertEquals(List.of(), errors); }

    void handle(HttpExchange exchange) throws IOException {
        String path = exchange.getRequestURI().getPath();
        String method = exchange.getRequestMethod();
        if (path.startsWith("/api/")) assertEquals("Token test-token", exchange.getRequestHeaders().getFirst("Authorization"));
        else assertNull(exchange.getRequestHeaders().getFirst("Authorization"));
        if (path.equals("/api/auth/account/")) {
            accountCalls++;
            respond(exchange, accountStatus, "{\"detail\":\"response-secret\"}");
        } else if (path.equals("/api/v2/archive/") && method.equals("GET")) {
            JSONArray results = new JSONArray();
            if (registered != null) results.put(remote());
            respond(exchange, 200, new JSONObject().put("count", results.length()).put("results", results).toString());
        } else if (path.equals("/api/v2/archive/") && method.equals("POST")
                || path.equals("/api/v2/archive/12/") && method.equals("PUT")) {
            registered = new JSONObject(new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8));
            assertEquals("group", registered.getString("group")); assertEquals(10, registered.getLong("file_size"));
            if (method.equals("PUT")) updated++; else created++;
            JSONObject session = new JSONObject().put("id", updated > 0 ? 23 : 12).put("version", updated > 0 ? 2 : 1)
                    .put("parts_url", new JSONArray(List.of(base + "/s3/1", base + "/s3/2", base + "/s3/3")))
                    .put("chunk_size", 4).put("checksum_update", true).put("upload_id", "upload")
                    .put("origin", "test").put("finish_url", base + "/finish");
            // Some deployments return a JSON-encoded string instead of a JSON object.
            respond(exchange, 201, JSONObject.quote(session.toString()));
        } else if (path.startsWith("/s3/")) {
            assertTrue(checkpoint.get(), "ID must be saved before upload");
            parts.add(exchange.getRequestBody().readAllBytes());
            exchange.getResponseHeaders().set("ETag", "\"part-" + parts.size() + "\"");
            respond(exchange, 200, "");
        } else if (path.equals("/finish")) {
            JSONObject finish = new JSONObject(new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8));
            assertEquals(3, finish.getJSONArray("parts").length()); uploaded = true;
            respond(exchange, finishStatus, "signed-secret-url and password must not reach batch logs");
        } else { respond(exchange, 404, "not found"); }
    }
    JSONObject remote() {
        return new JSONObject().put("id", updated > 0 ? 23 : 12).put("version", updated > 0 ? 2 : 1)
                .put("user_metadata", registered.getJSONObject("user_metadata"))
                .put("integrity_status", "0").put("data_status", new JSONObject().put("upload_status", uploaded ? "1" : "0"));
    }
    static void respond(HttpExchange exchange, int status, String text) throws IOException {
        byte[] data = text.getBytes(StandardCharsets.UTF_8);
        exchange.sendResponseHeaders(status, data.length); exchange.getResponseBody().write(data); exchange.close();
    }
    ArchivInfo request() {
        ArchivInfo record = new ArchivInfo(); record.setOperationId("operation"); return record;
    }
    Path archive() throws IOException { return Files.writeString(temp.resolve("archive.zip"), "0123456789"); }

    @Test void usesSuppliedTokenRegistersAndUploadsAllPartsWithoutSendingCredentialsToS3() throws Exception {
        try (var client = new LtpCesnetClient(config)) {
            var result = client.submit(archive().toFile(), new JSONObject().put("name", "uuid:unit"), request(), remote -> checkpoint.set(true));
            assertEquals("12", result.id()); assertEquals(ArchivInfo.State.UPLOADED, result.state());
            assertEquals(1, created); assertEquals(0, updated); assertTrue(uploaded);
            assertEquals(List.of("0123", "4567", "89"), parts.stream().map(bytes -> new String(bytes, StandardCharsets.UTF_8)).toList());
        }
    }
    @Test void validatesTokenWithoutRegisteringAnArchive() throws Exception {
        try (var client = new LtpCesnetClient(config)) { client.validateToken(); }
        assertEquals(1, accountCalls);
        assertEquals(0, created);
    }
    @Test void rejectedTokenIsReportedWithoutLeakingRemoteResponse() throws Exception {
        accountStatus = 401;
        AtomicBoolean rejected = new AtomicBoolean();
        try (var client = new LtpCesnetClient(config, new LtpCesnetClient.TokenProvider() {
            public String get() { return "test-token"; }
            public void rejected(String token) { assertEquals("test-token", token); rejected.set(true); }
        })) {
            var error = assertThrows(CesnetTokenException.class, client::validateToken);
            assertEquals(CesnetTokenException.Reason.REJECTED, error.getReason());
            assertFalse(error.getMessage().contains("response-secret"));
        }
        assertTrue(rejected.get());
    }
    @Test void repeatedExportUsesPostEvenWithLegacyPreviousPackageId() throws Exception {
        var record = request(); record.setPreviousPackageId("12");
        try (var client = new LtpCesnetClient(config)) {
            var result = client.submit(archive().toFile(), new JSONObject().put("name", "uuid:unit"), record, remote -> checkpoint.set(true));
            assertEquals("12", result.id()); assertEquals(1, result.version()); assertEquals(1, created); assertEquals(0, updated);
        }
    }
    @Test void rejectedCompletionIsFailureAndDoesNotExposeResponseSecrets() throws Exception {
        finishStatus = 400;
        try (var client = new LtpCesnetClient(config)) {
            IOException error = assertThrows(IOException.class, () -> client.submit(archive().toFile(),
                    new JSONObject().put("name", "uuid:unit"), request(), remote -> checkpoint.set(true)));
            assertTrue(error.getMessage().contains("400")); assertFalse(error.getMessage().contains("signed-secret"));
        }
    }
    @Test void lostRegistrationResponseIsRecoveredWithoutCreatingAnotherPackage() throws Exception {
        registered = new JSONObject().put("user_metadata", new JSONObject().put("proarc_operation", "operation")); uploaded = true;
        try (var client = new LtpCesnetClient(config)) {
            var result = client.submit(archive().toFile(), new JSONObject(), request(), remote -> checkpoint.set(true));
            assertEquals("12", result.id()); assertEquals(0, created); assertEquals(0, parts.size());
        }
    }
    @Test void storedRequiresBothSuccessfulStorageAndValidIntegrity() throws Exception {
        for (String integrity : List.of("0", "1", "2", "3", "4")) {
            JSONObject value = new JSONObject().put("id", 12).put("version", 1).put("integrity_status", integrity)
                    .put("data_status", new JSONObject().put("upload_status", "2"));
            assertEquals("1".equals(integrity), LtpCesnetClient.readPackage(value).state() == ArchivInfo.State.STORED);
        }
    }
}
