package cz.cas.lib.proarc.common.externalApp.kramerius;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Base64;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.configuration2.BaseConfiguration;
import org.apache.commons.configuration2.Configuration;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.json.JSONObject;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class KrameriusClientTest {

    private HttpServer server;

    @AfterEach
    void stopServer() {
        if (server != null) {
            server.stop(0);
        }
    }

    @Test
    void reusesAuthenticationForFollowingOperations() throws Exception {
        AtomicInteger authenticationCount = new AtomicInteger();
        AtomicReference<String> authorization = new AtomicReference<>();
        server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/login", exchange -> {
            authenticationCount.incrementAndGet();
            respond(exchange, 200, "{\"access_token\":\"test-token\"}");
        });
        server.createContext(
                "/search/api/admin/v7.0/items/uuid:test/akubra/updateManaged/"
                        + ModsStreamEditor.DATASTREAM_ID,
                exchange -> {
                    authorization.set(exchange.getRequestHeaders().getFirst("Authorization"));
                    respond(exchange, 200, "");
                });
        server.start();

        String apiUrl = "http://localhost:" + server.getAddress().getPort();
        KrameriusOptions.KrameriusInstance instance = createInstance(apiUrl);

        try (KrameriusClient client = new KrameriusClient(apiUrl)) {
            assertEquals("test-token", client.authenticate(instance));
            client.uploadXml(instance, "uuid:test", "<mods/>", ModsStreamEditor.DATASTREAM_ID);
        }

        assertEquals(1, authenticationCount.get());
        assertEquals("Bearer test-token", authorization.get());
    }

    @Test
    void recognizesSupportedVersionFormats() {
        assertEquals(KrameriusVersion.V5, KrameriusVersion.from("5.4.2"));
        assertEquals(KrameriusVersion.V7, KrameriusVersion.from("Kramerius 7.0"));
        assertEquals(KrameriusVersion.UNSUPPORTED, KrameriusVersion.from("6"));
        assertEquals(KrameriusVersion.UNSUPPORTED, KrameriusVersion.from(null));
    }

    @Test
    void rejectsUnsupportedExportTypeBeforeCallingKramerius(@TempDir Path exportFolder) throws Exception {
        String apiUrl = "http://localhost:1";
        KrameriusOptions.KrameriusInstance instance = createInstance(apiUrl);

        try (KrameriusClient client = new KrameriusClient(apiUrl)) {
            assertThrows(
                    IllegalArgumentException.class,
                    () -> client.importToKramerius(
                            instance, exportFolder.toFile(), false, "unsupported", null, null));
        }
    }

    @Test
    void importsToKramerius7(@TempDir Path exportFolder) throws Exception {
        AtomicReference<String> payload = new AtomicReference<>();
        server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/login", exchange -> respond(exchange, 200, "{\"access_token\":\"test-token\"}"));
        server.createContext("/import", exchange -> {
            payload.set(new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8));
            respond(exchange, 200, "{\"uuid\":\"process-7\"}");
        });
        server.createContext(
                "/state/process-7",
                exchange -> respond(
                        exchange,
                        200,
                        "{\"process\":{\"state\":\"FINISHED\"},\"batch\":{\"state\":\"FINISHED\"}}"));
        server.start();

        String apiUrl = "http://localhost:" + server.getAddress().getPort();
        KrameriusOptions.KrameriusInstance instance = createInstance(apiUrl, "7.0");
        try (KrameriusClient client = new KrameriusClient(apiUrl)) {
            KUtils.ImportState state = client.importToKramerius(
                    instance, exportFolder.toFile(), true, KUtils.EXPORT_KRAMERIUS, null, null);
            assertEquals(KUtils.KRAMERIUS_PROCESS_FINISHED, state.getProcessState());
            assertEquals(KUtils.KRAMERIUS_BATCH_FINISHED_V7, state.getBatchState());
        }

        JSONObject params = new JSONObject(payload.get()).getJSONObject("params");
        assertEquals(true, params.getBoolean("updateExisting"));
        assertEquals(
                "/kramerius/import/" + exportFolder.getFileName(),
                params.getString("inputDataDir"));
    }

    @Test
    void importsToKramerius5(@TempDir Path exportFolder) throws Exception {
        AtomicReference<String> authorization = new AtomicReference<>();
        AtomicReference<String> payload = new AtomicReference<>();
        server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/import", exchange -> {
            authorization.set(exchange.getRequestHeaders().getFirst("Authorization"));
            payload.set(new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8));
            respond(exchange, 200, "{\"uuid\":\"process-5\"}");
        });
        server.createContext(
                "/state/process-5",
                exchange -> respond(
                        exchange,
                        200,
                        "[{\"state\":\"FINISHED\",\"batchState\":\"BATCH_FINISHED\"}]"));
        server.start();

        String apiUrl = "http://localhost:" + server.getAddress().getPort();
        KrameriusOptions.KrameriusInstance instance = createInstance(apiUrl, "5.4");
        try (KrameriusClient client = new KrameriusClient(apiUrl)) {
            KUtils.ImportState state = client.importToKramerius(
                    instance, exportFolder.toFile(), false, KUtils.EXPORT_KRAMERIUS, null, null);
            assertEquals(KUtils.KRAMERIUS_PROCESS_FINISHED, state.getProcessState());
            assertEquals(KUtils.KRAMERIUS_BATCH_FINISHED_V5, state.getBatchState());
        }

        String credentials = Base64.getEncoder()
                .encodeToString("user:password".getBytes(StandardCharsets.UTF_8));
        assertEquals("Basic " + credentials, authorization.get());
        JSONObject mapping = new JSONObject(payload.get()).getJSONObject("mapping");
        assertEquals("false", mapping.getString("updateExisting"));
        assertEquals(
                "/kramerius/import/" + exportFolder.getFileName(),
                mapping.getString("importDirectory"));
    }

    private KrameriusOptions.KrameriusInstance createInstance(String apiUrl) {
        return createInstance(apiUrl, "7.0");
    }

    private KrameriusOptions.KrameriusInstance createInstance(String apiUrl, String version) {
        Configuration config = new BaseConfiguration();
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_VERSION, version);
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_URL, apiUrl);
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_URL_LOGIN, "/login");
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_URL_PARAMETRIZED_IMPORT_QUERY, "/import");
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_URL_STATE_QUERY, "/state/");
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_KRAMERIUS_IMPORT_FOXML_FOLDER, "/kramerius/import/");
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_USERNAME, "user");
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_PASSWORD, "password");
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_CLIENT_ID, "client");
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_CLIENT_SECRET, "secret");
        config.setProperty(KrameriusOptions.KrameriusInstance.PROPERTY_GRANT_TYPE, "password");
        return new KrameriusOptions.KrameriusInstance("test", config);
    }

    private void respond(HttpExchange exchange, int status, String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.sendResponseHeaders(status, bytes.length);
        exchange.getResponseBody().write(bytes);
        exchange.close();
    }
}
