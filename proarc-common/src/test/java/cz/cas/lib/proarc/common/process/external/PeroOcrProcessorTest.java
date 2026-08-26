package cz.cas.lib.proarc.common.process.external;

import com.sun.net.httpserver.HttpServer;
import java.io.File;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.commons.configuration2.BaseConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class PeroOcrProcessorTest {

    @TempDir
    File tempDir;

    @Test
    void usesDefaultTimeout() {
        PeroOcrProcessor processor = new PeroOcrProcessor(new BaseConfiguration(), null);

        assertEquals(600_000, processor.getTimeout());
    }

    @Test
    void usesConfiguredTimeout() {
        BaseConfiguration configuration = new BaseConfiguration();
        configuration.setProperty(PeroOcrProcessor.PROP_TIMEOUT, 1_000);
        PeroOcrProcessor processor = new PeroOcrProcessor(configuration, null);

        assertEquals(1_000, processor.getTimeout());
    }

    @Test
    void usesDefaultTimeoutForInvalidValue() {
        BaseConfiguration configuration = new BaseConfiguration();
        configuration.setProperty(PeroOcrProcessor.PROP_TIMEOUT, "invalid");
        PeroOcrProcessor processor = new PeroOcrProcessor(configuration, null);

        assertEquals(PeroOcrProcessor.DEFAULT_TIMEOUT, processor.getTimeout());
    }

    @Test
    void stopsStatusPollingAfterTimeout() throws Exception {
        AtomicInteger postRequests = new AtomicInteger();
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/post_processing_request", exchange -> {
            postRequests.incrementAndGet();
            byte[] response = "{\"status\":\"processing\",\"request_id\":\"request-1\"}"
                    .getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.start();

        try {
            BaseConfiguration configuration = new BaseConfiguration();
            configuration.setProperty(PeroOcrProcessor.PROP_URL,
                    "http://localhost:" + server.getAddress().getPort() + "/");
            configuration.setProperty(PeroOcrProcessor.PROP_TIMEOUT, 0);
            PeroOcrProcessor processor = new PeroOcrProcessor(configuration, null);

            assertFalse(processor.process("image.jpg", "output.txt", "output.xml"));
            assertEquals(1, postRequests.get());
        } finally {
            server.stop(0);
        }
    }

    @Test
    void stopsResultDownloadPollingAfterTimeout() throws Exception {
        AtomicInteger downloadRequests = new AtomicInteger();
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/post_processing_request", exchange -> {
            byte[] response = "{\"status\":\"success\",\"request_id\":\"request-1\"}"
                    .getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.createContext("/upload_image/", exchange -> {
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });
        server.createContext("/download_results/", exchange -> {
            downloadRequests.incrementAndGet();
            byte[] response = "{\"message\":\"not processed yet\"}"
                    .getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(400, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.start();

        try {
            BaseConfiguration configuration = new BaseConfiguration();
            configuration.setProperty(PeroOcrProcessor.PROP_URL,
                    "http://localhost:" + server.getAddress().getPort() + "/");
            configuration.setProperty(PeroOcrProcessor.PROP_TIMEOUT, 0);
            PeroOcrProcessor processor = new PeroOcrProcessor(configuration, null);
            File image = new File(tempDir, "image.jpg");
            image.createNewFile();

            assertFalse(processor.process(image.getAbsolutePath(),
                    new File(tempDir, "output.txt").getAbsolutePath(),
                    new File(tempDir, "output.xml").getAbsolutePath()));
            assertEquals(1, downloadRequests.get());
        } finally {
            server.stop(0);
        }
    }
}
