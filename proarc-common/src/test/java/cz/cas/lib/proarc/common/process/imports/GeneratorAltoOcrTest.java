package cz.cas.lib.proarc.common.process.imports;

import com.sun.net.httpserver.HttpServer;
import cz.cas.lib.proarc.common.process.BatchManager;
import java.io.File;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.commons.configuration2.BaseConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GeneratorAltoOcrTest {

    @TempDir
    File tempDir;

    @Test
    void generatesFromJpgAndNamesOutputsAfterTiff() throws Exception {
        AtomicReference<String> uploadedPath = new AtomicReference<>();
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/post_processing_request", exchange -> {
            byte[] response = "{\"status\":\"success\",\"request_id\":\"request-1\"}"
                    .getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.createContext("/upload_image/", exchange -> {
            uploadedPath.set(exchange.getRequestURI().getPath());
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });
        server.createContext("/download_results/", exchange -> {
            byte[] response = "result".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.start();

        try {
            BaseConfiguration configuration = new BaseConfiguration();
            configuration.setProperty(ImportProfile.OCR_GEN_PROCESSOR, "pero");
            configuration.setProperty("processor.pero.url",
                    "http://localhost:" + server.getAddress().getPort() + "/");
            ImportProfile profile = new ImportProfile(configuration);
            ImportProcess.ImportOptions options = new ImportProcess.ImportOptions(
                    tempDir, null, null, false, false, null, profile, null);
            File fullJpg = new File(tempDir, "fullInput.jpg");
            File tiff = new File(tempDir, "source.tif");
            fullJpg.createNewFile();
            tiff.createNewFile();

            File[] outputs = GeneratorAltoOcr.generateOcrAndAlto(fullJpg, tiff, options);

            assertTrue(uploadedPath.get().endsWith("/upload_image/request-1/fullInput"));
            assertEquals(new File(tempDir, "source.ocr.txt"), outputs[0]);
            assertEquals(new File(tempDir, "source.ocr.xml"), outputs[1]);
            assertEquals("result" + System.lineSeparator(),
                    Files.readString(outputs[0].toPath(), StandardCharsets.UTF_8));
            assertEquals("result" + System.lineSeparator(),
                    Files.readString(outputs[1].toPath(), StandardCharsets.UTF_8));
        } finally {
            server.stop(0);
        }
    }

    @Test
    void failureLogContainsOnlyTheFailedImage() throws Exception {
        AtomicInteger requests = new AtomicInteger();
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/post_processing_request", exchange -> {
            boolean first = requests.incrementAndGet() == 1;
            byte[] response = ("{\"status\":\"" + (first ? "success" : "processing")
                    + "\",\"request_id\":\"request-" + requests.get() + "\"}")
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
            byte[] response = "result".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, response.length);
            exchange.getResponseBody().write(response);
            exchange.close();
        });
        server.start();

        try {
            BaseConfiguration configuration = new BaseConfiguration();
            configuration.setProperty(ImportProfile.OCR_GEN_PROCESSOR, "pero");
            configuration.setProperty("processor.pero.url",
                    "http://localhost:" + server.getAddress().getPort() + "/");
            configuration.setProperty("processor.pero.timeout", 0);
            ImportProfile profile = new ImportProfile(configuration);
            ImportProcess.ImportOptions options = new ImportProcess.ImportOptions(
                    tempDir, null, null, false, true, null, profile, null);
            File first = new File(tempDir, "first.jpg");
            File second = new File(tempDir, "second.jpg");
            first.createNewFile();
            second.createNewFile();

            GeneratorAltoOcr.generateOcrAndAlto(first, first, options);
            IOException failure = assertThrows(IOException.class,
                    () -> GeneratorAltoOcr.generateOcrAndAlto(second, second, options));
            String batchLog = BatchManager.toString(failure);

            assertTrue(batchLog.contains(second.getAbsolutePath()));
            assertTrue(batchLog.contains("PERO OCR is not responding: request request-2"));
            assertFalse(batchLog.contains(first.getAbsolutePath()));
        } finally {
            server.stop(0);
        }
    }
}
