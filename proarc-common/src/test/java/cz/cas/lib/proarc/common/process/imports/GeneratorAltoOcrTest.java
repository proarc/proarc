package cz.cas.lib.proarc.common.process.imports;

import com.sun.net.httpserver.HttpServer;
import java.io.File;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.configuration2.BaseConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
                    tempDir, null, null, false, null, profile, null);
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
}
