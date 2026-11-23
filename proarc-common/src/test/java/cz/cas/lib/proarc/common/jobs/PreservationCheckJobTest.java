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
package cz.cas.lib.proarc.common.jobs;

import com.sun.net.httpserver.HttpServer;
import cz.cas.lib.proarc.common.externalApp.ltp.PreservationIndex;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import java.io.ByteArrayOutputStream;
import java.net.InetSocketAddress;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.util.JavaBinCodec;
import org.apache.solr.common.util.NamedList;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;

class PreservationCheckJobTest {
    @TempDir
    Path temp;

    @Test
    void missingAndEmptyPidsQueryAllPendingRecordsWhileSelectedPidsAddFilter() throws Exception {
        List<String> queries = new ArrayList<>();
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        NamedList<Object> response = new NamedList<>();
        response.add("response", new SolrDocumentList());
        response.add("nextCursorMark", "*");
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (JavaBinCodec codec = new JavaBinCodec()) {
            codec.marshal(response, bytes);
        }
        server.createContext("/solr/searchCore/select", exchange -> {
            queries.add(URLDecoder.decode(exchange.getRequestURI().getRawQuery(), StandardCharsets.UTF_8));
            exchange.getResponseHeaders().set("Content-Type", "application/octet-stream");
            exchange.sendResponseHeaders(200, bytes.size());
            try (var output = exchange.getResponseBody()) {
                output.write(bytes.toByteArray());
            }
        });
        server.start();
        try {
            Files.writeString(temp.resolve("akubra.cfg"), "solrSearchHost=http://127.0.0.1:"
                    + server.getAddress().getPort() + "/solr/searchCore\n");
            AkubraConfiguration akubra = new AkubraConfiguration(
                    Map.of(AkubraConfiguration.PROPERTY_APP_HOME, temp.toString()), temp.toFile());
            for (List<String> pids : java.util.Arrays.asList(null, List.<String>of(), List.of("uuid:one", "uuid:two"))) {
                assertEquals(0, PreservationCheckJob.checkAll(null, akubra, null,
                        message -> fail(message), pid -> fail("Unexpected catalog update"), pids));
            }
            assertEquals(3, queries.size());
            for (String query : queries) {
                assertTrue(query.contains("q=" + PreservationIndex.PENDING + ":true"), query);
                assertTrue(query.contains("cursorMark=*"), query);
                assertTrue(query.contains("sort=pid asc"), query);
            }
            assertFalse(queries.get(0).contains("fq="));
            assertFalse(queries.get(1).contains("fq="));
            assertTrue(queries.get(2).contains("fq=pid:(\"uuid\\:one\" OR \"uuid\\:two\")"), queries.get(2));
        } finally {
            server.stop(0);
        }
    }
}
