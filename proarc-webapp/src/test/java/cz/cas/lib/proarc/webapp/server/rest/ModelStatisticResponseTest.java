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
package cz.cas.lib.proarc.webapp.server.rest;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.*;

class ModelStatisticResponseTest {

    @org.junit.jupiter.api.Test
    void serializesZeroProductivityCountInResponseData() throws Exception {
        ObjectMapper mapper = new JacksonProvider().getContext(ProArcResponse.class);
        JsonNode root = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(
                new ProductivityStatistic(List.of("xy"), "2025-06-01", "2025-06-30", null, 0))));
        JsonNode data = root.path("response").path("data");
        assertTrue(data.isArray());
        assertEquals(1, data.size());
        assertEquals("xy", data.get(0).path("usernames").get(0).asText());
        assertEquals("2025-06-01", data.get(0).path("dateFrom").asText());
        assertEquals("2025-06-30", data.get(0).path("dateTo").asText());
        assertEquals(0L, data.get(0).path("count").asLong(-1));
        assertTrue(data.get(0).path("modelCounts").isArray());
        assertTrue(data.get(0).path("modelCounts").isEmpty());
        assertTrue(data.get(0).path("siglas").isArray());
        assertTrue(data.get(0).path("siglas").isEmpty());
    }

    @org.junit.jupiter.api.Test
    void serializesProductivityModelBreakdownAlongsideTotal() throws Exception {
        ObjectMapper mapper = new JacksonProvider().getContext(ProArcResponse.class);
        var statistic = new ProductivityStatistic(List.of(), "2026-01-01", "2026-10-19", List.of(), List.of("ABA001"),
                3_000_000_012L, List.of(new ModelStatistic("model:page", 3_000_000_000L),
                        new ModelStatistic("model:ndkmonograph", 12L)));
        JsonNode data = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(statistic)))
                .path("response").path("data").get(0);
        assertEquals(3_000_000_012L, data.path("count").asLong());
        assertEquals(2, data.path("modelCounts").size());
        assertEquals("ABA001", data.path("siglas").get(0).asText());
        assertEquals("model:page", data.path("modelCounts").get(0).path("model").asText());
        assertEquals(3_000_000_000L, data.path("modelCounts").get(0).path("count").asLong());
        assertEquals(12L, data.path("modelCounts").get(1).path("count").asLong());
    }

    static Stream<List<ModelStatistic>> statistics() {
        return Stream.of(
                Collections.emptyList(),
                List.of(new ModelStatistic("model:page", 583)),
                List.of(new ModelStatistic("model:page", 3_000_000_000L),
                        new ModelStatistic("model:ndkpage", 63)));
    }

    @ParameterizedTest
    @MethodSource("statistics")
    void serializesStatisticsAsResponseDataArray(List<ModelStatistic> statistics) throws Exception {
        ObjectMapper mapper = new JacksonProvider().getContext(ProArcResponse.class);
        String json = mapper.writeValueAsString(new ProArcResponse<>(statistics));
        JsonNode root = mapper.readTree(json);

        assertFalse(root.has("LinkedHashMap"));
        assertEquals(0, root.path("response").path("status").asInt(-1));
        JsonNode data = root.path("response").path("data");
        assertTrue(data.isArray(), json);
        assertEquals(statistics.size(), data.size());
        for (int index = 0; index < statistics.size(); index++) {
            assertEquals(statistics.get(index).getModel(), data.get(index).path("model").asText());
            assertTrue(data.get(index).path("count").isIntegralNumber());
            assertEquals(statistics.get(index).getCount(), data.get(index).path("count").asLong());
        }
    }
}
