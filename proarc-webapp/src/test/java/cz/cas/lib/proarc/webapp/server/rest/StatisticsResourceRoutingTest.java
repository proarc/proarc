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

import cz.cas.lib.proarc.webapp.server.rest.v1.DigitalObjectResourceV1;
import cz.cas.lib.proarc.webapp.server.rest.v1.StatisticsResourceV1;
import cz.cas.lib.proarc.webapp.server.rest.v2.DigitalObjectResource;
import cz.cas.lib.proarc.webapp.server.rest.v2.StatisticsResource;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.glassfish.jersey.server.model.Resource;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.*;

class StatisticsResourceRoutingTest {
    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void articleStatisticsExposeUniqueEnabledByDefaultAndOrganizationFilter(boolean v2) {
        Class<?> type = v2 ? StatisticsResource.class : StatisticsResourceV1.class;
        var method = java.util.Arrays.stream(type.getMethods())
                .filter(candidate -> candidate.getName().equals("getArticleExportStatistics")).findFirst().orElseThrow();
        var parameters = java.util.Arrays.stream(method.getParameters())
                .filter(parameter -> parameter.isAnnotationPresent(jakarta.ws.rs.QueryParam.class))
                .collect(Collectors.toMap(parameter -> parameter.getAnnotation(jakarta.ws.rs.QueryParam.class).value(),
                        parameter -> parameter));
        assertEquals("true", parameters.get("unique").getAnnotation(jakarta.ws.rs.DefaultValue.class).value());
        assertNotNull(parameters.get("respectOrganization"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void exposesAllStatisticsAsGetEndpointsUnderTheirOwnResource(boolean v2) {
        Resource resource = Resource.from(v2 ? StatisticsResource.class : StatisticsResourceV1.class);
        assertEquals((v2 ? "v2" : "v1") + "/statistics", resource.getPath());
        Map<String, Resource> endpoints = resource.getChildResources().stream()
                .collect(Collectors.toMap(Resource::getPath, child -> child));
        assertEquals(Set.of("models", "productivity", "productivity/options", "exports", "exports/options", "exports/articles"),
                endpoints.keySet());
        endpoints.values().forEach(endpoint -> {
            assertEquals(1, endpoint.getResourceMethods().size());
            assertEquals("GET", endpoint.getResourceMethods().get(0).getHttpMethod());
            assertFalse(endpoint.getResourceMethods().get(0).getProducedTypes().isEmpty());
        });
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void objectResourceNoLongerExposesStatistics(boolean v2) {
        Resource resource = Resource.from(v2 ? DigitalObjectResource.class : DigitalObjectResourceV1.class);
        assertTrue(resource.getChildResources().stream().noneMatch(child -> child.getPath().startsWith("statistics")));
    }
}
