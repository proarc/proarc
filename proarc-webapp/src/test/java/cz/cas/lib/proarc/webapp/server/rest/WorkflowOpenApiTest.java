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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.introspect.BeanPropertyDefinition;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.storage.StringEditor.StringRecord;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterView;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinitionView;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowItemView;
import cz.cas.lib.proarc.webapp.server.rest.v1.WorkflowResourceV1.TaskUpdate;
import cz.cas.lib.proarc.webapp.server.rest.v1.WorkflowResourceV1.TasksUpdate;
import cz.cas.lib.proarc.webapp.server.rest.v2.WorkflowResource;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.QueryParam;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class WorkflowOpenApiTest {
    private final ObjectMapper mapper = new JacksonProvider().getContext(ProArcResponse.class);

    @Test
    void allWorkflowOperationsMatchResourceReturnTypesAndParameters() throws Exception {
        JSONObject spec = loadSpec();
        JSONObject paths = spec.getJSONObject("paths");
        JSONObject schemas = spec.getJSONObject("components").getJSONObject("schemas");
        Set<String> documented = new HashSet<>();
        for (String path : paths.keySet()) {
            if (path.equals("/workflow") || path.startsWith("/workflow/")) {
                for (String verb : paths.getJSONObject(path).keySet()) {
                    documented.add(verb.toUpperCase() + " " + path);
                }
            }
        }
        Set<String> implemented = new HashSet<>();
        for (var method : WorkflowResource.class.getDeclaredMethods()) {
            for (var annotation : method.getAnnotations()) {
                HttpMethod httpMethod = annotation.annotationType().getAnnotation(HttpMethod.class);
                if (httpMethod == null) {
                    continue;
                }
                String path = "/workflow" + (method.isAnnotationPresent(Path.class)
                        ? "/" + method.getAnnotation(Path.class).value() : "");
                String operationName = httpMethod.value() + " " + path;
                implemented.add(operationName);
                JSONObject operation = paths.getJSONObject(path).getJSONObject(httpMethod.value().toLowerCase());
                JSONObject response = resolve(schemas, operation.getJSONObject("responses").getJSONObject("default")
                        .getJSONObject("content").getJSONObject("application/json").getJSONObject("schema"));
                if (method.getReturnType() == ProArcResponse.class) {
                    Type itemType = ((ParameterizedType) method.getGenericReturnType()).getActualTypeArguments()[0];
                    String itemSchema = itemType instanceof ParameterizedType
                            ? "DescriptionMetadataObject" : ((Class<?>) itemType).getSimpleName();
                    JSONObject data = response.getJSONObject("properties").getJSONObject("response")
                            .getJSONObject("properties").getJSONObject("data");
                    assertEquals("#/components/schemas/" + itemSchema,
                            data.getJSONObject("items").getString("$ref"), operationName);
                    assertEquals("array", data.getString("type"), operationName);
                } else {
                    assertEquals(StringRecord.class, method.getReturnType());
                    assertEquals("#/components/schemas/StringRecord", response.getJSONObject("properties")
                            .getJSONObject("record").getString("$ref"), operationName);
                }
                Set<String> queryNames = new HashSet<>();
                Set<String> formNames = new HashSet<>();
                for (var parameter : method.getParameters()) {
                    if (parameter.isAnnotationPresent(QueryParam.class)) {
                        String name = parameter.getAnnotation(QueryParam.class).value();
                        queryNames.add(name);
                        JSONObject documentedParameter = operation.getJSONArray("parameters").toList().stream()
                                .map(value -> new JSONObject((Map<?, ?>) value))
                                .filter(value -> name.equals(value.getString("name")))
                                .findFirst().orElseThrow();
                        assertEquals("query", documentedParameter.getString("in"));
                        assertParameterType(parameter.getParameterizedType(), documentedParameter.getJSONObject("schema"));
                    } else if (parameter.isAnnotationPresent(FormParam.class)) {
                        String name = parameter.getAnnotation(FormParam.class).value();
                        formNames.add(name);
                        JSONObject content = operation.getJSONObject("requestBody").getJSONObject("content");
                        assertTrue(content.has("application/x-www-form-urlencoded"), operationName);
                        assertParameterType(parameter.getParameterizedType(), content.getJSONObject("application/x-www-form-urlencoded")
                                .getJSONObject("schema").getJSONObject("properties").getJSONObject(name));
                    } else {
                        assertEquals("#/components/schemas/" + parameter.getType().getSimpleName(), operation
                                .getJSONObject("requestBody").getJSONObject("content").getJSONObject("application/json")
                                .getJSONObject("schema").getString("$ref"), operationName);
                    }
                }
                JSONArray parameters = operation.optJSONArray("parameters");
                assertEquals(queryNames.size(), parameters == null ? 0 : parameters.length(), operationName);
                if (!formNames.isEmpty()) {
                    assertEquals(formNames, operation.getJSONObject("requestBody").getJSONObject("content")
                            .getJSONObject("application/x-www-form-urlencoded").getJSONObject("schema")
                            .getJSONObject("properties").keySet(), operationName);
                }
            }
        }
        assertEquals(implemented, documented);
        assertEquals(16, implemented.size());
    }

    @Test
    void workflowDtoPropertiesMatchJacksonMapping() throws Exception {
        JSONObject schemas = loadSpec().getJSONObject("components").getJSONObject("schemas");
        Map<String, Class<?>> types = Map.of(
                "JobView", JobView.class, "TaskView", TaskView.class,
                "MaterialView", MaterialView.class, "TaskParameterView", TaskParameterView.class,
                "JobDefinitionView", JobDefinitionView.class, "WorkflowItemView", WorkflowItemView.class,
                "TaskUpdate", TaskUpdate.class, "TasksUpdate", TasksUpdate.class,
                "DescriptionMetadataObject", DescriptionMetadata.class, "StringRecord", StringRecord.class);
        Set<String> differences = new HashSet<>();
        for (var entry : types.entrySet()) {
            Set<String> serialized = mapper.getSerializationConfig().introspect(mapper.constructType(entry.getValue()))
                    .findProperties().stream().filter(BeanPropertyDefinition::couldSerialize)
                    .map(BeanPropertyDefinition::getName).collect(Collectors.toSet());
            Set<String> documented = schemas.getJSONObject(entry.getKey()).getJSONObject("properties").keySet();
            if (!serialized.equals(documented)) {
                differences.add(entry.getKey() + ": serialized=" + serialized + ", documented=" + documented);
            }
        }
        assertTrue(differences.isEmpty(), String.join("; ", differences));
    }

    @Test
    void responseSchemasMatchSerializedSuccessErrorAndPlainMetadata() throws Exception {
        JobView job = new JobView();
        job.setTimestamp(new Timestamp(123456789L));
        job.setTaskName("task.scan");
        job.setTaskLabel("Scanning");
        job.setTaskHint("Scan the document");
        job.setTaskDate(new Timestamp(123456789L));
        job.setTaskUser("1");
        job.setTaskUsername("scanner");
        job.setNextTaskId("task.control");
        job.setNextTaskLabel("Quality control");
        job.setNextTaskHint("Check the scans");
        JSONObject success = new JSONObject(mapper.writeValueAsString(new ProArcResponse<>(job)));
        JSONObject jobJson = success.getJSONObject("response").getJSONArray("data").getJSONObject(0);
        assertEquals("task.scan", jobJson.getString("taskDoneId"));
        assertEquals("Scanning", jobJson.getString("taskDoneLabel"));
        assertEquals(jobJson.getString("taskName"), jobJson.getString("taskDoneId"));
        assertEquals(jobJson.getString("taskLabel"), jobJson.getString("taskDoneLabel"));
        assertEquals("task.control", jobJson.getString("nextTaskId"));
        assertEquals("Quality control", jobJson.getString("nextTaskLabel"));
        assertEquals("Check the scans", jobJson.getString("nextTaskHint"));
        assertEquals("Scan the document", jobJson.getString("taskDoneHint"));
        assertEquals(jobJson.getString("taskHint"), jobJson.getString("taskDoneHint"));
        assertEquals(jobJson.getString("taskDate"), jobJson.getString("taskDoneDate"));
        assertEquals("1", jobJson.getString("taskDoneUserId"));
        assertEquals(jobJson.getString("taskUser"), jobJson.getString("taskDoneUserId"));
        assertEquals("scanner", jobJson.getString("taskDoneUsername"));
        assertEquals(jobJson.getString("taskUsername"), jobJson.getString("taskDoneUsername"));
        JSONObject failure = new JSONObject(mapper.writeValueAsString(ProArcResponse.asError("Workflow failed")));
        JSONObject schemas = loadSpec().getJSONObject("components").getJSONObject("schemas");
        JSONObject properties = schemas.getJSONObject("WorkflowJobResponse").getJSONObject("properties")
                .getJSONObject("response").getJSONObject("properties");
        assertEquals(Set.of("response"), success.keySet());
        assertEquals(properties.keySet(), mapper.getSerializationConfig().introspect(mapper.constructType(ProArcResponse.class))
                .findProperties().stream().filter(BeanPropertyDefinition::couldSerialize)
                .map(BeanPropertyDefinition::getName).collect(Collectors.toSet()));
        assertTrue(success.getJSONObject("response").get("data") instanceof JSONArray);
        assertEquals("Workflow failed", failure.getJSONObject("response").getString("dataAsError"));
        assertFalse(failure.getJSONObject("response").has("data"));
        assertEquals(-1, failure.getJSONObject("response").getInt("status"));
        JSONObject plain = new JSONObject(mapper.writeValueAsString(new StringRecord("<mods/>", 123L, "workflow:1")));
        assertEquals(Set.of("record"), plain.keySet());
        assertEquals("<mods/>", plain.getJSONObject("record").getString("content"));
    }

    private static void assertParameterType(Type type, JSONObject schema) {
        if (type instanceof ParameterizedType collection) {
            assertEquals("array", schema.getString("type"));
            assertParameterType(collection.getActualTypeArguments()[0], schema.getJSONObject("items"));
        } else {
            Class<?> javaType = (Class<?>) type;
            String expected = javaType == String.class || javaType.isEnum() ? "string"
                    : javaType == boolean.class || javaType == Boolean.class ? "boolean"
                    : javaType == java.math.BigDecimal.class ? "number" : "integer";
            assertEquals(expected, schema.getString("type"));
            if (javaType.isEnum()) {
                assertEquals(Arrays.stream(javaType.getEnumConstants()).map(Object::toString).collect(Collectors.toSet()),
                        new HashSet<>(schema.getJSONArray("enum").toList()));
            }
        }
    }

    private static JSONObject resolve(JSONObject schemas, JSONObject schema) {
        return schemas.getJSONObject(schema.getString("$ref").substring("#/components/schemas/".length()));
    }

    private static JSONObject loadSpec() throws Exception {
        try (var stream = WorkflowOpenApiTest.class.getClassLoader()
                .getResourceAsStream("cz/cas/lib/proarc/webapp/server/rest/proarc_openapi.json")) {
            assertNotNull(stream);
            return new JSONObject(new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        }
    }
}
