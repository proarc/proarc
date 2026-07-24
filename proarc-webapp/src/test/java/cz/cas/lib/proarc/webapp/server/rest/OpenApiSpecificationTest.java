package cz.cas.lib.proarc.webapp.server.rest;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OpenApiSpecificationTest {

    private static final Set<String> HTTP_METHODS = Set.of("get", "post", "put", "delete", "patch");

    @Test
    void proarcOpenApiDocumentsCurrentRestV2Contract() throws Exception {
        JSONObject spec = loadSpec();

        assertEquals("3.0.1", spec.getString("openapi"));
        assertEquals("ProArc REST API", spec.getJSONObject("info").getString("title"));
        assertEquals("2", spec.getJSONObject("info").getString("version"));
        assertEquals("../rest/v2", spec.getJSONArray("servers").getJSONObject(0).getString("url"));

        JSONObject paths = spec.getJSONObject("paths");
        assertEquals(166, paths.length());
        assertEquals(202, countOperations(paths));
        assertTrue(paths.has("/authorities"));
        assertTrue(paths.has("/bibliographies/query"));
        assertTrue(paths.has("/device"));
        assertTrue(paths.has("/import/batch"));
        assertTrue(paths.has("/info"));
        assertTrue(paths.has("/object"));
        assertTrue(paths.has("/object/mods/custom"));
        assertTrue(paths.has("/object/search"));
        assertTrue(paths.has("/software"));
        assertTrue(paths.has("/user"));
        assertTrue(paths.has("/workflow"));
        assertFalse(paths.has("/rest/v2/object"));
        assertFalse(paths.has("/swagger/openapi.json"));
    }

    @Test
    void applicationInfoDocumentsVersionEndpointAndSchema() throws Exception {
        JSONObject get = loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/info")
                .getJSONObject("get");

        assertEquals("getVersion", get.getString("operationId"));
        assertTrue(get.getJSONArray("parameters").toString().contains("loadFull"));
        assertDefaultJsonResponse(get, "#/components/schemas/response");

        JSONObject applicationInfo = loadSpec()
                .getJSONObject("components")
                .getJSONObject("schemas")
                .getJSONObject("ApplicationInfo");
        assertTrue(applicationInfo.getJSONObject("properties").has("version"));
        assertTrue(applicationInfo.getJSONObject("properties").has("revision"));
        assertTrue(applicationInfo.getJSONObject("properties").has("storage"));
        assertTrue(applicationInfo.getJSONObject("properties").has("database"));
        assertTrue(applicationInfo.getJSONObject("properties").has("configFile"));
    }

    @Test
    void deviceDocumentsCrudParametersAndDeviceSchema() throws Exception {
        JSONObject device = loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/device");

        JSONObject get = device.getJSONObject("get");
        assertEquals("getDevices", get.getString("operationId"));
        String getParameters = get.getJSONArray("parameters").toString();
        assertTrue(getParameters.contains("id"));
        assertTrue(getParameters.contains("_startRow"));
        assertDefaultJsonResponse(get, "#/components/schemas/response");

        JSONObject post = device.getJSONObject("post");
        assertEquals("newDevice", post.getString("operationId"));
        JSONObject postProperties = post.getJSONObject("requestBody")
                .getJSONObject("content")
                .getJSONObject("*/*")
                .getJSONObject("schema")
                .getJSONObject("properties");
        assertTrue(postProperties.has("id"));
        assertTrue(postProperties.has("label"));
        assertTrue(postProperties.has("model"));
        assertTrue(postProperties.has("timestamp"));

        JSONObject put = device.getJSONObject("put");
        assertEquals("updateDevice", put.getString("operationId"));
        assertTrue(put.getJSONObject("requestBody")
                .getJSONObject("content")
                .getJSONObject("*/*")
                .getJSONObject("schema")
                .getJSONObject("properties")
                .has("audiotimestamp"));

        JSONObject deviceSchema = loadSpec()
                .getJSONObject("components")
                .getJSONObject("schemas")
                .getJSONObject("Device");
        assertTrue(deviceSchema.getJSONObject("properties").has("id"));
        assertTrue(deviceSchema.getJSONObject("properties").has("audioDescription"));
        assertEquals("int64", deviceSchema.getJSONObject("properties").getJSONObject("timestamp").getString("format"));
    }

    @Test
    void objectSearchDocumentsSearchParametersAndResponse() throws Exception {
        JSONObject get = loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/object/search")
                .getJSONObject("get");

        assertEquals("search", get.getString("operationId"));
        String parameters = get.getJSONArray("parameters").toString();
        assertTrue(parameters.contains("owner"));
        assertTrue(parameters.contains("type"));
        assertTrue(parameters.contains("lastCreated"));
        assertTrue(parameters.contains("pid"));
        assertTrue(parameters.contains("batchId"));
        assertTrue(parameters.contains("_startRow"));
        assertTrue(parameters.contains("_sort"));
        assertDefaultJsonResponse(get, "#/components/schemas/response");

        JSONObject searchViewItem = loadSpec()
                .getJSONObject("components")
                .getJSONObject("schemas")
                .getJSONObject("SearchViewItem");
        assertTrue(searchViewItem.getJSONObject("properties").has("pid"));
        assertTrue(searchViewItem.getJSONObject("properties").has("model"));
        assertTrue(searchViewItem.getJSONObject("properties").has("label"));
    }

    @Test
    void objectModsCustomDocumentsMetadataReadAndUpdate() throws Exception {
        JSONObject modsCustom = loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/object/mods/custom");

        JSONObject get = modsCustom.getJSONObject("get");
        assertEquals("getDescriptionMetadata", get.getString("operationId"));
        String getParameters = get.getJSONArray("parameters").toString();
        assertTrue(getParameters.contains("pid"));
        assertTrue(getParameters.contains("batchId"));
        assertTrue(getParameters.contains("editorId"));
        assertDefaultJsonResponse(get, "#/components/schemas/response");

        JSONObject put = modsCustom.getJSONObject("put");
        assertEquals("updateDescriptionMetadata", put.getString("operationId"));
        JSONObject properties = put.getJSONObject("requestBody")
                .getJSONObject("content")
                .getJSONObject("*/*")
                .getJSONObject("schema")
                .getJSONObject("properties");
        assertTrue(properties.has("pid"));
        assertTrue(properties.has("timestamp"));
        assertTrue(properties.has("jsonData"));
        assertTrue(properties.has("xmlData"));
        assertTrue(properties.has("ignoreValidation"));
        assertDefaultJsonResponse(put, "#/components/schemas/response");
    }

    @Test
    void objectModsEditorObjectsDocumentsBulkUpdateFields() throws Exception {
        JSONObject put = loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/object/mods/editorObjects")
                .getJSONObject("put");

        assertEquals("updateDescriptionMetadataObjects", put.getString("operationId"));
        JSONObject properties = formRequestProperties(put);
        for (String property : List.of("pid", "partNumber", "signatura", "sigla", "title", "subTitle",
                "partName", "note", "publisher", "place", "dateIssued")) {
            assertTrue(properties.has(property), property);
        }
        assertDefaultJsonResponse(put, "#/components/schemas/response");
    }

    @Test
    void objectDisseminationDocumentsUploadAndBinaryDownload() throws Exception {
        JSONObject dissemination = loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/object/dissemination");

        JSONObject get = dissemination.getJSONObject("get");
        assertEquals("getDissemination", get.getString("operationId"));
        assertTrue(get.getJSONObject("responses")
                .getJSONObject("default")
                .getJSONObject("content")
                .has("*/*"));

        JSONObject post = dissemination.getJSONObject("post");
        assertEquals("updateDissemination", post.getString("operationId"));
        JSONObject uploadProperties = post.getJSONObject("requestBody")
                .getJSONObject("content")
                .getJSONObject("multipart/form-data")
                .getJSONObject("schema")
                .getJSONObject("properties");
        assertTrue(uploadProperties.has("pid"));
        assertTrue(uploadProperties.has("datastream"));
        assertTrue(uploadProperties.has("file"));
        assertTrue(uploadProperties.has("mime"));
        assertDefaultJsonResponse(post, "#/components/schemas/response");
    }

    @Test
    void importDocumentsMetaCheckBatchParameters() throws Exception {
        JSONObject paths = loadSpec().getJSONObject("paths");

        JSONObject newBatchProperties = formRequestProperties(paths
                .getJSONObject("/import/batch")
                .getJSONObject("post"));
        assertTrue(newBatchProperties.has("peroOcrEngine"));
        assertEquals("int32", newBatchProperties.getJSONObject("peroOcrEngine").getString("format"));
        assertTrue(newBatchProperties.has("metakatEngine"));
        assertEquals("int32", newBatchProperties.getJSONObject("metakatEngine").getString("format"));

        JSONObject newBatchesProperties = formRequestProperties(paths
                .getJSONObject("/import/batches")
                .getJSONObject("post"));
        assertTrue(newBatchesProperties.has("peroOcrEngine"));
        assertTrue(newBatchesProperties.has("metakatEngine"));

        JSONObject profileStateParams = loadSpec()
                .getJSONObject("components")
                .getJSONObject("schemas")
                .getJSONObject("ProfileStates")
                .getJSONObject("properties")
                .getJSONObject("params")
                .getJSONObject("properties");
        assertTrue(profileStateParams.has("software"));
        assertTrue(profileStateParams.has("ocrEngine"));
        assertTrue(profileStateParams.has("metakatEngine"));
    }

    @Test
    void krameriusExportDocumentsModsUpdateParameter() throws Exception {
        JSONObject properties = formRequestProperties(loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/export/kramerius4")
                .getJSONObject("post"));

        JSONObject updateMods = properties.getJSONObject("updateMods");
        assertEquals("boolean", updateMods.getString("type"));
        assertFalse(updateMods.getBoolean("default"));
    }

    @Test
    void userAndWorkflowDocumentsAdministrativeContracts() throws Exception {
        JSONObject user = loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/user");

        assertEquals("find_1", user.getJSONObject("get").getString("operationId"));
        assertTrue(user.getJSONObject("get").getJSONArray("parameters").toString().contains("name"));

        JSONObject newUserProperties = user.getJSONObject("post")
                .getJSONObject("requestBody")
                .getJSONObject("content")
                .getJSONObject("*/*")
                .getJSONObject("schema")
                .getJSONObject("properties");
        assertTrue(newUserProperties.has("name"));
        assertTrue(newUserProperties.has("password"));
        assertTrue(newUserProperties.has("sysAdminFunction"));

        JSONObject workflow = loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/workflow");
        assertEquals("deleteObject", workflow.getJSONObject("delete").getString("operationId"));
        assertEquals("addJob", workflow.getJSONObject("post").getString("operationId"));
        assertEquals("updateJob", workflow.getJSONObject("put").getString("operationId"));
        assertDefaultJsonResponse(workflow.getJSONObject("get"), "#/components/schemas/response");
    }

    @Test
    void commonResponseSchemaDocumentsProArcEnvelope() throws Exception {
        JSONObject schemas = loadSpec()
                .getJSONObject("components")
                .getJSONObject("schemas");
        assertEquals(226, schemas.length());

        JSONObject response = schemas.getJSONObject("response");
        JSONObject properties = response.getJSONObject("properties");
        assertTrue(properties.has("status"));
        assertTrue(properties.has("startRow"));
        assertTrue(properties.has("endRow"));
        assertTrue(properties.has("totalRows"));
        assertTrue(properties.has("data"));
        assertTrue(properties.has("errorMessage"));
        assertTrue(properties.has("errors"));
        assertEquals("response", response.getJSONObject("xml").getString("name"));
    }

    @Test
    void objectMemberDistributeDocumentsBatchDistribution() throws Exception {
        JSONObject put = loadSpec()
                .getJSONObject("paths")
                .getJSONObject("/object/member/distribute")
                .getJSONObject("put");

        assertEquals("#/components/schemas/DistributeMembersRequest", put.getJSONObject("requestBody")
                .getJSONObject("content")
                .getJSONObject("application/json")
                .getJSONObject("schema")
                .getString("$ref"));

        JSONObject requestProperties = loadSpec()
                .getJSONObject("components")
                .getJSONObject("schemas")
                .getJSONObject("DistributeMembersRequest")
                .getJSONObject("properties");
        assertTrue(requestProperties.has("targets"));
        assertTrue(requestProperties.has("runReindex"));
        assertEquals("#/components/schemas/DistributeMembersTarget", requestProperties
                .getJSONObject("targets")
                .getJSONObject("items")
                .getString("$ref"));

        JSONObject moveProperties = loadSpec()
                .getJSONObject("components")
                .getJSONObject("schemas")
                .getJSONObject("MoveMembersRequest")
                .getJSONObject("properties");
        assertFalse(moveProperties.has("targets"));
        assertFalse(moveProperties.has("runReindex"));
    }

    @Test
    void allDocumentedEndpointsHaveOperationsAndResponses() throws Exception {
        JSONObject paths = loadSpec().getJSONObject("paths");
        Set<String> operationIds = new HashSet<>();
        int operationCount = 0;
        int responseCount = 0;

        for (String path : paths.keySet()) {
            JSONObject pathItem = paths.getJSONObject(path);
            assertTrue(path.startsWith("/"), path);
            assertFalse(path.contains("//"), path);

            int pathOperationCount = 0;
            for (String method : pathItem.keySet()) {
                if (!HTTP_METHODS.contains(method)) {
                    continue;
                }

                String operationName = method.toUpperCase() + " " + path;
                JSONObject operation = pathItem.getJSONObject(method);
                operationCount++;
                pathOperationCount++;

                assertTrue(operation.has("operationId"), operationName);
                assertFalse(operation.getString("operationId").isBlank(), operationName);
                assertTrue(operationIds.add(operation.getString("operationId")), operationName);
                assertTrue(operation.has("responses"), operationName);

                JSONObject responses = operation.getJSONObject("responses");
                assertTrue(responses.has("default"), operationName);
                assertFalse(responses.isEmpty(), operationName);

                for (String responseStatus : responses.keySet()) {
                    String responseName = operationName + " response " + responseStatus;
                    JSONObject response = responses.getJSONObject(responseStatus);
                    responseCount++;

                    assertTrue(response.has("description"), responseName);
                    assertFalse(response.getString("description").isBlank(), responseName);
                    assertTrue(response.has("content"), responseName);

                    JSONObject content = response.getJSONObject("content");
                    assertFalse(content.isEmpty(), responseName);
                    for (String mediaType : content.keySet()) {
                        String contentName = responseName + " " + mediaType;
                        assertFalse(mediaType.isBlank(), contentName);
                        if (!"*/*".equals(mediaType) && !mediaType.startsWith("image/")) {
                            assertTrue(content.getJSONObject(mediaType).has("schema"), contentName);
                            assertFalse(content.getJSONObject(mediaType).getJSONObject("schema").isEmpty(), contentName);
                        }
                    }
                }
            }

            assertTrue(pathOperationCount > 0, path);
        }

        assertEquals(202, operationCount);
        assertEquals(202, responseCount);
    }

    @Test
    void openApiContractSnapshotContainsSameOperationsAndSchemas() throws Exception {
        JSONObject current = loadSpec();
        JSONObject snapshot = loadSnapshot();

        assertEquals(operations(snapshot.getJSONObject("paths")).keySet(), operations(current.getJSONObject("paths")).keySet(),
                "OpenAPI operations changed. Review added/removed endpoint methods, then update the contract snapshot if the change is intentional.");
        assertEquals(snapshot.getJSONObject("components").getJSONObject("schemas").keySet(),
                current.getJSONObject("components").getJSONObject("schemas").keySet(),
                "OpenAPI schemas changed. Review added/removed DTO schemas, then update the contract snapshot if the change is intentional.");
    }

    @TestFactory
    List<DynamicTest> documentedEndpointContractsMatchSnapshot() throws Exception {
        Map<String, JSONObject> currentOperations = operations(loadSpec().getJSONObject("paths"));
        Map<String, JSONObject> expectedOperations = operations(loadSnapshot().getJSONObject("paths"));

        List<DynamicTest> tests = new ArrayList<>();
        for (Map.Entry<String, JSONObject> expected : expectedOperations.entrySet()) {
            tests.add(DynamicTest.dynamicTest(expected.getKey(), () -> assertEquals(
                    canonicalJson(expected.getValue()),
                    canonicalJson(currentOperations.get(expected.getKey())),
                    "OpenAPI operation changed: " + expected.getKey())));
        }
        return tests;
    }

    @TestFactory
    List<DynamicTest> documentedSchemaContractsMatchSnapshot() throws Exception {
        JSONObject currentSchemas = loadSpec().getJSONObject("components").getJSONObject("schemas");
        JSONObject expectedSchemas = loadSnapshot().getJSONObject("components").getJSONObject("schemas");

        List<DynamicTest> tests = new ArrayList<>();
        for (String schema : expectedSchemas.keySet()) {
            tests.add(DynamicTest.dynamicTest(schema, () -> assertEquals(
                    canonicalJson(expectedSchemas.getJSONObject(schema)),
                    canonicalJson(currentSchemas.getJSONObject(schema)),
                    "OpenAPI schema changed: " + schema)));
        }
        return tests;
    }

    private static void assertDefaultJsonResponse(JSONObject operation, String schemaRef) {
        assertEquals(schemaRef, operation.getJSONObject("responses")
                .getJSONObject("default")
                .getJSONObject("content")
                .getJSONObject("application/json")
                .getJSONObject("schema")
                .getString("$ref"));
    }

    private static JSONObject formRequestProperties(JSONObject operation) {
        return operation.getJSONObject("requestBody")
                .getJSONObject("content")
                .getJSONObject("*/*")
                .getJSONObject("schema")
                .getJSONObject("properties");
    }

    private static int countOperations(JSONObject paths) {
        int count = 0;
        for (String path : paths.keySet()) {
            JSONObject methods = paths.getJSONObject(path);
            for (String method : methods.keySet()) {
                if ("get".equals(method) || "post".equals(method) || "put".equals(method)
                        || "delete".equals(method) || "patch".equals(method)) {
                    count++;
                }
            }
        }
        return count;
    }

    private static JSONObject loadSpec() throws Exception {
        return loadJsonResource("cz/cas/lib/proarc/webapp/server/rest/proarc_openapi.json");
    }

    private static JSONObject loadSnapshot() throws Exception {
        return loadJsonResource("cz/cas/lib/proarc/webapp/server/rest/proarc_openapi_snapshot.json");
    }

    private static JSONObject loadJsonResource(String resource) throws Exception {
        try (InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream(resource)) {
            if (stream == null) {
                throw new IllegalStateException(resource);
            }
            return new JSONObject(new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        }
    }

    private static Map<String, JSONObject> operations(JSONObject paths) {
        Map<String, JSONObject> operations = new LinkedHashMap<>();
        List<String> sortedPaths = new ArrayList<>(paths.keySet());
        sortedPaths.sort(String::compareTo);

        for (String path : sortedPaths) {
            JSONObject pathItem = paths.getJSONObject(path);
            for (String method : HTTP_METHODS) {
                if (pathItem.has(method)) {
                    operations.put(method.toUpperCase() + " " + path, pathItem.getJSONObject(method));
                }
            }
        }
        return operations;
    }

    private static String canonicalJson(Object value) {
        if (value instanceof JSONObject object) {
            List<String> keys = new ArrayList<>(object.keySet());
            keys.sort(String::compareTo);

            StringBuilder result = new StringBuilder("{");
            for (int i = 0; i < keys.size(); i++) {
                if (i > 0) {
                    result.append(',');
                }
                String key = keys.get(i);
                result.append(JSONObject.quote(key)).append(':').append(canonicalJson(object.get(key)));
            }
            return result.append('}').toString();
        }
        if (value instanceof JSONArray array) {
            StringBuilder result = new StringBuilder("[");
            for (int i = 0; i < array.length(); i++) {
                if (i > 0) {
                    result.append(',');
                }
                result.append(canonicalJson(array.get(i)));
            }
            return result.append(']').toString();
        }
        if (JSONObject.NULL.equals(value)) {
            return "null";
        }
        if (value instanceof String string) {
            return JSONObject.quote(string);
        }
        return String.valueOf(value);
    }
}
