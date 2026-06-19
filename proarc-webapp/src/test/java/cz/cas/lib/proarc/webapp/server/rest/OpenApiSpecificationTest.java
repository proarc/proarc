package cz.cas.lib.proarc.webapp.server.rest;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OpenApiSpecificationTest {

    @Test
    void proarcOpenApiDocumentsCurrentRestV2Contract() throws Exception {
        JSONObject spec = loadSpec();

        assertEquals("3.0.1", spec.getString("openapi"));
        assertEquals("ProArc REST API", spec.getJSONObject("info").getString("title"));
        assertEquals("2", spec.getJSONObject("info").getString("version"));
        assertEquals("../rest/v2", spec.getJSONArray("servers").getJSONObject(0).getString("url"));

        JSONObject paths = spec.getJSONObject("paths");
        assertEquals(163, paths.length());
        assertEquals(199, countOperations(paths));
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
        assertEquals(223, schemas.length());

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

    private static void assertDefaultJsonResponse(JSONObject operation, String schemaRef) {
        assertEquals(schemaRef, operation.getJSONObject("responses")
                .getJSONObject("default")
                .getJSONObject("content")
                .getJSONObject("application/json")
                .getJSONObject("schema")
                .getString("$ref"));
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
        String resource = "cz/cas/lib/proarc/webapp/server/rest/proarc_openapi.json";
        try (InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream(resource)) {
            if (stream == null) {
                throw new IllegalStateException(resource);
            }
            return new JSONObject(new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        }
    }
}
