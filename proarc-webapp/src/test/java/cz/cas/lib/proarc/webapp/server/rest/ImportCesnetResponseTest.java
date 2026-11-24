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

import cz.cas.lib.proarc.webapp.server.rest.v1.ImportResourceV1;
import cz.cas.lib.proarc.webapp.server.rest.v2.ImportResource;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.Path;
import java.nio.charset.StandardCharsets;
import java.util.List;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class ImportCesnetResponseTest {
    @Test
    void bulkResponseKeepsSchedulingErrorsAndConfirmationSeparatePerFolder() throws Exception {
        var scheduled = new ImportResourceV1.ImportCesnetResult();
        scheduled.folderPath = "parent/first/";
        scheduled.batchId = 123;
        var failed = new ImportResourceV1.ImportCesnetResult();
        failed.folderPath = "parent/missing/";
        failed.error = "Source does not exist";
        var repeated = new ImportResourceV1.ImportCesnetResult();
        repeated.folderPath = "parent/repeated/";
        repeated.confirmationRequired = true;
        var mapper = new JacksonProvider().getContext(ProArcResponse.class);
        var json = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(List.of(scheduled, failed, repeated))));
        var response = json.has("response") ? json.get("response") : json;
        var data = response.get("data");
        assertEquals(3, data.size());
        assertEquals(123, data.get(0).get("batchId").asInt());
        assertEquals(failed.error, data.get(1).get("error").asText());
        assertTrue(data.get(2).get("confirmationRequired").asBoolean());
    }

    @Test
    void folderListingSerializesTheAvailableAction() throws Exception {
        var folder = new ImportFolder("first", "NEW", "parent/", "parent/first/", List.of());
        var mapper = new JacksonProvider().getContext(ImportFolder.class);
        var disabled = mapper.readTree(mapper.writeValueAsString(folder));
        if (disabled.has("folder")) {
            disabled = disabled.get("folder");
        }
        assertEquals(0, disabled.get("actions").size());
        folder.setActions(List.of(ImportResourceApi.CESNET_ACTION));
        var enabled = mapper.readTree(mapper.writeValueAsString(folder));
        if (enabled.has("folder")) {
            enabled = enabled.get("folder");
        }
        assertEquals("bagit_upload_cesnet", enabled.get("actions").get(0).asText());
    }

    @Test
    void apiAcceptsRepeatedFolderPathsAndDocumentsTheFeatureFlag() throws Exception {
        var method = ImportResource.class.getMethod("uploadFoldersCesnet",
                List.class, boolean.class, String.class, String.class, boolean.class);
        assertEquals("cesnet", method.getAnnotation(Path.class).value());
        assertEquals("folderPath", method.getParameters()[0].getAnnotation(FormParam.class).value());
        try (var input = getClass().getClassLoader().getResourceAsStream("cz/cas/lib/proarc/webapp/server/rest/proarc_openapi.json")) {
            var spec = new JSONObject(new String(input.readAllBytes(), StandardCharsets.UTF_8));
            var operation = spec.getJSONObject("paths").getJSONObject("/import/cesnet").getJSONObject("post");
            assertTrue(operation.getString("description").contains("export.ltpCesnet.enable=true"));
            var form = operation.getJSONObject("requestBody").getJSONObject("content")
                    .getJSONObject("application/x-www-form-urlencoded");
            assertTrue(form.getJSONObject("encoding").getJSONObject("folderPath").getBoolean("explode"));
        }
    }
}
