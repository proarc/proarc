package cz.cas.lib.proarc.webapp.server;

import java.nio.charset.StandardCharsets;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SwaggerServletTest {
    @Test
    void keycloakSpecificationOffersOnlyBearerAndKeepsBootstrapPublic() throws Exception {
        JSONObject spec = SwaggerServlet.forAuthenticationMode(load(), true);
        JSONObject schemes = spec.getJSONObject("components").getJSONObject("securitySchemes");
        assertTrue(schemes.has("bearerAuth"));
        assertFalse(schemes.has("proarcLogin"));
        assertFalse(schemes.has("cookieAuth"));
        assertEquals(1, spec.getJSONArray("security").length());
        assertTrue(spec.getJSONArray("security").getJSONObject(0).has("bearerAuth"));
        assertTrue(spec.getJSONObject("paths").getJSONObject("/auth/config").getJSONObject("get")
                .getJSONArray("security").isEmpty());
    }

    @Test
    void localSpecificationDoesNotOfferBearerAuthentication() throws Exception {
        JSONObject spec = SwaggerServlet.forAuthenticationMode(load(), false);
        JSONObject schemes = spec.getJSONObject("components").getJSONObject("securitySchemes");
        assertFalse(schemes.has("bearerAuth"));
        assertTrue(schemes.has("proarcLogin"));
        assertTrue(schemes.has("cookieAuth"));
        assertEquals("proarc", spec.getString("x-proarc-authentication-mode"));
    }

    private JSONObject load() throws Exception {
        try (var stream = getClass().getClassLoader()
                .getResourceAsStream("cz/cas/lib/proarc/webapp/server/rest/proarc_openapi.json")) {
            assertNotNull(stream);
            return new JSONObject(new String(stream.readAllBytes(), StandardCharsets.UTF_8));
        }
    }
}
