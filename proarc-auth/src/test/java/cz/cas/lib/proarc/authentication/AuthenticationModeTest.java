package cz.cas.lib.proarc.authentication;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Proxy;
import java.util.Collections;
import java.util.List;
import org.apache.commons.configuration2.BaseConfiguration;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class AuthenticationModeTest {
    @Test
    void defaultIsLocalAndDoesNotNeedKeycloakConfiguration() {
        Authenticators auth = new Authenticators(new BaseConfiguration());
        assertFalse(auth.isKeycloak());
        assertEquals(1, auth.getAuthenticators().size());
        assertThrows(IllegalStateException.class, auth::getKeycloak);
        assertEquals(Collections.singletonMap("mode", "proarc"), auth.getPublicConfiguration());
    }

    @Test
    void mixedAndUnknownModesPreventStartup() {
        for (List<String> modes : List.of(List.of("keycloak", "proarc"), List.of("typo"), List.of(""))) {
            BaseConfiguration config = new BaseConfiguration();
            config.setProperty("authenticators", modes);
            assertThrows(IllegalArgumentException.class, () -> new Authenticators(config));
        }
    }

    @Test
    void keycloakUsesOnePublicClientAndKeepsTrustPolicyPrivate() {
        Authenticators auth = new Authenticators(keycloakConfig());
        assertTrue(auth.getAuthenticators().isEmpty());
        assertEquals(java.util.Set.of("mode", "issuer", "clientId"), auth.getPublicConfiguration().keySet());
        BaseConfiguration missing = keycloakConfig();
        missing.clearProperty("keycloak.trustedIssuers");
        assertThrows(IllegalArgumentException.class, () -> new Authenticators(missing));
    }

    @Test
    void keycloakRejectsMissingMalformedAndDuplicateHeadersWithoutReadingSession() throws Exception {
        Authenticators.setInstance(new Authenticators(keycloakConfig()));
        for (List<String> headers : List.of(List.<String>of(), List.of("Basic abc"),
                List.of("Bearer "), List.of("Bearer a b"), List.of("Bearer one", "Bearer two"),
                List.of("Bearer invalid"))) {
            Response response = new Response();
            new ProarcAuthFilter().doFilter(request("/rest/v2/user", headers), response.proxy,
                    (req, resp) -> fail("Protected resource was reached"));
            assertEquals(401, response.status);
        }
    }

    @Test
    void keycloakProtectsIsLoggedWithoutCreatingSession() throws Exception {
        Authenticators.setInstance(new Authenticators(keycloakConfig()));
        Response response = new Response();
        new ProarcAuthFilter().doFilter(request("/isLogged", List.of()), response.proxy,
                (req, resp) -> fail("isLogged was reached without a token"));
        assertEquals(401, response.status);
    }

    @Test
    void localModeRejectsBearerBeforeReadingSession() throws Exception {
        Authenticators.setInstance(new Authenticators(new BaseConfiguration()));
        Response response = new Response();
        new ProarcAuthFilter().doFilter(request("/rest/v2/user", List.of("Bearer token")), response.proxy,
                (req, resp) -> fail("Protected resource was reached"));
        assertEquals(401, response.status);
    }

    @Test
    void keycloakDisablesLocalLoginAndLocalLogout() throws Exception {
        Authenticators.setInstance(new Authenticators(keycloakConfig()));
        ProarcLoginServlet servlet = new ProarcLoginServlet();
        Response login = new Response();
        servlet.doPost(request("/proarclogin", List.of()), login.proxy);
        assertEquals(403, login.status);
        Response logout = new Response();
        servlet.doDelete(request("/proarclogin", List.of()), logout.proxy);
        assertEquals(405, logout.status);
    }

    private static BaseConfiguration keycloakConfig() {
        BaseConfiguration config = new BaseConfiguration();
        config.setProperty("authenticators", "keycloak");
        config.setProperty("keycloak.issuer", "https://keycloak.example.org/realms/kramerius");
        config.setProperty("keycloak.clientId", "proarc-client");
        config.setProperty("keycloak.audience", "proarc-api");
        config.setProperty("keycloak.trustedIssuers", "https://keycloak.example.org/realms/kramerius");
        return config;
    }

    private static HttpServletRequest request(String path, List<String> headers) {
        return (HttpServletRequest) Proxy.newProxyInstance(AuthenticationModeTest.class.getClassLoader(),
                new Class<?>[]{HttpServletRequest.class}, (proxy, method, args) -> switch (method.getName()) {
                    case "getSession", "getParameter" -> throw new AssertionError("Local authentication was accessed");
                    case "getContextPath" -> "";
                    case "getRequestURI", "getServletPath" -> path;
                    case "getRequestURL" -> new StringBuffer("https://proarc.example.org" + path);
                    case "getHeaders" -> Collections.enumeration(headers);
                    case "getHeader" -> headers.isEmpty() ? null : headers.get(0);
                    default -> null;
                });
    }

    private static final class Response {
        int status = 200;
        final StringWriter body = new StringWriter();
        final HttpServletResponse proxy = (HttpServletResponse) Proxy.newProxyInstance(
                AuthenticationModeTest.class.getClassLoader(), new Class<?>[]{HttpServletResponse.class},
                (p, method, args) -> {
                    if (method.getName().equals("setStatus")) status = (Integer) args[0];
                    if (method.getName().equals("getWriter")) return new PrintWriter(body);
                    return null;
                });
    }
}
