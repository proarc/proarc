package cz.cas.lib.proarc.authentication;

import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.crypto.RSASSASigner;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.RSAKey;
import com.nimbusds.jose.jwk.gen.RSAKeyGenerator;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;
import com.sun.net.httpserver.HttpServer;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Proxy;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.apache.commons.configuration2.BaseConfiguration;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class KeycloakFilterTest {
    private HttpServer server;
    private RSAKey key;
    private String issuer;
    private boolean active;
    private UserProfile local;
    private UserManager previous;
    private int lookups;

    @BeforeEach
    void start() throws Exception {
        active = true;
        key = new RSAKeyGenerator(2048).keyID("key").generate();
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        issuer = "http://127.0.0.1:" + server.getAddress().getPort() + "/realms/kramerius";
        server.createContext("/realms/kramerius/protocol/openid-connect/certs", exchange -> {
            byte[] body = new JWKSet(key.toPublicJWK()).toString().getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.createContext("/realms/kramerius/protocol/openid-connect/userinfo", exchange -> {
            byte[] body = "{\"sub\":\"subject\"}".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(active ? 200 : 401, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.start();
        BaseConfiguration config = new BaseConfiguration();
        config.setProperty("authenticators", "keycloak");
        config.setProperty("keycloak.issuer", issuer);
        config.setProperty("keycloak.clientId", "proarc-client");
        config.setProperty("keycloak.audience", "proarc-api");
        config.setProperty("keycloak.trustedIssuers", issuer);
        Authenticators.setInstance(new Authenticators(config));
        previous = UserUtil.getDefaultManger();
        local = new UserProfile();
        local.setId(7);
        local.setUserName("local-user");
        local.setCreateUserFunction(false);
        UserUtil.setDefaultManger((UserManager) Proxy.newProxyInstance(getClass().getClassLoader(),
                new Class<?>[]{UserManager.class}, (proxy, method, args) -> {
                    assertEquals("resolveKeycloakIdentity", method.getName(), "No local password fallback");
                    assertArrayEquals(new Object[]{issuer, "subject", "user@example.org"}, args);
                    lookups++;
                    return local;
                }));
    }

    @AfterEach
    void stop() {
        server.stop(0);
        UserUtil.setDefaultManger(previous);
    }

    @Test
    void authenticatesRequestAndPreservesLocalPermissionsWithoutSession() throws Exception {
        Response response = new Response();
        new ProarcAuthFilter().doFilter(request(token(List.of("proarc"))), response.proxy, (req, resp) -> {
            ProarcPrincipal principal = (ProarcPrincipal) ((HttpServletRequest) req).getUserPrincipal();
            assertEquals("local-user", principal.getName());
            assertEquals(7, principal.getAssociatedUserProfile().getId());
            assertFalse(principal.getAssociatedUserProfile().hasCreateUserFunction());
            assertSame(local, principal.getAssociatedUserProfile());
        });
        assertEquals(1, lookups);
        assertEquals(200, response.status);
    }

    @Test
    void rejectsRoleBeforeLookingUpAccount() throws Exception {
        Response response = new Response();
        new ProarcAuthFilter().doFilter(request(token(List.of("kramerius_admin"))), response.proxy,
                (req, resp) -> fail("Missing realm role was accepted"));
        assertEquals(403, response.status);
        assertEquals(0, lookups);
    }

    @Test
    void rejectsUnlinkedUserWith403() throws Exception {
        local = null;
        Response response = new Response();
        new ProarcAuthFilter().doFilter(request(token(List.of("proarc"))), response.proxy,
                (req, resp) -> fail("Unlinked account was accepted"));
        assertEquals(403, response.status);
    }

    @Test
    void rejectsLoggedOutSessionBeforeAccountLookup() throws Exception {
        active = false;
        Response response = new Response();
        new ProarcAuthFilter().doFilter(request(token(List.of("proarc"))), response.proxy,
                (req, resp) -> fail("Logged-out session was accepted"));
        assertEquals(401, response.status);
        assertEquals(0, lookups);
    }

    private HttpServletRequest request(String token) {
        return (HttpServletRequest) Proxy.newProxyInstance(getClass().getClassLoader(),
                new Class<?>[]{HttpServletRequest.class}, (proxy, method, args) -> switch (method.getName()) {
                    case "getSession" -> throw new AssertionError("HTTP session accessed in Keycloak mode");
                    case "getContextPath" -> "/proarc";
                    case "getRequestURI" -> "/proarc/rest/v2/user";
                    case "getServletPath" -> "/rest";
                    case "getHeaders" -> Collections.enumeration(List.of("Bearer " + token));
                    default -> null;
                });
    }

    private String token(List<String> roles) throws Exception {
        SignedJWT jwt = new SignedJWT(new JWSHeader.Builder(JWSAlgorithm.RS256).keyID("key").build(),
                new JWTClaimsSet.Builder().issuer(issuer).subject("subject").audience("proarc-api")
                        .expirationTime(Date.from(Instant.now().plusSeconds(120))).claim("typ", "Bearer")
                        .claim("email", "user@example.org").claim("email_verified", false)
                        .claim("azp", "proarc-client").claim("scope", "openid email")
                        .claim("realm_access", Map.of("roles", roles)).build());
        jwt.sign(new RSASSASigner(key));
        return jwt.serialize();
    }

    private static final class Response {
        int status = 200;
        final HttpServletResponse proxy = (HttpServletResponse) Proxy.newProxyInstance(
                KeycloakFilterTest.class.getClassLoader(), new Class<?>[]{HttpServletResponse.class},
                (p, method, args) -> {
                    if (method.getName().equals("setStatus")) status = (Integer) args[0];
                    if (method.getName().equals("getWriter")) return new PrintWriter(new StringWriter());
                    return null;
                });
    }
}
