package cz.cas.lib.proarc.authentication.keycloak;

import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.crypto.RSASSASigner;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.RSAKey;
import com.nimbusds.jose.jwk.gen.RSAKeyGenerator;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;
import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.commons.configuration2.BaseConfiguration;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JWTUtilsTest {
    // Deliberately the same kid in two independent realms.
    private static final RSAKey KEY = key("test");
    private static final RSAKey OTHER_KEY = key("test");
    private HttpServer server;
    private Realm primary;
    private Realm other;
    private BaseConfiguration config;
    private JWTUtils validator;

    @BeforeEach
    void start() throws Exception {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        primary = new Realm("kramerius", KEY);
        other = new Realm("other", OTHER_KEY);
        server.start();
        config = new BaseConfiguration();
        config.setProperty("keycloak.issuer", primary.issuer);
        config.setProperty("keycloak.clientId", "proarc");
        config.setProperty("keycloak.trustedIssuers", primary.issuer);
        validator = new JWTUtils(config); // One public client, no secret; audience defaults to clientId.
    }

    @AfterEach
    void stop() { server.stop(0); }

    @Test
    void checksSessionOnEveryRequestAndRejectsOldTokenAfterLogout() throws Exception {
        String token = sign(claims(primary), KEY);
        assertEquals("subject", validator.getClaims(token).get("sub"));
        validator.getClaims(token);
        assertEquals(1, primary.keyRequests.get());
        assertEquals(2, primary.userinfoRequests.get());
        assertEquals("Bearer " + token, primary.authorization);
        primary.active = false;
        assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(token));
        assertEquals(3, primary.userinfoRequests.get());
    }

    @Test
    void acceptsFalseOrMissingEmailVerified() throws Exception {
        assertEquals(false, validator.getClaims(sign(claims(primary).claim("email_verified", false), KEY))
                .get("email_verified"));
        assertNotNull(validator.getClaims(sign(claims(primary), KEY)));
    }

    @Test
    void rejectsUntrustedIssuersBeforeAnyNetworkAccess() throws Exception {
        for (String issuer : List.of(other.issuer, primary.issuer + "/evil",
                "https://attacker.example/realms/evil")) {
            String token = sign(claims(primary).issuer(issuer), KEY);
            assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(token));
        }
        assertEquals(0, primary.keyRequests.get());
        assertEquals(0, other.keyRequests.get());
        assertEquals(0, primary.userinfoRequests.get());
        assertEquals(0, other.userinfoRequests.get());
    }

    @Test
    void rejectsAudienceClientExpiryNotBeforeTypeAndScopeBeforeNetworkAccess() throws Exception {
        for (JWTClaimsSet.Builder invalid : List.of(
                claims(primary).issuer(null),
                claims(primary).audience("kramerius"),
                claims(primary).claim("azp", "other-client"),
                claims(primary).claim("azp", null),
                claims(primary).expirationTime(Date.from(Instant.now().minusSeconds(1))),
                claims(primary).expirationTime(null),
                claims(primary).notBeforeTime(Date.from(Instant.now().plusSeconds(60))),
                claims(primary).claim("typ", "ID"), claims(primary).subject(""),
                claims(primary).claim("scope", "email"),
                claims(primary).claim("scope", "openid offline_access"))) {
            String token = sign(invalid, KEY);
            assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(token));
        }
        assertEquals(0, primary.keyRequests.get());
        assertEquals(0, primary.userinfoRequests.get());
    }

    @Test
    void rejectsInvalidSignatureAndUnknownKeyWithoutUserinfo() throws Exception {
        String badSignature = sign(claims(primary), OTHER_KEY);
        assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(badSignature));
        String unknownKey = sign(claims(primary), key("unknown"));
        assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(unknownKey));
        assertEquals(0, primary.userinfoRequests.get());
    }

    @Test
    void trustsMultipleRealmsWithIndependentKeysEvenWithTheSameKid() throws Exception {
        config.setProperty("keycloak.trustedIssuers", primary.issuer + "," + other.issuer);
        validator = new JWTUtils(config);
        validator.getClaims(sign(claims(primary), KEY));
        validator.getClaims(sign(claims(other), OTHER_KEY));
        validator.getClaims(sign(claims(primary), KEY));
        assertEquals(1, primary.keyRequests.get());
        assertEquals(1, other.keyRequests.get());
        assertEquals(2, primary.userinfoRequests.get());
        assertEquals(1, other.userinfoRequests.get());
        String wrongRealmKey = sign(claims(primary), OTHER_KEY);
        assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(wrongRealmKey));
    }

    @Test
    void appliesClientAndAudienceOverridesOnlyToTheConfiguredIssuer() throws Exception {
        config.setProperty("keycloak.trustedIssuers", List.of(primary.issuer, other.issuer));
        config.setProperty("keycloak.issuerProfiles", "library");
        config.setProperty("keycloak.issuerProfiles.library.issuer", other.issuer);
        config.setProperty("keycloak.issuerProfiles.library.clientId", "other-proarc");
        config.setProperty("keycloak.issuerProfiles.library.audience", "other-api");
        validator = new JWTUtils(config);
        validator.getClaims(sign(claims(other).claim("azp", "other-proarc").audience("other-api"), OTHER_KEY));
        validator.getClaims(sign(claims(primary), KEY));
        String wrongClient = sign(claims(other).audience("other-api"), OTHER_KEY);
        assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(wrongClient));
    }

    @Test
    void requiresPrimaryIssuerInWhitelistAndRejectsUntrustedProfileOverrides() {
        config.clearProperty("keycloak.trustedIssuers");
        assertThrows(IllegalArgumentException.class, () -> new JWTUtils(config));
        config.setProperty("keycloak.trustedIssuers", primary.issuer);
        config.setProperty("keycloak.issuerProfiles", "untrusted");
        config.setProperty("keycloak.issuerProfiles.untrusted.issuer", other.issuer);
        assertThrows(IllegalArgumentException.class, () -> new JWTUtils(config));
    }

    @Test
    void rejectsMalformedIssuerConfiguration() {
        config.setProperty("keycloak.trustedIssuers", List.of(primary.issuer, "https://example.org/realm/"));
        assertThrows(IllegalArgumentException.class, () -> new JWTUtils(config));
        config.setProperty("keycloak.trustedIssuers", List.of(primary.issuer, "http://example.org/realm"));
        assertThrows(IllegalArgumentException.class, () -> new JWTUtils(config));
    }

    @Test
    void neverAcceptsMalformedOrUnsignedTokens() {
        for (String token : List.of("", "not-a-jwt", "eyJhbGciOiJub25lIn0.eyJzdWIiOiJhZG1pbiJ9.")) {
            assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(token));
        }
    }

    @Test
    void failsClosedOnUserinfoFailureOrRedirect() throws Exception {
        String token = sign(claims(primary), KEY);
        for (int status : List.of(503, 302)) {
            primary.status = status;
            assertThrows(IOException.class, () -> validator.getClaims(token));
        }
    }

    @Test
    void rejectsUserinfoDenialAndMismatchedSubject() throws Exception {
        String token = sign(claims(primary), KEY);
        primary.status = 403;
        assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(token));
        primary.status = 200;
        primary.subject = "another-user";
        assertThrows(JWTUtils.InvalidTokenException.class, () -> validator.getClaims(token));
    }

    @Test
    void onlyExactRealmRoleGrantsEntry() {
        assertTrue(JWTUtils.hasProarcRole(Map.of("realm_access", Map.of("roles", List.of("proarc")))));
        assertFalse(JWTUtils.hasProarcRole(Map.of("realm_access", Map.of("roles", List.of("PROARC_USER")))));
        assertFalse(JWTUtils.hasProarcRole(Map.of("resource_access", Map.of("proarc", Map.of("roles", List.of("proarc"))))));
        assertFalse(JWTUtils.hasProarcRole(Map.of()));
    }

    private JWTClaimsSet.Builder claims(Realm realm) {
        return new JWTClaimsSet.Builder().issuer(realm.issuer).subject("subject").audience("proarc")
                .expirationTime(Date.from(Instant.now().plusSeconds(120))).claim("typ", "Bearer")
                .claim("azp", "proarc").claim("scope", "openid email")
                .claim("email", "user@example.org").claim("realm_access", Map.of("roles", List.of("proarc")));
    }

    private static String sign(JWTClaimsSet.Builder claims, RSAKey key) throws Exception {
        SignedJWT jwt = new SignedJWT(new JWSHeader.Builder(JWSAlgorithm.RS256).keyID(key.getKeyID()).build(), claims.build());
        jwt.sign(new RSASSASigner(key));
        return jwt.serialize();
    }

    private static RSAKey key(String id) {
        try { return new RSAKeyGenerator(2048).keyID(id).generate(); }
        catch (Exception e) { throw new IllegalStateException(e); }
    }

    private final class Realm {
        final String issuer;
        final AtomicInteger keyRequests = new AtomicInteger();
        final AtomicInteger userinfoRequests = new AtomicInteger();
        boolean active = true;
        int status = 200;
        String subject = "subject";
        String authorization;

        Realm(String name, RSAKey key) {
            String path = "/realms/" + name;
            issuer = "http://127.0.0.1:" + server.getAddress().getPort() + path;
            server.createContext(path + "/protocol/openid-connect/certs", exchange -> {
                keyRequests.incrementAndGet();
                byte[] body = new JWKSet(key.toPublicJWK()).toString().getBytes(StandardCharsets.UTF_8);
                exchange.sendResponseHeaders(200, body.length);
                exchange.getResponseBody().write(body);
                exchange.close();
            });
            server.createContext(path + "/protocol/openid-connect/userinfo", exchange -> {
                userinfoRequests.incrementAndGet();
                authorization = exchange.getRequestHeaders().getFirst("Authorization");
                byte[] body = ("{\"sub\":\"" + subject + "\"}").getBytes(StandardCharsets.UTF_8);
                exchange.sendResponseHeaders(active ? status : 401, body.length);
                exchange.getResponseBody().write(body);
                exchange.close();
            });
        }
    }
}
