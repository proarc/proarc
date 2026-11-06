package cz.cas.lib.proarc.authentication.keycloak;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.crypto.RSASSAVerifier;
import com.nimbusds.jose.jwk.JWK;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.KeyUse;
import com.nimbusds.jose.jwk.RSAKey;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.configuration2.Configuration;

/** Strict access-token verification against explicitly trusted Keycloak realms. */
public final class JWTUtils {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final int MAX_RESPONSE_BYTES = 1024 * 1024;
    private static final long CACHE_TTL_MS = 3600000;
    private static final long KEY_REFRESH_INTERVAL_MS = 10000;

    private final Map<String, Issuer> issuers;
    private final int timeout;

    public JWTUtils(Configuration config) {
        String primary = required(config, "keycloak.issuer");
        String defaultClient = required(config, "keycloak.clientId");
        String defaultAudience = value(config, "keycloak.audience", defaultClient);
        timeout = config.getInt("keycloak.httpTimeoutMillis", 5000);
        if (timeout < 1 || timeout > 60000) {
            throw new IllegalArgumentException("Invalid keycloak.httpTimeoutMillis");
        }
        Map<String, Issuer> trusted = new LinkedHashMap<>();
        for (String issuer : list(config, "keycloak.trustedIssuers")) {
            validateIssuer(issuer);
            trusted.put(issuer, new Issuer(issuer, defaultClient, defaultAudience));
        }
        if (!trusted.containsKey(primary)) {
            throw new IllegalArgumentException("keycloak.issuer must be included in keycloak.trustedIssuers");
        }
        java.util.Set<String> overridden = new java.util.HashSet<>();
        for (String profile : list(config, "keycloak.issuerProfiles")) {
            if (!profile.matches("[A-Za-z0-9_-]+")) {
                throw new IllegalArgumentException("Invalid Keycloak issuer profile name");
            }
            String prefix = "keycloak.issuerProfiles." + profile + ".";
            String issuer = required(config, prefix + "issuer");
            if (!trusted.containsKey(issuer) || !overridden.add(issuer)) {
                throw new IllegalArgumentException("Issuer profiles must reference distinct whitelisted issuers");
            }
            String client = value(config, prefix + "clientId", defaultClient);
            String audience = value(config, prefix + "audience",
                    config.containsKey(prefix + "clientId") ? client : defaultAudience);
            if (primary.equals(issuer) && !defaultClient.equals(client)) {
                throw new IllegalArgumentException("Primary issuer clientId must match keycloak.clientId");
            }
            trusted.put(issuer, new Issuer(issuer, client, audience));
        }
        issuers = Map.copyOf(trusted);
    }

    private static String required(Configuration config, String name) {
        return value(config, name, null);
    }

    private static String value(Configuration config, String name, String fallback) {
        String value = config.getString(name, fallback);
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("Missing " + name);
        }
        return value.trim();
    }

    private static List<String> list(Configuration config, String name) {
        // Works with both comma-delimited property values and repeated configuration entries.
        return config.getList(name).stream().flatMap(v -> Arrays.stream(v.toString().split(",")))
                .map(String::trim).filter(v -> !v.isEmpty()).toList();
    }

    private static void validateIssuer(String issuer) {
        URI uri = URI.create(issuer);
        boolean loopback = "localhost".equals(uri.getHost()) || "127.0.0.1".equals(uri.getHost());
        if (uri.getHost() == null || uri.getUserInfo() != null || uri.getQuery() != null
                || uri.getFragment() != null || issuer.endsWith("/")
                || !("https".equals(uri.getScheme()) || (loopback && "http".equals(uri.getScheme())))) {
            throw new IllegalArgumentException("Trusted issuers must be HTTPS realm URLs without a trailing slash");
        }
    }

    /** Validates signature, issuer/client/audience, lifetime and the live Keycloak session. */
    public Map<String, Object> getClaims(String token) throws InvalidTokenException, IOException {
        SignedJWT jwt;
        JWTClaimsSet claims;
        Issuer issuer;
        try {
            if (token == null || token.length() > 65536) {
                throw new InvalidTokenException();
            }
            jwt = SignedJWT.parse(token);
            claims = jwt.getJWTClaimsSet();
            // A token-controlled URL is never contacted before exact whitelist matching.
            if (claims.getIssuer() == null) {
                throw new InvalidTokenException();
            }
            issuer = issuers.get(claims.getIssuer());
            if (issuer == null || !claims.getAudience().contains(issuer.audience)
                    || !issuer.clientId.equals(claims.getStringClaim("azp"))
                    || claims.getSubject() == null || claims.getSubject().isBlank()
                    || !"Bearer".equals(claims.getStringClaim("typ"))
                    || claims.getExpirationTime() == null
                    || !claims.getExpirationTime().toInstant().isAfter(Instant.now())
                    || (claims.getNotBeforeTime() != null
                        && claims.getNotBeforeTime().toInstant().isAfter(Instant.now()))
                    || !JWSAlgorithm.RS256.equals(jwt.getHeader().getAlgorithm())
                    || jwt.getHeader().getCriticalParams() != null
                    || jwt.getHeader().getKeyID() == null) {
                throw new InvalidTokenException();
            }
            String scope = claims.getStringClaim("scope");
            List<String> scopes = scope == null ? List.of() : Arrays.asList(scope.split(" +"));
            if (!scopes.contains("openid") || scopes.contains("offline_access")) {
                throw new InvalidTokenException();
            }
        } catch (ParseException | IllegalArgumentException e) {
            throw new InvalidTokenException();
        }

        JWK key = issuer.getJWKSet().getKeyByKeyId(jwt.getHeader().getKeyID());
        if (key == null) {
            issuer.refreshKeys();
            key = issuer.getJWKSet().getKeyByKeyId(jwt.getHeader().getKeyID());
        }
        try {
            if (!(key instanceof RSAKey rsa) || rsa.isPrivate()
                    || (rsa.getKeyUse() != null && !KeyUse.SIGNATURE.equals(rsa.getKeyUse()))
                    || (rsa.getAlgorithm() != null && !JWSAlgorithm.RS256.equals(rsa.getAlgorithm()))
                    || !jwt.verify(new RSASSAVerifier(rsa))) {
                throw new InvalidTokenException();
            }
        } catch (com.nimbusds.jose.JOSEException e) {
            throw new InvalidTokenException();
        }

        // No positive session cache: shared SSO logout affects the next protected request.
        HttpResult response = request(issuer.url + "/protocol/openid-connect/userinfo", token);
        if (response.status == 401 || response.status == 403) {
            throw new InvalidTokenException();
        }
        if (response.status != HttpURLConnection.HTTP_OK) {
            throw new IOException("Keycloak userinfo unavailable");
        }
        JsonNode userinfo = JSON.readTree(response.body);
        if (userinfo == null || !userinfo.path("sub").isTextual()
                || !claims.getSubject().equals(userinfo.path("sub").textValue())) {
            throw new InvalidTokenException();
        }
        // Identity, email and roles come only from the verified access token.
        return claims.getClaims();
    }

    public static boolean hasProarcRole(Map<String, Object> claims) {
        Object access = claims.get("realm_access");
        return access instanceof Map<?, ?> realm
                && realm.get("roles") instanceof Collection<?> roles && roles.contains("proarc");
    }

    private HttpResult request(String url, String bearer) throws IOException {
        HttpURLConnection connection = (HttpURLConnection) URI.create(url).toURL().openConnection();
        try {
            connection.setConnectTimeout(timeout);
            connection.setReadTimeout(timeout);
            connection.setInstanceFollowRedirects(false);
            connection.setRequestProperty("Accept", "application/json");
            if (bearer != null) {
                connection.setRequestProperty("Authorization", "Bearer " + bearer);
            }
            int status = connection.getResponseCode();
            if (status != HttpURLConnection.HTTP_OK) {
                // Never propagate endpoint error bodies or access tokens to logs.
                return new HttpResult(status, new byte[0]);
            }
            try (InputStream in = connection.getInputStream()) {
                byte[] bytes = in.readNBytes(MAX_RESPONSE_BYTES + 1);
                if (bytes.length > MAX_RESPONSE_BYTES) {
                    throw new IOException("Keycloak response too large");
                }
                return new HttpResult(status, bytes);
            }
        } finally {
            connection.disconnect();
        }
    }

    public void clearCache() {
        issuers.values().forEach(Issuer::clearCache);
    }

    private record HttpResult(int status, byte[] body) {}

    /** Separate key cache and refresh throttling for every configured realm. */
    private final class Issuer {
        private final String url;
        private final String clientId;
        private final String audience;
        private volatile JWKSet jwks;
        private volatile long keysLoadedAt;
        private long keysRequestedAt;

        private Issuer(String url, String clientId, String audience) {
            this.url = url;
            this.clientId = clientId;
            this.audience = audience;
        }

        private JWKSet getJWKSet() throws IOException {
            if (jwks == null || System.currentTimeMillis() - keysLoadedAt >= CACHE_TTL_MS) {
                refreshKeys();
            }
            JWKSet result = jwks;
            if (result == null || System.currentTimeMillis() - keysLoadedAt >= CACHE_TTL_MS) {
                throw new IOException("Keycloak signing keys unavailable");
            }
            return result;
        }

        private synchronized void refreshKeys() throws IOException {
            long now = System.currentTimeMillis();
            if (now - keysRequestedAt < KEY_REFRESH_INTERVAL_MS) {
                return;
            }
            keysRequestedAt = now;
            HttpResult response = request(url + "/protocol/openid-connect/certs", null);
            if (response.status != HttpURLConnection.HTTP_OK) {
                throw new IOException("Keycloak signing keys unavailable");
            }
            try {
                jwks = JWKSet.parse(new String(response.body, StandardCharsets.UTF_8));
                keysLoadedAt = now;
            } catch (ParseException e) {
                throw new IOException("Invalid Keycloak signing keys");
            }
        }

        private synchronized void clearCache() {
            jwks = null;
            keysLoadedAt = 0;
            keysRequestedAt = 0;
        }
    }

    public static final class InvalidTokenException extends Exception {
        public InvalidTokenException() {
            super("Invalid or inactive access token");
        }
    }
}
