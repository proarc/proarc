/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.authentication;

import cz.cas.lib.proarc.authentication.keycloak.JWTUtils;
import cz.cas.lib.proarc.authentication.proarc.ProArcAuthenticator;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.configuration2.Configuration;

/**
 * The authenticators configuration.
 *
 * @author Jan Pokorsky
 */
public final class Authenticators {

    public static final String PROPERTY_AUTHENTICATORS = "authenticators";
    /**
     * The internal authenticator.
     */
    public static final String TYPE_PROARC = "proarc";
    public static final String TYPE_KEYCLOAK = "keycloak";
    private static Authenticators INSTANCE;

    private final Configuration conf;
    private final String loginType;
    private final JWTUtils keycloak;

    public static Authenticators getInstance() {
        if (INSTANCE == null) {
            throw new IllegalStateException("set instance first!");
        }
        return INSTANCE;
    }

    public static void setInstance(Authenticators authenticators) {
        INSTANCE = authenticators;
    }

    public Authenticators(Configuration conf) {
        this.conf = conf;
        List<Object> ids = conf.getList(PROPERTY_AUTHENTICATORS);
        loginType = ids.isEmpty() ? TYPE_PROARC : ids.get(0).toString().trim();
        if (ids.size() > 1 || !(TYPE_PROARC.equals(loginType) || TYPE_KEYCLOAK.equals(loginType))) {
            throw new IllegalArgumentException("Configure exactly one authenticator: proarc or keycloak");
        }
        if (isKeycloak()) {
            if (conf.getString("keycloak.clientId", "").isBlank()) {
                throw new IllegalArgumentException("Missing keycloak.clientId (public browser client)");
            }
            keycloak = new JWTUtils(conf);
        } else {
            keycloak = null;
        }
    }

    public List<Authenticator> getAuthenticators() {
        return isKeycloak() ? Collections.emptyList()
                : Collections.singletonList(new ProArcAuthenticator());
    }

    public String getLoginType() {
        return loginType;
    }

    public boolean isKeycloak() {
        return TYPE_KEYCLOAK.equals(loginType);
    }

    public JWTUtils getKeycloak() {
        if (!isKeycloak()) {
            throw new IllegalStateException("Keycloak authentication is disabled");
        }
        return keycloak;
    }

    /** Browser login configuration only; issuer trust policy stays on the API. */
    public Map<String, String> getPublicConfiguration() {
        Map<String, String> result = new LinkedHashMap<>();
        result.put("mode", loginType);
        if (isKeycloak()) {
            result.put("issuer", conf.getString("keycloak.issuer").trim());
            result.put("clientId", conf.getString("keycloak.clientId").trim());
        }
        return result;
    }

}
