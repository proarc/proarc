/*
 * Copyright (C) 2013 Pavel Stastny
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.authentication;

import cz.cas.lib.proarc.authentication.keycloak.JWTUtils;
import cz.cas.lib.proarc.authentication.utils.AuthUtils;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Authentication filter
 *
 * @author pavels
 */
public class ProarcAuthFilter implements Filter {

    public static final Logger LOGGER = Logger.getLogger(ProarcAuthFilter.class.getName());

    public static final String SESSION_KEY = "user";
    public static final String KEYCLOAK_CLAIMS = "proarc.keycloak.claims";

    @Override
    public void destroy() {
    }

    @Override
    public void doFilter(ServletRequest arg0, ServletResponse arg1,
                         FilterChain chain) throws IOException, ServletException {

        securedContent(arg0, arg1, chain);
    }

    /**
     * Filter secured content
     *
     * @param arg0  {@link HttpServletRequest}
     * @param arg1  {@link HttpServletResponse}
     * @param chain {@link FilterChain}
     * @throws IOException
     * @throws ServletException
     */
    public void securedContent(ServletRequest arg0, ServletResponse arg1,
                               FilterChain chain) throws IOException, ServletException {

        HttpServletRequest httpReq = (HttpServletRequest) arg0;
        HttpServletResponse httpResp = (HttpServletResponse) arg1;

        httpResp.setHeader("Cache-Control", "no-cache, no-store, must-revalidate"); // HTTP 1.1
        httpResp.setHeader("Pragma", "no-cache"); // HTTP 1.0
        httpResp.setDateHeader("Expires", 0); // Proxies.

        if (Authenticators.getInstance().isKeycloak()) {
            securedKeycloakContent(httpReq, httpResp, chain);
            return;
        }

        // A bearer token is never an alternative to local session authentication.
        if (httpReq.getHeader("Authorization") != null && isForbinddenResource(httpReq.getRequestURL().toString())) {
            sendError(httpResp, HttpServletResponse.SC_UNAUTHORIZED, "bearer_disabled");
            return;
        }

        HttpSession session = httpReq.getSession();
        if (session != null) {
            ProarcPrincipal p = (ProarcPrincipal) session.getAttribute(SESSION_KEY);
            if (p != null) {
                // already logged
                HttpServletRequest authenticatedRequest = ProarcAuthenticatedHTTPRequest.newInstance(httpReq, p, p.getName());
                chain.doFilter(authenticatedRequest, httpResp);
            } else {
                // not logged -> test forbidden resource ?
                if (isForbinddenResource(httpReq.getRequestURL().toString())) {
                    AuthUtils.setLoginRequiredResponse(httpResp);
                } else {
                    // not forbidden resource -> chain
                    chain.doFilter(httpReq, httpResp);
                }
            }
        } else {
            // no session -> not logged -> forbidden resource ?
            if (isForbinddenResource(httpReq.getRequestURL().toString())) {
                AuthUtils.setLoginRequiredResponse(httpResp);
            } else {
                chain.doFilter(httpReq, httpResp);
            }
        }
    }

    private void securedKeycloakContent(HttpServletRequest req, HttpServletResponse resp, FilterChain chain)
            throws IOException, ServletException {
        String path = req.getRequestURI().substring(req.getContextPath().length());
        String servletPath = req.getServletPath();
        if (!("/rest".equals(servletPath) || "/isLogged".equals(servletPath)
                || path.startsWith("/rest/") || path.equals("/rest") || path.equals("/isLogged"))) {
            chain.doFilter(req, resp);
            return;
        }
        List<String> headers = Collections.list(req.getHeaders("Authorization"));
        String header = headers.size() == 1 ? headers.get(0) : null;
        if (header == null || header.length() < 8 || !header.regionMatches(true, 0, "Bearer ", 0, 7)
                || header.substring(7).isBlank() || header.substring(7).chars().anyMatch(Character::isWhitespace)) {
            sendError(resp, HttpServletResponse.SC_UNAUTHORIZED, "invalid_token");
            return;
        }
        Map<String, Object> claims;
        try {
            claims = Authenticators.getInstance().getKeycloak().getClaims(header.substring(7));
        } catch (JWTUtils.InvalidTokenException e) {
            sendError(resp, HttpServletResponse.SC_UNAUTHORIZED, "invalid_token");
            return;
        } catch (IOException e) {
            // No fallback and no successful session-check cache during a Keycloak outage.
            sendError(resp, HttpServletResponse.SC_SERVICE_UNAVAILABLE, "identity_provider_unavailable");
            return;
        }
        if (!JWTUtils.hasProarcRole(claims)) {
            sendError(resp, HttpServletResponse.SC_FORBIDDEN, "missing_realm_role");
            return;
        }
        UserProfile user;
        try {
            Object email = claims.get("email");
            user = UserUtil.getDefaultManger().resolveKeycloakIdentity((String) claims.get("iss"),
                    (String) claims.get("sub"), email instanceof String ? (String) email : null);
        } catch (RuntimeException e) {
            LOGGER.warning("Keycloak account lookup failed");
            sendError(resp, HttpServletResponse.SC_SERVICE_UNAVAILABLE, "account_lookup_unavailable");
            return;
        }
        if (user == null) {
            sendError(resp, HttpServletResponse.SC_FORBIDDEN, "account_not_linked");
            return;
        }
        ProarcPrincipal principal = new ProarcPrincipal(user.getUserName());
        principal.associateUserProfile(user);
        req.setAttribute(KEYCLOAK_CLAIMS, claims);
        // Only request scope; never read, create or trust an HTTP session in this mode.
        chain.doFilter(ProarcAuthenticatedHTTPRequest.newInstance(req, principal, principal.getName()), resp);
    }

    static void sendError(HttpServletResponse resp, int status, String error) throws IOException {
        resp.setStatus(status);
        resp.setContentType("application/json");
        resp.setCharacterEncoding("UTF-8");
        resp.setHeader("Cache-Control", "no-store");
        if (status == HttpServletResponse.SC_UNAUTHORIZED) {
            resp.setHeader("WWW-Authenticate", "Bearer realm=\"proarc\"");
        }
        resp.getWriter().write("{\"error\":\"" + error + "\"}");
    }

    private boolean isForbinddenResource(String reqUrl) {
        return reqUrl != null && reqUrl.contains("/rest/");
    }

    @Override
    public void init(FilterConfig arg0) throws ServletException {
    }

}
