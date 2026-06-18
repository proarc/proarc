/*
 * Copyright (C) 2023 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.server.rest.v1;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.shared.rest.NewClientResourceApi;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.Cookie;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Request;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.ws.rs.core.UriInfo;
import java.io.IOException;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_1;

/**
 * Resource to manage new client information.
 *
 * @author Lukáš Sýkora
 */
@Deprecated
@Path(URL_API_VERSION_1 + "/client")

public class NewClientResourceV1 {

    private static final Logger LOG = Logger.getLogger(NewClientResourceV1.class.getName());
    private final AppConfiguration appConfig;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;
    private final SessionContext session;
    private final HttpServletResponse httpResponse;

    public NewClientResourceV1(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest,
            @Context HttpServletResponse httpResponse
    ) throws AppConfigurationException {
        this.httpRequest = request;
        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.session = SessionContext.from(httpRequest);
        this.httpResponse = httpResponse;
    }

    @GET
    @Path(NewClientResourceApi.URL_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public HttpServletResponse redirectToNewClient() throws IOException {
        String url = appConfig.getClientOptions().getClientUrl();
        LOG.info("Redirected to " + url);
        this.httpResponse.sendRedirect(url);
        setCookies();
        return httpResponse;
    }

    private void setCookies() {
        for (String cookieKey: this.httpHeaders.getCookies().keySet()) {
            Cookie cookie = this.httpHeaders.getCookies().get(cookieKey);
            this.httpResponse.addCookie(new jakarta.servlet.http.Cookie(cookieKey, cookie.getValue()));
        }
    }
}
