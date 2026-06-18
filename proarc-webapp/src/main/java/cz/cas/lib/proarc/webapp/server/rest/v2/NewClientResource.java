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
package cz.cas.lib.proarc.webapp.server.rest.v2;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.webapp.server.rest.v1.NewClientResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.NewClientResourceApi;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Request;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.ws.rs.core.UriInfo;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_2;

/**
 * Resource to manage new client information.
 *
 * @author Lukáš Sýkora
 */
@Path(URL_API_VERSION_2 + "/" + NewClientResourceApi.PATH)

public class NewClientResource extends NewClientResourceV1 {

    private static final Logger LOG = Logger.getLogger(NewClientResource.class.getName());

    public NewClientResource(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest,
            @Context HttpServletResponse httpResponse
    ) throws AppConfigurationException {
        super(request, securityCtx, httpHeaders, uriInfo, httpRequest, httpResponse);
    }

    @GET
    @Path(NewClientResourceApi.URL_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public HttpServletResponse redirectToNewClient() throws IOException {
        try {
            return super.redirectToNewClient();
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            throw t;
        }
    }
}
