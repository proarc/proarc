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
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.rest.v1.NewClientResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.NewClientResourceApi;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

/**
 * Resource to manage new client information.
 *
 * @author Lukáš Sýkora
 */
@Deprecated
@Path(RestConfig.URL_API_VERSION_2 + "/" + NewClientResourceApi.PATH)

public class NewClientResource extends NewClientResourceV1 {

    private static final Logger LOG = Logger.getLogger(DeviceResource.class.getName());

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
