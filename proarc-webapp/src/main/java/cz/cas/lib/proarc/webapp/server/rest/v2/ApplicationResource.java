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
import cz.cas.lib.proarc.common.info.ApplicationInfo;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.ApplicationResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.ApplicationResourceApi;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

/**
 * Resource to manage application version.
 *
 * @author Lukáš Sýkora
 */
@Path(RestConfig.URL_API_VERSION_2 + "/" + ApplicationResourceApi.PATH)
public class ApplicationResource extends ApplicationResourceV1 {

    private static final Logger LOG = Logger.getLogger(ApplicationResource.class.getName());

    public ApplicationResource(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
        ) throws AppConfigurationException, IOException {
        super(request, securityCtx, httpHeaders, uriInfo, httpRequest);
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ApplicationInfo> getVersion(
            @QueryParam(ApplicationResourceApi.QUERY_FULL_LOAD) Boolean fullLoad
    ) {
        try {
            return super.getVersion(fullLoad);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(ApplicationResourceApi.FILE_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ApplicationInfo> getFile(
            @QueryParam(ApplicationResourceApi.QUERY_FILE_TYPE) String fileType
    ) {
        try {
            return super.getFile(fileType);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
