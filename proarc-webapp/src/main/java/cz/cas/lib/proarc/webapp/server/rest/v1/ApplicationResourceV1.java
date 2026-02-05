/*
 * Copyright (C) 2021 Lukas Sykora
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
import cz.cas.lib.proarc.common.info.ApplicationInfo;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.ProArcResponse;
import cz.cas.lib.proarc.webapp.shared.rest.ApplicationResourceApi;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.DefaultValue;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Request;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.ws.rs.core.UriInfo;
import java.io.IOException;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_1;

/**
 * Resource to manage application version.
 *
 * @author Lukáš Sýkora
 */
@Deprecated
@Path(URL_API_VERSION_1 + "/" + ApplicationResourceApi.PATH)
public class ApplicationResourceV1 {

    private static final Logger LOG = Logger.getLogger(ApplicationResourceV1.class.getName());
    private final AppConfiguration appConfig;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;
    private final SessionContext session;

    public ApplicationResourceV1(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
        ) throws AppConfigurationException, IOException {
        this.httpRequest = request;
        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.session = SessionContext.from(httpRequest);
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ApplicationInfo> getVersion(
            @DefaultValue("false") @QueryParam(ApplicationResourceApi.QUERY_FULL_LOAD) Boolean fullLoad
    ) throws IOException {
        ApplicationInfo version = new ApplicationInfo();
        version.initValues(appConfig, fullLoad);
        return new ProArcResponse<ApplicationInfo>(version);
    }

    @GET
    @Path(ApplicationResourceApi.FILE_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ApplicationInfo> getFile(
            @QueryParam(ApplicationResourceApi.QUERY_FILE_TYPE) String fileType
    ) throws IOException {
        ApplicationInfo appInfo = new ApplicationInfo();
        appInfo.loadFile(appConfig, fileType);
        return new ProArcResponse<ApplicationInfo>(appInfo);
    }
}
