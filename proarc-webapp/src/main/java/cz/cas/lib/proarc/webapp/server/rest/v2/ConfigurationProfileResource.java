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
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.webapp.server.rest.ProArcResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.ConfigurationProfileResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi.ProfileGroup;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.ws.rs.core.UriInfo;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_2;

/**
 * The configuration profile resource.
 *
 * @author Lukas Sykora
 */
@Path(URL_API_VERSION_2 + "/" + ConfigurationProfileResourceApi.PATH)
public class ConfigurationProfileResource extends ConfigurationProfileResourceV1 {

    private static final Logger LOG = Logger.getLogger(ConfigurationProfileResource.class.getName());

    public ConfigurationProfileResource(
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
            ) throws AppConfigurationException {
        super(securityCtx, httpHeaders, uriInfo, httpRequest);
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ConfigurationProfile> listProfiles(
            @QueryParam(ConfigurationProfileResourceApi.PROFILE_GROUP_PARAM) ProfileGroup profileGroup
            ) {
        try {
            return super.listProfiles(profileGroup);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

}
