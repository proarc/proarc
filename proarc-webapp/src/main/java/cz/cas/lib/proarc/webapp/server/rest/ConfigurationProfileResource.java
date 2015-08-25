/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi.ProfileGroup;
import java.io.FileNotFoundException;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

/**
 * The configuration profile resource.
 *
 * @author Jan Pokorsky
 */
@Path(ConfigurationProfileResourceApi.PATH)
public class ConfigurationProfileResource {

    private static final Logger LOG = Logger.getLogger(ConfigurationProfileResource.class.getName());

    private final HttpHeaders httpHeaders;
    private final AppConfiguration appConfig;
    private final SessionContext session;

    public ConfigurationProfileResource(
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
            ) throws AppConfigurationException {

        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.session = SessionContext.from(httpRequest);
    }

    /**
     * Gets profiles of the given group.
     * @param profileGroup a group to search
     * @return the list of profiles
     */
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ConfigurationProfile> listProfiles(
            @QueryParam(ConfigurationProfileResourceApi.PROFILE_GROUP_PARAM) ProfileGroup profileGroup
            ) {

        if (profileGroup == null) {
            return new SmartGwtResponse<ConfigurationProfile>(Collections.<ConfigurationProfile>emptyList());
        }
        List<ConfigurationProfile> profiles = appConfig.getProfiles().getProfiles(profileGroup.getId());
        return new SmartGwtResponse<ConfigurationProfile>(profiles);
    }

}
