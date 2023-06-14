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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.server.rest.v2;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.UrnNbnResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.UrnNbnResourceApi;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;

/**
 *
 * @author Lukas Sykora
 */
@Path(RestConfig.URL_API_VERSION_2 + "/" + UrnNbnResourceApi.PATH)
public class UrnNbnResource extends UrnNbnResourceV1 {

    private static final Logger LOG = Logger.getLogger(UrnNbnResource.class.getName());

    public UrnNbnResource(
            @Context HttpHeaders httpHeaders
    ) throws AppConfigurationException {
        super(httpHeaders);
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<ResolverDescriptor> findCatalog(
            @QueryParam(UrnNbnResourceApi.RESOLVER_ID) String id
    ) {
        try {
            return super.findCatalog(id);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
