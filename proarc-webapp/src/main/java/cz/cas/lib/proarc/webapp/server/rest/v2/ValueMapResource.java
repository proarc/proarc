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
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.ValueMapResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.ValueMapResourceApi;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Provides value maps used by form pick lists.
 *
 * @author Lukas Sykora
 */
@Path(RestConfig.URL_API_VERSION_2 + "/" + ValueMapResourceApi.PATH)
public class ValueMapResource extends ValueMapResourceV1 {

    private static final Logger LOG = Logger.getLogger(ValueMapResource.class.getName());

    public ValueMapResource(
            @Context HttpHeaders httpHeaders,
            @Context HttpServletRequest httpRequest
    ) throws AppConfigurationException {
    super(httpHeaders, httpRequest);
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ValueMap> getValueMaps() {
        try {
            return super.getValueMaps();
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(ValueMapResourceApi.PERO_OCR_ENGINE)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ValueMap> getPeroOcrEngines() {
        try {
            return super.getPeroOcrEngines();
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

}
