/*
 * Copyright (C) 2025 Lukas Sykora
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
import cz.cas.lib.proarc.common.software.Software;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.SoftwareResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.SoftwareResourceApi;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETERS;

/**
 * Resource to manage software producing digital objects.
 *
 * @author Lukas Sykora
 */
@Path(RestConfig.URL_API_VERSION_2 + "/" + SoftwareResourceApi.PATH)
public class SoftwareResource extends SoftwareResourceV1 {

    private static final Logger LOG = Logger.getLogger(SoftwareResource.class.getName());

    public SoftwareResource(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
            ) throws AppConfigurationException, IOException {
        super(request, securityCtx, httpHeaders, uriInfo, httpRequest);
    }

    @DELETE
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> deleteSoftware(
            @QueryParam(SoftwareResourceApi.SOFTWARE_ITEM_ID) String id
            ) {
        try {
            return super.deleteSoftware(id);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> getSoftwares(
            @QueryParam(SoftwareResourceApi.SOFTWARE_ITEM_ID) String id,
            @QueryParam(SoftwareResourceApi.SOFTWARE_ITEM_MODEL) String model,
            @QueryParam(SoftwareResourceApi.SOFTWARE_START_ROW_PARAM) int startRow
            ) {
        try {
            return super.getSoftwares(id, model, startRow);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    /**
     * Gets list of software with define preview.
     *
     * @param id software ID
     * @return list of softwares
     */
    @GET
    @Path(SoftwareResourceApi.PATH_PREVIEW)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> getSoftwarePreview(
            @QueryParam(SoftwareResourceApi.SOFTWARE_ITEM_ID) String id
    ) {
        try {
            return super.getSoftwarePreview(id);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> newSoftware(
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_LABEL) String label,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_MODEL) String model,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_MEMBERS) List<String> setOfIds,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_DESCRIPTION) String description,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_TIMESTAMP) Long timestamp
            ) {
        try {
            return super.newSoftware(label, model, setOfIds, description, timestamp);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> updateSoftware(
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_ID) String id,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_LABEL) String label,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_MODEL) String model,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_MEMBERS) List<String> setOfIds,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_DESCRIPTION) String description,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_TIMESTAMP) Long timestamp
            ) {
        if (id == null || label == null || label.isEmpty() || model == null || model.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETERS, SoftwareResourceApi.SOFTWARE_ITEM_ID, SoftwareResourceApi.SOFTWARE_ITEM_MODEL));
        }
        try {
            return super.updateSoftware(id, label, model, setOfIds, description, timestamp);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
