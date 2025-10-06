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
import cz.cas.lib.proarc.common.device.Device;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.DeviceResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.DeviceResourceApi;
import java.io.IOException;
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
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_NO_PERMISSION;
import static cz.cas.lib.proarc.webapp.server.rest.UserPermission.hasPermission;

/**
 * Resource to manage devices producing digital objects.
 *
 * @author Lukas Sykora
 */
@Path(RestConfig.URL_API_VERSION_2 + "/" + DeviceResourceApi.PATH)
public class DeviceResource extends DeviceResourceV1 {

    private static final Logger LOG = Logger.getLogger(DeviceResource.class.getName());

    public DeviceResource(
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
    public SmartGwtResponse<Device> deleteDevice(
            @QueryParam(DeviceResourceApi.DEVICE_ITEM_ID) String id
            ) {
        if (!hasPermission(user, UserRole.PERMISSION_DEVICE_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        try {
            return super.deleteDevice(id);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Device> getDevices(
            @QueryParam(DeviceResourceApi.DEVICE_ITEM_ID) String id,
            @QueryParam(DeviceResourceApi.DEVICE_START_ROW_PARAM) int startRow
            ) {
        try {
            return super.getDevices(id, startRow);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Device> newDevice(
            @FormParam(DeviceResourceApi.DEVICE_ITEM_ID) String id,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_LABEL) String label,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_MODEL) String model,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_DESCRIPTION) String description,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_PREMIS) String premis,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_TIMESTAMP) Long timestamp,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_AUDIO_TIMESTAMP) Long audiotimestamp
            ) {
        if (!hasPermission(user, UserRole.PERMISSION_DEVICE_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        try {
            return super.newDevice(id, label, model, description, premis, timestamp, audiotimestamp);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Device> updateDevice(
            @FormParam(DeviceResourceApi.DEVICE_ITEM_ID) String id,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_LABEL) String label,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_MODEL) String model,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_DESCRIPTION) String description,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_PREMIS) String premis,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_TIMESTAMP) Long timestamp,
            @FormParam(DeviceResourceApi.DEVICE_ITEM_AUDIO_TIMESTAMP) Long audiotimestamp
            ) {
        if (!hasPermission(user, UserRole.PERMISSION_DEVICE_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (id == null || label == null || label.isEmpty() || model == null || model.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETERS, DeviceResourceApi.DEVICE_ITEM_ID, DeviceResourceApi.DEVICE_ITEM_MODEL));
        }
        try {
            return super.updateDevice(id, label, model, description, premis, timestamp, audiotimestamp);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
