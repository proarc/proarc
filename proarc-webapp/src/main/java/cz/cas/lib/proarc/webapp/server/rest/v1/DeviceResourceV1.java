/*
 * Copyright (C) 2013 Jan Pokorsky
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

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.device.Device;
import cz.cas.lib.proarc.common.device.DeviceException;
import cz.cas.lib.proarc.common.device.DeviceNotFoundException;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.ServerMessages;
import cz.cas.lib.proarc.webapp.server.rest.RestException;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.shared.rest.DeviceResourceApi;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
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
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

import static cz.cas.lib.proarc.common.device.DeviceRepository.getModelLabel;

/**
 * Resource to manage devices producing digital objects.
 *
 * @author Jan Pokorsky
 */
@Deprecated
@Path(RestConfig.URL_API_VERSION_1 + "/" + DeviceResourceApi.PATH)
public class DeviceResourceV1 {

    private static final Logger LOG = Logger.getLogger(DeviceResourceV1.class.getName());
    private final AppConfiguration appConfig;
    private final AkubraConfiguration akubraConfiguration;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;
    private final SessionContext session;
    private final DeviceRepository devRepo;

    public DeviceResourceV1(
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
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            this.devRepo = new DeviceRepository(RemoteStorage.getInstance(appConfig));
            this.akubraConfiguration = null;
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
            this.devRepo = new DeviceRepository(AkubraStorage.getInstance(akubraConfiguration));
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
    }

    @DELETE
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Device> deleteDevice(
            @QueryParam(DeviceResourceApi.DEVICE_ITEM_ID) String id
            ) throws DeviceException {

        try {
            boolean deleted = devRepo.deleteDevice(id, session.asFedoraLog());
            if (!deleted) {
                Locale locale = session.getLocale(httpHeaders);
                throw RestException.plainText(Status.FORBIDDEN,
                        ServerMessages.get(locale).DeviceResource_Delete_InUse_Msg());
            }
            Device device = new Device();
            device.setId(id);
            return new SmartGwtResponse<Device>(device);
        } catch (DeviceNotFoundException ex) {
//            LOG.log(Level.SEVERE, id, ex);
            throw RestException.plainNotFound(DeviceResourceApi.DEVICE_ITEM_ID, id);
        }
    }

    /**
     * Gets list of devices.
     *
     * @param id device ID. If omitted all device are returned but without description.
     * @return list of devices
     */
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Device> getDevices(
            @QueryParam(DeviceResourceApi.DEVICE_ITEM_ID) String id,
            @QueryParam(DeviceResourceApi.DEVICE_START_ROW_PARAM) int startRow
            ) throws DeviceException, IOException {

        int total = 0;
        boolean fetchDescription = id != null && !id.isEmpty();
        List<Device> result = new ArrayList<>();

        if (id == null) {
            total = devRepo.findAllDevices(appConfig, 0).size();
            result = devRepo.findAllDevices(appConfig, startRow);
        } else {
            result = devRepo.find(null, id, fetchDescription, startRow);
            total = result.size();
        }
        int endRow = startRow + result.size() - 1;
        return new SmartGwtResponse<Device>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, result);
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
        try {
            String owner = session.getUser().getUserName();
            Device device = devRepo.addDevice(owner, model, "?", session.asFedoraLog());
            return new SmartGwtResponse<Device>(device);
        } catch (DeviceException ex) {
            throw new WebApplicationException(ex);
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
            ) throws IOException, DeviceException {

        if (id == null || label == null || label.isEmpty() || model == null || model.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing device!");
        }
        Device update = new Device();
        update.setId(id);
        update.setLabel(label);
        update.setModel(model);
        if (description != null && !description.isEmpty()) {
            ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
            Mix mix = jsMapper.readValue(description, Mix.class);
            update.setDescription(mix);
        }
        if (premis != null && !premis.isEmpty()) {
            update.create(premis);
        }
        update.setTimestamp(timestamp);
        update.setAudioTimestamp(audiotimestamp);
        try {
            Device updated = devRepo.update(update, session.asFedoraLog());
            updated.setModel(getModelLabel(updated.getModel()));
            return new SmartGwtResponse<Device>(updated);
        } catch (DeviceException ex) {
            throw new WebApplicationException(ex);
        }
    }

    protected String returnLocalizedMessage(String key, Object... arguments) {
        Locale locale = session.getLocale(httpHeaders);
        ServerMessages msgs = ServerMessages.get(locale);
        return msgs.getFormattedMessage(key, arguments);
    }

}
