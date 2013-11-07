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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.incad.pas.editor.shared.rest.DeviceResourceApi;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Resource to manage devices producing digital objects.
 *
 * @author Jan Pokorsky
 */
@Path(DeviceResourceApi.PATH)
public class DeviceResource {

    private static final Logger LOG = Logger.getLogger(DeviceResource.class.getName());
    private final AppConfiguration appConfig;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;
    private final SessionContext session;

    public DeviceResource(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
            ) throws AppConfigurationException {

        this.httpRequest = request;
        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.session = SessionContext.from(httpRequest);
    }

    /**
     * Gets list of devices.
     *
     * @param id device ID. If omitted all device are returned.
     * @return list of devices
     */
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DeviceItem> getDevices(
            @QueryParam(DeviceResourceApi.DEVICE_ITEM_ID) String id
            ) {

        DeviceRepository devRepo = DeviceRepository.getInstance();
        return new SmartGwtResponse<DeviceItem>(devRepo.find(id));
    }

    // XXX temporary device repository
    private static final class DeviceRepository {

        private static DeviceRepository INSTANCE;

        private final Map<String, DeviceItem> items = new LinkedHashMap<String, DeviceItem>();

        public static DeviceRepository getInstance() {
            if (INSTANCE == null) {
                INSTANCE = new DeviceRepository();
            }
            return INSTANCE;
        }

        private DeviceRepository() {
            addDevice("device:digibook_suprascan_10000rgb", "Digibook Suprascan 10000 RGB");
            addDevice("device:panasonic_kv_s1025c", "Panasonic KV-S1025C");
            addDevice("device:proserv_scanntech_600i", "ProServ ScannTech 600i");
            addDevice("device:scanrobot_sr301", "ScanRobot SR301");
            addDevice("device:zeutschel_7000", "Zeutschel OS 7000");
        }

        public List<DeviceItem> find(String id) {
            if (id != null) {
                DeviceItem di = items.get(id);
                return di == null ? Collections.<DeviceItem>emptyList() : Collections.singletonList(di);
            } else {
                return new ArrayList<DeviceItem>(items.values());
            }
        }

        private DeviceItem addDevice(String id, String label) {
            DeviceItem device = new DeviceItem();
            device.id = id;
            device.label = label;
            items.put(id, device);
            return device;
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class DeviceItem {

        @XmlElement(name = DeviceResourceApi.DEVICE_ITEM_ID)
        private String id;
        @XmlElement(name = DeviceResourceApi.DEVICE_ITEM_LABEL)
        private String label;

        public DeviceItem() {
        }
    }

}
