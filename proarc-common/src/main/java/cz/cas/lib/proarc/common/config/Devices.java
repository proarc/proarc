/*
 * Copyright (C) 2022 Lukas Sykora
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
package cz.cas.lib.proarc.common.config;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

/**
 * Manages device configurations.
 *
 * @author Lukas Sykora
 */
public final class Devices {

    private static final Logger LOG = Logger.getLogger(Devices.class.getName());

    static final String PROPERTY_DEVICE_PREFIX = "device";
    static final String PROPERTY_DEVICES = "devices";

    private final String DEVICE_PREFIX = "device:";

    private final Configuration config;

    public Devices(Configuration config) {
        this.config = config;
    }

    public String getMainUUid(String pid) {
        DeviceConfiguration device = findDevice(pid);
        if (device != null) {
            String mainUuid = device.getUuid();
            return !mainUuid.startsWith(DEVICE_PREFIX) ? DEVICE_PREFIX + mainUuid : mainUuid;
        }
        return null;
    }

    public DeviceConfiguration findDevice(String uuid) {
        if (!uuid.startsWith(DEVICE_PREFIX)) {
            uuid = DEVICE_PREFIX + uuid;
        }
        for (DeviceConfiguration device : getAllDevices()) {
            List<String> othersUuid = device.getOtherUuid();
            for (String otherUuid : othersUuid) {
                if (!otherUuid.startsWith(DEVICE_PREFIX)) {
                    otherUuid = DEVICE_PREFIX + otherUuid;
                }
                if (uuid.equals(otherUuid)) {
                    return device;
                }
            }
        }
        return null;
    }

    public List<DeviceConfiguration> getAllDevices() {
        ArrayList<DeviceConfiguration> devices = new ArrayList<>();
        for (String deviceId : config.getStringArray(PROPERTY_DEVICES)) {
            DeviceConfiguration device = readConfiguration(deviceId);
            if (device != null) {
                devices.add(device);
            }
        }
        return devices;
    }

    private DeviceConfiguration readConfiguration(String deviceId) {
        String devicePrefix = PROPERTY_DEVICE_PREFIX + "." + deviceId;
        Configuration deviceConfig = config.subset(devicePrefix);
        DeviceConfiguration device = new DeviceConfiguration(deviceId, devicePrefix, deviceConfig);
        if (!isValidProperty(devicePrefix, DeviceConfiguration.PROPERTY_MAIN_UUID, device.getUuid())) {
            return null;
        }
        return device;
    }

    private static boolean isValidProperty(String prefix, String name, String value) {
        if (value == null || value.isEmpty()) {
            LOG.log(Level.WARNING, "Missing {0}.{1} property!", new Object[]{prefix, name});
            return false;
        }
        return true;
    }
}
