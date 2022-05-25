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

import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

/**
 * Device configuration.
 *
 * @author Lukas Sykora
 */
public class DeviceConfiguration {

    private static final Logger LOG = Logger.getLogger(DeviceConfiguration.class.getName());

    public static final String PROPERTY_MAIN_UUID = "mainUuid";
    public static final String PROPERTY_OTHER_UUID = "otherUuid";

    private final String id;
    private final String prefix;
    private final Configuration properties;

    public DeviceConfiguration(String id, String prefix, Configuration properties) {
        this.id = id;
        this.prefix = prefix;
        this.properties = properties;
    }

    public String getId() {
        return id;
    }

    public String getUuid() {
        return properties.getString(PROPERTY_MAIN_UUID);
    }

    public List<String> getOtherUuid() {
        String[] others = properties.getStringArray(PROPERTY_OTHER_UUID);
        return Arrays.asList(others);
    }
}
