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

import cz.cas.lib.proarc.common.device.Device;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.webapp.shared.rest.DeviceResourceApi;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;

/**
 * Helper class to annotate {@link Device} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedDevice extends Device {

    @XmlElement(name = DeviceResourceApi.DEVICE_ITEM_ID)
    @Override
    public String getId() {
        return super.getId();
    }

    @XmlElement(name = DeviceResourceApi.DEVICE_ITEM_LABEL)
    @Override
    public String getLabel() {
        return super.getLabel();
    }

    @XmlElement(name = DeviceResourceApi.DEVICE_ITEM_DESCRIPTION)
    @Override
    public abstract Mix getDescription();

    @XmlElement(name = DeviceResourceApi.DEVICE_ITEM_TIMESTAMP)
    @Override
    public abstract Long getTimestamp();

    @XmlElement(name = DeviceResourceApi.DEVICE_ITEM_PREMIS)
    @Override
    public abstract Mets getAudioDescription();

    @XmlElement(name = DeviceResourceApi.DEVICE_ITEM_AUDIO_TIMESTAMP)
    @Override
    public abstract Long getAudioTimestamp();

    @XmlElement(name = DeviceResourceApi.DEVICE_ITEM_MODEL)
    @Override
    public String getModel() {
        return super.getModel();
    }

}
