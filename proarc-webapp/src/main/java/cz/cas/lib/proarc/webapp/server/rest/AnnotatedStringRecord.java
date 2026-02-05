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

import cz.cas.lib.proarc.common.storage.StringEditor.StringRecord;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.KrameriusResourceApi;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;

/**
 * Helper class to annotate {@link StringRecord} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name = DigitalObjectResourceApi.STRINGRECORD_ELEMENT)
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedStringRecord extends StringRecord {

    @XmlElement(name = DigitalObjectResourceApi.BATCHID_PARAM)
    @Override
    public abstract Integer getBatchId();

    @XmlElement(name = DigitalObjectResourceApi.STRINGRECORD_CONTENT)
    @Override
    public abstract String getContent();

    @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_PID)
    @Override
    public abstract String getPid();

    @XmlElement(name = DigitalObjectResourceApi.TIMESTAMP_PARAM)
    @Override
    public abstract long getTimestamp();

    @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_MODEL)
    @Override
    public abstract String getModel();

    @XmlElement(name = KrameriusResourceApi.KRAMERIUS_INSTANCE)
    @Override
    public abstract String getKrameriusInstanceId();

    @XmlElement(name = "data")
    @Override
    public abstract Object getData();

    @XmlElement(name = "status")
    @Override
    public abstract int getStatus();
}
