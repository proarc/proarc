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

import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * Helper class to annotate {@link DublinCoreRecord} properties.
 *
 * <p> XXX requires Jackson 2.1 and XML producer or rewrite DC client to JSON!
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name = DigitalObjectResourceApi.DUBLINCORERECORD_ELEMENT,
        namespace = DigitalObjectResourceApi.DUBLINCORERECORD_NS)
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(namespace = DigitalObjectResourceApi.DUBLINCORERECORD_NS)
public abstract class AnnotatedDublinCoreRecord extends DublinCoreRecord {

    @XmlElement(name = DigitalObjectResourceApi.DUBLINCORERECORD_BATCHID,
            namespace = DigitalObjectResourceApi.DUBLINCORERECORD_NS,
            nillable = true)
    @Override
    public abstract Integer getBatchId();

    @XmlElement(name = DigitalObjectResourceApi.DUBLINCORERECORD_DC,
            namespace = DigitalObjectResourceApi.DUBLINCORERECORD_NS_OAIDC,
            required = true)
    @Override
    public abstract OaiDcType getDc();

    @XmlElement(name = DigitalObjectResourceApi.DUBLINCORERECORD_PID,
            namespace = DigitalObjectResourceApi.DUBLINCORERECORD_NS)
    @Override
    public abstract String getPid();

    @XmlElement(name = DigitalObjectResourceApi.DUBLINCORERECORD_TIMESTAMP,
            namespace = DigitalObjectResourceApi.DUBLINCORERECORD_NS)
    @Override
    public abstract long getTimestamp();

}
