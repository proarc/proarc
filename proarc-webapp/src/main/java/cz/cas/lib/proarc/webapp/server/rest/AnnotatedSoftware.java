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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.software.Software;
import cz.cas.lib.proarc.webapp.shared.rest.SoftwareResourceApi;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import java.util.List;

/**
 * Helper class to annotate {@link cz.cas.lib.proarc.common.software.Software} properties.
 *
 * @see JacksonProvider
 *
 * @author Lukas Sykora
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedSoftware extends Software {

    @XmlElement(name = SoftwareResourceApi.SOFTWARE_ITEM_ID)
    @Override
    public String getId() {
        return super.getId();
    }

    @XmlElement(name = SoftwareResourceApi.SOFTWARE_ITEM_LABEL)
    @Override
    public String getLabel() {
        return super.getLabel();
    }

    @XmlElement(name = SoftwareResourceApi.SOFTWARE_ITEM_MODEL)
    @Override
    public String getModel() {
        return super.getModel();
    }

    @XmlElement(name = SoftwareResourceApi.SOFTWARE_ITEM_DESCRIPTION)
    @Override
    public String getDescriptionAsXml() {
        return super.getDescriptionAsXml();
    }

    @XmlElement(name = SoftwareResourceApi.SOFTWARE_ITEM_MEMBERS)
    @Override
    public List<String> getSetOfLinkedIds() {
        return super.getSetOfLinkedIds();
    }

    @XmlElement(name = SoftwareResourceApi.SOFTWARE_ITEM_TIMESTAMP)
    @Override
    public Long getTimestamp() {
        return super.getTimestamp();
    }
}
