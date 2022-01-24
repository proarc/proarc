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

import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;

/**
 * Helper class to annotate {@link Item} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedSearchViewItem extends Item {

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_CREATED)
    @Override
    public abstract String getCreated();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_LABEL)
    @Override
    public abstract String getLabel();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_MODEL)
    @Override
    public abstract String getModel();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_MODIFIED)
    @Override
    public abstract String getModified();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_OWNER)
    @Override
    public abstract String getOwner();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PID)
    @Override
    public abstract String getPid();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_STATE)
    @Override
    public abstract String getState();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PARENT)
    @Override
    public abstract String getParentPid();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID)
    @Override
    public abstract Integer getBatchId();

    @XmlTransient
    @Override
    public abstract String getK0();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_ARCHIVE_EXPORT)
    @Override
    public abstract Integer getHasArchiveExport();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_KRAMERIUS_EXPORT)
    @Override
    public abstract Integer getHasKrameriusExport();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_NDK_EXPORT)
    @Override
    public abstract Integer getHasNdkExport();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_CROSSREF_EXPORT)
    @Override
    public abstract Integer getHasCrossrefExport();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_EXPORT)
    @Override
    public abstract Integer getHasExport();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_ORGANIZATION)
    @Override
    public abstract String getOrganization();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_USER)
    @Override
    public abstract String getUser();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_STATUS)
    @Override
    public abstract String getStatus();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_LOCKED)
    @Override
    public abstract Integer isLocked();

    @XmlElement(name = DigitalObjectResourceApi.ITEM_VALIDATION)
    @Override
    public abstract String getValidation();

}
