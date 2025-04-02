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

import cz.cas.lib.proarc.common.storage.AtmEditor.AtmItem;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.Date;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Helper class to annotate {@link AtmItem} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedAtmItem extends AtmItem {

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_BATCHID)
    @Override
    public abstract Integer getBatchId();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_PID)
    @Override
    public abstract String getPid();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_OWNER)
    @Override
    public abstract String getOwner();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_MODEL)
    @Override
    public abstract String getModel();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_STATE)
    @Override
    public abstract String getState();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_CREATED)
    @Override
    public abstract String getCreated();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_MODIFIED)
    @Override
    public abstract String getModified();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_DEVICE)
    @Override
    public abstract String getDeviceId();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_SOFTWARE)
    @Override
    public abstract String getSoftwareId();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_FILENAME)
    @Override
    public abstract String getImportFile();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_EXPORTRESULT)
    @Override
    public abstract String getExport();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_ORGANIZATION)
    @Override
    public abstract String getOrganization();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_USER)
    @Override
    public abstract String getUser();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_NDK_EXPORT)
    @Override
    public abstract String getNdkExport();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_KRAMERIUS_EXPORT)
    @Override
    public abstract  String getKrameriusExport();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_ARCHIVE_EXPORT)
    @Override
    public abstract  String getArchiveExport();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_CROSSREF_EXPORT)
    @Override
    public abstract String getCrossrefExport();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_STATUS)
    @Override
    public abstract String getStatus();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_LOCKED)
    @Override
    public abstract boolean isLocked();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_LOCKED_BY)
    @Override
    public abstract String getLockedBy();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_LOCKED_DATE)
    @Override
    public abstract Date getLockedDate();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_DONATOR)
    @Override
    public abstract String getDonator();

    @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_ARCHIVAL_COPIES)
    @Override
    public abstract String getArchivalCopies();
}
