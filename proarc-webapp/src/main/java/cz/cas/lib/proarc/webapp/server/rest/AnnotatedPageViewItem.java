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

import cz.cas.lib.proarc.common.fedora.PageView.Item;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Helper class to annotate {@link Item} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedPageViewItem extends Item {

    @XmlElement(name = ImportResourceApi.BATCHITEM_BATCHID)
    @Override
    public abstract Integer getBatchId();

    @XmlElement(name = ImportResourceApi.BATCHITEM_FILENAME)
    @Override
    public abstract String getFilename();

    @XmlElement(name = ImportResourceApi.BATCHITEM_PID)
    @Override
    public abstract String getPid();

    @XmlElement(name = ImportResourceApi.BATCHITEM_MODEL)
    @Override
    public abstract String getModel();

    @XmlElement(name = ImportResourceApi.BATCHITEM_PAGEINDEX)
    @Override
    public abstract String getPageIndex();

    @XmlElement(name = ImportResourceApi.BATCHITEM_PAGENUMBER)
    @Override
    public abstract String getPageNumber();

    @XmlElement(name = ImportResourceApi.BATCHITEM_PAGETYPE)
    @Override
    public abstract String getPageType();

    @XmlElement(name = ImportResourceApi.BATCHITEM_PAGETYPELABEL)
    @Override
    public abstract String getPageTypeLabel();

    @XmlElement(name = ImportResourceApi.BATCHITEM_TIMESTAMP)
    @Override
    public abstract long getTimestamp();

    @XmlElement(name = ImportResourceApi.BATCHITEM_USER)
    @Override
    public abstract String getUser();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_LABEL)
    @Override
    public abstract String getLabel();

    @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PAGE_POSITION)
    @Override
    public abstract String getPagePosition();

}
