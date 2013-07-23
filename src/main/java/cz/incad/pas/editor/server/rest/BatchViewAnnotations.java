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
package cz.incad.pas.editor.server.rest;

import cz.cas.lib.proarc.common.dao.BatchView;
import cz.incad.pas.editor.shared.rest.ImportResourceApi;
import java.sql.Timestamp;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;

/**
 * Helper class to annotate {@link BatchView} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@javax.xml.bind.annotation.XmlRootElement(name = ImportResourceApi.IMPORT_BATCH_ELEMENT)
@javax.xml.bind.annotation.XmlAccessorType(XmlAccessType.NONE)
public abstract class BatchViewAnnotations extends BatchView {

    @XmlElement(required = true, name = ImportResourceApi.IMPORT_BATCH_ID)
    @Override
    public Integer getId() {
        return super.getId();
    }

    @XmlTransient
    @Override
    public abstract String getFolder();

    @XmlElement(name = ImportResourceApi.IMPORT_BATCH_DESCRIPTION)
    @Override
    public abstract String getTitle();

    @XmlSchemaType(name = "dateTime")
    @XmlElement(name = ImportResourceApi.IMPORT_BATCH_TIMESTAMP)
    @Override
    public abstract Timestamp getCreate();

    @XmlElement(name = ImportResourceApi.IMPORT_BATCH_STATE)
    @Override
    public abstract String getState();

    @XmlElement(name = ImportResourceApi.IMPORT_BATCH_USERID)
    @Override
    public abstract Integer getUserId();

    @XmlElement(name = ImportResourceApi.IMPORT_BATCH_USER)
    @Override
    public abstract String getUsername();

    @XmlElement(name = ImportResourceApi.IMPORT_BATCH_PARENTPID)
    @Override
    public abstract String getParentPid();

}
