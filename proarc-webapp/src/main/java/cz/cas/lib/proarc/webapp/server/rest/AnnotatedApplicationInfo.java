/*
 * Copyright (C) 2021 Lukas Sykora
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

import cz.cas.lib.proarc.common.info.ApplicationInfo;
import cz.cas.lib.proarc.webapp.shared.rest.ApplicationResourceApi;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Helper class to annotate {@link ApplicationInfo} properties.
 *
 * @author Lukáš Sýkora
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedApplicationInfo extends ApplicationInfo {

    @XmlElement(name = ApplicationResourceApi.VERSION)
    @Override
    public String getVersion() {
        return super.getVersion();
    }

    @XmlElement(name = ApplicationResourceApi.REVISION)
    @Override
    public String getRevision() {
        return super.getRevision();
    }

    @XmlElement(name = ApplicationResourceApi.TIMESTAMP)
    @Override
    public String getTimestamp() {
        return super.getTimestamp();
    }

    @XmlElement(name = ApplicationResourceApi.RDFLOW_VERSION)
    @Override
    public String getRdflowVersion() {
        return super.getRdflowVersion();
    }

    @XmlElement(name = ApplicationResourceApi.STORAGE)
    @Override
    public String getStorage() {
        return super.getStorage();
    }

    @XmlElement(name = ApplicationResourceApi.DATABASE)
    @Override
    public String getDatabase() {
        return super.getDatabase();
    }
}
