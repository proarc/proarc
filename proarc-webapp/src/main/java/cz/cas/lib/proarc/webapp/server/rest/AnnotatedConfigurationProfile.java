/*
 * Copyright (C) 2015 Jan Pokorsky
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

import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import java.io.File;

/**
 * Helper class to annotate {@link ConfigurationProfile} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedConfigurationProfile extends ConfigurationProfile {

    public AnnotatedConfigurationProfile(String id) {
        super(id);
    }

    @XmlElement(name = ConfigurationProfileResourceApi.PROFILE_ID)
    @Override
    public abstract String getId();

    @XmlElement(name = ConfigurationProfileResourceApi.PROFILE_LABEL)
    @Override
    public abstract String getLabel();

    @XmlElement(name = ConfigurationProfileResourceApi.PROFILE_DESCRIPTION)
    @Override
    public abstract String getDescription();

    @XmlElement(name = ConfigurationProfileResourceApi.PROFILE_ERROR)
    @Override
    public abstract String getError();

    @XmlTransient
    @Override
    public abstract File getFile();

}
