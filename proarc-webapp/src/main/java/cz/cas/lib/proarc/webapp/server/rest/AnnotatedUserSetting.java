/*
 * Copyright (C) 2015 Lukas Sykora
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

import cz.cas.lib.proarc.common.user.UserSetting;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;
import java.sql.Timestamp;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Helper class to annotate {@link cz.cas.lib.proarc.common.user.UserSetting} properties.
 *
 * @see JacksonProvider
 *
 * @author Lukas Sykora
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AnnotatedUserSetting extends UserSetting {

    @XmlElement(name = UserResourceApi.USER_ID)
    @Override
    public abstract Integer getUserId();

    @XmlElement(name = UserResourceApi.USER_SETTING)
    @Override
    public abstract String getUserSetting();

    @XmlElement(name = UserResourceApi.USER_TIMESTAMP)
    @Override
    public abstract Timestamp getTimestamp();

    @XmlElement(name = "id")
    @Override
    public abstract Integer getId();

    @XmlElement(name = "validation")
    @Override
    public abstract String getValidation();
}
