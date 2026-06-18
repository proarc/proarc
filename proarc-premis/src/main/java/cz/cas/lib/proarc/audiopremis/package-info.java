/*
 * Copyright (C) 2018 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * Defines name space/prefix mapping for AUDIOPREMIS.
 */
@XmlSchema(
        namespace = AudioPremisUtils.NS,
        elementFormDefault = jakarta.xml.bind.annotation.XmlNsForm.QUALIFIED,
        xmlns = {
                @XmlNs(namespaceURI = AudioPremisUtils.NS, prefix = "nk"),
                @XmlNs(namespaceURI = "http://www.w3.org/1999/xlink", prefix = "xlink")
        })
package cz.cas.lib.proarc.audiopremis;

import jakarta.xml.bind.annotation.XmlNs;
import jakarta.xml.bind.annotation.XmlSchema;

