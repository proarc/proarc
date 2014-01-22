/*
 * Copyright (C) 2014 Robert Simonovsky
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

/**
 * METS Schema.
 *
 * <p>Requires JDK 1.6.0_24 or later or com.sun.xml.bind:jaxb-impl:2.2.2 dependency.
 */
@XmlSchema(namespace = cz.cas.lib.proarc.mets.MetsConstants.NS_METS,
        elementFormDefault = javax.xml.bind.annotation.XmlNsForm.QUALIFIED,
        xmlns = { @XmlNs(namespaceURI = cz.cas.lib.proarc.mets.MetsConstants.NS_METS,
                prefix = cz.cas.lib.proarc.mets.MetsConstants.PREFIX_NS_METS) })
package cz.cas.lib.proarc.mets;

import javax.xml.bind.annotation.XmlNs;
import javax.xml.bind.annotation.XmlSchema;
