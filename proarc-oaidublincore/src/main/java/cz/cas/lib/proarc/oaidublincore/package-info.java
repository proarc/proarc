/*
 * Copyright (C) 2011 Jan Pokorsky
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
@XmlSchema(
        namespace = "http://www.openarchives.org/OAI/2.0/oai_dc/",
        elementFormDefault = javax.xml.bind.annotation.XmlNsForm.QUALIFIED,
        xmlns = {
            @XmlNs(namespaceURI = "http://www.openarchives.org/OAI/2.0/oai_dc/", prefix = "oai_dc"),
            @XmlNs(namespaceURI = "http://purl.org/dc/elements/1.1/", prefix = "dc")
        })
package cz.cas.lib.proarc.oaidublincore;

import javax.xml.bind.annotation.XmlNs;
import javax.xml.bind.annotation.XmlSchema;

