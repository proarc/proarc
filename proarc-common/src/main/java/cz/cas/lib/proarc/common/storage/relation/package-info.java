/*
 * Copyright (C) 2012 Jan Pokorsky
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
        namespace = Relations.RDF_NS,
        elementFormDefault = XmlNsForm.QUALIFIED,
        xmlns = {
            @XmlNs(prefix = "rdf", namespaceURI = Relations.RDF_NS),
            @XmlNs(prefix = "fedora-model", namespaceURI = Relations.FEDORA_MODEL_NS),
            @XmlNs(prefix = "fedora-rels-ext", namespaceURI = Relations.FEDORA_EXTERNALS_NS),
            @XmlNs(prefix = "proarc-rels", namespaceURI = Relations.PROARC_RELS_NS)
        })
package cz.cas.lib.proarc.common.storage.relation;

import jakarta.xml.bind.annotation.XmlNs;
import jakarta.xml.bind.annotation.XmlNsForm;
import jakarta.xml.bind.annotation.XmlSchema;