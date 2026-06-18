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
package cz.cas.lib.proarc.common.storage.relation;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;

/**
 * Holder of RDF descriptions of digital objects. Used for RELS-EXT
 * and RELS-INT datastreams.
 * <p/>For now it supports only RELS-EXT.
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name = "RDF")
@XmlAccessorType(XmlAccessType.FIELD)
public class Rdf {

    @XmlElement(name = "Description")
    private RdfDescription description;

    private Rdf() {
    }

    /**
     * @param pid PID of described object
     */
    public Rdf(String pid) {
        this.description = new RdfDescription(pid);
    }

    public RdfDescription getDescription() {
        return description;
    }

    public void setDescription(RdfDescription description) {
        this.description = description;
    }

}
