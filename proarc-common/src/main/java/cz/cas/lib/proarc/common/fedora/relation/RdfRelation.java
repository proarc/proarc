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
package cz.cas.lib.proarc.common.fedora.relation;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Generic RDF relation.
 *
 * <p/>{@code <fedora-model:hasModel rdf:resource="info:fedora/mode:page"/>}
 *
 * @see <a href='https://wiki.duraspace.org/display/FEDORA35/Digital+Object+Relationships'>
 *      Digital Object Relationships</a>
 * @see <a href='https://wiki.duraspace.org/display/FEDORA35/Content+Model+Architecture'>
 *      Content Model Architecture</a>
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class RdfRelation {

    @XmlAttribute(namespace = Relations.RDF_NS)
    private RelationResource resource;

    public static RdfRelation fromPid(String pid) {
        return pid == null ? null : new RdfRelation(RelationResource.fromPid(pid));
    }

    public static String toPid(RdfRelation relation) {
        return relation == null ? null : relation.getResourcePid();
    }

    public RdfRelation() {
    }

    public RdfRelation(RelationResource resource) {
        this.resource = resource;
    }

    public RelationResource getResource() {
        return resource;
    }

    public void setResource(RelationResource resource) {
        this.resource = resource;
    }
    
    public String getResourcePid() {
        return resource.getPid();
    }

}
