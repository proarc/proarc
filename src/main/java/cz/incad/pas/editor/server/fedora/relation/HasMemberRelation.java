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
package cz.incad.pas.editor.server.fedora.relation;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

/**
 * RDF relation referencing members of the digital object.
 * 
 * <p/>{@code <fedora-rels-ext:hasMember rdf:resource="info:fedora/uuid:ebbd1d68-f5e0-4074-a2a6-19ecef6a6759"/>}
 *
 * @see <a href='http://www.fedora.info/definitions/1/0/fedora-relsext-ontology.rdfs'>
 *      Fedora Relationships</a>
 * 
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(namespace = Relations.FEDORA_EXTERNALS_NS)
public class HasMemberRelation {

    @XmlAttribute(namespace = Relations.RDF_NS)
    private RelationResource resource;

    public HasMemberRelation() {
    }

    public HasMemberRelation(String memberPid) {
        this.resource = RelationResource.fromPid(memberPid);
    }

    public RelationResource getResource() {
        return resource;
    }

    public void setResource(RelationResource resource) {
        this.resource = resource;
    }

    /**
     * @return PID of the member
     */
    public String getMember() {
        return resource.getPid();
    }

}
