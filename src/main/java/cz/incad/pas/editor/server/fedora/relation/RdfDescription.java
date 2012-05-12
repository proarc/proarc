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

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import org.w3c.dom.Element;

/**
 * RDF description of given object.
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class RdfDescription {

    @XmlAttribute(namespace = Relations.RDF_NS)
    private RelationResource about;

    @XmlElement(namespace = Relations.FEDORA_MODEL_NS)
    private HasModelRelation hasModel;

    @XmlElement(name = "hasMember", namespace = Relations.FEDORA_EXTERNALS_NS)
    private List<HasMemberRelation> hasMembers;
    
    @XmlAnyElement
    private List<Element> relations;

    private RdfDescription() {
    }

    /**
     * @param pid PID of described object
     */
    public RdfDescription(String pid) {
        this.about = RelationResource.fromPid(pid);
    }

    /**
     * Gets described object.
     * @return the Fedora URI
     */
    public RelationResource getAbout() {
        return about;
    }

    /**
     * Sets Fedora URI of described object.
     * @param about URI
     */
    public void setAbout(RelationResource about) {
        this.about = about;
    }

    /**
     * Other relations.
     * @return list of DOM elements
     */
    public List<Element> getRelations() {
        if (relations == null) {
            relations = new ArrayList<Element>();
        }
        return relations;
    }

    /**
     * Relations defining object hierarchy graph.
     * @return list of members
     */
    public List<HasMemberRelation> getMemberRelations() {
        if (hasMembers == null) {
            hasMembers = new ArrayList<HasMemberRelation>();
        }
        return hasMembers;
    }

    public HasModelRelation getModel() {
        return hasModel;
    }

    public void setModel(String model) {
        this.hasModel = new HasModelRelation(model);
    }

}
