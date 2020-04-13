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
package cz.cas.lib.proarc.common.fedora.relation;

import org.w3c.dom.Element;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import java.util.ArrayList;
import java.util.List;

/**
 * RDF description of given object.
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class RdfDescription {

    @XmlElement(namespace = Relations.PROARC_RELS_NS)
    private RdfRelation organization;

    @XmlAttribute(namespace = Relations.RDF_NS)
    private RelationResource about;

    /**
     * RDF relation defines model of the digital object. E.g.:
     *
     * <p/>{@code <fedora-model:hasModel rdf:resource="info:fedora/mode:page"/>}
     *
     * @see <a href='https://wiki.duraspace.org/display/FEDORA35/Content+Model+Architecture'>
     *      Content Model Architecture</a>
     */
    @XmlElement(namespace = Relations.FEDORA_MODEL_NS)
    private RdfRelation hasModel;

    /**
     * RDF relation defines digital device of the digital object. E.g.:
     *
     * <p/>{@code <proarc-rels:hasDevice rdf:resource="info:fedora/device:scanner1"/>}
     */
    @XmlElement(namespace = Relations.PROARC_RELS_NS)
    private RdfRelation hasDevice;

    /**
     * RDF relation defines filename of the imported digital content. E.g.:
     *
     * <p/>{@code <proarc-rels:importFile>ABA00726009905207199800001.tif<importFile/>}
     */
    @XmlElement(namespace = Relations.PROARC_RELS_NS)
    private String importFile;

    /**
     * RDF relation defines existence of any export. It can be some SIP ID or folder E.g.:
     *
     * <p/>{@code <proarc-rels:hasExport>1234<hasExport/>}
     * <p>It is expected to be overridden by the latest export for now.
     */
    @XmlElement(namespace = Relations.PROARC_RELS_NS)
    private String hasExport;

    /**
     * RDF relation referencing members of the digital object.
     *
     * <p/>{@code <fedora-rels-ext:hasMember rdf:resource="info:fedora/uuid:ebbd1d68-f5e0-4074-a2a6-19ecef6a6759"/>}
     *
     * @see <a href='http://www.fedora.info/definitions/1/0/fedora-relsext-ontology.rdfs'>
     *      Fedora Relationships</a>
     */
    @XmlElement(name = "hasMember", namespace = Relations.FEDORA_EXTERNALS_NS)
    private List<RdfRelation> hasMembers;

    /**
     * RDF relation defines a membership of the digital object.
     *
     * <p/>{@code <fedora-rels-ext:isMemberOf rdf:resource="info:fedora/group:user_group"/>}
     *
     * @see <a href='http://www.fedora.info/definitions/1/0/fedora-relsext-ontology.rdfs'>
     *      Fedora Relationships</a>
     */
    @XmlElement(name = "isMemberOf", namespace = Relations.FEDORA_EXTERNALS_NS)
    private List<RdfRelation> memberships;

    /**
     * RDF relation referencing members of the digital object.
     *
     * <p/>{@code <proarc-rels:hasOwner rdf:resource="info:fedora/group:owner"/>}
     */
    @XmlElement(name = "hasOwner", namespace = Relations.PROARC_RELS_NS)
    private List<RdfRelation> owners;

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
    public List<RdfRelation> getMemberRelations() {
        if (hasMembers == null) {
            hasMembers = new ArrayList<RdfRelation>();
        }
        return hasMembers;
    }

    /**
     * Relations defining the reverse object hierarchy graph (isMemberOf).
     * @return list of objects, where the object is a member.
     */
    public List<RdfRelation> getMembershipRelations() {
        if (memberships == null) {
            memberships = new ArrayList<RdfRelation>();
        }
        return memberships;
    }

    /**
     * Relations defining ownership of the object.
     * @return list of owners
     */
    public List<RdfRelation> getOwners() {
        if (owners == null) {
            owners = new ArrayList<RdfRelation>();
        }
        return owners;
    }

    public RdfRelation getModel() {
        return hasModel;
    }

    public void setModel(RdfRelation model) {
        this.hasModel = model;
    }

    public RdfRelation getDevice() {
        return hasDevice;
    }

    public void setDevice(RdfRelation hasDevice) {
        this.hasDevice = hasDevice;
    }

    public String getImportFile() {
        return importFile;
    }

    public void setImportFile(String importFile) {
        this.importFile = importFile;
    }

    public String getHasExport() {
        return hasExport;
    }

    public void setHasExport(String hasExport) {
        this.hasExport = hasExport;
    }

    public RdfRelation getOrganization() {
        return organization;
    }

    public void setOrganization(RdfRelation organization) {
        this.organization = organization;
    }
}
