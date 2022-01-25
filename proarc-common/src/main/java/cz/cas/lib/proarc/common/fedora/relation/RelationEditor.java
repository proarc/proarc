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

import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.WorkflowStorage;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor.EditorResult;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import javax.xml.transform.Source;
import org.w3c.dom.Element;

/**
 * RDF relations editor.
 *
 * @see <a href='https://wiki.duraspace.org/display/FEDORA35/Digital+Object+Relationships'>
 *      Digital Object Relationships</a>
 * @see <a href='https://wiki.duraspace.org/display/FEDORA35/Resource+Index'>
 *      Resource Index</a>
 * @author Jan Pokorsky
 */
public final class RelationEditor {

    public static final String DATASTREAM_ID = "RELS-EXT";
    public static final String DATASTREAM_FORMAT_URI = "info:fedora/fedora-system:FedoraRELSExt-1.0";
    public static final String DATASTREAM_LABEL = "RDF Statements about this object";

    private final XmlStreamEditor editor;
    private final FedoraObject fobject;
    private Rdf relsExt;

    public RelationEditor(FedoraObject fobject) {
        this.fobject = fobject;
        this.editor = fobject.getEditor(profile());
    }

    public static DatastreamProfile profile() {
        return FoxmlUtils.inlineProfile(DATASTREAM_ID, DATASTREAM_FORMAT_URI, DATASTREAM_LABEL);
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    /**
     * @param model PID of the model
     */
    public void setModel(String model) throws DigitalObjectException {
        Rdf rdf = getRdf();
        rdf.getDescription().setModel(RdfRelation.fromPid(model));
    }

    /**
     * @return PID of the model
     */
    public String getModel() throws DigitalObjectException {
        Rdf rdf = getRdf();
        return RdfRelation.toPid(rdf.getDescription().getModel());
    }

    /**
     * @param organization of model
     */
    public void setOrganization(String organization) throws DigitalObjectException {
        if (organization == null) {
            organization = ".";
        }
        Rdf rdf = getRdf();
        rdf.getDescription().setOrganization(RdfRelation.fromPid(organization));
    }


    /**
     * use for Kramerius Export
     */
    public void setEmptyOrganization() throws DigitalObjectException {
        Rdf rdf = getRdf();
        rdf.getDescription().setOrganization(RdfRelation.fromPid(null));
    }

    /**
     * @return organization of model
     */
    public String getOrganization() throws DigitalObjectException {
        Rdf rdf = getRdf();
        return RdfRelation.toPid(rdf.getDescription().getOrganization());
    }

    /**
     * @return user of model
     */
    public String getUser() throws DigitalObjectException {
        Rdf rdf = getRdf();
        return RdfRelation.toPid(rdf.getDescription().getUser());
    }

    /**
     * @param user of model
     */
    public void setUser(String user) throws DigitalObjectException {
        if (user == null) {
            user = "all";
        }
        Rdf rdf = getRdf();
        rdf.getDescription().setUser(RdfRelation.fromPid(user));
    }

    /**
     * use for Kramerius Export
     */
    public void setEmptyUser() throws DigitalObjectException {
        Rdf rdf = getRdf();
        rdf.getDescription().setUser(RdfRelation.fromPid(null));
    }

    /**
     * @return state of model
     */
    public String getStatus() throws DigitalObjectException {
        Rdf rdf = getRdf();
        return RdfRelation.toPid(rdf.getDescription().getStatus());
    }

    /**
     * @param status of model
     */
    public void setStatus(String status) throws DigitalObjectException {
        Rdf rdf = getRdf();
        rdf.getDescription().setStatus(RdfRelation.fromPid(status));
    }

    /**
     * @param device PID of the device
     */
    public void setDevice(String device) throws DigitalObjectException {
        Rdf rdf = getRdf();
        rdf.getDescription().setDevice(RdfRelation.fromPid(device));
    }

    /**
     * @return PID of the device
     */
    public String getDevice() throws DigitalObjectException {
        Rdf rdf = getRdf();
        return RdfRelation.toPid(rdf.getDescription().getDevice());
    }

    /**
     * @param filename filename of the imported digital content
     */
    public void setImportFile(String filename) throws DigitalObjectException {
        getRdf().getDescription().setImportFile(filename);
    }

    /**
     *
     * @return filename of the imported digital content
     */
    public String getImportFile() throws DigitalObjectException {
        return getRdf().getDescription().getImportFile();
    }

    /**
     * Sets some identifier of the export action.
     * @param result e.g. SIP ID or folder
     * @throws DigitalObjectException failure
     */
    public void setExportResult(String result) throws DigitalObjectException {
        getRdf().getDescription().setHasExport(result);
    }

    /**
     * Gets an export identifier.
     * @return the identifier
     * @throws DigitalObjectException  failure
     */
    public String getExportResult() throws DigitalObjectException {
        return getRdf().getDescription().getHasExport();
    }

    /**
     * Sets some identifier of the Ndk export action.
     * @param result e.g. SIP ID or folder
     * @throws DigitalObjectException failure
     */
    public void setNdkExportResult(String result) throws DigitalObjectException {
        getRdf().getDescription().setHasNdkExport(result);
    }

    /**
     * Gets a NDK export identifier.
     * @return the identifier
     * @throws DigitalObjectException  failure
     */
    public String getNdkExportResult() throws DigitalObjectException {
        return getRdf().getDescription().getHasNdkExport();
    }

    /**
     * Sets some identifier of the Kramerius export action.
     * @param result e.g. SIP ID or folder
     * @throws DigitalObjectException failure
     */
    public void setKrameriusExportResult(String result) throws DigitalObjectException {
        getRdf().getDescription().setHasKrameriusExport(result);
    }

    /**
     * Gets a Kramerius export identifier.
     * @return the identifier
     * @throws DigitalObjectException  failure
     */
    public String getKrameriusExportResult() throws DigitalObjectException {
        return getRdf().getDescription().getHasKrameriusExport();
    }

    /**
     * Sets some identifier of the Archive export action.
     * @param result e.g. SIP ID or folder
     * @throws DigitalObjectException failure
     */
    public void setArchiveExportResult(String result) throws DigitalObjectException {
        getRdf().getDescription().setHasArchiveExport(result);
    }

    /**
     * Gets a Archice export identifier.
     * @return the identifier
     * @throws DigitalObjectException  failure
     */
    public String getArchiveExportResult() throws DigitalObjectException {
        return getRdf().getDescription().getHasArchiveExport();
    }

    /**
     * Sets some identifier of the Ndk export action.
     * @param result e.g. SIP ID or folder
     * @throws DigitalObjectException failure
     */
    public void setCrossrefExportResult(String result) throws DigitalObjectException {
        getRdf().getDescription().setHasCrossrefExport(result);
    }

    /**
     * Gets a NDK export identifier.
     * @return the identifier
     * @throws DigitalObjectException  failure
     */
    public String getCrossrefExportResult() throws DigitalObjectException {
        return getRdf().getDescription().getHasCrossrefExport();
    }

    /**
     * Set date and user who locked object
     */
    public void setLock(String name) throws DigitalObjectException {
        getRdf().getDescription().setIsLocked(Boolean.TRUE.toString());
        getRdf().getDescription().setLockedDate(new Date());
        getRdf().getDescription().setLockedUser(name);
    }

    /**
     * Set unlocked object
     */
    public void setUnlock() throws DigitalObjectException {
        getRdf().getDescription().setIsLocked(null);
        getRdf().getDescription().setLockedDate(null);
        getRdf().getDescription().setLockedUser(null);
    }

    public boolean isLocked() throws DigitalObjectException {
        return Boolean.parseBoolean(getRdf().getDescription().getIsLocked());
    }

    public String getLockedBy() throws DigitalObjectException {
        return getRdf().getDescription().getLockedUser();
    }

    public Date getLockedDate() throws DigitalObjectException {
        return getRdf().getDescription().getLockedDate();
    }

    /**
     * Relations defining object hierarchy graph.
     *
     * @return list of PIDs.
     */
    public List<String> getMembers() throws DigitalObjectException {
        Rdf rdf = getRdf();
        return relationAsPid(rdf.getDescription().getMemberRelations());
    }

    /**
     * Sets relations defining object hierarchy graph.
     * Relations should be ordered for each model.
     *
     * @param members list of PIDs
     */
    public void setMembers(List<String> members) throws DigitalObjectException {
        Rdf rdf = getRdf();
        List<RdfRelation> oldies = rdf.getDescription().getMemberRelations();
        oldies.clear();
        oldies.addAll(pidAsRelation(members));
    }

    /**
     * Relations defining the reverse object hierarchy graph (isMemberOf).
     *
     * @return list of PIDs of objects, where the object is a member.
     */
    public Collection<String> getMembership() throws DigitalObjectException {
        Rdf rdf = getRdf();
        return relationAsPid(rdf.getDescription().getMembershipRelations());
    }

    /**
     * Sets relations defining the reverse object hierarchy graph (isMemberOf).
     *
     * @param subjects list of PIDs of objects, where the object is a member.
     */
    public void setMembership(Collection<String> subjects) throws DigitalObjectException {
        Rdf rdf = getRdf();
        List<RdfRelation> oldies = rdf.getDescription().getMembershipRelations();
        oldies.clear();
        oldies.addAll(pidAsRelation(subjects));
    }

    /**
     * Relations defining ownership of the object.
     *
     * @return list of PIDs of owners
     */
    public Collection<String> getOwners() throws DigitalObjectException {
        Rdf rdf = getRdf();
        return relationAsPid(rdf.getDescription().getOwners());
    }

    /**
     * Sets relations defining the reverse object hierarchy graph (isMemberOf).
     *
     * @param owners list of PIDs of objects, where the object is a member.
     */
    public void setOwners(Collection<String> owners) throws DigitalObjectException {
        Rdf rdf = getRdf();
        List<RdfRelation> oldies = rdf.getDescription().getOwners();
        oldies.clear();
        oldies.addAll(pidAsRelation(owners));
    }

    /**
     * Gets unrecognized relations.
     *
     * @return list of relations
     */
    public List<Element> getRelations() throws DigitalObjectException {
        Rdf rdf = getRdf();
        List<Element> elms = rdf.getDescription().getRelations();
        return new ArrayList<Element>(elms);
    }

    /**
     * Sets relations unrecognized by RelationEditor.
     * <b>Do not use for members, model, ...</b>
     * 
     * @param elms list of custom relations
     */
    public void setRelations(List<Element> elms) throws DigitalObjectException {
        Rdf rdf = getRdf();
        List<Element> relations = rdf.getDescription().getRelations();
        relations.clear();
        relations.addAll(elms);
    }

    /**
     * Replaces all relations.
     * @param rdf the relations
     */
    public void setRdf(Rdf rdf) {
        this.relsExt = rdf;
    }

    /**
     * Prepares updates for {@link FedoraObject#flush() }
     * @param timestamp timestamp
     */
    public void write(long timestamp, String message) throws DigitalObjectException {
        EditorResult result = editor.createResult();
        Relations.marshal(result, relsExt, false);
        editor.write(result, timestamp, message);
    }

    private Rdf getRdf() throws DigitalObjectException {
        if (relsExt != null) {
            return relsExt;
        }
        Source source = editor.read();
        if (source == null) {
            if (fobject instanceof RemoteObject) {
                // it should never arise; broken Fedora?
                throw new DigitalObjectException(fobject.getPid(), "missing RELS-EXT!");
            }
            relsExt = new Rdf(fobject.getPid());

            // WorkflowObject RDF cannot be unmarshalled from RELS-EXT datastream (WorkflowObject has only MODS)
            if (fobject instanceof WorkflowStorage.WorkflowObject) {
                WorkflowStorage.WorkflowObject workflowObject = (WorkflowStorage.WorkflowObject) fobject;
                relsExt.getDescription().setModel(RdfRelation.fromPid(workflowObject.getModel()));
            }

        } else {
            relsExt = Relations.unmarshal(source, Rdf.class);
        }

        return relsExt;
    }

    /**
     * Converts a list of RDF relation IDS to the list of PIDs.
     */
    public static List<String> relationAsPid(List<RdfRelation> relations) {
        ArrayList<String> result = new ArrayList<String>(relations.size());
        for (RdfRelation relation : relations) {
            result.add(RdfRelation.toPid(relation));
        }
        return result;
    }

    private static List<RdfRelation> pidAsRelation(Collection<String> pids) {
        ArrayList<RdfRelation> relations = new ArrayList<RdfRelation>(pids.size());
        for (String pid : pids) {
            relations.add(RdfRelation.fromPid(pid));
        }
        return relations;
    }
}
