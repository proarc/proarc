/*
 * Copyright (C) 2022 Lukas Sykora
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
package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.fedora.PurgeFedoraObject.PurgeException;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public final class PurgeAkubraObject {

    private final AkubraStorage akubraStorage;
    /**
     * set of PIDs to purge
     */
    private final Set<String> toPurge;
    /**
     * set of PIDs to update
     */
    private final Set<String> toUpdate;
    private String logMessage;

    public PurgeAkubraObject(AkubraStorage storage) {
        this.akubraStorage = storage;
        this.toPurge = new LinkedHashSet<String>();
        this.toUpdate = new HashSet<String>();
    }

    public void delete(String pid, boolean hierarchy, String message) throws PurgeException {
        process(Collections.singletonList(pid), hierarchy, true, false, message);
    }

    public void delete(List<String> pids, boolean hierarchy, String message) throws PurgeException {
        process(pids, hierarchy, true, false, message);
    }

    public void purge(String pid, boolean hierarchy, String message) throws PurgeException {
        purge(Collections.singletonList(pid), hierarchy, message);
    }

    public void purge(List<String> pids, boolean hierarchy, String message) throws PurgeException {
        process(pids, hierarchy, false, false, message);
    }

    public void restore(String pid, String message) throws PurgeException {
        restore(Collections.singletonList(pid), false, false, true, message);
    }

    public void restore(List<String> pids, String message) throws PurgeException {
        restore(pids, false, false, true, message);
    }

    private void restore(List<String> pids, boolean hierarchy, boolean setDeleted, boolean isRestore, String message) throws PurgeException {
        process(pids, hierarchy, setDeleted, isRestore, message);
    }

    private void process(List<String> pids, boolean hierarchy, boolean setDeleted, boolean isRestore, String message) throws PurgeException {
        toPurge.clear();
        toUpdate.clear();
        this.logMessage = message;
        for (String pid : pids) {
            process(pid, hierarchy);
        }
        try {
            updateRelations(toUpdate);
        } catch (DigitalObjectException ex) {
            throw new PurgeException(ex);
        }
        if (setDeleted) {
            setDeleted(toPurge);
        } else if (isRestore) {
            setRestore(toPurge);
        } else {
            purge(toPurge);
        }
    }

    private void process(String pid, boolean hierarchy) throws PurgeException {
        if (!toPurge.add(pid)) {
            return ;
        }
        if (hierarchy) {
            List<SearchViewItem> items = getHierarchy(pid);
            for (SearchViewItem item : items) {
                toPurge.add(item.getPid());
            }
        }

        // For now it presumes that objects are referenced by one parent thus
        // only roots are queried for parent.
        // Later it should ask for parents of each deleted object. It will be
        // expensive operation that could be optimized with ITQL queries created
        // for each ~20 items not to exceed query length (missing specification).
        // Query:
        //select $pid
        //from <#ri>
        //where
        //$pid <info:fedora/fedora-system:def/relations-external#hasMember> <info:fedora/uuid:tree1-child1-child1-child1>
        //or $pid <info:fedora/fedora-system:def/relations-external#hasMember> <info:fedora/uuid:tree1-child1-child1>
        List<SearchViewItem> parents = getParent(pid);
        for (SearchViewItem parent : parents) {
            toUpdate.add(parent.getPid());
        }
    }

    private List<SearchViewItem> getHierarchy(String pid) throws PurgeException {
        try {
            List<SearchViewItem> pids = akubraStorage.getSearch().findChildrenHierarchy(pid);
            return pids;
        } catch (DigitalObjectException | IOException ex) {
            throw new PurgeException(pid, ex);
        }
    }

    private List<SearchViewItem> getParent(String pid) throws PurgeException {
        List<SearchViewItem> pids = null;
        try {
            pids = akubraStorage.getSearch().findReferrers(pid);
        } catch (IOException ex) {
            throw new PurgeException(pid, ex);
        }
        return pids;
    }

    /**
     * Removes all objects that will be deleted from PID's relations in RELS-EXT.
     * @param pid object to update
     */
    private void updateRelations(String pid) throws DigitalObjectException {
        AkubraObject fedoraObject = akubraStorage.find(pid);
        RelationEditor editor = new RelationEditor(fedoraObject);
        List<String> members = editor.getMembers();
        members.removeAll(toPurge);
        editor.setMembers(members);
        editor.write(editor.getLastModified(), logMessage);
        fedoraObject.flush();
    }

    /**
     * @see #updateRelations(java.lang.String)
     */
    private void updateRelations(Iterable<String> pids) throws DigitalObjectException {
        for (String pid : pids) {
            updateRelations(pid);
        }
    }

    private void purge(String pid) throws PurgeException {
        try {
            AkubraObject remote = akubraStorage.find(pid);
            remote.purge(logMessage);
        } catch (DigitalObjectException ex) {
            throw new PurgeException(pid, ex);
        }
    }

    private void purge(Set<String> pids) throws PurgeException {
        for (String pid : pids) {
            purge(pid);
        }
    }

    private void setDeleted(Set<String> pids) throws PurgeException {
        for (String pid : pids) {
            setDeleted(pid);
        }
    }

    private void setDeleted(String pid) throws PurgeException {
        try {
            AkubraObject remote = akubraStorage.find(pid);
            remote.delete(logMessage);
        } catch (DigitalObjectException ex) {
            throw new PurgeException(pid, ex);
        }
    }

    private void setRestore(Set<String> pids) throws PurgeException {
        for (String pid : pids) {
            setRestore(pid);
        }
    }

    private void setRestore(String pid) throws PurgeException {
        try {
            AkubraObject remote = akubraStorage.find(pid);
            remote.restore(logMessage);
        } catch (DigitalObjectException ex) {
            throw new PurgeException(pid, ex);
        }
    }
}
