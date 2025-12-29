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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.storage.fedora;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Deletes object or hierarchy of objects from the remote storage. It also updates
 * RELS-EXT of objects referencing deleted PIDs.
 * <p>There are 2 options to delete objects. The first is to
 * {@link #purge(java.util.List, boolean, java.lang.String) purge} them. It removes
 * all deleted objects from the repository. The second is to mark them as
 * {@link #delete(java.util.List, boolean, java.lang.String) deleted}.
 * The second option should be preferred to keep the history or in case of
 * e.g. OAI usage.</p>
 *
 * @author Jan Pokorsky
 */
public final class PurgeFedoraObject {

    private final FedoraStorage storage;
    /**
     * set of PIDs to purge
     */
    private final Set<String> toPurge;
    /**
     * set of PIDs to update
     */
    private final Set<String> toUpdate;
    private String logMessage;

    public PurgeFedoraObject(FedoraStorage storage) {
        this.storage = storage;
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
        List<String> reversedPurge = new ArrayList<>(toPurge);
        Collections.reverse(reversedPurge);
        if (setDeleted) {
            setDeleted(reversedPurge);
        } else if (isRestore) {
            setRestore(reversedPurge);
        } else {
            purge(reversedPurge);
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
            List<SearchViewItem> pids = storage.getSearch().findChildrenHierarchy(pid);
            return pids;
        } catch (FedoraClientException ex) {
            throw new PurgeException(pid, ex);
        } catch (IOException ex) {
            throw new PurgeException(pid, ex);
        }
    }

    private List<SearchViewItem> getParent(String pid) throws PurgeException {
        List<SearchViewItem> pids = null;
        try {
            pids = storage.getSearch().findReferrers(pid);
        } catch (IOException ex) {
            throw new PurgeException(pid, ex);
        } catch (FedoraClientException ex) {
            throw new PurgeException(pid, ex);
        }
        return pids;
    }

    /**
     * Removes all objects that will be deleted from PID's relations in RELS-EXT.
     * @param pid object to update
     */
    private void updateRelations(String pid) throws DigitalObjectException {
        RemoteObject remote = storage.find(pid);
        RelationEditor editor = new RelationEditor(remote);
        List<String> members = editor.getMembers();
        members.removeAll(toPurge);
        editor.setMembers(members);
        editor.write(editor.getLastModified(), logMessage);
        remote.flush();
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
            RemoteObject remote = storage.find(pid);
            remote.purge(logMessage);
        } catch (DigitalObjectException ex) {
            throw new PurgeException(pid, ex);
        }
    }

    private void purge(List<String> pids) throws PurgeException {
        for (String pid : pids) {
            purge(pid);
        }
    }

    private void setDeleted(List<String> pids) throws PurgeException {
        for (String pid : pids) {
            setDeleted(pid);
        }
    }

    private void setDeleted(String pid) throws PurgeException {
        try {
            RemoteObject remote = storage.find(pid);
            remote.delete(logMessage);
        } catch (DigitalObjectException ex) {
            throw new PurgeException(pid, ex);
        }
    }

    private void setRestore(List<String> pids) throws PurgeException {
        for (String pid : pids) {
            setRestore(pid);
        }
    }

    private void setRestore(String pid) throws PurgeException {
        try {
            RemoteObject remote = storage.find(pid);
            remote.restore(logMessage);
        } catch (DigitalObjectException ex) {
            throw new PurgeException(pid, ex);
        }
    }


    public static class PurgeException extends Exception {

        public PurgeException(Throwable cause) {
            super(cause);
        }

        public PurgeException(String message, Throwable cause) {
            super(message, cause);
        }

    }
}
