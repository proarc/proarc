/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.user;

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.fedora.FedoraDao;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectExistException;

/**
 * Manages user groups stored in the Fedora storage.
 *
 * <p>Groups are referenced by digital objects with {@code <proarc-rels:hasOwner>}
 * RDF relations and {@code <dc.rights>} properties. It allows to query
 * digital objects that belongs to a given user or an user group.
 * <p>Groups are reference by user objects with {@code <fedora-rels-ext:isMemberOf>}
 * RDF relations.
 *
 * @author Jan Pokorsky
 */
public final class FedoraGroupDao extends FedoraDao {

    public static final String METAMODEL_ID = "proarc:group";
    public static final String PID_PREFIX = "group:";
    private final LocalStorage localStorage;

    public FedoraGroupDao() {
        this.localStorage = new LocalStorage();
    }

    public String addNewGroup(Group group, String owner, String logMessage) {
        return addGroup(group, true, owner, logMessage);
    }

    public String addGroup(Group group, String owner, String logMessage) {
        boolean isRemote = group.getRemoteName() != null;
        return addGroup(group, isRemote, owner, logMessage);
    }

    String addGroup(Group group, boolean findFreePid, String owner, String logMessage) {
        String pid = group.getName();
        LocalObject lobject = newGroup(pid, group, findFreePid, owner, logMessage);
        group.setName(lobject.getPid());
        if (group.getTitle() == null) {
            group.setTitle(lobject.getLabel());
        }
        return lobject.getPid();
    }

    LocalObject newGroup(String pid, Group group, boolean findFreePid, String owner, String logMessage) {
        LocalObject newObject = newGroup(pid, group, owner, logMessage);
        if (newObject != null) {
            return newObject;
        }
        for (int i = 1; findFreePid && i < 1000; i++) {
            newObject = newGroup(String.format("%s_%s", pid, i), group, owner, logMessage);
            if (newObject != null) {
                return newObject;
            }
        }
        throw new IllegalStateException("Group exists: " + pid);
    }

    private LocalObject newGroup(String pid, Group group, String owner, String logMessage) {
        try {
            LocalObject lobject = localStorage.create(pid);
            lobject.setOwner(owner);
            lobject.setLabel(group.getTitle() == null ? toGroupName(pid) : group.getTitle());
            RelationEditor relationEditor = new RelationEditor(lobject);
            relationEditor.setModel(METAMODEL_ID);
            relationEditor.write(relationEditor.getLastModified(), logMessage);
            lobject.flush();
            return ingets(lobject, owner, logMessage);
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(pid, ex);
        }
    }

    LocalObject ingets(LocalObject lobject, String owner, String logMessage) throws DigitalObjectException {
        try {
            getRemoteStorage().ingest(lobject, owner, logMessage);
            tx.addPid(lobject.getPid());
            return lobject;
        } catch (DigitalObjectExistException ex) {
            return null;
        }
    }

    static String toGroupName(String pid) {
        return pid.substring(PID_PREFIX.length());
    }

}
