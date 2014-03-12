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

import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraDao;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectExistException;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.logging.Logger;

/**
 * Manages users stored in the Fedora storage.
 *
 * @author Jan Pokorsky
 */
public class FedoraUserDao extends FedoraDao {

    public static final String METAMODEL_ID = "proarc:user";
    public static final String PID_PREFIX = "user:";
    private static final Logger LOG = Logger.getLogger(FedoraUserDao.class.getName());
    private final LocalStorage localStorage;

    public FedoraUserDao() {
        this.localStorage = new LocalStorage();
    }

    public String add(UserProfile user, String owner, String logMessage) {
        String pid = user.getUserNameAsPid();
        boolean isRemote = user.getRemoteName() != null;
        LocalObject newObject = newObject(pid, user, isRemote, owner, logMessage);
        user.setUserName(getUserName(newObject.getPid()));
        return newObject.getPid();
    }

    public void addMembership(UserProfile user, Collection<Group> groups, String logMessage) throws DigitalObjectException {
        String pid = user.getUserNameAsPid();
        RemoteObject fobject = getRemoteStorage().find(pid);
        RelationEditor relationEditor = new RelationEditor(fobject);
        LinkedHashSet<String> memberships = new LinkedHashSet<String>(relationEditor.getMembership());
        memberships.addAll(UserUtil.toGroupPid(groups));
        relationEditor.setMembership(memberships);
        relationEditor.write(relationEditor.getLastModified(), logMessage);
        fobject.flush();
    }

    public void setMembership(UserProfile user, Collection<Group> groups, String logMessage) throws DigitalObjectException {
        String pid = user.getUserNameAsPid();
        RemoteObject fobject = getRemoteStorage().find(pid);
        RelationEditor relationEditor = new RelationEditor(fobject);
        relationEditor.setMembership(UserUtil.toGroupPid(groups));
        relationEditor.write(relationEditor.getLastModified(), logMessage);
        fobject.flush();
    }

    static String getUserName(String pid) {
        return pid.substring(PID_PREFIX.length());
    }

    LocalObject newObject(String pid, UserProfile user, boolean findFreePid, String owner, String logMessage) {
        LocalObject newObject = newObject(pid, user, owner, logMessage);
        if (newObject != null) {
            return newObject;
        }
        for (int i = 1; findFreePid && i < 1000; i++) {
            newObject = newObject(String.format("%s_%s", pid, i), user, owner, logMessage);
            if (newObject != null) {
                return newObject;
            }
        }
        throw new IllegalStateException("User exists: " + pid);
    }

    LocalObject newObject(String pid, UserProfile user, String owner, String logMessage) {
        try {
            LocalObject lobject = localStorage.create(pid);
            lobject.setLabel(getUserName(pid));
            lobject.setOwner(owner);
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

}
