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
package cz.cas.lib.proarc.common.user;

import java.sql.Timestamp;

/**
 * The user group.
 *
 * @author Jan Pokorsky
 */
public final class Group {

    private Integer id;
    /**
     * The unique group name. Used as fedora PID.
     */
    private String name;
    private String title;
    private String remoteName;
    private String remoteType;
    private Timestamp created;
    /**
     * The optimistic lock.
     */
    private Timestamp timestamp;

    public static Group create(String simpleName, String title) {
        if (simpleName == null || simpleName.isEmpty()) {
            throw new IllegalArgumentException("simpleName");
        }
        Group group = new Group();
        group.setName(FedoraGroupDao.PID_PREFIX + simpleName);
        group.setTitle(title);
        return group;
    }

    public static Group createRemote(String simpleName, String title, String remoteName, String remoteType) {
        if (simpleName == null || simpleName.isEmpty()) {
            throw new IllegalArgumentException("simpleName");
        }
        if (remoteName == null || remoteName.isEmpty()) {
            throw new IllegalArgumentException("remoteName");
        }
        if (remoteType == null || remoteType.isEmpty()) {
            throw new IllegalArgumentException("remoteType");
        }
        Group group = new Group();
        group.setName(FedoraGroupDao.PID_PREFIX + simpleName);
        group.setTitle(title);
        group.setRemoteName(remoteName);
        group.setRemoteType(remoteType);
        return group;
    }

    public Group() {
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getRemoteName() {
        return remoteName;
    }

    public void setRemoteName(String remoteName) {
        this.remoteName = remoteName;
    }

    public String getRemoteType() {
        return remoteType;
    }

    public void setRemoteType(String remoteType) {
        this.remoteType = remoteType;
    }

    public Timestamp getCreated() {
        return created;
    }

    public void setCreated(Timestamp created) {
        this.created = created;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    @Override
    public String toString() {
        return "Group{" + "id=" + id + ", name=" + name + ", title=" + title
                + ", remoteName=" + remoteName + ", remoteType=" + remoteType
                + ", created=" + created + ", timestamp=" + timestamp + '}';
    }

}
