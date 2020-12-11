/*
 * Copyright (C) 2011 Jan Pokorsky
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

import java.util.List;
import java.util.Set;

/**
 * Manages users, groups and their permissions in ProArc.
 *
 * @author Jan Pokorsky
 */
public interface UserManager {

    /**
     * Authenticates a local user.
     * @return {@code null} or the authenticated user
     */
    UserProfile authenticate(String userName, String passwd);
    UserProfile find(String userName) throws IllegalArgumentException;
    UserProfile find(int userId) throws IllegalArgumentException;
    UserProfile find(String remoteName, String remoteType);

    List<UserProfile> findAll();

    /**
     * Adds a new user. It creates also the user group ({@link UserProfile#getUserGroup() })
     * that can define permissions or override permissions inherited from other user groups.
     *
     * @param profile user properties
     * @param groups groups for the user membership
     * @param owner who adds the user
     * @param log message
     * @return the profile with updated properties (id, userName, home, ...)
     * @throws IllegalArgumentException for invalid parameters
     */
    UserProfile add(UserProfile profile, List<Group> groups, String owner, String log);

    /**
     * Updates user profile.
     * @param profile to change password set password not digest
     */
    void update(UserProfile profile, String owner, String log);

    Group addGroup(Group group, List<Permission> permissions, String owner, String log);

    Group findGroup(int groupId);

    Group findRemoteGroup(String remoteName, String remoteType);

    List<Group> findGroups();

    List<Group> findUserGroups(int userId);

    void setUserGroups(UserProfile user, List<Group> groups, String owner, String log);

    void removePermissions(int groupId);

    void setPermissions(int groupId, Permission... permissions);

    Set<Permission> findUserPermissions(int userId);

    String findUserRole(int id);
}
