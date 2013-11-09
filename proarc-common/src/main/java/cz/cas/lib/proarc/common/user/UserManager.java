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
 *
 * @author Jan Pokorsky
 */
public interface UserManager {

    // XXX change before release!!!
    public static final String GUEST_ID = "admin";

    UserProfile find(String userName) throws IllegalArgumentException;
    UserProfile find(int userId) throws IllegalArgumentException;

    List<UserProfile> findAll();

    /**
     * Adds a new user.
     *
     * @param profile required properties:
     * <br><b>username</b> cannot be {@code null} or empty. Valid content is {@code "[a-z][a-z0-9]*"}.
     *      The name is used as user home folder when {@code userHomePath} is {@code null}
     * <br><b>userHomePath</b> - platform specific absolute path or {@code null}. The path cannot be used by other profile.
     *      <pre>UNIX: /tmp/imports/</pre> or <pre>MS Win: c:\imports</pre>
     *      or <pre>UNC MS Win: \\laptop\My Documents\</pre> are valid options.
     * <br><b>password</b> - length must be >= 6
     * @return new profile
     * @throws IllegalArgumentException for invalid parameters
     */
    UserProfile add(UserProfile profile);

    /**
     * Updates user profile.
     * @param profile to change password set password not digest
     */
    void update(UserProfile profile);

    Group addGroup(Group group);

    Group findGroup(int groupId);

    List<Group> findGroups();

    List<Group> findUserGroups(int userId);

    void removePermissions(int groupId);

    void setPermissions(int groupId, Permission... permissions);

    Set<Permission> findUserPermissions(int userId);

    void setUserGroups(int userId, Group... groups);

}
