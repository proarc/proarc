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
package cz.incad.pas.editor.server.user;

import cz.incad.pas.editor.server.config.AppConfiguration;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author Jan Pokorsky
 */
final class UserManagerMemoryImpl implements UserManager {
    
    private static final Map<AppConfiguration, UserManagerMemoryImpl> INSTANCES = new HashMap<AppConfiguration, UserManagerMemoryImpl>();
    /** memory storage for now */
    private final Map<String, UserProfile> map = new HashMap<String, UserProfile>();
    private final AppConfiguration appConfig;

    static UserManager getInstance(AppConfiguration config) {
        UserManagerMemoryImpl mngr;
        synchronized (INSTANCES) {
            mngr = INSTANCES.get(config);
            if (mngr == null) {
                mngr = new UserManagerMemoryImpl(config);
            } else {
                return mngr;
            }
        }
        try {
            File usersHome = config.getDefaultUsersHome();
            File adminHome = new File(usersHome, "admin");
            mngr.add("admin", "Administrator", adminHome.getPath());
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
        return mngr;
    }

    private UserManagerMemoryImpl(AppConfiguration appConfig) {
        this.appConfig = appConfig;
    }

    @Override
    public List<UserProfile> findAll() {
        synchronized (map) {
            return new ArrayList<UserProfile>(map.values());
        }
    }

    @Override
    public UserProfile find(String userName) throws IllegalArgumentException {
        UserProfile up = findImpl(userName);
        if (up == null) {
            throw new IllegalArgumentException("User not found.");
        }
        return up;
    }

    private UserProfile findImpl(String userName) {
        UserProfile up = null;
        if (userName != null) {
            synchronized (map) {
                up = map.get(userName);
            }
        }
        return up;
    }

    @Override
    public UserProfile find(int userId) throws IllegalArgumentException {
        synchronized (map) {
            for (UserProfile up : map.values()) {
                if (up.getId() == userId) {
                    return up;
                }
            }
        }
        throw new IllegalArgumentException("User not found.");
    }

    private UserProfile findHome(File f) {
        synchronized (map) {
            for (UserProfile up : map.values()) {
                if (f.equals(new File(up.getUserHomeUri()))) {
                    return up;
                }
            }
        }
        return null;
    }

    @Override
    public UserProfile add(UserProfile profile) {
        return add(profile.getUserName(), profile.getSurname(), profile.getUserHome());
    }

    /**
     * Adds a new user.
     *
     * @param userName username cannot be {@code null} or empty. Valid content is {@code "[a-z][a-z0-9]*"}.
     *      The name is used as user home folder when {@code userHomePath} is {@code null}
     * @param displayName profile's descriptor. If {@code null} {@code userName} is used.
     * @param userHomePath platform specific absolute path or {@code null}. The path cannot be used by other profile.
     *      <pre>UNIX: /tmp/imports/</pre> or <pre>MS Win: c:\imports</pre>
     *      or <pre>UNC MS Win: \\laptop\My Documents\</pre> are valid options.
     * @return new profile
     * @throws IllegalArgumentException for invalid parameters
     */
    public UserProfile add(String userName, String displayName, String userHomePath) {
        UserUtil.validateUsername(userName);
        if (displayName == null) {
            displayName = userName;
        }

        File toRollback = null;
        try {
            File allUsersHome = appConfig.getDefaultUsersHome();
            synchronized (map) {
                if (findImpl(userName) != null) {
                    throw new IllegalArgumentException("Invalid user name: " + userName);
                }
                File userHome;
                if (userHomePath == null) {
                    toRollback = userHome = UserUtil.createUserHome(userName, userHomePath, allUsersHome);
                } else {
                    toRollback = userHome = new File(userHomePath);
                    if (!userHome.mkdir()) { // already exists
                        toRollback = null;
                        if (!userHome.isDirectory()) {
                            throw new IOException(String.format("Not a folder: '%s'!", userHome));
                        }
                        if (findHome(userHome) != null) {
                            throw new IllegalArgumentException("user home already used by another user.");
                        }
                    }
                }

                UserUtil.createUserSubfolders(userHome);
                UserProfile up = new UserProfile(map.size() + 1, UserUtil.toUri(userHome), userName, displayName);
                map.put(userName, up);
                return up;
            }
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        } finally {
            if (toRollback != null) {
                toRollback.delete();
            }
        }
    }

    @Override
    public void update(UserProfile profile) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Group addGroup(Group group) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Group findGroup(int groupId) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public List<Group> findGroups() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public List<Group> findUserGroups(int userId) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void removePermissions(int groupId) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void setPermissions(int groupId, Permission... permissions) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Set<Permission> findUserPermissions(int userId) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void setUserGroups(int userId, Group... groups) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
