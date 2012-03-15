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

import cz.incad.pas.editor.server.config.PasConfiguration;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

/**
 *
 * @author Jan Pokorsky
 */
final class UserManagerMemoryImpl implements UserManager {
    
    public static final String IMPORT_FOLDER_NAME = "import";

    private static final Map<PasConfiguration, UserManagerMemoryImpl> INSTANCES = new HashMap<PasConfiguration, UserManagerMemoryImpl>();
    /** allows only lower case characters to prevent confusion */
    private static final Pattern USERNAME_PATTERN = Pattern.compile("[a-z][a-z0-9]*");
    /** memory storage for now */
    private final Map<String, UserProfile> map = new HashMap<String, UserProfile>();
    private final PasConfiguration pasConfig;

    static UserManager getInstance(PasConfiguration config) {
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

    private UserManagerMemoryImpl(PasConfiguration pasConfig) {
        this.pasConfig = pasConfig;
    }

    @Override
    public Collection<UserProfile> findAll() {
        return map.values();
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
                if (f.equals(new File(up.getUserHome()))) {
                    return up;
                }
            }
        }
        return null;
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
        if (userName == null || !USERNAME_PATTERN.matcher(userName).matches()) {
            throw new IllegalArgumentException("Invalid user name: " + userName);
        }
        if (displayName == null) {
            displayName = userName;
        }

        try {
            File userHome;
            File allUsersHome = pasConfig.getDefaultUsersHome();
            synchronized (map) {
                if (findImpl(userName) != null) {
                    throw new IllegalArgumentException("Invalid user name: " + userName);
                }
                if (userHomePath == null) {
                    String fileNameBase = userName.toLowerCase();
                    userHome = new File(allUsersHome, fileNameBase);
                    for (int index = 1; userHome.exists(); index++) {
                        userHome = new File(allUsersHome, fileNameBase + "_" + index);
                    }
                } else {
                    userHome = new File(userHomePath);
                }
                userHome = userHome.getCanonicalFile();
                if (userHome.exists() && !userHome.isDirectory()) {
                    throw new IOException(String.format("Not a folder: '%s'!", userHome));
                }
                if (userHome.exists() && findHome(userHome) != null) {
                    throw new IllegalArgumentException("user home already used by another user.");
                }
                File importHome = new File(userHome, IMPORT_FOLDER_NAME);
                File exportHome = new File(userHome, "export");
                File imagesHome = new File(userHome, "images");
                importHome.mkdirs();
                exportHome.mkdir();
                imagesHome.mkdir();
                UserProfile up = new UserProfile(map.size() + 1, toUri(userHome), userName, displayName);
                map.put(userName, up);
                return up;
            }
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Translates platform specific path to independent form as URI
     *
     * @param folderpath platform specific path as <pre>UNIX: /tmp/imports/</pre>
     *      or <pre>MS Win: c:\imports</pre> or <pre>UNC MS Win: \\laptop\My Documents\</pre>
     *      are valid options
     * @return an abstract path
     */
    private static URI toUri(File folder) {
        if (folder.exists() && !folder.isDirectory()) {
            throw new IllegalArgumentException("Invalid folder path: '" + folder + '\'');
        }
        // File.toURI always terminates folder path with slash but the folder must exists
        return folder.toURI().normalize();
    }

}
