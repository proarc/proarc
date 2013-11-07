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

import cz.cas.lib.proarc.common.config.AppConfiguration;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.net.URI;
import java.security.MessageDigest;
import java.util.Set;
import java.util.regex.Pattern;
import javax.sql.DataSource;

/**
 *
 * @author Jan Pokorsky
 */
public final class UserUtil {

    static final String IMPORT_FOLDER_NAME = "import";
    static final String EXPORT_FOLDER_NAME = "export";
    static final String EDIT_FOLDER_NAME = "images";
    /** allows only lower case characters to prevent confusion */
    static final Pattern USERNAME_PATTERN = Pattern.compile("[a-z][a-z0-9]{4,}");

    private static UserManager MANGER = null;

    public static UserManager createUserManagerMemoryImpl(AppConfiguration config) {
        return UserManagerMemoryImpl.getInstance(config);
    }

    public static UserManager createUserManagerPostgressImpl(AppConfiguration config, DataSource source) throws IOException {
        return new UserManagerSql(source, config.getDefaultUsersHome());
    }

    public static UserManager getDefaultManger() {
        return MANGER;
    }

    public static void setDefaultManger(UserManager manger) {
        UserUtil.MANGER = manger;
    }

    /**
     * creates new user folder
     * @param userName
     * @param userHomePath
     * @param allUsersHome
     * @return
     * @throws IOException
     */
    public static File createUserHome(String userName, String userHomePath, File allUsersHome) throws IOException {
        File userHome;
        if (userHomePath == null) {
            String fileNameBase = userName.toLowerCase();
            userHome = new File(allUsersHome, fileNameBase);
            for (int index = 1; !userHome.mkdir(); index++) {
                userHome = new File(allUsersHome, fileNameBase + "_" + index);
            }
        } else {
            userHome = new File(userHomePath);
            if (userHome.exists() && !userHome.isDirectory()) {
                throw new IOException(String.format("Not a folder: '%s'!", userHome));
            }
        }
        userHome = userHome.getCanonicalFile();
        return userHome;
    }

    public static void createUserSubfolders(File userHome) {
        File importHome = new File(userHome, IMPORT_FOLDER_NAME);
        File exportHome = new File(userHome, EXPORT_FOLDER_NAME);
        File imagesHome = new File(userHome, EDIT_FOLDER_NAME);
        importHome.mkdirs();
        exportHome.mkdir();
        imagesHome.mkdir();
    }

    /**
     * Translates platform specific path to independent form as URI
     *
     * @param folderpath platform specific path as <pre>UNIX: /tmp/imports/</pre>
     *      or <pre>MS Win: c:\imports</pre> or <pre>UNC MS Win: \\laptop\My Documents\</pre>
     *      are valid options
     * @return an abstract path
     */
    static URI toUri(File folder) {
        if (folder.exists() && !folder.isDirectory()) {
            throw new IllegalArgumentException("Invalid folder path: '" + folder + '\'');
        }
        // File.toURI always terminates folder path with slash but the folder must exists
        return folder.toURI().normalize();
    }

    static void validateUsername(String userName) {
        if (userName == null || !USERNAME_PATTERN.matcher(userName).matches()) {
            throw new IllegalArgumentException("Invalid user name: " + userName);
        }
    }

    static String getDigist(String s) {
        try {
            MessageDigest instance = MessageDigest.getInstance("SHA");
            byte[] digest = instance.digest(s.getBytes("UTF-8"));
            String toHex = toHex(digest);
            return toHex;
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
    }

    public static String toHex(byte[] bytes) {
        BigInteger bi = new BigInteger(1, bytes);
        return String.format("%0" + (bytes.length << 1) + "x", bi);
    }

    public static void initDefaultAdmin() {
        UserManager mgr = getDefaultManger();
        UserProfile admin = mgr.find("proarc");
        if (admin == null) {
            admin = new UserProfile();
            admin.setSurname("Administrátor");
            admin.setUserName("proarc");
            admin.setUserPassword("proarcAdmin");
            mgr.add(admin);
        }
        Group gadmin = mgr.findGroup(1);
        if (gadmin == null) {
            gadmin = new Group();
            gadmin.setName("Administrátoři");
            gadmin = mgr.addGroup(gadmin);
            assert gadmin.getId() == 1;
            mgr.setUserGroups(admin.getId(), gadmin);
        }
        Set<Permission> adminPermissions = mgr.findUserPermissions(admin.getId());
        if (!adminPermissions.contains(Permissions.ADMIN)) {
            mgr.setPermissions(gadmin.getId(), Permissions.ADMIN);
        }
    }

}
