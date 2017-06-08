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
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.net.URI;
import java.security.MessageDigest;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
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
    static final Pattern USERNAME_PATTERN = Pattern.compile("[a-z][a-z0-9_-]{2,}");
    /** The pattern to replace forbidden chars in remote user names. */
    static final Pattern USERNAME_REPLACE_PATTERN = Pattern.compile("[^a-zA-Z0-9_-]+");
    private static final Pattern GROUP_PATTERN = Pattern.compile(
            "^" + FedoraGroupDao.PID_PREFIX + "[a-z][a-z0-9_-]{4,}$");

    private static UserManager MANGER = null;

    public static UserManager createUserManagerPostgressImpl(
            AppConfiguration config, DataSource source, DaoFactory daos) throws IOException {

        return new UserManagerSql(source,
                config.getDefaultUsersHome(),
                RemoteStorage.getInstance(config),
                daos);
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

    public static void validateUsername(String userName) {
        if (!isValidUserName(userName)) {
            throw new IllegalArgumentException("Invalid user name: " + userName);
        }
    }

    static boolean isValidUserName(String name) {
        return name != null && name.length() + FedoraUserDao.PID_PREFIX.length() <= 64
                && USERNAME_PATTERN.matcher(name).matches();
    }

    /** Replaces forbidden chars in remoteName and fixes length. */
    public static String toUserName(String remoteName) {
        return toUserName("", remoteName);
    }

    /** Replaces forbidden chars in remoteName and fixes length. */
    public static String toUserName(String remotePrefix, String remoteName) {
        if (remoteName == null || remoteName.isEmpty()) {
            throw new IllegalArgumentException(remoteName);
        }
        String name = remoteName;
        // fedora PID max length 64
        int maxLength = 64 - FedoraUserDao.PID_PREFIX.length() - 4;
        if (remoteName.length() > maxLength) {
            name = remoteName.substring(0, maxLength);
        }
        name = USERNAME_REPLACE_PATTERN.matcher(name).replaceAll("_");
        name = name.replaceFirst("^_+", ""); // remove heading '_'
        name = name.toLowerCase();
        if (name.length() == 0 || name.isEmpty()) {
            // replace unfriendly name with unsigned hash code
            name = 'u' + String.valueOf(remoteName.hashCode() & 0x00000000ffffffffL);
        }
        if (remotePrefix != null && !remotePrefix.isEmpty()) {
            name = remotePrefix + '_' + name;
        }
        return name;
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
        final String proarcName = "proarc";
        final String logMsg = "Init ProArc.";
        Group gadmin = mgr.findGroup(1);
        if (gadmin == null) {
            gadmin = Group.create("admin", "Administrátoři");
            gadmin = mgr.addGroup(gadmin, Arrays.asList(Permissions.ADMIN), proarcName, logMsg);
            assert gadmin.getId() == 1;
        }
        UserProfile admin = mgr.find(proarcName);
        if (admin == null) {
            admin = UserProfile.create(proarcName, "proarcAdmin", "Administrátor");
            mgr.add(admin, Arrays.asList(gadmin), admin.getUserName(), logMsg);
        }
        Set<Permission> adminPermissions = mgr.findUserPermissions(admin.getId());
        if (!adminPermissions.contains(Permissions.ADMIN)) {
            mgr.setPermissions(gadmin.getId(), Permissions.ADMIN);
        }
    }

    public static Collection<String> toGroupPid(Collection<Group> groups) {
        LinkedHashSet<String> result = new LinkedHashSet<String>(groups.size());
        for (Group group : groups) {
            result.add(toGroupPid(group));
        }
        return result;
    }

    public static String toGroupPid(Group group) {
        return group.getName();
    }

    public static String toGroupPid(String groupObjectId) {
        return FedoraGroupDao.PID_PREFIX + groupObjectId;
    }

    public static boolean isValidGroupPid(String pid) {
        return pid != null && pid.length() <= 64 && GROUP_PATTERN.matcher(pid).matches();
    }

    public static String toUserPid(UserProfile user) {
        return FedoraUserDao.PID_PREFIX + user.getUserName();
    }

}
