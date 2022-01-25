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

import java.net.URI;
import java.sql.Timestamp;
import java.util.Date;

/**
 * User settings.
 *
 * User home should contain folders import (scanned TIFF files),
 * export (FOXMLs for publishing), images (JPEGs for external editing)
 *
 * @author Jan Pokorsky
 */
public class UserProfile {

    private Integer userId;
    /** holds folder path in platform independent form */
    private transient URI userHomeUri;
    private String userHome;
    private transient URI exportFolder;
    private transient URI importFolder;
    private String userName;
    private String remoteName;
    private String remoteType;
    private transient String userPassword;
    private transient String userPasswordDigest;
    private String surname;
    private String forename;
    private String organization;
    private String role;
    private String email;
    private Boolean changeModelFunction;
    private Boolean updateModelFunction;
    private Boolean lockObjectFunction;
    private Boolean unlockObjectFunction;
    private Date created;
    private Date lastLogin;
    private String status;
    private Integer defaultGroup;
    private Integer userGroup;
    private Timestamp timestamp;

    public static UserProfile create(String userName, String passwd, String surname) {
        UserProfile user = new UserProfile();
        user.setUserName(userName);
        user.setUserPassword(passwd);
        user.setSurname(surname);
        return user;
    }

    public static UserProfile createRemote(String remoteName, String remoteType, String surname) {
        UserProfile user = new UserProfile();
        user.setRemoteName(remoteName);
        user.setRemoteType(remoteType);
        user.setSurname(surname);
        return user;
    }

    public UserProfile() {
    }

    public Integer getId() {
        return userId;
    }

    public void setId(Integer id) {
        this.userId = id;
    }

    /**
     * user folder with import hierarchy
     * @return folder path always terminated with '/'.
     */
    public URI getImportFolder() {
        if (importFolder == null) {
            URI u = getUserHomeUri();
            if (u != null) {
                importFolder = URI.create(this.userHomeUri + UserUtil.IMPORT_FOLDER_NAME + '/');
            }
        }
        return importFolder;
    }

    public URI getExportFolder() {
        if (exportFolder == null) {
            URI u = getUserHomeUri();
            if (u != null) {
                exportFolder = URI.create(this.userHomeUri + UserUtil.EXPORT_FOLDER_NAME + '/');
            }
        }
        return exportFolder;
    }

    public URI getUserHomeUri() {
        if (userHomeUri == null && userHome != null) {
            userHomeUri = URI.create(userHome);
        }
        return userHomeUri;
    }

    public String getUserHome() {
        return userHome;
    }

    public void setUserHome(URI userHome) {
        this.userHomeUri = userHome;
        this.userHome = userHome.toASCIIString();
    }

    public void setUserHome(String userHome) {
        this.userHome = userHome;
    }

    public String getUserName() {
        return userName;
    }

    public String getUserNameAsPid() {
        return userName == null ? null : UserUtil.toUserPid(this);
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getUserPassword() {
        return userPassword;
    }

    public void setUserPassword(String userPassword) {
        this.userPassword = userPassword;
    }

    public String getUserPasswordDigest() {
        return userPasswordDigest;
    }

    public void setUserPasswordDigest(String userPasswordDigest) {
        this.userPasswordDigest = userPasswordDigest;
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

    public Date getCreated() {
        return created;
    }

    public void setCreated(Date created) {
        this.created = created;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getForename() {
        return forename;
    }

    public void setForename(String forename) {
        this.forename = forename;
    }

    public String getOrganization() {
        return organization;
    }

    public void setOrganization(String organization) {
        this.organization = organization;
    }

    public String getRole() {
        return role;
    }

    public void setRole(String role) {
        this.role = role;
    }

    public String getSurname() {
        return surname;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }

    public Date getLastLogin() {
        return lastLogin;
    }

    public void setLastLogin(Date lastLogin) {
        this.lastLogin = lastLogin;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Integer getDefaultGroup() {
        return defaultGroup;
    }

    public void setDefaultGroup(Integer defaultGroup) {
        this.defaultGroup = defaultGroup;
    }

    public Integer getUserGroup() {
        return userGroup;
    }

    public void setUserGroup(Integer userGroup) {
        this.userGroup = userGroup;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public Boolean getChangeModelFunction() {
        return changeModelFunction;
    }

    public void setChangeModelFunction(Boolean changeModelFunction) {
        this.changeModelFunction = changeModelFunction;
    }

    public Boolean getUpdateModelFunction() {
        return updateModelFunction;
    }

    public void setUpdateModelFunction(Boolean updateModelFunction) {
        this.updateModelFunction = updateModelFunction;
    }

    public Boolean getLockObjectFunction() {
        return lockObjectFunction;
    }

    public void setLockObjectFunction(Boolean lockObjectFunction) {
        this.lockObjectFunction = lockObjectFunction;
    }

    public Boolean getUnlockObjectFunction() {
        return unlockObjectFunction;
    }

    public void setUnlockObjectFunction(Boolean unlockObjectFunction) {
        this.unlockObjectFunction = unlockObjectFunction;
    }

    @Override
    public String toString() {
        return String.format("UserProfile[id:%s, username:%s, defGroup: %s, userGroup: %s,"
                + " created:%s, lastLogin:%s, email:%s,"
                + " remoteName:%s, remoteType:%s"
                + " forename:%s, surname:%s, userHome:%s, userHomeUri:%s, userPasswordDigest:%s]",
                userId, userName, defaultGroup, userGroup, created, lastLogin, email,
                remoteName, remoteType,
                forename, surname, userHome, userHomeUri, userPasswordDigest);
    }

    void validateAsNew () {
        if (userName == null || !UserUtil.isValidUserName(userName)) {
            throw new IllegalArgumentException("Invalid user name: " + userName);
        }

        if (remoteName == null) {
            if (userPassword == null || userPassword.length() < 6) {
                throw new IllegalArgumentException("Invalid password");
            }

            userPasswordDigest = UserUtil.getDigist(userPassword);
        }
    }

}
