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
 * <p>
 * User home should contain folders import (scanned TIFF files),
 * export (FOXMLs for publishing), images (JPEGs for external editing)
 *
 * @author Jan Pokorsky
 */
public class UserProfile {

    private Integer userId;
    /**
     * holds folder path in platform independent form
     */
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
    private Boolean importToProdFunction;
    private Boolean czidloFunction;
    private Boolean wfDeleteJobFunction;
    private Boolean importToCatalogFunction;
    private Boolean changeObjectsOwnerFunction;
    private Boolean deviceFunction;
    private Boolean changePagesFunction;
    private Boolean wfCreateJobFunction;
    private Boolean createUserFunction;
    private Boolean updateUserFunction;
    private Boolean deleteUserFunction;
    private Boolean solrFunction;
    private Boolean deleteActionFunction;
    private Boolean allObjectsFunction; // vidi vsechny objekty v obrazovce hledat (bez ohledu na organizaci)
    private Boolean prepareBatchFunction;
    private Boolean sysAdminFunction;
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
     *
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

    public Boolean hasChangeModelFunction() {
        return Boolean.TRUE.equals(changeModelFunction);
    }

    public void setChangeModelFunction(Boolean changeModelFunction) {
        this.changeModelFunction = changeModelFunction;
    }

    public Boolean getUpdateModelFunction() {
        return updateModelFunction;
    }

    public Boolean hasUpdateModelFunction() {
        return Boolean.TRUE.equals(updateModelFunction);
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

    public Boolean hasLockObjectFunction() {
        return Boolean.TRUE.equals(lockObjectFunction);
    }

    public Boolean getUnlockObjectFunction() {
        return unlockObjectFunction;
    }

    public void setUnlockObjectFunction(Boolean unlockObjectFunction) {
        this.unlockObjectFunction = unlockObjectFunction;
    }

    public Boolean hasUnlockObjectFunction() {
        return Boolean.TRUE.equals(unlockObjectFunction);
    }

    public Boolean getImportToProdFunction() {
        return importToProdFunction;
    }

    public Boolean hasPermissionToImportToProdFunction() {
        return Boolean.TRUE.equals(importToProdFunction);
    }

    public void setImportToProdFunction(Boolean importToProdFunction) {
        this.importToProdFunction = importToProdFunction;
    }

    public Boolean getCzidloFunction() {
        return czidloFunction;
    }

    public Boolean hasCzidloFunction() {
        return Boolean.TRUE.equals(czidloFunction);
    }

    public void setCzidloFunction(Boolean czidloFunction) {
        this.czidloFunction = czidloFunction;
    }

    public Boolean getWfDeleteJobFunction() {
        return wfDeleteJobFunction;
    }

    public void setWfDeleteJobFunction(Boolean wfDeleteJobFunction) {
        this.wfDeleteJobFunction = wfDeleteJobFunction;
    }

    public Boolean hasWfDeleteJobFunction() {
        return Boolean.TRUE.equals(wfDeleteJobFunction);
    }

    public Boolean getImportToCatalogFunction() {
        return importToCatalogFunction;
    }

    public Boolean hasImportToCatalogFunction() {
        return Boolean.TRUE.equals(importToCatalogFunction);
    }

    public void setImportToCatalogFunction(Boolean importToCatalogFunction) {
        this.importToCatalogFunction = importToCatalogFunction;
    }

    public Boolean getChangeObjectsOwnerFunction() {
        return changeObjectsOwnerFunction;
    }

    public Boolean hasChangeObjectsOwnerFunction() {
        return Boolean.TRUE.equals(changeObjectsOwnerFunction);
    }

    public void setChangeObjectsOwnerFunction(Boolean changeObjectsOwnerFunction) {
        this.changeObjectsOwnerFunction = changeObjectsOwnerFunction;
    }

    public Boolean getChangePagesFunction() {
        return changePagesFunction;
    }

    public Boolean hasChangePagesFunction() {
        return Boolean.TRUE.equals(changePagesFunction);
    }

    public void setChangePagesFunction(Boolean changePagesFunction) {
        this.changePagesFunction = changePagesFunction;
    }

    public Boolean getDeviceFunction() {
        return deviceFunction;
    }

    public Boolean hasDeviceFunction() {
        return Boolean.TRUE.equals(deviceFunction);
    }

    public void setDeviceFunction(Boolean deviceFunction) {
        this.deviceFunction = deviceFunction;
    }

    public Boolean getWfCreateJobFunction() {
        return wfCreateJobFunction;
    }

    public Boolean hasWfCreateJobFunction() {
        return Boolean.TRUE.equals(wfCreateJobFunction);
    }

    public void setWfCreateJobFunction(Boolean wfCreateJobFunction) {
        this.wfCreateJobFunction = wfCreateJobFunction;
    }

    public Boolean getCreateUserFunction() {
        return createUserFunction;
    }

    public Boolean hasCreateUserFunction() {
        return Boolean.TRUE.equals(createUserFunction);
    }

    public void setCreateUserFunction(Boolean createUserFunction) {
        this.createUserFunction = createUserFunction;
    }

    public Boolean getUpdateUserFunction() {
        return updateUserFunction;
    }

    public Boolean hasUpdateUserFunction() {
        return Boolean.TRUE.equals(updateUserFunction);
    }

    public void setUpdateUserFunction(Boolean updateUserFunction) {
        this.updateUserFunction = updateUserFunction;
    }

    public Boolean getDeleteUserFunction() {
        return deleteUserFunction;
    }

    public Boolean hasDeleteUserFunction() {
        return Boolean.TRUE.equals(deleteUserFunction);
    }

    public void setDeleteUserFunction(Boolean deleteUserFunction) {
        this.deleteUserFunction = deleteUserFunction;
    }

    public Boolean getSolrFunction() {
        return solrFunction;
    }

    public Boolean hasSolrFunction() {
        return Boolean.TRUE.equals(solrFunction);
    }

    public void setSolrFunction(Boolean solrFunction) {
        this.solrFunction = solrFunction;
    }

    public Boolean getDeleteActionFunction() {
        return deleteActionFunction;
    }

    public Boolean hasDeleteActionFunction() {
        return Boolean.TRUE.equals(deleteActionFunction);
    }

    public void setDeleteActionFunction(Boolean deleteActionFunction) {
        this.deleteActionFunction = deleteActionFunction;
    }

    public Boolean getAllObjectsFunction() {
        return allObjectsFunction;
    }

    public Boolean hasAllObjectsFunction() {
        return Boolean.TRUE.equals(allObjectsFunction);
    }

    public void setAllObjectsFunction(Boolean allObjectsFunction) {
        this.allObjectsFunction = allObjectsFunction;
    }

    public Boolean getPrepareBatchFunction() {
        return prepareBatchFunction;
    }

    public Boolean hasPrepareBatchFunction() {
        return Boolean.TRUE.equals(prepareBatchFunction);
    }

    public void setPrepareBatchFunction(Boolean prepareBatchFunction) {
        this.prepareBatchFunction = prepareBatchFunction;
    }

    public Boolean getSysAdminFunction() {
        return sysAdminFunction;
    }

    public Boolean hasSysAdminFunction() {
        return Boolean.TRUE.equals(sysAdminFunction);
    }

    public void setSysAdminFunction(Boolean sysAdminFunction) {
        this.sysAdminFunction = sysAdminFunction;
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

    void validateAsNew() {
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
