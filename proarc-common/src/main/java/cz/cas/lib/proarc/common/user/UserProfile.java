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
import java.net.URISyntaxException;
import java.util.Date;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * User settings.
 *
 * User home should contain folders import (scanned TIFF files),
 * export (FOXMLs for publishing), images (JPEGs for external editing)
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public final class UserProfile {

    private Integer userId;
    /** holds folder path in platform independent form */
    private transient URI userHomeUri;
    private String userHome;
    private transient URI exportFolder;
    private transient URI importFolder;
    private String userName;
    private transient String userPassword;
    private transient String userPasswordDigest;
    private transient String displayName; // XXX remove
    private String surname;
    private String forename;
    private String email;
    private Date created;
    private Date lastLogin;

    private boolean proarcuser=true;
    
    public UserProfile() {
    }

    UserProfile(Integer id, URI userHomeUri, String userName, String displayName) throws URISyntaxException {
        this.userId = id;
        this.userHomeUri = userHomeUri;
        this.userHome = userHomeUri.toASCIIString();
        this.userName = userName;
        this.displayName = displayName;

    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
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

    
    
    public boolean isProarcuser() {
		return proarcuser;
	}

	public void setProarcuser(boolean proarcuser) {
		this.proarcuser = proarcuser;
	}

	@Override
    public String toString() {
        return String.format("UserProfile[id:%s, username:%s, created:%s, lastLogin:%s, email:%s,"
                + " forename:%s, surname:%s, userHome:%s, userHomeUri:%s, userPasswordDigest:%s]",
                userId, userName, created, lastLogin, email, forename, surname, userHome, userHomeUri, userPasswordDigest);
    }

    void validateAsNew () {
    	// validation only for proarc users
    	if (this.proarcuser) {
        	if (userName == null || !UserUtil.USERNAME_PATTERN.matcher(userName).matches()) {
                throw new IllegalArgumentException("Invalid user name: " + userName);
            }

            if (userPassword == null || userPassword.length() < 6) {
                throw new IllegalArgumentException("Invalid password");
            }

            userPasswordDigest = UserUtil.getDigist(userPassword);
    	}
    }

}
