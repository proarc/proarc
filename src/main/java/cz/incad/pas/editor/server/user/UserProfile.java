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

import java.net.URI;
import java.net.URISyntaxException;

/**
 * User settings.
 *
 * User home should contain folders import (scanned TIFF files),
 * export (FOXMLs for publishing), images (JPEGs for external editing)
 *
 * @author Jan Pokorsky
 */
public class UserProfile {

    private int id;
    /** holds folder path in platform independent form */
    private URI userHome;
    private URI importFolder;
    private String userName;
    private String displayName;

    public UserProfile(int id, URI userHome, String userName, String displayName) throws URISyntaxException {
        this.id = id;
        this.userHome = userHome;
        this.importFolder = new URI(this.userHome + "import/");
        this.userName = userName;
        this.displayName = displayName;

    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    /**
     * user folder with import hierarchy
     * @return folder path always terminated with '/'.
     */
    public URI getImportFolder() {
        return importFolder;
    }

    public URI getUserHome() {
        return userHome;
    }

    public void setUserHome(URI userHome) {
        this.userHome = userHome;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    @Override
    public String toString() {
        return String.format("UserProfile[%s, %s, %s, %s]", id, userName, displayName, userHome);
    }

}
