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

import java.io.File;
import java.net.URI;

/**
 * User settings.
 *
 * @author Jan Pokorsky
 */
public class UserProfile {

    private int id;
    /** holds folder path in platform independent form */
    private URI importFolder;
    private String userName;
    private String displayName;

    public UserProfile(int id, String importFolder, String userName, String displayName) {
        this.id = id;
        this.importFolder = toUri(importFolder);
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

    public void setImportFolder(String importFolder) {
        this.importFolder = toUri(importFolder);
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    @Override
    public String toString() {
        return String.format("UserProfile[%s, %s, %s, %s]", id, userName, displayName, importFolder);
    }

    /**
     * Translates platform specific path to independent form as URI
     *
     * @param folderpath platform specific path as <pre>UNIX: /tmp/imports/</pre>
     *      or <pre>MS Win: c:\imports</pre> or <pre>UNC MS Win: \\laptop\My Documents\</pre>
     *      are valid options
     * @return an abstract path
     */
    private static URI toUri(String folderpath) {
        File folder = new File(folderpath);
        if (!(folder.exists() && folder.isDirectory())) {
            throw new IllegalArgumentException("Invalid import folder path: '" + folderpath + '\'');
        }
        // File.toURI always terminates folder path with slash
        return folder.toURI().normalize();
    }

}
