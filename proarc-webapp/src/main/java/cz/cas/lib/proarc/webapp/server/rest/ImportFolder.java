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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import java.util.List;
/**
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name = ImportResourceApi.IMPORT_FOLDER_ELEMENT)
@XmlAccessorType(XmlAccessType.FIELD)
public class ImportFolder {

    private transient String name;
    @XmlElement(name = ImportResourceApi.IMPORT_FOLDER_STATE)
    private String state;
    private transient String parent;
    @XmlElement(name = ImportResourceApi.IMPORT_FOLDER_PATH)
    private String path;
    @XmlElement(name = "states")
    private List<ProfileStates> states;


    public ImportFolder(String name, String state, String parent, String path, List<ProfileStates> states) {
        this.name = name;
        this.state = state;
        this.parent = parent;
        this.path = path;
        this.states = states;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getParent() {
        return parent;
    }

    public void setParent(String parent) {
        this.parent = parent;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public List<ProfileStates> getStates() {
        return states;
    }

    public void setStates(List<ProfileStates> states) {
        this.states = states;
    }

    @Override
    public String toString() {
        return String.format("ImportFolder{name=%s, state=%s, parent=%s, path=%s}",
                name, state, parent, path);
    }

}
