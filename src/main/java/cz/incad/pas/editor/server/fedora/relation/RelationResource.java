/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.incad.pas.editor.server.fedora.relation;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlValue;

/**
 * Holds Fedora Object URI ({@code info:fedora/demo:1}).
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public final class RelationResource {
    
    @XmlValue
    private String resource;

    private RelationResource() {
    }

    public RelationResource(String resource) {
        this.resource = resource;
    }

    public static RelationResource fromPid(String pid) {
        return new RelationResource("info:fedora/" + pid);
    }

    public static String toPid(String resource) {
        return resource.substring("info:fedora/".length());
    }

    public String getResource() {
        return resource;
    }

    public String getPid() {
        return toPid(resource);
    }

}
