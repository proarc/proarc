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
package cz.incad.pas.editor.server.mods.custom;

import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.TypeOfResourceType;

/**
 *
 * @author Jan Pokorsky
 */
final class TypeOfResourceMapper {

    public ModsType map(ModsType mods, Type type) {
        return map(mods, type.xml);
    }
    
    public ModsType map(ModsType mods, String type) {
        TypeOfResourceType typeOfResource = MapperUtils.findFirst(mods.getModsGroup(), TypeOfResourceType.class);
        if (typeOfResource == null) {
            typeOfResource = new TypeOfResourceType();
            typeOfResource.setValue(type);
            MapperUtils.add(mods, typeOfResource);
        }
        return mods;
    }

    /**
     * Resource types.
     * @see <a href='http://www.loc.gov/standards/mods/userguide/typeofresource.html'>MODS resource types</a>
     */
    public enum Type {

        TEXT("text"), CARTOGRAPHIC("cartographic");
        
        private String xml;

        private Type(String xml) {
            this.xml = xml;
        }

    }
}
