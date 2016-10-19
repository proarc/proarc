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
package cz.cas.lib.proarc.common.mods.custom;

import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;

/**
 *
 * @author Jan Pokorsky
 */
final class TypeOfResourceMapper {

    public ModsDefinition map(ModsDefinition mods, Type type) {
        return map(mods, type.xml);
    }
    
    public ModsDefinition map(ModsDefinition mods, String type) {
        TypeOfResourceDefinition typeOfResource = mods.getTypeOfResource().stream().findFirst().orElse(null);
        if (typeOfResource == null) {
            typeOfResource = new TypeOfResourceDefinition();
            typeOfResource.setValue(type);
            mods.getTypeOfResource().add(typeOfResource);
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
