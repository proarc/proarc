/*
 * Copyright (C) 2015 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.object.oldprint;

import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
public class OldPrintMapperUtils {

    /**
     * Adds {@code mods/typeOfResource[@manuscript="yes"]="text"}
     * @param mods 
     */
    public static void addTypeOfResource(ModsDefinition mods) {
        List<TypeOfResourceDefinition> typeOfResources = mods.getTypeOfResource();
        TypeOfResourceDefinition reqTypeOfResource = null;
        for (TypeOfResourceDefinition typeOfResource : typeOfResources) {
            if ("text".equals(typeOfResource.getValue())) {
                reqTypeOfResource = typeOfResource;
                typeOfResource.setManuscript("yes");
                break;
            }
        }
        if (reqTypeOfResource == null) {
            TypeOfResourceDefinition type = new TypeOfResourceDefinition();
            type.setValue("text");
            type.setManuscript("yes");
            typeOfResources.add(0, type);
        }
    }
}
