/*
 * Copyright (C) 2017 Lukas Sykora
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
package cz.cas.lib.proarc.common.mods.ndk;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.*;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.util.List;

/**
 *
 * @author Lukas Sykora
 */
public class NdkSongMapper extends NdkMapper {

    /**
     * Updates missing required attribute and elements.
     */
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);

        //  mods/typeOfResource="text" or fill with MARC21 06; see NDK
        List<TypeOfResourceDefinition> typeOfResources = mods.getTypeOfResource();
        TypeOfResourceDefinition reqTypeOfResource = null;
        for (TypeOfResourceDefinition typeOfResource : typeOfResources) {
            if ("text".equals(typeOfResource.getValue())) {
                reqTypeOfResource = typeOfResource;
                break;
            }
        }
        if (reqTypeOfResource == null) {
            TypeOfResourceDefinition type = new TypeOfResourceDefinition();
            type.setValue("text");
            typeOfResources.add(0, type);
        }
        // mods/language/languageTerm @type=code, @authority="iso639‐2b"
        fillLanguage(mods);

        fillRecordInfo(mods);
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        return dc;
    }
}
