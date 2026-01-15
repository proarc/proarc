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

import cz.cas.lib.proarc.mods.Extent;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.mods.TypeOfResourceDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addElementType;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addGenre;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addName;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNameIdentifier;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addNonSort;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addPartNumber;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addStringPlusLanguage;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addTitle;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.fillLanguage;

/**
 *
 * @author Lukas Sykora
 */
public class NdkSoundPartMapper extends RdaNdkMapper {

    /**
     * Updates missing required attribute and elements.
     */
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);

        for (TypeOfResourceDefinition typeOfResource : mods.getTypeOfResource()) {
            typeOfResource.setValue("sound recording");
        }

        addGenre(mods, "sound part");
        for (GenreDefinition genre : mods.getGenre()) {
            if ("sound part".equals(genre.getValue())) {
                genre.setTypeString("model");
            }
        }

        fillLanguage(mods);
    }

    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);

        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            StringBuilder title = new StringBuilder();
            addNonSort(title, titleInfo);
            addTitle(title, titleInfo);
            addPartNumber(title, titleInfo);
            addElementType(dc.getTitles(), title.toString());
        }

        addName(mods.getName(), dc.getCreators());
        addNameIdentifier(mods.getName(), dc.getCreators());

        for (TypeOfResourceDefinition typeOfResource : mods.getTypeOfResource()) {
            addElementType(dc.getTypes(), typeOfResource.getValue());
        }

        for (PhysicalDescriptionDefinition physicalDescription : mods.getPhysicalDescription()) {
            for (Extent extent : physicalDescription.getExtent()) {
                addElementType(dc.getCoverages(), extent.getValue());
            }
        }

        addStringPlusLanguage(dc.getDescriptions(), mods.getNote());
        return dc;
    }
}
