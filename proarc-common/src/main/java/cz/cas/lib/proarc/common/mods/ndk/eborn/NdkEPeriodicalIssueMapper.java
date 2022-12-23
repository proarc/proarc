/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.common.mods.ndk.eborn;

import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.mods.ndk.MapperUtils;
import cz.cas.lib.proarc.common.mods.ndk.NdkPeriodicalIssueMapper;
import cz.cas.lib.proarc.mods.DigitalOriginDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addDigitalOrigin;

public class NdkEPeriodicalIssueMapper extends NdkPeriodicalIssueMapper {

    /**
     * Updates missing required attribute and elements.
     */
    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        PhysicalDescriptionDefinition reqPhysicalDescription = null;

        for (PhysicalDescriptionDefinition pd : mods.getPhysicalDescription()) {
            reqPhysicalDescription = pd;
        }
        if (reqPhysicalDescription == null) {
            reqPhysicalDescription = new PhysicalDescriptionDefinition();
            reqPhysicalDescription.getDigitalOrigin().add(DigitalOriginDefinition.BORN_DIGITAL);
            mods.getPhysicalDescription().add(reqPhysicalDescription);
        } else {
            mods.getPhysicalDescription().stream().map(PhysicalDescriptionDefinition::getDigitalOrigin).filter(origin -> origin.isEmpty()).forEach(origin -> origin.add(DigitalOriginDefinition.BORN_DIGITAL));
        }
    }

    @Override
    protected GenreDefinition addGenre(ModsDefinition mods) {
        //  mods/genre="electronic_issue"
        MapperUtils.removeGenre(mods, "electronic issue");
        return MapperUtils.addGenre(mods, Const.GENRE_EISSUE);
    }


    @Override
    protected OaiDcType createDc(ModsDefinition mods, Context ctx) {
        OaiDcType dc = super.createDc(mods, ctx);
        for (PhysicalDescriptionDefinition physicalDescription : mods.getPhysicalDescription()) {
            addDigitalOrigin(dc.getDescriptions(), physicalDescription.getDigitalOrigin());
        }
        return dc;
    }
}
