/*
 * Copyright (C) 2023 Lukas Sykora
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

package cz.cas.lib.proarc.common.mods.ndk.eborn;

import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.mods.ndk.MapperUtils;
import cz.cas.lib.proarc.common.mods.ndk.NdkPeriodicalSupplementMapper;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.mods.DigitalOriginDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addDigitalOrigin;

public class NdkEPeriodicalSupplementMapper extends NdkPeriodicalSupplementMapper {

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
    protected void fixAndAddGenre(ModsDefinition mods, Context ctx) {
        MapperUtils.removeGenre(mods, Const.GENRE_SUPPLEMENT);
        if (mods.getGenre().size() == 0) {
            //  mods/genre="supplement"
            MapperUtils.addGenre(mods, Const.GENRE_ESUPPLEMENT);
        }
        for (GenreDefinition genre : mods.getGenre()) {
            String type = null;
            if (genre.getValue() == null || "".equals(genre.getValue())) {
                genre.setValue(Const.GENRE_ESUPPLEMENT);
            } else if (genre.getValue() != null && Const.GENRE_ESUPPLEMENT.equals(genre.getValue()) && genre.getType() == null) {
                if (NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(ctx.getParentModel())) {
                    genre.setType("electronic_issue_supplement");
                } else if (NdkEbornPlugin.MODEL_EPERIODICALVOLUME.equals(ctx.getParentModel())) {
                    genre.setType("electronic_volume_supplement");
                }
            } else if (genre.getValue() != null && !Const.GENRE_ESUPPLEMENT.equals(genre.getValue())) {
                if ("electronic_volume_supplement".equals(genre.getValue()) || "electronic_issue_supplement".equals(genre.getValue())) {
                    type = genre.getValue();
                    genre.setValue(Const.GENRE_ESUPPLEMENT);
                    if (genre.getType() == null || genre.getType().isEmpty()) {
                        genre.setType(type);
                    }
                }
            }
        }
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
