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
import cz.cas.lib.proarc.common.mods.ndk.NdkMonographSupplementMapper;
import cz.cas.lib.proarc.mods.DigitalOriginDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.addDigitalOrigin;

public class NdkEMonographSupplementMapper extends NdkMonographSupplementMapper {
    private boolean addTextResource = true;

    public boolean isAddTextResource() {
        return addTextResource;
    }

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        super.createMods(mods, ctx);
        mods.getPhysicalDescription().stream().map(PhysicalDescriptionDefinition::getDigitalOrigin).filter(origin -> origin.isEmpty()).forEach(origin -> origin.add(DigitalOriginDefinition.BORN_DIGITAL));
    }

    @Override
    protected void addGenre(ModsDefinition mods) {
        //  mods/genre="electronic_volume"
        MapperUtils.addGenre(mods, Const.GENRE_ESUPPLEMENT);
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
