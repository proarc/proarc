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

import cz.cas.lib.proarc.common.mods.ndk.NdkMonographVolumeMapper;
import cz.cas.lib.proarc.mods.ModsDefinition;

/**
 *
 * @author Jan Pokorsky
 */
public class OldPrintVolumeMapper extends NdkMonographVolumeMapper {

    @Override
    public void createMods(ModsDefinition mods, Context ctx) {
        setAddTextResource(false);
        super.createMods(mods, ctx);
        //  mods/typeOfResource="text"
//        OldPrintMapperUtils.addTypeOfResource(mods);
    }

    @Override
    protected String createObjectLabel(ModsDefinition mods) {
        return OldPrintMapperFactory.createObjectLabel(mods);
    }

}
