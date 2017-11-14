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

import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;

/**
 * It is expected to handle MODS of old prints the same way like NDK.
 *
 * @author Jan Pokorsky
 */
public class OldPrintMapperFactory extends NdkMapperFactory {

    @Override
    public NdkMapper get(String modelId) {
        if (OldPrintPlugin.MODEL_PAGE.equals(modelId)) {
            return new OldPrintPageMapper();
        } else if (OldPrintPlugin.MODEL_VOLUME.equals(modelId)) {
            return new OldPrintVolumeMapper();
        } else if (OldPrintPlugin.MODEL_SUPPLEMENT.equals(modelId)) {
            return new OldPrintSupplementMapper();
        } else if (OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(modelId)) {
            return new OldPrintMonographTitleMapper();
        } else {
            throw new IllegalStateException("Unsupported model: " + modelId);
        }
    }

}
