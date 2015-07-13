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
package cz.cas.lib.proarc.common.object.emods;

import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;

/**
 * It is expected to handle MODS the same way like NDK.
 *
 * @author Jan Pokorsky
 */
public class BornDigitalModsMapperFactory extends NdkMapperFactory {

    @Override
    public NdkMapper get(String modelId) {
        if (BornDigitalModsPlugin.MODEL_ARTICLE.equals(modelId)) {
            return new BdmArticleMapper();
        } else {
            throw new IllegalStateException("Unsupported model: " + modelId);
        }
    }

}
