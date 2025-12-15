/*
 * Copyright (C) 2022 Lukas Sykora
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
package cz.cas.lib.proarc.common.object.k4;

import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;
import cz.cas.lib.proarc.common.object.K4Plugin;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * It is expected to handle MODS of k4 the same way like NDK.
 *
 * @author Lukas Sykora
 */
@Deprecated
public class K4MapperFactory extends NdkMapperFactory {

    private static final Map<String, Supplier<NdkMapper>> mappers = new HashMap<>();

    static {
        mappers.put(K4Plugin.MODEL_PERIODICAL, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_PERIODICALVOLUME, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_PERIODICALITEM, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_MONOGRAPH, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_MONOGRAPHUNIT, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_ARTICLE, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_MAP, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_SUPPLEMENT, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_PICTURE, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_SHEETMUSIC, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_INTERNALPART, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_CONVOLUTE, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_GRAPHIC, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_SOUNDRECORDING, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_SOUNDUNIT, K4Mapper::new);
        mappers.put(K4Plugin.MODEL_TRACK, K4Mapper::new);
    }

    @Override
    public NdkMapper get(String modelId) {
        Optional<Supplier<NdkMapper>> ndkMapper = Optional.ofNullable(mappers.get(modelId));
        return ndkMapper.map(s -> s.get()).orElseThrow(() -> new IllegalStateException("Unsupported model: " + modelId));
    }
}
