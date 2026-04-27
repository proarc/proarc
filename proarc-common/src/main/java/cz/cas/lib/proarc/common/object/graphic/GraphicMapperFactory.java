/*
 * Copyright (C) 2021 Lukas Sykora
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
package cz.cas.lib.proarc.common.object.graphic;

import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.createTitleString;
import static cz.cas.lib.proarc.common.mods.ndk.MapperUtils.toValue;

/**
 * Creates Graphics mappers. Subclasses can implement own mappings.
 *
 * @author Lukas Sykora
 */
public class GraphicMapperFactory extends NdkMapperFactory {

    private static final Map<String, Supplier<NdkMapper>> mappers = new HashMap<>();

    static {
        mappers.put(GraphicPlugin.MODEL_GRAPHIC, GraphicMapper::new);
        mappers.put(GraphicPlugin.MODEL_PAGE, NdkPageMapper::new);
        mappers.put(ModsCutomEditorType.EDITOR_PAGE, NdkPageMapper::new);
    }

    /**
     * Gets a NDK mapper for the given model ID.
     *
     * @param modelId model ID
     * @return the mapper
     */
    @Override
    public NdkMapper get(String modelId) {
        Optional<Supplier<NdkMapper>> ndkMapper = Optional.ofNullable(mappers.get(modelId));
        return ndkMapper.map(s -> s.get()).orElseThrow(() -> new IllegalStateException("Unsupported model: " + modelId));
    }

    public static String createObjectLabel(ModsDefinition mods) {
        for (TitleInfoDefinition ti : mods.getTitleInfo()) {
            if (toValue(ti.getType()) != null) {
                continue;
            }
            return createTitleString(ti);
        }
        for (TitleInfoDefinition ti : mods.getTitleInfo()) {
            if ("abbreviated".equalsIgnoreCase(toValue(ti.getType()))) {
                return createTitleString(ti);
            }
        }
        for (TitleInfoDefinition ti : mods.getTitleInfo()) {
            return createTitleString(ti);
        }
        return null;
    }
}
