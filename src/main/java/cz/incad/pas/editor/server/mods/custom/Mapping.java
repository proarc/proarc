/*
 * Copyright (C) 2012 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.mods.custom;

import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.server.mods.custom.MonographMapper.Monograph;
import cz.incad.pas.editor.server.mods.custom.MonographUnitMapper.MonographUnit;
import cz.incad.pas.editor.server.mods.custom.PageMapper.Page;
import cz.incad.pas.editor.server.mods.custom.PeriodicalIssueMapper.PeriodicalIssue;
import cz.incad.pas.editor.server.mods.custom.PeriodicalMapper.Periodical;
import cz.incad.pas.editor.server.mods.custom.PeriodicalVolumeMapper.PeriodicalVolume;
import java.util.HashMap;
import java.util.Map;

/**
 * Use for mapping from/to custom and full MODS objects.
 * Later this should be pluggable.
 *
 * @author Jan Pokorsky
 */
public final class Mapping {

    private static final Map<String, MapperItem<?>> MAPPERS = new HashMap<String, MapperItem<?>>();
    
    static {
        addModel(MetaModelDataSource.EDITOR_PAGE, new PageMapper(), Page.class);
        addModel(MetaModelDataSource.EDITOR_PERIODICAL, new PeriodicalMapper(), Periodical.class);
        addModel(MetaModelDataSource.EDITOR_PERIODICAL_VOLUME, new PeriodicalVolumeMapper(), PeriodicalVolume.class);
        addModel(MetaModelDataSource.EDITOR_PERIODICAL_ISSUE, new PeriodicalIssueMapper(), PeriodicalIssue.class);
        addModel(MetaModelDataSource.EDITOR_MONOGRAPH, new MonographMapper(), Monograph.class);
        addModel(MetaModelDataSource.EDITOR_MONOGRAPH_UNIT, new MonographUnitMapper(), MonographUnit.class);
    }

    private static <T> void addModel(String mapperId, Mapper<T> mapper, Class<T> type) {
        MAPPERS.put(mapperId, new MapperItem<T>(mapperId, mapper, type));
    }

    public Object read(ModsType mods, String mapperId) {
        MapperItem<?> mi = MAPPERS.get(mapperId);
        if (mi != null) {
            return mi.getMapper().map(mods);
        }
        throw new IllegalArgumentException(mapperId);
    }

    public ModsType update(ModsType mods, Object javascript, String mapperId) {
        MapperItem<?> mi = MAPPERS.get(mapperId);
        if (mi != null) {
            return mi.map(mods, javascript);
        }
        throw new IllegalArgumentException(mapperId);
    }

    public Class<?> getType(String modelId) {
        MapperItem<?> model = MAPPERS.get(modelId);
        return model == null ? null : model.type;
    }

    private static final class MapperItem<T> {
        private final String mapperId;
        private final Mapper<T> mapper;
        private final Class<T> type;

        public MapperItem(String mapperId, Mapper<T> mapper, Class<T> type) {
            this.mapperId = mapperId;
            this.mapper = mapper;
            this.type = type;
        }

        public Mapper<T> getMapper() {
            return mapper;
        }

        public ModsType map(ModsType mods, Object value) {
            if (type.isInstance(value)) {
                return mapper.map(mods, (T) value);
            }
            throw new ClassCastException(String.format("%s does not support: %s", value));
        }

    }

    /**
     * Maps MODS to/from custom object that holds only subset of MODS properties.
     * The custom object must be serializable by JAXB.
     *
     * @param <T> custom object type
     */
    public interface Mapper<T> {
        /**
         * Implement this to map required properties from MODS to custom object
         * @param mods
         * @return
         */
        T map(ModsType mods);

        /**
         * Implement this to update passed {@code mods} with values from custom object.
         *
         * @param mods full MODS to update
         * @param value properties for update
         * @return modified MODS
         */
        ModsType map(ModsType mods, T value);
    }
}
