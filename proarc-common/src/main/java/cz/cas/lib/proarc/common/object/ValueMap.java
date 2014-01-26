/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.object;

import java.util.List;

/**
 * Describes a value map used in client forms.
 *
 * @author Jan Pokorsky
 */
public class ValueMap<T> {

    private String mapId;

    private List<T> values;

    public ValueMap(String mapId, List<T> values) {
        this.mapId = mapId;
        this.values = values;
    }

    public ValueMap() {
    }

    /**
     * Gets the unique map ID.
     */
    public String getMapId() {
        return mapId;
    }

    public void setMapId(String mapId) {
        this.mapId = mapId;
    }

    /**
     * Gets list of POJOs. Their fields are used as key/value.
     */
    public List<T> getValues() {
        return values;
    }

    public void setValues(List<T> values) {
        this.values = values;
    }

}
