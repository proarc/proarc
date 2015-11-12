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
package cz.cas.lib.proarc.common.workflow.profile;

/**
 * The source where is defined a value map.
 *
 * @author Jan Pokorsky
 */
public enum ValueMapSource {

    /**
     * The value map defined in workflow XML.
     */
    INTERNAL,
    /**
     * The value map provided by the system.
     */
    PROARC;

    public static ValueMapSource fromValue(String s) {
        if (s == null) {
            return INTERNAL;
        }
        for (ValueMapSource item : values()) {
            if (item.name().equals(s)) {
                return item;
            }
        }
        throw new IllegalArgumentException(s);
    }

}
