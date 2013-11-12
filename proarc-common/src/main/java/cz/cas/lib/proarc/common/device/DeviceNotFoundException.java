/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.device;

/**
 * A device cannot be found.
 *
 * @author Jan Pokorsky
 */
public class DeviceNotFoundException extends DeviceException {

    private static final long serialVersionUID = 1L;
    private final String[] ids;

    public DeviceNotFoundException(String message, Throwable cause, String... id) {
        super(message, cause);
        this.ids = id;
    }

    public DeviceNotFoundException(String message, String... id) {
        this(message, null, id);
    }

    public String[] getIds() {
        return ids;
    }

}
