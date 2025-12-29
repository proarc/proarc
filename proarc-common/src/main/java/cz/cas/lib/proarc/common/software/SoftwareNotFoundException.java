/*
 * Copyright (C) 2025 Lukas Sykora
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
package cz.cas.lib.proarc.common.software;

/**
 * A software cannot be found.
 *
 * @author Lukas Sykora
 */
public class SoftwareNotFoundException extends SoftwareException {

    private static final long serialVersionUID = 1L;
    private final String[] ids;

    public SoftwareNotFoundException(String message, Throwable cause, String... id) {
        super(message, cause);
        this.ids = id;
    }

    public SoftwareNotFoundException(String message, String... id) {
        this(message, null, id);
    }

    public String[] getIds() {
        return ids;
    }

}
