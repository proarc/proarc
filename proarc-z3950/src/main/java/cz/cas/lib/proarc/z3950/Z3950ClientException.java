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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.z3950;

/**
 *
 * @author Jan Pokorsky
 */
public final class Z3950ClientException extends Exception {
    private static final long serialVersionUID = 1L;

    public Z3950ClientException() {
    }

    public Z3950ClientException(Z3950Client client, String message) {
        super(client + ", msg: " + message);
    }

    public Z3950ClientException(Z3950Client client, String message, Throwable cause) {
        super(client + ", msg: " + message, cause);
    }

    public Z3950ClientException(Z3950Client client, Throwable cause) {
        super(String.valueOf(client), cause);
    }

}
