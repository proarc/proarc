/*
 * Copyright (C) 2017 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.server.rest;

import java.time.LocalDate;

/**
 * Parameter reader for ISO Date.
 *
 * @author Jan Pokorsky
 * @see <a href='http://en.wikipedia.org/wiki/ISO_8601'>ISO 8601</a>
 */
public class LocalDateParam {

    private final LocalDate localDate;

    public LocalDateParam(LocalDate localDate) {
        this.localDate = localDate;
    }

    public static LocalDateParam fromString(String v) {
        return new LocalDateParam(LocalDate.parse(v));
    }

    public LocalDate getLocalDate() {
        return localDate;
    }

}
