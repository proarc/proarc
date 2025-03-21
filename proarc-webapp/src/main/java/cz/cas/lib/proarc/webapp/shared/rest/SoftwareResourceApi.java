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
package cz.cas.lib.proarc.webapp.shared.rest;

/**
 * API to share constants between client and server code.
 *
 * @author Lukas Sykora
 */
public final class SoftwareResourceApi {

    // resource /software
    public static final String PATH = "software";
    public static final String PATH_PREVIEW = "preview";

    public static final String SOFTWARE_ITEM_ID = "id";
    public static final String SOFTWARE_ITEM_LABEL = "label";
    public static final String SOFTWARE_ITEM_MODEL = "model";
    public static final String SOFTWARE_ITEM_DESCRIPTION = "description";
    public static final String SOFTWARE_ITEM_TIMESTAMP = "timestamp";
    public static final String SOFTWARE_ITEM_MEMBERS = "members";
    public static final String SOFTWARE_START_ROW_PARAM = "_startRow";
}
