/*
 * Copyright (C) 2023 Lukas Sykora
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

public class KrameriusResourceApi {

    // class path
    public static final String PATH = "kramerius";

    // method path
    public static final String VIEW_MODS = "viewMods";
    public static final String VIEW_IMAGE = "viewImage";
    public static final String UPDATE_MODS = "updateMods";
    public static final String IMPORT_2_PROARC = "importToProArc";
    public static final String IMPORT_2_KRAMERIUS = "importToKramerius";

    //parameters
    public static final String KRAMERIUS_OBJECT_PID = "pid";
    public static final String KRAMERIUS_INSTANCE = "instance";
    public static final String KRAMERIUS_IMPORT_INSTANCE = "importInstance";
    public static final String KRAMERIUD_RERUN = "rerun";
}
