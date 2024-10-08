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
package cz.cas.lib.proarc.webapp.shared.rest;

/**
 * API to share information about application.
 *
 * @author Lukáš Sýkora
 */
public class ApplicationResourceApi {

    public static final String PATH = "info";
    public static final String FILE_PATH = "file";

    public static final String QUERY_FULL_LOAD = "loadFull";
    public static final String QUERY_FILE_TYPE = "type";

    public static final String REVISION = "revision";
    public static final String TIMESTAMP = "timestamp";
    public static final String VERSION = "version";
    public static final String STORAGE = "storage";
    public static final String DATABASE = "database";
    public static final String RDFLOW_VERSION = "rdflowVersion";
    public static final String STABLE_CONFIG_FILE = "configFile";
    public static final String STABLE_CONFIG = "config";
    public static final String STABLE_LANGUAGE_CS_FILE = "languageCsFile";
    public static final String STABLE_LANGUAGE_CS = "languageCs";
    public static final String STABLE_LANGUAGE_CSEN_FILE = "languageCsEnFile";
    public static final String STABLE_LANGUAGE_CSEN = "languageCsEn";
    public static final String STABLE_LANGUAGE_EN_FILE = "languageEnFile";
    public static final String STABLE_LANGUAGE_EN = "languageEn";
    public static final String ERROR = "error";

}
