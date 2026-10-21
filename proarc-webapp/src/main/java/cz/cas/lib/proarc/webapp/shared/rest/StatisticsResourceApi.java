/*
 * Copyright (C) 2026 Lukas Sykora
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

/** Paths and query parameters shared by statistics resources. */
public final class StatisticsResourceApi {
    public static final String PATH = "statistics";
    public static final String MODELS_PATH = "models";
    public static final String PRODUCTIVITY_PATH = "productivity";
    public static final String PRODUCTIVITY_OPTIONS_PATH = "productivity/options";
    public static final String EXPORTS_PATH = "exports";
    public static final String ARTICLE_EXPORTS_PATH = "exports/articles";
    public static final String EXPORT_OPTIONS_PATH = "exports/options";

    public static final String ACTIVE_PARAM = "active";
    public static final String RESPECT_ORGANIZATION_PARAM = "respectOrganization";
    public static final String USERNAME_PARAM = "username";
    public static final String DATE_FROM_PARAM = "dateFrom";
    public static final String DATE_TO_PARAM = "dateTo";
    public static final String MODEL_PARAM = "model";
    public static final String SIGLA_PARAM = "sigla";
    public static final String EXPORT_TYPE_PARAM = "exportType";
    public static final String DEVICE_PARAM = "device";
    public static final String SOFTWARE_PARAM = "software";
    public static final String GROUP_BY_PARAM = "groupBy";
    public static final String UNIQUE_PARAM = "unique";
    public static final String ARTICLES_PARAM = "articles";
    public static final String OFFSET_PARAM = "offset";
    public static final String LIMIT_PARAM = "limit";

    private StatisticsResourceApi() { }
}
