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
 *
 * @author Lukáš Sýkora
 */
public final class AuthorityCatalogResourceApi {
    public static final String PATH = "authorities";

    public static final String FIND_PATH = "query";

    public static final String FIND_CATALOG_PARAM = "catalog";
    public static final String FIND_FIELDNAME_PARAM = "fieldName";
    public static final String FIND_VALUE_PARAM = "value";

    public static final String CATALOG_ID = "id";
    public static final String CATALOG_NAME = "name";
    public static final String CATALOG_FIELDS = "fields";
    public static final String CATALOG_FIELD_ID = "fieldId";
    public static final String CATALOG_FIELD_TITLE = "fieldTitle";

    public static final String FIELD_TYPE = "type";
}
