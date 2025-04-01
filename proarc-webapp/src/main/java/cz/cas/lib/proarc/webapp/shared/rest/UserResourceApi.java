/*
 * Copyright (C) 2014 Jan Pokorsky
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

import cz.cas.lib.proarc.webapp.server.rest.v1.UserResourceV1;

/**
 * Constants for {@link UserResourceV1}
 * shared by GWT client.
 *
 * @author Jan Pokorsky
 */
public final class UserResourceApi {

    public static final String PATH = "user";
    public static final String PATH_USER_SETTING = "userSetting";

    public static final String USER_WHOAMI_PARAM = "whoAmI";

    public static final String USER_ID = "userId";
    public static final String USER_HOME = "home";
    public static final String USER_NAME = "name";
    public static final String USER_REMOTENAME = "remoteName";
    public static final String USER_REMOTETYPE = "remoteType";
    public static final String USER_CREATED = "created";
    public static final String USER_EMAIL = "email";
    public static final String USER_FORENAME = "forename";
    public static final String USER_SURNAME = "surname";
    public static final String USER_ORGANIZATION = "organization";
    public static final String USER_ROLE = "role";
    public static final String USER_LASTLOGIN = "lastLogin";
    public static final String USER_STATUS = "status";
    public static final String USER_DEFAULTGROUP = "defaultGroup";
    public static final String USER_USERGROUP = "userGroup";
    public static final String USER_TIMESTAMP = "timestamp";
    public static final String USER_PASSWORD = "password";
    public static final String USER_RUN_CHANGE_MODEL_FUNCTION = "changeModelFunction";
    public static final String USER_RUN_UPDATE_MODEL_FUNCTION = "updateModelFunction";
    public static final String USER_RUN_LOCK_OBJECT_FUNCTION = "lockObjectFunction";
    public static final String USER_RUN_UNLOCK_OBJECT_FUNCTION = "unlockObjectFunction";
    public static final String USER_IMPORT_TO_PROD_FUNCTION = "importToProdFunction";
    public static final String USER_CZIDLO_FUNCTION = "czidloFunction";
    public static final String USER_WF_DELETE_JOB_FUNCTION = "wfDeleteJobFunction";
    public static final String USER_IMPORT_TO_CATALOG_FUNCTION = "importToCatalogFunction";
    public static final String USER_START_ROW_PARAM = "_startRow";
    public static final String USER_SETTING = "userSetting";
}
