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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.server.rest.v2;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.user.Permission;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserSetting;
import cz.cas.lib.proarc.webapp.server.rest.ProArcResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.UserResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.DefaultValue;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.SecurityContext;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_NO_PERMISSION;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_UNSUPPORTED_VALUE;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_CREATE_USER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_DELETE_USER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_UPDATE_USER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_2;
import static cz.cas.lib.proarc.webapp.server.rest.UserPermission.hasPermission;

/**
 *
 * @author Lukas Sykora
 */
@Path(URL_API_VERSION_2 + "/" + UserResourceApi.PATH)
public class UserResource extends UserResourceV1 {

    private static final Logger LOG = Logger.getLogger(UserResource.class.getName());

    public UserResource(
            @Context HttpServletRequest httpRequest,
            @Context HttpHeaders httpHeaders,
            @Context SecurityContext securityCtx
            ) throws AppConfigurationException {
        super(httpRequest, httpHeaders, securityCtx);
    }

    @DELETE
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<UserProfile> deleteUser(
            @QueryParam(UserResourceApi.USER_ID) Integer userId
    ) {
        if (!hasPermission(user, PERMISSION_FUNCTION_DELETE_USER)) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }

        try {
            return super.deleteUser(userId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<UserProfile> find(
            @QueryParam(UserResourceApi.USER_ID) Integer userId,
            @QueryParam(UserResourceApi.USER_NAME) String userName,
            @QueryParam(UserResourceApi.USER_WHOAMI_PARAM) Boolean whoAmI,
            @QueryParam(UserResourceApi.USER_START_ROW_PARAM) @DefaultValue("-1") int startRow
    ) {
        try {
            return super.find(userId, userName, whoAmI, startRow);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<UserProfile> add(
            @FormParam(UserResourceApi.USER_NAME) String userName,
            @FormParam(UserResourceApi.USER_PASSWORD) String passwd,
            @FormParam(UserResourceApi.USER_SURNAME) String surname,
            @FormParam(UserResourceApi.USER_FORENAME) String forename,
            @FormParam(UserResourceApi.USER_EMAIL) String email,
            @FormParam(UserResourceApi.USER_ORGANIZATION) String organization,
            @FormParam(UserResourceApi.FUNCTION_CHANGE_MODEL) Boolean changeModelFunction,
            @FormParam(UserResourceApi.FUNCTION_UPDATE_MODEL) Boolean updateModelFunction,
            @FormParam(UserResourceApi.FUNCTION_LOCK_OBJECT) Boolean lockObjectFuction,
            @FormParam(UserResourceApi.FUNCTION_UNLOCK_OBJECT) Boolean unlockObjectFuction,
            @FormParam(UserResourceApi.FUNCTION_IMPORT_TO_PROD) Boolean importToProdFunction,
            @FormParam(UserResourceApi.FUNCTION_CZIDLO) Boolean czidloFunction,
            @FormParam(UserResourceApi.FUNCTION_WF_DELETE_JOB) Boolean wfDeleteJobFunction,
            @FormParam(UserResourceApi.FUNCTION_IMPORT_TO_CATALOG) Boolean importToCatalogFunction,
            @FormParam(UserResourceApi.FUNCTION_CHANGE_OBJECTS_OWNER) Boolean changeObjectsOwnerFunction,
            @FormParam(UserResourceApi.FUNCTION_CHANGE_PAGES) Boolean changePagesFunction,
            @FormParam(UserResourceApi.FUNCTION_DEVICE) Boolean deviceFunction,
            @FormParam(UserResourceApi.FUNCTION_WF_CREATE_JOB) Boolean wfCreateJobFunction,
            @FormParam(UserResourceApi.FUNCTION_CREATE_USER) Boolean createUserFunction,
            @FormParam(UserResourceApi.FUNCTION_UPDATE_USER) Boolean updateUserFunction,
            @FormParam(UserResourceApi.FUNCTION_DELETE_USER) Boolean deleteUserFunction,
            @FormParam(UserResourceApi.FUNCTION_SOLR) Boolean solrFunction,
            @FormParam(UserResourceApi.FUNCTION_DELETE_ACTION) Boolean deleteActionFunction,
            @FormParam(UserResourceApi.FUNCTION_ALL_OBJECTS) Boolean allObjectsFunction,
            @FormParam(UserResourceApi.FUNCTION_PREPARE_BATCH) Boolean prepareBatchFunction,
            @FormParam(UserResourceApi.FUNCTION_SYS_ADMIN) Boolean sysAdminFunction
    ) {
        if (!hasPermission(user, PERMISSION_FUNCTION_CREATE_USER)) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }

        if (userName == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, UserResourceApi.USER_NAME));
        }
        UserProfile found = userManager.find(userName);
        if (found != null) {
            return ProArcResponse.asError(ERR_UNSUPPORTED_VALUE, UserResourceApi.USER_NAME);
        }
        try {
            return super.add(userName, passwd, surname, forename, email, organization, changeModelFunction,
                    updateModelFunction, lockObjectFuction, unlockObjectFuction, importToProdFunction, czidloFunction, wfDeleteJobFunction, importToCatalogFunction, changeObjectsOwnerFunction, changePagesFunction, deviceFunction, wfCreateJobFunction, createUserFunction, updateUserFunction, deleteUserFunction, solrFunction, deleteActionFunction, allObjectsFunction, prepareBatchFunction, sysAdminFunction);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @PUT
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<UserProfile> update(
            @FormParam(UserResourceApi.USER_ID) Integer userId,
            @FormParam(UserResourceApi.USER_PASSWORD) String passwd,
            @FormParam(UserResourceApi.USER_SURNAME) String surname,
            @FormParam(UserResourceApi.USER_FORENAME) String forename,
            @FormParam(UserResourceApi.USER_EMAIL) String email,
            @FormParam(UserResourceApi.USER_ORGANIZATION) String organization,
            @FormParam(UserResourceApi.FUNCTION_CHANGE_MODEL) Boolean changeModelFunction,
            @FormParam(UserResourceApi.FUNCTION_UPDATE_MODEL) Boolean updateModelFunction,
            @FormParam(UserResourceApi.FUNCTION_LOCK_OBJECT) Boolean lockObjectFuction,
            @FormParam(UserResourceApi.FUNCTION_UNLOCK_OBJECT) Boolean unlockObjectFuction,
            @FormParam(UserResourceApi.FUNCTION_IMPORT_TO_PROD) Boolean importToProdFunction,
            @FormParam(UserResourceApi.FUNCTION_CZIDLO) Boolean czidloFunction,
            @FormParam(UserResourceApi.FUNCTION_WF_DELETE_JOB) Boolean wfDeleteJobFunction,
            @FormParam(UserResourceApi.FUNCTION_IMPORT_TO_CATALOG) Boolean importToCatalogFunction,
            @FormParam(UserResourceApi.FUNCTION_CHANGE_OBJECTS_OWNER) Boolean changeObjectsOwnerFunction,
            @FormParam(UserResourceApi.FUNCTION_CHANGE_PAGES) Boolean changePagesFunction,
            @FormParam(UserResourceApi.FUNCTION_DEVICE) Boolean deviceFunction,
            @FormParam(UserResourceApi.FUNCTION_WF_CREATE_JOB) Boolean wfCreateJobFunction,
            @FormParam(UserResourceApi.FUNCTION_CREATE_USER) Boolean createUserFunction,
            @FormParam(UserResourceApi.FUNCTION_UPDATE_USER) Boolean updateUserFunction,
            @FormParam(UserResourceApi.FUNCTION_DELETE_USER) Boolean deleteUserFunction,
            @FormParam(UserResourceApi.FUNCTION_SOLR) Boolean solrFunction,
            @FormParam(UserResourceApi.FUNCTION_DELETE_ACTION) Boolean deleteActionFunction,
            @FormParam(UserResourceApi.FUNCTION_ALL_OBJECTS) Boolean allObjectsFunction,
            @FormParam(UserResourceApi.FUNCTION_PREPARE_BATCH) Boolean prepareBatchFunction,
            @FormParam(UserResourceApi.FUNCTION_SYS_ADMIN) Boolean sysAdminFunction
    ) {
        if (!hasPermission(user, PERMISSION_FUNCTION_UPDATE_USER)) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        try {
            return super.update(userId, passwd, surname, forename, email, organization, changeModelFunction,
                    updateModelFunction, lockObjectFuction, unlockObjectFuction, importToProdFunction, czidloFunction, wfDeleteJobFunction, importToCatalogFunction,
                    changeObjectsOwnerFunction, changePagesFunction, deviceFunction, wfCreateJobFunction, createUserFunction, updateUserFunction, deleteUserFunction, solrFunction, deleteActionFunction, allObjectsFunction, prepareBatchFunction, sysAdminFunction);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @Path("permissions")
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<Permission> findPermissions(
            @QueryParam("userId") Integer userId
    ) {
        try {
            return super.findPermissions(userId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path(UserResourceApi.PATH_USER_SETTING)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<UserSetting> getUserSetting(
//            @QueryParam(UserResourceApi.USER_ID) Integer userId
    ) {
        try {
            return super.getUserSetting();
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(UserResourceApi.PATH_USER_SETTING)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<UserSetting> updateUserSetting(
//            @FormParam(UserResourceApi.USER_ID) Integer userId,
            @FormParam(UserResourceApi.USER_SETTING) String settingJson
    ) {
        try {
            return super.updateUserSetting(settingJson);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }
}
