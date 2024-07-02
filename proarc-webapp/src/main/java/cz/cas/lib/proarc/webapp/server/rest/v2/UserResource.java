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
import cz.cas.lib.proarc.common.user.Permissions;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.UserResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;
import java.util.Arrays;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.SecurityContext;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_NO_PERMISSION;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_UNSUPPORTED_VALUE;

/**
 *
 * @author Lukas Sykora
 */
@Path(RestConfig.URL_API_VERSION_2 + "/" + UserResourceApi.PATH)
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
    public SmartGwtResponse<UserProfile> deleteUser(
            @QueryParam(UserResourceApi.USER_ID) Integer userId
    ) {
        try {
            return super.deleteUser(userId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<UserProfile> find(
            @QueryParam(UserResourceApi.USER_ID) Integer userId,
            @QueryParam(UserResourceApi.USER_NAME) String userName,
            @QueryParam(UserResourceApi.USER_WHOAMI_PARAM) Boolean whoAmI,
            @QueryParam(UserResourceApi.USER_START_ROW_PARAM) @DefaultValue("-1") int startRow
    ) {
        try {
            return super.find(userId, userName, whoAmI, startRow);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<UserProfile> add(
            @FormParam(UserResourceApi.USER_NAME) String userName,
            @FormParam(UserResourceApi.USER_PASSWORD) String passwd,
            @FormParam(UserResourceApi.USER_SURNAME) String surname,
            @FormParam(UserResourceApi.USER_FORENAME) String forename,
            @FormParam(UserResourceApi.USER_EMAIL) String email,
            @FormParam(UserResourceApi.USER_ORGANIZATION) String organization,
            @FormParam(UserResourceApi.USER_ROLE) String role,
            @FormParam(UserResourceApi.USER_RUN_CHANGE_MODEL_FUNCTION) Boolean changeModelFunction,
            @FormParam(UserResourceApi.USER_RUN_UPDATE_MODEL_FUNCTION) Boolean updateModelFunction,
            @FormParam(UserResourceApi.USER_RUN_LOCK_OBJECT_FUNCTION) Boolean lockObjectFuction,
            @FormParam(UserResourceApi.USER_RUN_UNLOCK_OBJECT_FUNCTION) Boolean unlockObjectFuction,
            @FormParam(UserResourceApi.USER_IMPORT_TO_PROD_FUNCTION) Boolean importToProdFunction,
            @FormParam(UserResourceApi.USER_CZIDLO_FUNCTION) Boolean czidloFunction,
            @FormParam(UserResourceApi.USER_WF_DELETE_JOB_FUNCTION) Boolean wfDeleteJobFunction
    ) {
        Locale locale = session.getLocale(httpHeaders);
        try {
            checkAccess(session.getUser(), Arrays.asList(UserRole.ROLE_SUPERADMIN, UserRole.ROLE_ADMIN), Permissions.ADMIN, Permissions.USERS_CREATE);
        } catch (WebApplicationException e) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (userName == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, UserResourceApi.USER_NAME));
        }
        UserProfile found = userManager.find(userName);
        if (found != null) {
            return SmartGwtResponse.asError(ERR_UNSUPPORTED_VALUE, UserResourceApi.USER_NAME);
        }
        try {
            return super.add(userName, passwd, surname, forename, email, organization, role, changeModelFunction,
                    updateModelFunction, lockObjectFuction, unlockObjectFuction, importToProdFunction, czidloFunction, wfDeleteJobFunction);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<UserProfile> update(
            @FormParam(UserResourceApi.USER_ID) Integer userId,
            @FormParam(UserResourceApi.USER_PASSWORD) String passwd,
            @FormParam(UserResourceApi.USER_SURNAME) String surname,
            @FormParam(UserResourceApi.USER_FORENAME) String forename,
            @FormParam(UserResourceApi.USER_EMAIL) String email,
            @FormParam(UserResourceApi.USER_ORGANIZATION) String organization,
            @FormParam(UserResourceApi.USER_ROLE) String role,
            @FormParam(UserResourceApi.USER_RUN_CHANGE_MODEL_FUNCTION) Boolean changeModelFunction,
            @FormParam(UserResourceApi.USER_RUN_UPDATE_MODEL_FUNCTION) Boolean updateModelFunction,
            @FormParam(UserResourceApi.USER_RUN_LOCK_OBJECT_FUNCTION) Boolean lockObjectFuction,
            @FormParam(UserResourceApi.USER_RUN_UNLOCK_OBJECT_FUNCTION) Boolean unlockObjectFuction,
            @FormParam(UserResourceApi.USER_IMPORT_TO_PROD_FUNCTION) Boolean importToProdFunction,
            @FormParam(UserResourceApi.USER_CZIDLO_FUNCTION) Boolean czidloFunction,
            @FormParam(UserResourceApi.USER_WF_DELETE_JOB_FUNCTION) Boolean wfDeleteJobFunction
    ) {
        try {
            checkAccess(session.getUser(), Arrays.asList(UserRole.ROLE_SUPERADMIN, UserRole.ROLE_ADMIN));
        } catch (WebApplicationException e) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        try {
            return super.update(userId, passwd, surname, forename, email, organization, role, changeModelFunction,
                    updateModelFunction, lockObjectFuction, unlockObjectFuction, importToProdFunction, czidloFunction, wfDeleteJobFunction);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @Path("permissions")
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Permission> findPermissions(
            @QueryParam("userId") Integer userId
    ) {
        try {
            return super.findPermissions(userId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
