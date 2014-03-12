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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.user.Group;
import cz.cas.lib.proarc.common.user.Permission;
import cz.cas.lib.proarc.common.user.Permissions;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

/**
 *
 * @author Jan Pokorsky
 */
@Path("user")
public final class UserResource {

    private static final Logger LOG = Logger.getLogger(UserResource.class.getName());
    private final UserManager userManager;
    private final SessionContext session;

    public UserResource(
            @Context HttpServletRequest httpRequest,
            @Context SecurityContext securityCtx
            ) {
        this.session = SessionContext.from(httpRequest);
        this.userManager = UserUtil.getDefaultManger();
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<UserProfile> find(
            @QueryParam("userId") Integer userId,
            @QueryParam("userName") String userName,
            @QueryParam("whoAmI") Boolean whoAmI
            ) {

        if (whoAmI != null && whoAmI) {
            userId = null;
            userName = session.getUser().getUserName();
        }
        if (userId != null) {
            UserProfile found = userManager.find(userId);
            return new SmartGwtResponse<UserProfile>(found);
        } else if (userName != null && !userName.isEmpty()) {
            UserProfile found = userManager.find(userName);
            return new SmartGwtResponse<UserProfile>(found);
        }
        List<UserProfile> findAll = userManager.findAll();
        return new SmartGwtResponse<UserProfile>(findAll);
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<UserProfile> add(
            @FormParam("userName") String userName,
            @FormParam("userPassword") String passwd,
            @FormParam("surname") String surname,
            @FormParam("forename") String forename,
            @FormParam("email") String email
//            @FormParam("userHome") String home
            ) {

        checkAccess(session.getUser(), Permissions.ADMIN, Permissions.USERS_CREATE);
        if (userName == null) {
            return SmartGwtResponse.<UserProfile>asError().error("userName", "missing").build();
        }
        UserProfile found = userManager.find(userName);
        if (found != null) {
            return SmartGwtResponse.<UserProfile>asError().error("userName", "already exists").build();
        }
        UserProfile newProfile = new UserProfile();
        newProfile.setEmail(email);
        newProfile.setForename(forename);
        newProfile.setSurname(surname);
        newProfile.setUserName(userName);
        newProfile.setUserPassword(passwd);
        newProfile = userManager.add(newProfile, Collections.<Group>emptyList(),
                session.getUser().getUserName(), session.asFedoraLog());
        return new SmartGwtResponse<UserProfile>(newProfile);
    }

    @PUT
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<UserProfile> update(
            @FormParam("userId") Integer userId,
//            @FormParam("userName") String userName,
            @FormParam("userPassword") String passwd,
            @FormParam("surname") String surname,
            @FormParam("forename") String forename,
            @FormParam("email") String email
//            @FormParam("userHome") String home
            ) {

        UserProfile sessionUser = session.getUser();
        // check for admin or the same user
        UserProfile update = userId == null ? null : userManager.find(userId);
        boolean fullUpdate;
        if (update != null && update.getUserName().equals(sessionUser.getUserName())) {
            Set<Permission> grants = checkAccess(sessionUser, (Permission) null);
            fullUpdate = grants.contains(Permissions.ADMIN);
        } else {
            checkAccess(sessionUser, Permissions.ADMIN);
            fullUpdate = true;
        }
        if (update == null) {
            return SmartGwtResponse.<UserProfile>asError().error("userId", "not found").build();
        }
        if (passwd != null && update.getRemoteType() == null) {
            update.setUserPassword(passwd);
        }
        if (fullUpdate && surname != null) {
            update.setSurname(surname);
        }
        if (fullUpdate && forename != null) {
            update.setForename(forename);
        }
        if (fullUpdate && email != null) {
            update.setForename(forename);
        }

        userManager.update(update, sessionUser.getUserName(), session.asFedoraLog());
        return new SmartGwtResponse<UserProfile>(update);
    }

    @Path("permissions")
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Permission> findPermissions(
            @QueryParam("userId") Integer userId
            ) {

        List<Permission> result = Collections.emptyList();
        if (userId == null) {
            userId = session.getUser().getId();
        }
        if (userId != null) {
            Set<Permission> permissions = userManager.findUserPermissions(userId);
            result = new ArrayList<Permission>(permissions);
        }
        return new SmartGwtResponse<Permission>(result);
    }

    Set<Permission> checkAccess(UserProfile user, Permission... permissions) {
        if (user != null) {
            Set<Permission> grants = userManager.findUserPermissions(user.getId());
            for (Permission permission : permissions) {
                if (permission == null || grants.contains(permission)) {
                    return grants;
                }
            }
        }
        throw new WebApplicationException(Response.Status.FORBIDDEN);
    }

}
