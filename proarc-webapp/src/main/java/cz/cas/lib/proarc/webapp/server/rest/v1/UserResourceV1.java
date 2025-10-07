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
package cz.cas.lib.proarc.webapp.server.rest.v1;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.dao.BatchViewFilter;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.user.Group;
import cz.cas.lib.proarc.common.user.Permission;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserSetting;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.ServerMessages;
import cz.cas.lib.proarc.webapp.server.rest.RestException;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Set;
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
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.SecurityContext;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_UNKNOWN_USER;
import static cz.cas.lib.proarc.webapp.server.rest.UserPermission.checkPermission;

/**
 *
 * @author Jan Pokorsky
 */
@Deprecated
@Path(RestConfig.URL_API_VERSION_1 + "/" + UserResourceApi.PATH)
public class UserResourceV1 {

    private static final Logger LOG = Logger.getLogger(UserResourceV1.class.getName());
    protected final UserManager userManager;
    protected final SessionContext session;
    protected final HttpHeaders httpHeaders;
    private final AppConfiguration appConfig;
    private final AkubraConfiguration akubraConfiguration;
    protected final UserProfile user;

    public UserResourceV1(
            @Context HttpServletRequest httpRequest,
            @Context HttpHeaders httpHeaders,
            @Context SecurityContext securityCtx
            ) throws AppConfigurationException {
        this.httpHeaders = httpHeaders;
        this.session = SessionContext.from(httpRequest);
        this.userManager = UserUtil.getDefaultManger();
        this.user = session.getUser();
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }
    }

    @DELETE
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<UserProfile> deleteUser(
            @QueryParam(UserResourceApi.USER_ID) Integer userId
    ) {

        checkPermission(user, UserRole.PERMISSION_FUNCTION_DELETE_USER);
        Locale locale = session.getLocale(httpHeaders);

        if (userId == null) {
            throw RestException.plainBadRequest(UserResourceApi.USER_ID, String.valueOf(userId));
        }

        try {
            UserProfile user2Delete = userManager.find(userId);

            if (user2Delete == null) {
                return SmartGwtResponse.<UserProfile>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_UserId_NotFound"))
                        .build();
            }

            SearchView search = null;

            if (UserUtil.DEFAULT_ADMIN_USER.equals(user2Delete.getUserName())) {
                return SmartGwtResponse.<UserProfile>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_User_cant_be_deleted", user2Delete.getUserName()))
                        .build();
            }


            if (user.getUserName().equals(user2Delete.getUserName())) {
                return SmartGwtResponse.<UserProfile>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_User_cant_be_deleted", user2Delete.getUserName()))
                        .build();
            }

            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                FedoraStorage remote = FedoraStorage.getInstance(appConfig);
                search = remote.getSearch(locale);
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubra = AkubraStorage.getInstance(akubraConfiguration);
                search = akubra.getSearch(locale);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }

            int count = search.countByOwner(user2Delete.getUserName());
            if (count != 0) {
                return SmartGwtResponse.<UserProfile>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_User_has_objects", user2Delete.getUserName(), count))
                        .build();
            }
            List<SearchViewItem> items = search.findByProcessor(user2Delete.getUserName());
            count = items.size();
            if (count != 0) {
                return SmartGwtResponse.<UserProfile>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_User_has_objects", user2Delete.getUserName(), count))
                        .build();
            }
            BatchManager batchManager = BatchManager.getInstance();
            List<BatchView> batchViewList = batchManager.viewBatch(new BatchViewFilter().setCreatorId(user2Delete.getId()).setMaxCount(9999));
            count = batchViewList.size();
            if (count != 0) {
                return SmartGwtResponse.<UserProfile>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_User_has_objects", user2Delete.getUserName(), count))
                        .build();
            }

            WorkflowManager workflowManager = WorkflowManager.getInstance();
            WorkflowProfiles workflowProfiles = WorkflowProfiles.getInstance();
            WorkflowDefinition workflow = workflowProfiles.getProfiles();
            JobFilter jobFilter = new JobFilter();
            jobFilter.setUserId(new BigDecimal(user2Delete.getId()));
            jobFilter.setMaxCount(9999);
            jobFilter.setLocale(session.getLocale(httpHeaders));
            List<JobView> jobViewList = workflowManager.findJob(jobFilter);
            count = jobViewList.size();
            if (count != 0) {
                return SmartGwtResponse.<UserProfile>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_User_has_objects", user2Delete.getUserName(), count))
                        .build();
            }

            TaskFilter taskFilter = new TaskFilter();
            taskFilter.setUserId(Collections.singletonList(new BigDecimal(user2Delete.getId())));
            taskFilter.setMaxCount(9999);
            taskFilter.setLocale(session.getLocale(httpHeaders));
            List<TaskView> taskViewList = workflowManager.tasks().findTask(taskFilter, workflow);
            count = taskViewList.size();
            if (count != 0) {
                return SmartGwtResponse.<UserProfile>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_User_has_objects", user2Delete.getUserName(), count))
                        .build();
            }

            userManager.deleteUser(userId);
            UserProfile found = userManager.find(userId);
            return new SmartGwtResponse<UserProfile>(SmartGwtResponse.STATUS_OBJECT_SUCCESFULLY_DELETED, 0, 0, 1, found != null ? Collections.singletonList(found) : Collections.emptyList());
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
        UserProfile user = session.getUser();
        boolean allowAllOrganization = user.hasSysAdminFunction();
        if (!allowAllOrganization) {
            List<UserProfile> findAll = userManager.findMyOrganization(session.getUser().getOrganization());
            List<UserProfile> selectedUsers = new ArrayList<>();
            if (startRow < 0) {
                selectedUsers.addAll(findAll);
            } else {
                for (int i = startRow; i < startRow + 100; i++) {
                    if (findAll.size() - 1 < i) {
                        break;
                    }
                    selectedUsers.add(findAll.get(i));
                }
            }
            int endRow = startRow + selectedUsers.size() - 1;
            int total = findAll.size();
            return new SmartGwtResponse<UserProfile>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, selectedUsers);

        } else {
            List<UserProfile> findAll = userManager.findAll();
            List<UserProfile> selectedUsers = new ArrayList<>();
            if (startRow < 0) {
                selectedUsers.addAll(findAll);
            } else {
                for (int i = startRow; i < startRow + 100; i++) {
                    if (findAll.size() - 1 < i) {
                        break;
                    }
                    selectedUsers.add(findAll.get(i));
                }
            }
            int endRow = startRow + selectedUsers.size() - 1;
            int total = findAll.size();
            return new SmartGwtResponse<UserProfile>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, selectedUsers);
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
        Locale locale = session.getLocale(httpHeaders);
        checkPermission(user, UserRole.PERMISSION_FUNCTION_CREATE_USER);
        if (userName == null) {
            return SmartGwtResponse.<UserProfile>asError()
                    .error(UserResourceApi.PATH,  ServerMessages.get(locale).getFormattedMessage("UserResouce_Username_Required"))
                    .build();
        }
        UserProfile found = userManager.find(userName);
        if (found != null) {
            return SmartGwtResponse.<UserProfile>asError()
                    .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_Username_Existing"))
                    .build();
        }
        UserProfile newProfile = new UserProfile();
        newProfile.setEmail(email);
        newProfile.setForename(forename);
        newProfile.setSurname(surname);
        newProfile.setUserName(userName);
        newProfile.setUserPassword(passwd);
        newProfile.setOrganization(organization);
        newProfile.setChangeModelFunction(changeModelFunction);
        newProfile.setUpdateModelFunction(updateModelFunction);
        newProfile.setLockObjectFunction(lockObjectFuction);
        newProfile.setUnlockObjectFunction(unlockObjectFuction);
        newProfile.setImportToProdFunction(importToProdFunction);
        newProfile.setCzidloFunction(czidloFunction);
        newProfile.setWfDeleteJobFunction(wfDeleteJobFunction);
        newProfile.setImportToCatalogFunction(importToCatalogFunction);
        newProfile.setChangeObjectsOwnerFunction(changeObjectsOwnerFunction);
        newProfile.setChangePagesFunction(changePagesFunction);
        newProfile.setDeviceFunction(deviceFunction);
        newProfile.setWfCreateJobFunction(wfCreateJobFunction);
        newProfile.setCreateUserFunction(createUserFunction);
        newProfile.setUpdateUserFunction(updateUserFunction);
        newProfile.setDeleteUserFunction(deleteUserFunction);
        newProfile.setSolrFunction(solrFunction);
        newProfile.setDeleteActionFunction(deleteActionFunction);
        newProfile.setAllObjectsFunction(allObjectsFunction);
        newProfile.setPrepareBatchFunction(prepareBatchFunction);
        newProfile.setSysAdminFunction(sysAdminFunction);
        try {
            newProfile = userManager.add(newProfile, Collections.<Group>emptyList(),
                    session.getUser().getUserName(), session.asFedoraLog());
        } catch (IllegalStateException | IllegalArgumentException ex) {
            String message = ex.getMessage();
            if (ex.getMessage().startsWith("Invalid user name")) {
                message = ServerMessages.get(locale).getFormattedMessage("UserResouce_Username_Invalid");
            } else if (ex.getMessage().startsWith("Invalid password")) {
                message = ServerMessages.get(locale).getFormattedMessage("UserResouce_Password_Invalid");
            }
            return SmartGwtResponse.<UserProfile>asError()
                    .error(UserResourceApi.PATH, message)
                    .build();
        }
        return new SmartGwtResponse<UserProfile>(newProfile);
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

        Locale locale = session.getLocale(httpHeaders);
        UserProfile sessionUser = session.getUser();
        // check for admin or the same user
        UserProfile update = userId == null ? null : userManager.find(userId);
        boolean fullUpdate;
        if (update != null && update.getUserName().equals(sessionUser.getUserName())) {
            checkPermission(user, UserRole.PERMISSION_FUNCTION_UPDATE_USER);
//            fullUpdate = grants.contains(Permissions.ADMIN);
            fullUpdate = true;
        } else {
            checkPermission(user, UserRole.PERMISSION_FUNCTION_UPDATE_USER);
            fullUpdate = false;
        }
        if (update == null) {
            return SmartGwtResponse.<UserProfile>asError()
                    .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_UserId_NotFound")).build();
        }
        if (passwd != null && update.getRemoteType() == null) {
            update.setUserPassword(passwd);
        }
        if (fullUpdate) {
            update.setEmail(email);
            update.setForename(forename);
            update.setOrganization(organization);
            update.setChangeModelFunction(changeModelFunction);
            update.setUpdateModelFunction(updateModelFunction);
            update.setLockObjectFunction(lockObjectFuction);
            update.setUnlockObjectFunction(unlockObjectFuction);
            update.setImportToProdFunction(importToProdFunction);
            update.setCzidloFunction(czidloFunction);
            update.setWfDeleteJobFunction(wfDeleteJobFunction);
            update.setImportToCatalogFunction(importToCatalogFunction);
            update.setChangeObjectsOwnerFunction(changeObjectsOwnerFunction);
            update.setChangePagesFunction(changePagesFunction);
            update.setDeviceFunction(deviceFunction);
            update.setWfCreateJobFunction(wfCreateJobFunction);
            update.setCreateUserFunction(createUserFunction);
            update.setUpdateUserFunction(updateUserFunction);
            update.setDeleteUserFunction(deleteUserFunction);
            update.setSolrFunction(solrFunction);
            update.setDeleteActionFunction(deleteActionFunction);
            update.setAllObjectsFunction(allObjectsFunction);
            update.setPrepareBatchFunction(prepareBatchFunction);
            update.setSysAdminFunction(sysAdminFunction);
            if (surname == null || surname.isEmpty()) {
                return SmartGwtResponse.<UserProfile>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_Surname_Required")).build();
            }
            update.setSurname(surname);
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

    @GET
    @Path(UserResourceApi.PATH_USER_SETTING)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<UserSetting> getUserSetting(
//            @QueryParam(UserResourceApi.USER_ID) Integer userId
    ) {
        Integer userId = session.getUser().getId();

        if (userId == null) {
            UserSetting userSetting = new UserSetting();
            userSetting.setValidation(returnLocalizedMessage(ERR_UNKNOWN_USER));
            return new SmartGwtResponse<>(Collections.singletonList(userSetting));
        }

        List<UserSetting> userSetting = userManager.getUserSetting(userId);
        return new SmartGwtResponse<>(userSetting);
    }

    @POST
    @Path(UserResourceApi.PATH_USER_SETTING)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<UserSetting> updateUserSetting(
//            @FormParam(UserResourceApi.USER_ID) Integer userId,
            @FormParam(UserResourceApi.USER_SETTING) String settingJson
    ) {
        Integer userId = session.getUser().getId();
        if (userId == null) {
            UserSetting userSetting = new UserSetting();
            userSetting.setValidation(returnLocalizedMessage(ERR_UNKNOWN_USER));
            return new SmartGwtResponse<>(Collections.singletonList(userSetting));
        }

        if (settingJson == null || settingJson.isEmpty()) {
            UserSetting userSetting = new UserSetting();
            userSetting.setValidation(returnLocalizedMessage(ERR_MISSING_PARAMETER, UserResourceApi.USER_SETTING));
            return new SmartGwtResponse<>(Collections.singletonList(userSetting));
        }

        List<UserSetting> settings = userManager.getUserSetting(userId);
        if (settings.isEmpty()) {
            UserSetting userSetting = new UserSetting();
            userSetting.setUserId(userId);
            userSetting.setUserSetting(settingJson);
            userManager.addUserSetting(userSetting);
        } else {
            for (UserSetting setting : settings) {
                setting.setUserSetting(settingJson);
                userManager.updateUserSetting(setting);
            }
        }

        settings = userManager.getUserSetting(userId);
        return new SmartGwtResponse<UserSetting>(settings);
    }

    protected String returnLocalizedMessage(String key, Object... arguments) {
        Locale locale = session.getLocale(httpHeaders);
        ServerMessages msgs = ServerMessages.get(locale);
        return msgs.getFormattedMessage(key, arguments);
    }

}
