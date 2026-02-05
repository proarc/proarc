package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.user.UserProfile;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_ALL_OBJECTS;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_CHANGE_MODEL;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_CHANGE_OBJECTS_OWNER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_CHANGE_PAGES;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_CREATE_USER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_CZIDLO;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_DELETE_ACTION;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_DELETE_USER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_DEVICE;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_IMPORT_TO_CATALOG;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_IMPORT_TO_PROD;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_LOCK_OBJECT;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_PREPARE_BATCH;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_SOLR;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_SYS_ADMIN;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_UNLOCK_OBJECT;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_UPDATE_MODEL;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_UPDATE_USER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_WF_CREATE_JOB;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_WF_DELETE_JOB;

public class UserPermission {

    public static void checkPermission(UserProfile user, String... attributes) {
        if (!hasPermission(user, attributes)) {
            throw new WebApplicationException(Response.Status.FORBIDDEN);
        }
    }

    public static boolean hasPermission(UserProfile user, String... attributes) {
        return hasAttribute(user, attributes);
    }

    private static boolean hasAttribute(UserProfile user, String[] attributes) {
        for (String attribute : attributes) {
            if (PERMISSION_FUNCTION_LOCK_OBJECT.equals(attribute) && Boolean.TRUE.equals(user.hasLockObjectFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_UNLOCK_OBJECT.equals(attribute) && Boolean.TRUE.equals(user.hasUnlockObjectFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_UPDATE_MODEL.equals(attribute) && Boolean.TRUE.equals(user.hasUpdateModelFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_CHANGE_MODEL.equals(attribute) && Boolean.TRUE.equals(user.hasChangeModelFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_IMPORT_TO_PROD.equals(attribute) && Boolean.TRUE.equals(user.hasPermissionToImportToProdFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_CZIDLO.equals(attribute) && Boolean.TRUE.equals(user.hasCzidloFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_WF_DELETE_JOB.equals(attribute) && Boolean.TRUE.equals(user.hasWfDeleteJobFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_IMPORT_TO_CATALOG.equals(attribute) && Boolean.TRUE.equals(user.hasImportToCatalogFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_CHANGE_OBJECTS_OWNER.equals(attribute) && Boolean.TRUE.equals(user.hasChangeObjectsOwnerFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_CHANGE_PAGES.equals(attribute) && Boolean.TRUE.equals(user.hasChangePagesFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_DEVICE.equals(attribute) && Boolean.TRUE.equals(user.hasDeviceFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_WF_CREATE_JOB.equals(attribute) && Boolean.TRUE.equals(user.hasWfCreateJobFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_CREATE_USER.equals(attribute) && Boolean.TRUE.equals(user.hasCreateUserFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_UPDATE_USER.equals(attribute) && Boolean.TRUE.equals(user.hasUpdateUserFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_DELETE_USER.equals(attribute) && Boolean.TRUE.equals(user.hasDeleteUserFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_SOLR.equals(attribute) && Boolean.TRUE.equals(user.hasSolrFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_DELETE_ACTION.equals(attribute) && Boolean.TRUE.equals(user.hasDeleteActionFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_ALL_OBJECTS.equals(attribute) && Boolean.TRUE.equals(user.hasAllObjectsFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_PREPARE_BATCH.equals(attribute) && Boolean.TRUE.equals(user.hasPrepareBatchFunction())) {
                return true;
            }
            if (PERMISSION_FUNCTION_SYS_ADMIN.equals(attribute) && Boolean.TRUE.equals(user.hasSysAdminFunction())) {
                return true;
            }
        }
        return false;
    }
}
