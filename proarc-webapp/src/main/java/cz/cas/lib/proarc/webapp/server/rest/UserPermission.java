package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;

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
            if (UserRole.PERMISSION_RUN_LOCK_OBJECT_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasLockObjectFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_RUN_UNLOCK_OBJECT_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasUnlockObjectFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_RUN_UPDATE_MODEL_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasUpdateModelFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasChangeModelFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_IMPORT_TO_PROD_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasPermissionToImportToProdFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_CZIDLO_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasCzidloFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_WF_DELETE_JOB_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasWfDeleteJobFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_IMPORT_TO_CATALOG_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasImportToCatalogFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_CHANGE_OBJECTS_OWNER_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasChangeObjectsOwnerFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_CHANGE_PAGES_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasChangePagesFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_DEVICE_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasDeviceFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_WF_CREATE_JOB_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasWfCreateJobFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_CREATE_USER_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasCreateUserFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_UPDATE_USER_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasUpdateUserFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_UPDATE_USER_PERMISSION_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasUpdateUserPermissionFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_DELETE_USER_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasDeleteUserFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_SOLR_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasSolrFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_DELETE_ACTION_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasDeleteActionFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_ALL_OBJECTS_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasAllObjectsFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_PREPARE_BATCH_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasPrepareBatchFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_SYS_ADMIN_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasSysAdminFunction())) {
                return true;
            }
        }
        return false;
    }
}
