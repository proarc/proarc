package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.user.Permission;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;

public class UserPermission {

    public static void checkPermission(SessionContext session, UserProfile user, String role, Permission permission, String... attributes) {
        if (!hasPermission(session, user, role, permission, attributes)) {
            throw new WebApplicationException(Response.Status.FORBIDDEN);
        }
    }

    public static boolean hasPermission(SessionContext session, UserProfile user, String role, Permission permission, String... attributes) {
        return session.checkPermission(permission) || session.checkRole(role) || hasAttribute(user, attributes);
    }

    private static boolean hasAttribute(UserProfile user, String[] attributes) {
        for (String attribute : attributes) {
            if (UserRole.PERMISSION_RUN_LOCK_OBJECT_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.getLockObjectFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_RUN_UNLOCK_OBJECT_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.getUnlockObjectFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_RUN_UPDATE_MODEL_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.getUpdateModelFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.getChangeModelFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_IMPORT_TO_PROD_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasPermissionToImportToProdFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_CZIDLO_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasCzidloFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_WF_DELETE_JOB_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.getWfDeleteJobFunction())) {
                return true;
            }
            if (UserRole.PERMISSION_IMPORT_TO_CATALOG_FUNCTION.equals(attribute) && Boolean.TRUE.equals(user.hasImportToCatalogFunction())) {
                return true;
            }
        }
        return false;
    }
}
