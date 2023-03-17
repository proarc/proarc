package cz.cas.lib.proarc.webapp.client.widget;

import java.util.LinkedHashMap;

public class UserRole {

    public static final String ROLE_SUPERADMIN = "superAdmin";
    public static final String ROLE_ADMIN = "admin";
    public static final String ROLE_USER = "user";

    public static final String PERMISSION_RUN_CHANGE_MODEL_FUNCTION = "runChangeModelFunction";
    public static final String PERMISSION_RUN_UPDATE_ALL_OBJECTS_FUNCTION = "runUpdateAllObjectsFunction";
    public static final String PERMISSION_RUN_RESTORE_OBJECTS_FUNCTION = "runRestoreObjectsFunction";
    public static final String PERMISSION_RUN_UPDATE_MODEL_FUNCTION = "runUpdateObjectsFunction";
    public static final String PERMISSION_RUN_LOCK_OBJECT_FUNCTION = "runLockObjectFunction";
    public static final String PERMISSION_RUN_UNLOCK_OBJECT_FUNCTION = "runUnlockObjectFunction";
    public static final String PERMISSION_IMPORT_TO_PROD_FUNCTION = "importToProdFunction";

    public static LinkedHashMap<String, String> getMap() {
        LinkedHashMap<String, String> valueMap = new LinkedHashMap();
        valueMap.put(ROLE_SUPERADMIN, "Super Admin");
        valueMap.put(ROLE_ADMIN, "Admin");
        valueMap.put(ROLE_USER, "User");
        return valueMap;
    }
}
