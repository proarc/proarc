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
    public static final String PERMISSION_CZIDLO_FUNCTION = "czidloFunction";
    public static final String PERMISSION_WF_DELETE_JOB_FUNCTION = "wfDeleteJobFunction";
    public static final String PERMISSION_IMPORT_TO_CATALOG_FUNCTION = "importToCatalogFunction";
    public static final String PERMISSION_CHANGE_OBJECTS_OWNER_FUNCTION = "changeObjectsOwnerFunction";
    public static final String PERMISSION_DEVICE_FUNCTION = "deviceFunction";
    public static final String PERMISSION_CHANGE_PAGES_FUNCTION = "changePagesFunction";
    public static final String PERMISSION_WF_CREATE_JOB_FUNCTION = "wfCreateJobFunction";
    public static final String PERMISSION_CREATE_USER_FUNCTION = "createUserFunction";
    public static final String PERMISSION_UPDATE_USER_FUNCTION = "updateUserFunction";
    public static final String PERMISSION_UPDATE_USER_PERMISSION_FUNCTION = "updateUserPermissionFunction";
    public static final String PERMISSION_DELETE_USER_FUNCTION = "deleteUserFunction";
    public static final String PERMISSION_SOLR_FUNCTION = "solrFunction";
    public static final String PERMISSION_DELETE_ACTION_FUNCTION = "deleteActionFunction";
    public static final String PERMISSION_ALL_OBJECTS_FUNCTION = "allObjectsFunction";
    public static final String PERMISSION_PREPARE_BATCH_FUNCTION = "prepareBatchFunction";
    public static final String PERMISSION_SYS_ADMIN_FUNCTION = "sysAdminFunction";

    //TODO

    public static LinkedHashMap<String, String> getMap() {
        LinkedHashMap<String, String> valueMap = new LinkedHashMap();
        valueMap.put(ROLE_SUPERADMIN, "Super Admin");
        valueMap.put(ROLE_ADMIN, "Admin");
        valueMap.put(ROLE_USER, "User");
        return valueMap;
    }
}
