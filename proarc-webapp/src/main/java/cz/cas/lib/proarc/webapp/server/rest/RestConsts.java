package cz.cas.lib.proarc.webapp.server.rest;

public class RestConsts {

    public static final String URL_API_VERSION_1 = "v1";
    public static final String URL_API_VERSION_2 = "v2";

    public static final String PERMISSION_FUNCTION_CHANGE_MODEL = "runChangeModelFunction";
    public static final String PERMISSION_FUNCTION_UPDATE_ALL_OBJECTS = "runUpdateAllObjectsFunction";
    public static final String PERMISSION_FUNCTION_RESTORE_OBJECTS = "runRestoreObjectsFunction";
    public static final String PERMISSION_FUNCTION_UPDATE_MODEL = "runUpdateObjectsFunction";
    public static final String PERMISSION_FUNCTION_LOCK_OBJECT = "runLockObjectFunction";
    public static final String PERMISSION_FUNCTION_UNLOCK_OBJECT = "runUnlockObjectFunction";
    public static final String PERMISSION_FUNCTION_IMPORT_TO_PROD = "importToProdFunction";
    public static final String PERMISSION_FUNCTION_CZIDLO = "czidloFunction";
    public static final String PERMISSION_FUNCTION_WF_DELETE_JOB = "wfDeleteJobFunction";
    public static final String PERMISSION_FUNCTION_IMPORT_TO_CATALOG = "importToCatalogFunction";
    public static final String PERMISSION_FUNCTION_CHANGE_OBJECTS_OWNER = "changeObjectsOwnerFunction";
    public static final String PERMISSION_FUNCTION_DEVICE = "deviceFunction";
    public static final String PERMISSION_FUNCTION_CHANGE_PAGES = "changePagesFunction";
    public static final String PERMISSION_FUNCTION_WF_CREATE_JOB = "wfCreateJobFunction";
    public static final String PERMISSION_FUNCTION_CREATE_USER = "createUserFunction";
    public static final String PERMISSION_FUNCTION_UPDATE_USER = "updateUserFunction";
    public static final String PERMISSION_FUNCTION_DELETE_USER = "deleteUserFunction";
    public static final String PERMISSION_FUNCTION_SOLR = "solrFunction";
    public static final String PERMISSION_FUNCTION_DELETE_ACTION = "deleteActionFunction";
    public static final String PERMISSION_FUNCTION_ALL_OBJECTS = "allObjectsFunction";
    public static final String PERMISSION_FUNCTION_PREPARE_BATCH = "prepareBatchFunction";
    public static final String PERMISSION_FUNCTION_SYS_ADMIN = "sysAdminFunction";

    public static final String FIELD_MODELOBJECT = "MetaModelRecord";

    public static final String ERR_IS_LOCKED = "Err_is_locked";
    public static final String ERR_CHANGING_MODEL_FAILED = "Err_changing_model_failed"; // 1 parameter
    public static final String ERR_UNLOCKING_OBJECT_FAILED = "Err_unlocking_object_failed"; // 1 parameter
    public static final String ERR_ADDING_REFERENCE_FAILED = "Err_adding_reference_failed"; // 1 parameter
    public static final String ERR_MISSING_PARAMETER = "Err_missing_parameter"; // 1 parameter
    public static final String ERR_MISSING_PARAMETERS = "Err_missing_parameters"; // 2 parameters
    public static final String ERR_SAME_PID_AND_PARENT = "Err_same_pid_and_parent"; // 2 parameters
    public static final String ERR_DUPLICATES = "Err_duplicates"; // 1 parameter
    public static final String ERR_NO_PERMISSION = "Err_no_permission"; // 0 parameters
    public static final String ERR_IN_GETTING_CHILDREN = "Err_in_getting_children";
    public static final String ERR_UNSUPPORTED_VALUE = "Err_unsupported_value"; // 1 parameter
    public static final String ERR_BATCH_CANNOT_BE_STOPED = "Err_batch_cannot_be_stoped"; // 0 parameters
    public static final String STATUS_LOCKED = "locked";
    public static final String STATUS_DONT_BE_IGNORED = "dontIgnored";
    public static final String ERR_USER_WITHOUT_ORGANIZATION = "UserResource_User_without_organization";
    public static final String ERR_UNKNOWN_USER = "Err_Unknown_user"; // 0 parameters

    public static final String ERR_SOFTWARE_IN_USE= "SoftwareResource_Delete_InUse_Msg";

}
