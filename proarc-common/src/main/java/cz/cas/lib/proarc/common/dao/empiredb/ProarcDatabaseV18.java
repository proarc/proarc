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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.dao.empiredb;

import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireUtils.EnhancedDBTable;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.ValueType;
import cz.cas.lib.proarc.common.workflow.profile.Way;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.empire.data.DataMode;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBCmdType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBDatabase;
import org.apache.empire.db.DBDatabaseDriver;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.DBTableColumn;
import org.apache.empire.db.exceptions.QueryFailedException;
import org.apache.empire.db.postgresql.DBDatabaseDriverPostgreSQL;

/**
 * Database schema version 18. It adds user permission.
 *
 * <p><b>Warning:</b> declare sequence names the same way like PostgreSql
 * ({@code {tablename}_{column_name}_seq}).
 *
 * @author Lukas Sykora
 */
@Deprecated
public class ProarcDatabaseV18 extends DBDatabase {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(ProarcDatabaseV18.class.getName());
    /** the schema version */
    public static final int VERSION = 18;

    public final ProarcVersionTable tableProarcVersion = new ProarcVersionTable(this);
    public final BatchTable tableBatch = new BatchTable(this);
    public final BatchItemTable tableBatchItem = new BatchItemTable(this);
    public final UserTable tableUser = new UserTable(this);
    public final UserGroupTable tableUserGroup = new UserGroupTable(this);
    public final UserSettingTable tableUserSetting = new UserSettingTable(this);
    public final GroupMemberTable tableGroupMember = new GroupMemberTable(this);
    public final GroupPermissionTable tableGroupPermission = new GroupPermissionTable(this);
    public final WorkflowJobTable tableWorkflowJob = new WorkflowJobTable(this);
    public final WorkflowTaskTable tableWorkflowTask = new WorkflowTaskTable(this);
    public final WorkflowMaterialInTaskTable tableWorkflowMaterialInTask = new WorkflowMaterialInTaskTable(this);
    public final WorkflowParameterTable tableWorkflowParameter = new WorkflowParameterTable(this);
    public final WorkflowMaterialTable tableWorkflowMaterial = new WorkflowMaterialTable(this);
    public final WorkflowFolderTable tableWorkflowFolder = new WorkflowFolderTable(this);
    public final WorkflowDigObjTable tableWorkflowDigObj = new WorkflowDigObjTable(this);
    public final WorkflowPhysicalDocTable tableWorkflowPhysicalDoc = new WorkflowPhysicalDocTable(this);

    // relations
    public final DBRelation relationWorkflowJob_ParentId_Fk;
    public final DBRelation relationWorkflowMaterialInTask_MaterialId_Fk;
    public final DBRelation relationWorkflowMaterialInTask_TaskId_Fk;

    public static int upgradeToVersion19(
            int currentSchemaVersion, ProarcDatabase schema,
            Connection conn, EmpireConfiguration conf) throws SQLException {

        if (currentSchemaVersion < VERSION) {
            LOG.log(Level.INFO, "Upgrading ProArc schema from version " + currentSchemaVersion + ".");
            currentSchemaVersion = ProarcDatabaseV17.upgradeToVersion18(currentSchemaVersion, conn, conf);
        }
        if (currentSchemaVersion > VERSION) {
            // ignore higher versions
            return currentSchemaVersion;
        } else if (currentSchemaVersion != VERSION) {
            throw new SQLException("Cannot upgrade from schema version " + currentSchemaVersion);
        }
//        ProarcDatabaseV19 schema = new ProarcDatabaseV19();
        try {
            schema.open(conf.getDriver(), conn);
            upgradeDdl(schema, conn);
            LOG.log(Level.INFO, "Upgrading ProArc schema from version " + currentSchemaVersion + ".");
            int schemaVersion = schema.initVersion(conn, VERSION);

            conn.commit();
            return schemaVersion;
        } finally {
//            schema.close(conn);
        }
    }

    private static void upgradeDdl(ProarcDatabase schema, Connection conn) throws SQLException {
        try {
            conn.setAutoCommit(true);
            DBDatabaseDriver driver = schema.getDriver();
            DBSQLScript script = new DBSQLScript();

            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.changeObjectsOwnerFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.deviceFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.changePagesFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.wfCreateJobFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.createUserFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.updateUserFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.updateUserPermissionFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.deleteUserFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.solrFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.deleteActionFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.allObjectsFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.prepareBatchFunction, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.sysAdminFunction, script);

            LOG.fine(script.toString());
            script.run(driver, conn);
        } finally {
            conn.setAutoCommit(false);
        }
    }

    public static class ProarcVersionTable extends DBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn id;
        public final DBTableColumn schemaVersion;

        public ProarcVersionTable(DBDatabase db) {
            super("PROARC_VERSION", db);
            id = addColumn("ID", DataType.INTEGER, 0, true);
            schemaVersion = addColumn("SCHEMA_VERSION", DataType.INTEGER, 0, true);
            setPrimaryKey(id);
        }

    }

    public static class BatchTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn id;
        public final DBTableColumn folder;
        public final DBTableColumn title;
        public final DBTableColumn userId;
        public final DBTableColumn state;
        public final DBTableColumn parentPid;
        public final DBTableColumn estimateItemNumber;
        public final DBTableColumn create; // date of creation
        public final DBTableColumn updated; // date of update
        public final DBTableColumn timestamp; // optimistic lock
        public final DBTableColumn itemUpdated; // date of items update
        public final DBTableColumn device; // digitization device ID (PID)
        public final DBTableColumn software; // digitization software ID (PID)
        public final DBTableColumn generateIndices;
        public final DBTableColumn log;
        public final DBTableColumn profileId;
        public final DBTableColumn priority;
        public final DBTableColumn params;

        public BatchTable(DBDatabase db) {
            super("PROARC_BATCH", db);
            id = addSequenceColumn("ID");
            folder = addColumn("FOLDER", DataType.CLOB, 0, true);
            title = addColumn("TITLE", DataType.TEXT, 2000, true);
            userId = addColumn("USER_ID", DataType.INTEGER, 0, true);
            state = addColumn("STATE", DataType.TEXT, 20, true);
            state.setBeanPropertyName("stateAsString");
            parentPid = addColumn("PARENT_PID", DataType.TEXT, 41, false);
            estimateItemNumber = addColumn("ESTIMATE_NUMBER", DataType.INTEGER, 0, false);
            estimateItemNumber.setBeanPropertyName("estimateItemNumber");
            create = addColumn("CREATE", DataType.DATETIME, 0, true);
            timestamp = addTimestampColumn("TIMESTAMP");
            device = addColumn("DEVICE", DataType.TEXT, 2000, false);
            software = addColumn("SOFTWARE", DataType.TEXT, 2000, false);
            generateIndices = addColumn("GENERATE_INDICES", DataType.BOOL, 0, false);
            log = addColumn("LOG", DataType.CLOB, 0, false);
            profileId = addColumn("PROFILE_ID", DataType.TEXT, 2000, false);
            priority = addColumn("PRIORITY", DataType.TEXT, 50, false);
            params = addColumn("PARAMS", DataType.TEXT, 10000, false);
            itemUpdated = addColumn("ITEM_UPDATED", DataType.DATETIME, 0, false);
            itemUpdated.setBeanPropertyName("itemUpdated");
            updated = addColumn("UPDATED", DataType.DATETIME, 0, false);
            updated.setBeanPropertyName("updated");
            setPrimaryKey(id);
            addIndex(String.format("%s_IDX", getName()), false, new DBColumn[] { create, state, title, userId });
        }

    }

    public static final class BatchItemTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn id;
        public final DBTableColumn batchId;
        public final DBTableColumn pid; // prefix + UUID
        public final DBTableColumn dsId; // datastream
        public final DBTableColumn file; // target or source; subpath from users.home
        public final DBTableColumn state;
        public final DBTableColumn type; // item type: DATASTREAM, FILE, OBJECT
        public final DBTableColumn log; // logging
        public final DBTableColumn timestamp; // optimistic lock

        public BatchItemTable(DBDatabase db) {
            super("PROARC_BATCH_ITEM", db);
            id = addSequenceColumn("ID");
            batchId = addColumn("BATCH_ID", DataType.INTEGER, 0, true);
            pid = addColumn("PID", DataType.TEXT, 50, false);
            dsId = addColumn("DS_ID", DataType.TEXT, 200, false);
            file = addColumn("FILE", DataType.TEXT, 2000, false);
            state = addColumn("STATE", DataType.TEXT, 100, true);
            type = addColumn("TYPE", DataType.TEXT, 100, false);
            type.setBeanPropertyName("typeAsString");
            type.setOptions(toOptions(BatchItem.Type.values()));
            log = addColumn("LOG", DataType.CLOB, 0, false);
            timestamp = addTimestampColumn("TIMESTAMP");
            setPrimaryKey(id);
            addIndex(String.format("%s_UNIQ_IDX", getName()), true, new DBColumn[] { batchId, pid, dsId, type });
            addIndex(String.format("%s_IDX", getName()), false, new DBColumn[] { batchId, pid, dsId, state, type });
        }

    }

    public static final class UserTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn id;
        public final DBTableColumn username;
        public final DBTableColumn passwd;
        public final DBTableColumn forename;
        public final DBTableColumn surname;
        public final DBTableColumn email;
        public final DBTableColumn state;
        public final DBTableColumn created;
        public final DBTableColumn lastLogin;
        public final DBTableColumn home;
        public final DBTableColumn organization;
        public final DBTableColumn role;
        /** group to use as owner for newly created objects */
        public final DBTableColumn defaultGroup;
        /** group that can contain single member; it can hold overridden permissions */
        public final DBTableColumn userGroup;
        /** use to identify external user. */
        public final DBTableColumn remoteName;
        /** type of the remote user null(PROARC), DESA, LDAP, ... */
        public final DBTableColumn remoteType;
        public final DBTableColumn timestamp;
        public final DBTableColumn changeModelFunction;
        public final DBTableColumn updateModelFunction;
        public final DBTableColumn lockObjectFunction;
        public final DBTableColumn unlockObjectFunction;
        public final DBTableColumn importToProdFunction;
        public final DBTableColumn czidloFunction;
        public final DBTableColumn wfDeleteJobFunction;
        public final DBTableColumn importToCatalogFunction;

        public UserTable(DBDatabase db) {
            super("PROARC_USERS", db);
            id = addSequenceColumn("USERID");
            id.setBeanPropertyName("id");
            username = addColumn("USERNAME", DataType.TEXT, 255, true);
            username.setBeanPropertyName("userName");
            passwd = addColumn("PASSWD", DataType.TEXT, 255, false);
            passwd.setBeanPropertyName("userPasswordDigest");
            forename = addColumn("FORENAME", DataType.TEXT, 100, false);
            surname = addColumn("SURNAME", DataType.TEXT, 255, true);
            email = addColumn("EMAIL", DataType.TEXT, 255, false);
            state = addColumn("STATUS", DataType.TEXT, 20, false);
            created = addColumn("CREATED", DataType.DATETIME, 0, DataMode.NotNull, SYSDATE);
            lastLogin = addColumn("LASTLOGIN", DataType.DATETIME, 0, false);
            lastLogin.setBeanPropertyName("lastLogin");
            home = addColumn("HOME", DataType.TEXT, 2000, true);
            home.setBeanPropertyName("userHome");
            defaultGroup = addColumn("DEFAULT_GROUP", DataType.INTEGER, 0, false);
            userGroup = addColumn("USER_GROUP", DataType.INTEGER, 0, false);
            remoteName = addColumn("REMOTE_NAME", DataType.TEXT, 255, false);
            remoteType = addColumn("REMOTE_TYPE", DataType.TEXT, 2000, false);
            timestamp = addTimestampColumn("TIMESTAMP");
            organization = addColumn("ORGANIZATION", DataType.TEXT, 100, false);
            role = addColumn("ROLE", DataType.TEXT, 100, false);
            changeModelFunction = addColumn("CHANGE_MODEL_FUNCTION", DataType.BOOL, 0, false);
            updateModelFunction = addColumn("UPDATE_MODEL_FUNCTION", DataType.BOOL, 0, false);
            lockObjectFunction = addColumn("LOCK_OBJECT_FUNCTION", DataType.BOOL, 0, false);
            unlockObjectFunction = addColumn("UNLOCK_OBJECT_FUNCTION", DataType.BOOL, 0, false);
            importToProdFunction = addColumn("IMPORT_TO_PROD_FUNCTION", DataType.BOOL, 0, false);
            czidloFunction = addColumn("CZIDLO_FUNCTION", DataType.BOOL, 0, false);
            wfDeleteJobFunction = addColumn("WF_DELETE_JOB_FUNCTION", DataType.BOOL, 0, false);
            importToCatalogFunction = addColumn("IMPORT_TO_CATALOG_FUNCTION", DataType.BOOL, 0, false);
            setPrimaryKey(id);
            addIndex(String.format("%s_%s_IDX", getName(), username.getName()), true, new DBColumn[] { username });
        }

    }

    public static final class UserGroupTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        public final DBTableColumn id;
        /**
         * The unique group name. Used as fedora PID object ID.
         */
        public final DBTableColumn groupname;
        public final DBTableColumn title;
        /** use to identify group of external users. */
        public final DBTableColumn remoteName;
        /** type of the remote group null(PROARC), DESA, LDAP, ... */
        public final DBTableColumn remoteType;
        public final DBTableColumn created;
        public final DBTableColumn timestamp;

        public UserGroupTable(DBDatabase db) {
            super("PROARC_GROUPS", db);
            id = addSequenceColumn("GROUPID");
            id.setBeanPropertyName("id");
            groupname = addColumn("NAME", DataType.TEXT, 64, true);
            title = addColumn("TITLE", DataType.TEXT, 255, false);
            remoteName = addColumn("REMOTE_NAME", DataType.TEXT, 255, false);
            remoteType = addColumn("REMOTE_TYPE", DataType.TEXT, 2000, false);
            created = addColumn("CREATED", DataType.DATETIME, 0, DataMode.NotNull, SYSDATE);
            timestamp = addTimestampColumn("TIMESTAMP");
            setPrimaryKey(id);
            // unique group name
            addIndex(String.format("%s_%s_IDX", getName(), groupname.getName()), true, new DBColumn[] { groupname });
        }

    }

    public static final class UserSettingTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn id;
        public final DBTableColumn userId;
        public final DBTableColumn userSetting;
        public final DBTableColumn timestamp;

        public UserSettingTable(DBDatabase db) {
            super("PROARC_USER_SETTING", db);
            id = addSequenceColumn("ID");
            userId = addColumn("USER_ID", DataType.INTEGER, 0, true);
            userSetting = addColumn("USER_SETTING", DataType.CLOB, 0, false);
            timestamp = addTimestampColumn("TIMESTAMP");
            setPrimaryKey(id);
            // unique group name
            addIndex(String.format("%s_%s_IDX", getName(), userId.getName()), true, new DBColumn[] { userId });
        }

    }

    public static final class GroupMemberTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        public final DBTableColumn groupid;
        public final DBTableColumn userid;

        public GroupMemberTable(DBDatabase db) {
            super("PROARC_GROUP_MEMBERS", db);
            groupid = addColumn("GROUPID", DataType.INTEGER, 0, true);
            userid = addColumn("USERID", DataType.INTEGER, 0, true);
            setPrimaryKey(groupid, userid);
        }

    }

    public static final class GroupPermissionTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        public final DBTableColumn groupid;
        public final DBTableColumn objectid;
        public final DBTableColumn permissionid;
        /** type to override inherited permission in user group. Options: null, disabled, enabled. */
        public final DBTableColumn type;

        public GroupPermissionTable(DBDatabase db) {
            super("PROARC_GROUP_PERMISSIONS", db);
            groupid = addColumn("GROUPID", DataType.INTEGER, 0, true);
            objectid = addColumn("OBJECTID", DataType.TEXT, 2000, false);
            permissionid = addColumn("PERMISSIONID", DataType.TEXT, 2000, true);
            type = addColumn("TYPE", DataType.TEXT, 255, false);
        }

    }

    public static final class WorkflowJobTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        public final DBTableColumn created;
        public final DBTableColumn id;
        public final DBTableColumn parentId;
        public final DBTableColumn financed;
        public final DBTableColumn label;
        public final DBTableColumn note;
        public final DBTableColumn ownerId;
        public final DBTableColumn priority;
        public final DBTableColumn profileName;
        public final DBTableColumn state;
        public final DBTableColumn model;
        public final DBTableColumn timestamp;

        public WorkflowJobTable(DBDatabase db) {
            super("PROARC_WF_JOB", db);
            id = addSequenceColumn("ID");
            parentId = addColumn("PARENT_ID", DataType.INTEGER, 0, false);
            ownerId = addColumn("OWNER_ID", DataType.INTEGER, 0, false);
            profileName = addColumn("PROFILE_NAME", DataType.TEXT, 500, true);
            model = addColumn("MODEL", DataType.TEXT, 500, false);
            state = addColumn("STATE", DataType.TEXT, 100, true);
            state.setOptions(toOptions(Job.State.values()));
            state.setBeanPropertyName("stateAsString");
            priority = addColumn("PRIORITY", DataType.INTEGER, 0, true);
            label = addColumn("LABEL", DataType.TEXT, 2000, true);
            financed = addColumn("FINANCED", DataType.TEXT, 2000, false);
            note = addColumn("NOTE", DataType.TEXT, 2000, false);
            created = addColumn("CREATED", DataType.DATETIME, 0, true);
            timestamp = addTimestampColumn("TIMESTAMP");
            setPrimaryKey(id);
//            addIndex(String.format("%s_IDX", getName()), false, new DBColumn[] {
//                ownerId, created, timestamp, state, priority, financed });
        }
    }

    public static final class WorkflowTaskTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        public final DBTableColumn created;
        public final DBTableColumn id;
        public final DBTableColumn jobId;
        public final DBTableColumn note;
        public final DBTableColumn ownerId;
        public final DBTableColumn priority;
//        public final DBTableColumn queueNumber;
        public final DBTableColumn state;
        /** The name of a task type in workflow profile. */
        public final DBTableColumn typeRef;
        public final DBTableColumn timestamp;
        public final DBTableColumn order;

        public WorkflowTaskTable(DBDatabase db) {
            super("PROARC_WF_TASK", db);
            id = addSequenceColumn("ID");
            typeRef = addColumn("TYPE_REF", DataType.TEXT, 500, true);
            jobId = addColumn("JOB_ID", DataType.INTEGER, 0, true);
            ownerId = addColumn("OWNER_ID", DataType.INTEGER, 0, false);
            state = addColumn("STATE", DataType.TEXT, 100, true);
            state.setOptions(toOptions(Task.State.values()));
            state.setBeanPropertyName("stateAsString");
            priority = addColumn("PRIORITY", DataType.INTEGER, 0, true);
//            queueNumber = addColumn("QUEUE_NUMBER", DataType.DECIMAL, 0, true);
            note = addColumn("NOTE", DataType.TEXT, 2000, false);
            created = addColumn("CREATED", DataType.DATETIME, 0, true);
            timestamp = addTimestampColumn("TIMESTAMP");
            order = addColumn("ORDER", DataType.INTEGER, 0, false);
            setPrimaryKey(id);
        }
    }

    public static final class WorkflowParameterTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn taskId;
        /** The name of a parameter type in workflow profile. */
        public final DBTableColumn paramRef;
        public final DBTableColumn valueType;
        public final DBTableColumn value;
        public final DBTableColumn number;
        public final DBTableColumn dateTime;

        public WorkflowParameterTable(DBDatabase db) {
            super("PROARC_WF_PARAMETER", db);
            taskId = addColumn("TASK_ID", DataType.INTEGER, 0, true);
            paramRef = addColumn("PARAM_REF", DataType.TEXT, 500, true);
            valueType = addColumn("VALUE_TYPE", DataType.TEXT, 20, true);
            valueType.setOptions(toOptions(ValueType.values()));
            valueType.setBeanPropertyName("valueTypeAsString");
            value = addColumn("VALUE_STRING", DataType.TEXT, 2000, false);
            number = addColumn("VALUE_NUMBER", DataType.DECIMAL, 20.9, false);
            dateTime = addColumn("VALUE_DATETIME", DataType.DATETIME, 0, false);
            dateTime.setBeanPropertyName("valueDateTime");
        }
    }

    public static final class WorkflowMaterialTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn id;
        /** The description of a material's value */
        public final DBTableColumn label;
        public final DBTableColumn name;
        public final DBTableColumn note;
        public final DBTableColumn state;
        public final DBTableColumn type;

        public WorkflowMaterialTable(DBDatabase db) {
            super("PROARC_WF_MATERIAL", db);
            id = addSequenceColumn("ID");
            type = addColumn("TYPE", DataType.TEXT, 100, true);
            type.setOptions(toOptions(MaterialType.values()));
            type.setBeanPropertyName("typeAsString");
            state = addColumn("STATE", DataType.TEXT, 100, false);
            name = addColumn("NAME", DataType.TEXT, 500, true);
            label = addColumn("LABEL", DataType.TEXT, 2000, false);
            note = addColumn("NOTE", DataType.TEXT, 2000, false);
            setPrimaryKey(id);
        }
    }

    public static final class WorkflowFolderTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn materialId;
        public final DBTableColumn path;

        public WorkflowFolderTable(DBDatabase db) {
            super("PROARC_WF_FOLDER", db);
            materialId = addColumn("MATERIAL_ID", DataType.INTEGER, 0, true);
            materialId.setBeanPropertyName("id");
            path = addColumn("PATH", DataType.TEXT, 2000, false);
            setPrimaryKey(materialId);
        }
    }

    public static final class WorkflowDigObjTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn materialId;
        public final DBTableColumn pid;

        public WorkflowDigObjTable(DBDatabase db) {
            super("PROARC_WF_DIGITAL_DOCUMENT", db);
            materialId = addColumn("MATERIAL_ID", DataType.INTEGER, 0, true);
            materialId.setBeanPropertyName("id");
            pid = addColumn("PID", DataType.TEXT, 100, false);
            setPrimaryKey(materialId);
        }
    }

    public static final class WorkflowPhysicalDocTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn materialId;
        public final DBTableColumn barcode;
        public final DBTableColumn field001;
        public final DBTableColumn rdczId;
        public final DBTableColumn signature;
        /** The URL to a catalog. */
        public final DBTableColumn source;
        /** MODS. */
        public final DBTableColumn metadata;
        public final DBTableColumn detail;
        public final DBTableColumn issue;
        /** The sigla format {@code [A-Z][A-Z][A-Z][0-9][0-9][0-9]}. */
        public final DBTableColumn sigla;
        public final DBTableColumn volume;
        public final DBTableColumn year;
        public final DBTableColumn edition;

        public WorkflowPhysicalDocTable(DBDatabase db) {
            super("PROARC_WF_PHYSICAL_DOCUMENT", db);
            materialId = addColumn("MATERIAL_ID", DataType.INTEGER, 0, true);
            materialId.setBeanPropertyName("id");
            rdczId = addColumn("RDCZ_ID", DataType.INTEGER, 0, false);
            barcode = addColumn("BARCODE", DataType.TEXT, 100, false);
            field001 = addColumn("FIELD001", DataType.TEXT, 100, false);
            signature = addColumn("SIGNATURE", DataType.TEXT, 2000, false);
            source = addColumn("SOURCE", DataType.TEXT, 2000, false);
            metadata = addColumn("METADATA", DataType.CLOB, 0, false);
            detail = addColumn("DETAIL", DataType.TEXT, 200, false);
            issue = addColumn("ISSUE", DataType.TEXT, 100, false);
            sigla = addColumn("SIGLA", DataType.TEXT, 6, false);
            volume = addColumn("VOLUME", DataType.TEXT, 100, false);
            year = addColumn("YEAR", DataType.TEXT, 100, false);
            edition = addColumn("EDITION", DataType.TEXT, 2000, false);
            setPrimaryKey(materialId);
        }
    }

    public static final class WorkflowMaterialInTaskTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn materialId;
        public final DBTableColumn taskId;
        public final DBTableColumn way;

        public WorkflowMaterialInTaskTable(DBDatabase db) {
            super("PROARC_WF_MATERIAL_IN_TASK", db);
            taskId = addColumn("TASK_ID", DataType.INTEGER, 0, true);
            materialId = addColumn("MATERIAL_ID", DataType.INTEGER, 0, true);
            way = addColumn("WAY", DataType.TEXT, 100, true);
            way.setBeanPropertyName("wayAsString");
            way.setOptions(toOptions(Way.values()));
            setPrimaryKey(taskId, materialId, way);
        }
    }

    public ProarcDatabaseV18() {
        addRelation(tableBatch.userId.referenceOn(tableUser.id));
        addRelation(tableBatchItem.batchId.referenceOn(tableBatch.id));
        // users
        addRelation(tableUser.defaultGroup.referenceOn(tableUserGroup.id));
        addRelation(tableGroupMember.groupid.referenceOn(tableUserGroup.id));
        addRelation(tableGroupMember.userid.referenceOn(tableUser.id));
        addRelation(tableUserSetting.userId.referenceOn(tableUser.id));
        addRelation(tableGroupPermission.groupid.referenceOn(tableUserGroup.id));
        // workflow
        addRelation(tableWorkflowJob.ownerId.referenceOn(tableUser.id));
        relationWorkflowJob_ParentId_Fk = addRelation(tableWorkflowJob.parentId.referenceOn(tableWorkflowJob.id));
        addRelation(tableWorkflowTask.jobId.referenceOn(tableWorkflowJob.id));
        addRelation(tableWorkflowTask.ownerId.referenceOn(tableUser.id));
        addRelation(tableWorkflowParameter.taskId.referenceOn(tableWorkflowTask.id));
        addRelation(tableWorkflowFolder.materialId.referenceOn(tableWorkflowMaterial.id));
        addRelation(tableWorkflowDigObj.materialId.referenceOn(tableWorkflowMaterial.id));
        addRelation(tableWorkflowPhysicalDoc.materialId.referenceOn(tableWorkflowMaterial.id));
        relationWorkflowMaterialInTask_MaterialId_Fk =
                addRelation(tableWorkflowMaterialInTask.materialId.referenceOn(tableWorkflowMaterial.id));
        relationWorkflowMaterialInTask_TaskId_Fk =
                addRelation(tableWorkflowMaterialInTask.taskId.referenceOn(tableWorkflowTask.id));
    }

//    void init(EmpireConfiguration conf) throws SQLException {
//        DBDatabaseDriver drv = conf.getDriver();
//        Connection conn = conf.getConnection();
//        open(drv, conn);
//        try {
//            int schemaVersion = schemaExists(this, conn);
//            if (schemaVersion > 0) {
//                LOG.log(Level.INFO, "Upgrading ProArc schema from version " + schemaVersion + ".");
//                schemaVersion = ProarcDatabaseV17.upgradeToVersion18(
//                        schemaVersion, this, conn, conf);
//                if (schemaVersion != VERSION) {
//                    throw new SQLException("Invalid schema version " + schemaVersion);
//                }
//            } else {
//                createSchema(this, conn);
//            }
//        } finally {
//            conn.close();
//        }
//    }

    static int schemaExists(ProarcDatabaseV18 db, Connection c) {
        try {
            DBCommand cmd = db.createCommand();
            cmd.select(db.tableProarcVersion.schemaVersion);
            int version = db.querySingleInt(cmd, -1, c);
            return version;
        } catch (QueryFailedException ex) {
            return -1;
        }
    }

    private static void createSchema(ProarcDatabaseV18 db, Connection conn) throws SQLException {
        if (db.getDriver() instanceof DBDatabaseDriverPostgreSQL) {
            conn.setAutoCommit(true);
        }
        DBSQLScript script = new DBSQLScript();
        db.getCreateDDLScript(db.getDriver(), script);
        LOG.fine(script.toString());
        script.run(db.getDriver(), conn);
        db.initVersion(conn, null);
        db.commit(conn);
        conn.setAutoCommit(false);
    }

    int initVersion(Connection conn, Integer oldVersion) {
        ProarcDatabaseV18 db = this;
        DBRecord dbRecord = new DBRecord();
        if (oldVersion != null) {
            dbRecord.init(db.tableProarcVersion, new Integer[] {0}, false);
        } else {
            dbRecord.create(db.tableProarcVersion);
            dbRecord.setValue(db.tableProarcVersion.id, 0);
        }

        dbRecord.setValue(db.tableProarcVersion.schemaVersion, VERSION);
        dbRecord.update(conn);
        return VERSION;
    }

}
