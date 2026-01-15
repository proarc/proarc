/*
 * Copyright (C) 2014 Jan Pokorsky
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
import cz.cas.lib.proarc.common.user.UserUtil;
import java.sql.SQLException;
import java.util.logging.Logger;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBContext;
import org.apache.empire.db.DBDDLGenerator;
import org.apache.empire.db.DBDatabase;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBRelation.DBReference;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.DBTableColumn;
import org.apache.empire.db.exceptions.QueryFailedException;
import org.apache.empire.dbms.postgresql.DBMSHandlerPostgreSQL;

/**
 * Database schema version 1.
 *
 * @author Jan Pokorsky
 * @deprecated Replaced with {@link ProarcDatabase}. <b>Use only for tests and upgrade purposes!</b>
 */
@Deprecated
public class ProarcDatabaseV1 extends DBDatabase {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(ProarcDatabaseV1.class.getName());
    /** the schema version */
    public static final int VERSION = 1;

    public final ProarcVersionTable tableProarcVersion = new ProarcVersionTable(this);
    public final BatchTable tableBatch = new BatchTable(this);
    public final BatchItemTable tableBatchItem = new BatchItemTable(this);
    public final UserTable tableUser = new UserTable(this);
    public final UserGroupTable tableUserGroup = new UserGroupTable(this);
    public final GroupMemberTable tableGroupMember = new GroupMemberTable(this);
    public final GroupPermissionTable tableGroupPermission = new GroupPermissionTable(this);
    public final TomcatUserTable tableTomcatUser = new TomcatUserTable(this);
    public final TomcatRoleTable tableTomcatRole = new TomcatRoleTable(this);
    public final DBRelation relUsername2TomcatUser;
    public final DBRelation relUsername2TomcatUserAlias;
    public final DBRelation relTomcatRoleUser2TomcatUser;
    public final DBRelation relTomcatRoleUser2TomcatUserAlias;

    public static int upgradeToVersion2(
            int currentSchemaVersion, DBContext context,
            EmpireConfiguration conf) throws SQLException {

        if (currentSchemaVersion > VERSION) {
            // ignore higher versions
            return currentSchemaVersion;
        } else if (currentSchemaVersion != VERSION) {
            throw new SQLException("Cannot upgrade from schema version " + currentSchemaVersion);
        }
        ProarcDatabaseV2 schema = new ProarcDatabaseV2();
        ProarcDatabaseV1 schemaV1 = new ProarcDatabaseV1();
        try {
            schema.open(context);
            // the driver instance must be same!
            schemaV1.open(context);
            upgradeDdl(schema, schemaV1, context);
            // copy passwords from tomcat
//            upgradeUsers(schema, schemaV1, context);
            // update user groups to comply with new columns
            upgradeGroups(schema, schemaV1, context);

            int schemaVersion = schema.initVersion(context, VERSION);

            context.commit();

            return schemaVersion;
        } finally {
            schema.close(context);
            schemaV1.close(context);
        }
    }

    private static void upgradeDdl(ProarcDatabaseV2 schema, ProarcDatabaseV1 schemaV1, DBContext context) throws SQLException {
        try {
            context.getConnection().setAutoCommit(true);

            DBSQLScript script = new DBSQLScript(context);
            // add UserTable columns
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUser.passwd, script);
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUser.remoteName, script);
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUser.remoteType, script);
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUser.defaultGroup, script);
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUser.userGroup, script);
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUser.timestamp, script);
            // add UserGroupTable columns
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUserGroup.title, script);
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUserGroup.remoteName, script);
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUserGroup.remoteType, script);
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUserGroup.created, script);
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableUserGroup.timestamp, script);
            // add GroupPermissionTable columns
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.CREATE, schema.tableGroupPermission.type, script);
            script.executeAll();
            // drop table references
            script.clear();
            EmpireUtils.dropRelation(schemaV1.relUsername2TomcatUser, context, script);
            EmpireUtils.dropRelation(schemaV1.relUsername2TomcatUserAlias, context, script);
            EmpireUtils.dropRelation(schemaV1.relTomcatRoleUser2TomcatUser, context, script);
            EmpireUtils.dropRelation(schemaV1.relTomcatRoleUser2TomcatUserAlias, context, script);

            script.executeAll();
        } finally {
            context.getConnection().setAutoCommit(false);
        }
    }

    private static void upgradeGroups(ProarcDatabaseV2 schema, ProarcDatabaseV1 schemaV1, DBContext context) {
        DBCommand cmd = schema.createCommand();
        cmd.set(schema.tableUserGroup.groupname.to(UserUtil.toGroupPid("admin")));
        cmd.set(schema.tableUserGroup.title.to("Administrátoři"));
        cmd.where(schema.tableUserGroup.groupname.is("Administrátoři"));

        context.executeUpdate(cmd);
    }

//    private static void upgradeUsers(ProarcDatabaseV2 schema, ProarcDatabaseV1 schemaV1, DBContext context) {
//        DBRecord dbr = new DBRecord(context, schema.tableUser);
//        DBReader dbReader = new DBReader(context);
//        DBCommand cmd = schema.createCommand();
//        cmd.select(schema.tableUser.getColumns());
//        cmd.select(schemaV1.tableTomcatUser.getColumns());
//        cmd.join(schema.tableUser.username, schemaV1.tableTomcatUser.username);
//        try {
//            dbReader.open(cmd);
//            while (dbReader.moveNext()) {
//                dbReader.initRecord(schema.tableUser, dbr);
//                dbr.setValue(schema.tableUser.passwd, dbReader.getValue(schemaV1.tableTomcatUser.userpass));
//                dbr.update(conn);
//            }
//        } finally {
//            dbReader.close();
//        }
//    }

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
        public final DBTableColumn timestamp; // optimistic lock
        public final DBTableColumn device; // digitization device ID (PID)
        public final DBTableColumn generateIndices;
        public final DBTableColumn log;
//        public final DBTableColumn model; // default batch item model

        public BatchTable(DBDatabase db) {
            super("PROARC_BATCH", db);
            id = addSequenceColumn("ID");
            folder = addColumn("FOLDER", DataType.CLOB, 0, true);
            title = addColumn("TITLE", DataType.VARCHAR, 2000, true);
            userId = addColumn("USER_ID", DataType.INTEGER, 0, true);
            state = addColumn("STATE", DataType.VARCHAR, 20, true);
            state.setBeanPropertyName("stateAsString");
            parentPid = addColumn("PARENT_PID", DataType.VARCHAR, 41, false);
            estimateItemNumber = addColumn("ESTIMATE_NUMBER", DataType.INTEGER, 0, false);
            estimateItemNumber.setBeanPropertyName("estimateItemNumber");
            create = addColumn("CREATE", DataType.DATETIME, 0, true);
            timestamp = addTimestamp("TIMESTAMP");
            device = addColumn("DEVICE", DataType.VARCHAR, 2000, false);
            generateIndices = addColumn("GENERATE_INDICES", DataType.BOOL, 0, false);
            log = addColumn("LOG", DataType.CLOB, 0, false);
            setPrimaryKey(id);
            addIndex(String.format("%s_IDX", getName()), false, new DBColumn[] { create, state, title, userId });
        }

    }

    public static final class BatchItemTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn id;
        public final DBTableColumn batchId;
        public final DBTableColumn pid; // UUID
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
            pid = addColumn("PID", DataType.VARCHAR, 41, false);
            dsId = addColumn("DS_ID", DataType.VARCHAR, 200, false);
            file = addColumn("FILE", DataType.VARCHAR, 2000, false);
            state = addColumn("STATE", DataType.VARCHAR, 100, true);
            type = addColumn("TYPE", DataType.VARCHAR, 100, false);
            type.setBeanPropertyName("typeAsString");
            type.setOptions(toOptions(BatchItem.Type.values()));
            log = addColumn("LOG", DataType.CLOB, 0, false);
            timestamp = addTimestamp("TIMESTAMP");
            setPrimaryKey(id);
            addIndex(String.format("%s_UNIQ_IDX", getName()), true, new DBColumn[] { batchId, pid, dsId, type });
            addIndex(String.format("%s_IDX", getName()), false, new DBColumn[] { batchId, pid, dsId, state, type });
        }

    }

    public static final class UserTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;

        public final DBTableColumn id;
        public final DBTableColumn username;
        public final DBTableColumn forename;
        public final DBTableColumn surname;
        public final DBTableColumn email;
        public final DBTableColumn state;
        public final DBTableColumn created;
        public final DBTableColumn lastLogin;
        public final DBTableColumn home;
//        public final DBTableColumn timestamp;

        public UserTable(DBDatabase db) {
            super("PROARC_USERS", db);
            id = addSequenceColumn("USERID");
            username = addColumn("USERNAME", DataType.VARCHAR, 255, true);
            forename = addColumn("FORENAME", DataType.VARCHAR, 100, false);
            surname = addColumn("SURNAME", DataType.VARCHAR, 255, true);
            email = addColumn("EMAIL", DataType.VARCHAR, 255, false);
            state = addColumn("STATUS", DataType.VARCHAR, 20, false);
            created = addColumn("CREATED", DataType.DATETIME, 0, true, SYSDATE);
            lastLogin = addColumn("LASTLOGIN", DataType.DATETIME, 0, false);
            home = addColumn("HOME", DataType.VARCHAR, 2000, true);
//            timestamp = addTimestamp("TIMESTAMP");
            setPrimaryKey(id);
            addIndex(String.format("%s_%s_IDX", getName(), username.getName()), true, new DBColumn[] { username });
        }

    }

    public static final class UserGroupTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        private final DBTableColumn id;
        private final DBTableColumn groupname;

        public UserGroupTable(DBDatabase db) {
            super("PROARC_GROUPS", db);
            id = addSequenceColumn("GROUPID");
            groupname = addColumn("NAME", DataType.VARCHAR, 255, true);
            setPrimaryKey(id);
            // unique group name
            addIndex(String.format("%s_%s_IDX", getName(), groupname.getName()), true, new DBColumn[] { groupname });
        }

    }

    public static final class GroupMemberTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        private final DBTableColumn groupid;
        private final DBTableColumn userid;

        public GroupMemberTable(DBDatabase db) {
            super("PROARC_GROUP_MEMBERS", db);
            groupid = addColumn("GROUPID", DataType.INTEGER, 0, true);
            userid = addColumn("USERID", DataType.INTEGER, 0, true);
            setPrimaryKey(groupid, userid);
        }

    }

    public static final class GroupPermissionTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        private final DBTableColumn groupid;
        private final DBTableColumn objectid;
        private final DBTableColumn permissionid;

        public GroupPermissionTable(DBDatabase db) {
            super("PROARC_GROUP_PERMISSIONS", db);
            groupid = addColumn("GROUPID", DataType.INTEGER, 0, true);
            objectid = addColumn("OBJECTID", DataType.VARCHAR, 2000, false);
            permissionid = addColumn("PERMISSIONID", DataType.VARCHAR, 2000, true);

        }

    }

    public static final class TomcatUserTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        public final DBTableColumn username;
        public final DBTableColumn userpass;

        public TomcatUserTable(DBDatabase db) {
            super("TOMCAT_USERS", db);
            username = addColumn("USERNAME", DataType.VARCHAR, 255, true);
            userpass = addColumn("USERPASS", DataType.VARCHAR, 255, true);
            setPrimaryKey(username);
        }

    }

    public static final class TomcatRoleTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        public final DBTableColumn username;
        public final DBTableColumn rolename;

        public TomcatRoleTable(DBDatabase db) {
            super("TOMCAT_ROLES", db);
            username = addColumn("USERNAME", DataType.VARCHAR, 255, true);
            rolename = addColumn("ROLENAME", DataType.VARCHAR, 255, true);
            setPrimaryKey(username, rolename);
        }

    }

    public ProarcDatabaseV1() {
        addRelation(tableBatch.userId.referenceOn(tableUser.id));
        addRelation(tableBatchItem.batchId.referenceOn(tableBatch.id));
        relUsername2TomcatUser = addRelation(tableUser.username.referenceOn(tableTomcatUser.username));
        // define duplicate foreign key with alias to match the pre-EmpireDB declaration
        relUsername2TomcatUserAlias = addRelation("proarc_users_username_fkey",
                new DBReference[] {tableUser.username.referenceOn(tableTomcatUser.username)});
        addRelation(tableGroupMember.groupid.referenceOn(tableUserGroup.id));
        addRelation(tableGroupMember.userid.referenceOn(tableUser.id));
        addRelation(tableGroupPermission.groupid.referenceOn(tableUserGroup.id));
        relTomcatRoleUser2TomcatUser = addRelation(tableTomcatRole.username.referenceOn(tableTomcatUser.username));
        // define duplicate foreign key with alias to match the pre-EmpireDB declaration
        relTomcatRoleUser2TomcatUserAlias = addRelation("tomcat_roles_username_fkey",
                new DBReference[] {tableTomcatRole.username.referenceOn(tableTomcatUser.username)});
    }

    void init(EmpireConfiguration conf) throws SQLException {
        DBContext context = conf.getContext();
        try {
            if (schemaExists(this, context)) {
                // XXX upgrade
            } else {
                createSchema(this, context);
            }
        } finally {
            context.discard();
        }
    }

    static boolean schemaExists(ProarcDatabaseV1 db, DBContext context) {
        try {
            DBCommand cmd = db.createCommand();
            cmd.select(db.tableProarcVersion.schemaVersion);
            cmd.orderBy(db.tableProarcVersion.schemaVersion.desc());
            cmd.limitRows(1);

            DBReader reader = new DBReader(context);
            reader.open(cmd);

            while (reader.moveNext()) {
                int version = reader.getInt(db.tableProarcVersion.schemaVersion);
                return version > 0;
            }
        } catch (QueryFailedException ex) {
            return false;
        }
        return false;
    }

    private static void createSchema(ProarcDatabaseV1 db, DBContext context) throws SQLException {
        if (context.getDbms() instanceof DBMSHandlerPostgreSQL) {
            context.getConnection().setAutoCommit(true);
        }
        DBSQLScript script = new DBSQLScript(context);
        db.getCreateDDLScript(script);
        LOG.fine(script.toString());

        script.executeAll();
        initProarcVersion(db, context);

        context.commit();
        context.getConnection().setAutoCommit(false);
    }

    private static void initProarcVersion(ProarcDatabaseV1 db, DBContext context) {
        DBRecord dbRecord = new DBRecord(context, db.tableProarcVersion);
        dbRecord.create();
        dbRecord.set(db.tableProarcVersion.id, 0);
        dbRecord.set(db.tableProarcVersion.schemaVersion, VERSION);
        dbRecord.update();
    }

}
