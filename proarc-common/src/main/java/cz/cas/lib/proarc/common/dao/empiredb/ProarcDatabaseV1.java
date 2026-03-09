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
import java.sql.Connection;
import java.sql.SQLException;
import java.util.logging.Logger;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBCmdType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBDatabase;
import org.apache.empire.db.DBDatabaseDriver;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBRelation.DBReference;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.DBTableColumn;
import org.apache.empire.db.exceptions.QueryFailedException;
import org.apache.empire.db.postgresql.DBDatabaseDriverPostgreSQL;

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
            int currentSchemaVersion, Connection conn,
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
            schema.open(conf.getDriver(), conn);
            // the driver instance must be same!
            schemaV1.open(schema.getDriver(), conn);
            upgradeDdl(schema, schemaV1, conn);
            // copy passwords from tomcat
            upgradeUsers(schema, schemaV1, conn);
            // update user groups to comply with new columns
            upgradeGroups(schema, schemaV1, conn);

            int schemaVersion = schema.initVersion(conn, VERSION);

            conn.commit();

            // drop tomcat tables
            // do not drop as they may be still be used by tomcat configuration!
//            script.clear();
//            driver.getDDLScript(DBCmdType.DROP, schemaV1.tableTomcatUser, script);
//            driver.getDDLScript(DBCmdType.DROP, schemaV1.tableTomcatRole, script);
//            conn.setAutoCommit(true);
//            script.run(driver, conn);
//            conn.setAutoCommit(false);
            return schemaVersion;
        } finally {
            schema.close(conn);
            schemaV1.close(conn);
        }
    }

    private static void upgradeDdl(ProarcDatabaseV2 schema, ProarcDatabaseV1 schemaV1, Connection conn) throws SQLException {
        try {
            conn.setAutoCommit(true);
            DBDatabaseDriver driver = schema.getDriver();
            DBSQLScript script = new DBSQLScript();
            // add UserTable columns
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.passwd, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.remoteName, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.remoteType, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.defaultGroup, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.userGroup, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUser.timestamp, script);
            // add UserGroupTable columns
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUserGroup.title, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUserGroup.remoteName, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUserGroup.remoteType, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUserGroup.created, script);
            driver.getDDLScript(DBCmdType.CREATE, schema.tableUserGroup.timestamp, script);
            // add GroupPermissionTable columns
            driver.getDDLScript(DBCmdType.CREATE, schema.tableGroupPermission.type, script);
            script.run(driver, conn);
            // drop table references
            script.clear();
            EmpireUtils.dropRelation(schemaV1.relUsername2TomcatUser, schema.getDriver(), script);
            EmpireUtils.dropRelation(schemaV1.relUsername2TomcatUserAlias, schema.getDriver(), script);
            EmpireUtils.dropRelation(schemaV1.relTomcatRoleUser2TomcatUser, schema.getDriver(), script);
            EmpireUtils.dropRelation(schemaV1.relTomcatRoleUser2TomcatUserAlias, schema.getDriver(), script);
            script.run(driver, conn, true);
        } finally {
            conn.setAutoCommit(false);
        }
    }

    private static void upgradeGroups(ProarcDatabaseV2 schema, ProarcDatabaseV1 schemaV1, Connection conn) {
        DBCommand cmd = schema.createCommand();
        cmd.set(schema.tableUserGroup.groupname.to(UserUtil.toGroupPid("admin")));
        cmd.set(schema.tableUserGroup.title.to("Administrátoři"));
        cmd.where(schema.tableUserGroup.groupname.is("Administrátoři"));
        schema.executeUpdate(cmd, conn);
    }

    private static void upgradeUsers(ProarcDatabaseV2 schema, ProarcDatabaseV1 schemaV1, Connection conn) {
        DBRecord dbr = new DBRecord();
        DBReader dbReader = new DBReader();
        DBCommand cmd = schema.createCommand();
        cmd.select(schema.tableUser.getColumns());
        cmd.select(schemaV1.tableTomcatUser.getColumns());
        cmd.join(schema.tableUser.username, schemaV1.tableTomcatUser.username);
        try {
            dbReader.open(cmd, conn);
            while (dbReader.moveNext()) {
                dbReader.initRecord(schema.tableUser, dbr);
                dbr.setValue(schema.tableUser.passwd, dbReader.getValue(schemaV1.tableTomcatUser.userpass));
                dbr.update(conn);
            }
        } finally {
            dbReader.close();
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
        public final DBTableColumn timestamp; // optimistic lock
        public final DBTableColumn device; // digitization device ID (PID)
        public final DBTableColumn generateIndices;
        public final DBTableColumn log;
//        public final DBTableColumn model; // default batch item model

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
            pid = addColumn("PID", DataType.TEXT, 41, false);
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
            username = addColumn("USERNAME", DataType.TEXT, 255, true);
            forename = addColumn("FORENAME", DataType.TEXT, 100, false);
            surname = addColumn("SURNAME", DataType.TEXT, 255, true);
            email = addColumn("EMAIL", DataType.TEXT, 255, false);
            state = addColumn("STATUS", DataType.TEXT, 20, false);
            created = addColumn("CREATED", DataType.DATETIME, 0, true, SYSDATE);
            lastLogin = addColumn("LASTLOGIN", DataType.DATETIME, 0, false);
            home = addColumn("HOME", DataType.TEXT, 2000, true);
//            timestamp = addTimestampColumn("TIMESTAMP");
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
            groupname = addColumn("NAME", DataType.TEXT, 255, true);
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
            objectid = addColumn("OBJECTID", DataType.TEXT, 2000, false);
            permissionid = addColumn("PERMISSIONID", DataType.TEXT, 2000, true);

        }

    }

    public static final class TomcatUserTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        public final DBTableColumn username;
        public final DBTableColumn userpass;

        public TomcatUserTable(DBDatabase db) {
            super("TOMCAT_USERS", db);
            username = addColumn("USERNAME", DataType.TEXT, 255, true);
            userpass = addColumn("USERPASS", DataType.TEXT, 255, true);
            setPrimaryKey(username);
        }

    }

    public static final class TomcatRoleTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        public final DBTableColumn username;
        public final DBTableColumn rolename;

        public TomcatRoleTable(DBDatabase db) {
            super("TOMCAT_ROLES", db);
            username = addColumn("USERNAME", DataType.TEXT, 255, true);
            rolename = addColumn("ROLENAME", DataType.TEXT, 255, true);
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
        DBDatabaseDriver drv = conf.getDriver();
        Connection conn = conf.getConnection();
        open(drv, conn);
        try {
            if (schemaExists(this, conn)) {
                // XXX upgrade
            } else {
                createSchema(this, conn);
            }
        } finally {
            conn.close();
        }
    }

    static boolean schemaExists(ProarcDatabaseV1 db, Connection c) {
        try {
            DBCommand cmd = db.createCommand();
            cmd.select(db.tableProarcVersion.count());
            int count = db.querySingleInt(cmd, -1, c);
            return count >= 0;
        } catch (QueryFailedException ex) {
            return false;
        }
    }

    private static void createSchema(ProarcDatabaseV1 db, Connection conn) throws SQLException {
        if (db.getDriver() instanceof DBDatabaseDriverPostgreSQL) {
            conn.setAutoCommit(true);
        }
        DBSQLScript script = new DBSQLScript();
        db.getCreateDDLScript(db.getDriver(), script);
        LOG.fine(script.toString());
        script.run(db.getDriver(), conn);
        initProarcVersion(db, conn);
        db.commit(conn);
        conn.setAutoCommit(false);
    }

    private static void initProarcVersion(ProarcDatabaseV1 db, Connection conn) {
        DBRecord dbRecord = new DBRecord();
        dbRecord.create(db.tableProarcVersion);
        dbRecord.setValue(db.tableProarcVersion.id, 0);
        dbRecord.setValue(db.tableProarcVersion.schemaVersion, VERSION);
        dbRecord.update(conn);
    }

}
