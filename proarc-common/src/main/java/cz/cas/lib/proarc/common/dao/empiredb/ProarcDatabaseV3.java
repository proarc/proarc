/*
 * Copyright (C) 2013 Jan Pokorsky
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
import java.sql.Connection;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.empire.commons.OptionEntry;
import org.apache.empire.commons.Options;
import org.apache.empire.data.DataMode;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBCmdType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBDatabase;
import static org.apache.empire.db.DBDatabase.SYSDATE;
import org.apache.empire.db.DBDatabaseDriver;
import org.apache.empire.db.DBExpr;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.DBTableColumn;
import org.apache.empire.db.exceptions.QueryFailedException;
import org.apache.empire.db.postgresql.DBDatabaseDriverPostgreSQL;

/**
 * Database schema version 3.
 *
 * <p><b>Warning:</b> declare sequence names the same way like PostgreSql
 * ({@code {tablename}_{column_name}_seq}).
 *
 * @author Jan Pokorsky
 * @deprecated Replaced with {@link ProarcDatabase}. <b>Use only for tests and upgrade purposes!</b>
 */
@Deprecated
public class ProarcDatabaseV3 extends DBDatabase {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(ProarcDatabaseV3.class.getName());
    /** the schema version */
    public static final int VERSION = 3;

    public final ProarcVersionTable tableProarcVersion = new ProarcVersionTable(this);
    public final BatchTable tableBatch = new BatchTable(this);
    public final BatchItemTable tableBatchItem = new BatchItemTable(this);
    public final UserTable tableUser = new UserTable(this);
    public final UserGroupTable tableUserGroup = new UserGroupTable(this);
    public final GroupMemberTable tableGroupMember = new GroupMemberTable(this);
    public final GroupPermissionTable tableGroupPermission = new GroupPermissionTable(this);

    public static int upgradeToVersion4(
            int currentSchemaVersion, ProarcDatabase schema,
            Connection conn, EmpireConfiguration conf) throws SQLException {

        if (currentSchemaVersion < VERSION) {
            currentSchemaVersion = ProarcDatabaseV2.upgradeToVersion3(currentSchemaVersion, conn, conf);
        }
        if (currentSchemaVersion > VERSION) {
            // ignore higher versions
            return currentSchemaVersion;
        } else if (currentSchemaVersion != VERSION) {
            throw new SQLException("Cannot upgrade from schema version " + currentSchemaVersion);
        }
//        ProarcDatabaseV3 schema = new ProarcDatabaseV3();
        try {
            upgradeDdl(schema, conn);
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
            // add workflow tables
            EmpireUtils.addTable(schema.tableWorkflowJob, driver, script);
            EmpireUtils.addTable(schema.tableWorkflowTask, driver, script);
            EmpireUtils.addTable(schema.tableWorkflowMaterial, driver, script);
            EmpireUtils.addTable(schema.tableWorkflowDigObj, driver, script);
            EmpireUtils.addTable(schema.tableWorkflowFolder, driver, script);
            EmpireUtils.addTable(schema.tableWorkflowPhysicalDoc, driver, script);
            EmpireUtils.addTable(schema.tableWorkflowMaterialInTask, driver, script);
            EmpireUtils.addTable(schema.tableWorkflowParameter, driver, script);
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
        public final DBTableColumn timestamp; // optimistic lock
        public final DBTableColumn device; // digitization device ID (PID)
        public final DBTableColumn generateIndices;
        public final DBTableColumn log;
        public final DBTableColumn profileId;

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
            profileId = addColumn("PROFILE_ID", DataType.TEXT, 2000, false);
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
        public final DBTableColumn passwd;
        public final DBTableColumn forename;
        public final DBTableColumn surname;
        public final DBTableColumn email;
        public final DBTableColumn state;
        public final DBTableColumn created;
        public final DBTableColumn lastLogin;
        public final DBTableColumn home;
        /** group to use as owner for newly created objects */
        public final DBTableColumn defaultGroup;
        /** group that can contain single member; it can hold overridden permissions */
        public final DBTableColumn userGroup;
        /** use to identify external user. */
        public final DBTableColumn remoteName;
        /** type of the remote user null(PROARC), DESA, LDAP, ... */
        public final DBTableColumn remoteType;
        public final DBTableColumn timestamp;

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

    public ProarcDatabaseV3() {
        addRelation(tableBatch.userId.referenceOn(tableUser.id));
        addRelation(tableBatchItem.batchId.referenceOn(tableBatch.id));
        addRelation(tableUser.defaultGroup.referenceOn(tableUserGroup.id));
        addRelation(tableGroupMember.groupid.referenceOn(tableUserGroup.id));
        addRelation(tableGroupMember.userid.referenceOn(tableUser.id));
        addRelation(tableGroupPermission.groupid.referenceOn(tableUserGroup.id));
    }

    void dropRelation(DBSQLScript script, DBRelation relation) {
        StringBuilder sql = new StringBuilder();
        DBSQLScript helper = new DBSQLScript();
        DBTable sourceTable = relation.getForeignKeyTable();
        sql.append("-- drop foreign key constraint ");
        sql.append(relation.getName());
        sql.append(" --\r\n");
        sql.append("ALTER TABLE ");
        sourceTable.addSQL(sql, DBExpr.CTX_FULLNAME);

        driver.getDDLScript(DBCmdType.DROP, relation, helper);
        sql.append(' ');
        sql.append(helper.getStmt(0));
        script.addStmt(sql);
    }

    void init(EmpireConfiguration conf) throws SQLException {
        DBDatabaseDriver drv = conf.getDriver();
        Connection conn = conf.getConnection();
        open(drv, conn);
        try {
            int schemaVersion = schemaExists(this, conn);
            if (schemaVersion > 0) {
                LOG.log(Level.WARNING, "Do not init the obsolete schema {0}!"
                        + " Only #upgradeToVersion3() is supported now!", schemaVersion);
                schemaVersion = ProarcDatabaseV2.upgradeToVersion3(
                        schemaVersion, conn, conf);
                if (schemaVersion != VERSION) {
                    throw new SQLException("Invalid schema version " + schemaVersion);
                }
            } else {
                createSchema(this, conn);
            }
        } finally {
            conn.close();
        }
    }

    static int schemaExists(ProarcDatabaseV3 db, Connection c) {
        try {
            DBCommand cmd = db.createCommand();
            cmd.select(db.tableProarcVersion.schemaVersion);
            int version = db.querySingleInt(cmd, -1, c);
            return version;
        } catch (QueryFailedException ex) {
            return -1;
        }
    }

    private static void createSchema(ProarcDatabaseV3 db, Connection conn) throws SQLException {
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
        ProarcDatabaseV3 db = this;
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

    /**
     * Improves and fixes {@link DBTable}.
     */
    static abstract class EnhancedDBTable extends DBTable {

        public EnhancedDBTable(String name, DBDatabase db) {
            super(name, db);
        }

        /**
         * Adds a timestamp column to the table used for optimistic locking.
         *
         * <p>This implementation creates column that can be modified with
         * {@link DBRecord#setBeanValues } or {@link DBRecord#setValue } as
         * the column is not read-only or auto generated.
         *
         * @param columnName the column name
         *
         * @return the timestamp table column object
         */
        @Override
        public DBTableColumn addTimestampColumn(String columnName) {
            return addTimestampColumn(columnName, DataMode.NotNull);
        }

        /**
         * Adds a timestamp column to the table used for optimistic locking.
         *
         * @param columnName the column name
         * @param mode use AutoGenerated in case user bean does not handle timestamp
         *
         * @return the timestamp table column object
         */
        public DBTableColumn addTimestampColumn(String columnName, DataMode mode) {
            // Do NOT change to DataMode.AutoGenerated; otherwise DBRecord.setBeanValues ignores user timestamp!
            // DBDatabase.SYSDATE used mainly for ALTER TABLE ADD column to fill default values.
            // DBRecord.setBeanValues MUST set any timestamp for the new record!
            DBTableColumn col = addColumn(columnName, DataType.DATETIME, 0, mode, DBDatabase.SYSDATE);
            setTimestampColumn(col);
            return col;
        }

        public DBTableColumn addSequenceColumn(String columnName) {
            String seqName = postgreSequenceName(this, columnName);
            return addSequenceColumn(columnName, seqName);
        }

        public DBTableColumn addSequenceColumn(String columnName, String seqName) {
            return addColumn(columnName, DataType.AUTOINC, 0, DataMode.AutoGenerated, seqName);
        }

        public boolean isTimestamp(DBColumn c) {
            return c == this.timestampColumn;
        }

        static Options toOptions(Object[] options) {
            Options result = new Options();
            for (Object option : options) {
                result.add(new OptionEntry(option, String.valueOf(option)));
            }
            return result;
        }

        private static String postgreSequenceName(DBTable table, String columnName) {
            String sqnName = String.format("%s_%s_SEQ", table.getName(), columnName);
            return sqnName;
        }

    }

}
