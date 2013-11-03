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
import java.util.logging.Logger;
import org.apache.empire.commons.OptionEntry;
import org.apache.empire.commons.Options;
import org.apache.empire.data.DataMode;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBDatabase;
import org.apache.empire.db.DBDatabaseDriver;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.DBTableColumn;
import org.apache.empire.db.exceptions.QueryFailedException;
import org.apache.empire.db.postgresql.DBDatabaseDriverPostgreSQL;

/**
 * Database schema.
 *
 * <p><b>Warning:</b> declare sequence names the same way like PostgreSql
 * ({@code {tablename}_{column_name}_seq}).
 *
 * @author Jan Pokorsky
 */
public class ProarcDatabase extends DBDatabase {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(ProarcDatabase.class.getName());

    public final ProarcVersionTable tableProarcVersion = new ProarcVersionTable(this);
    public final BatchTable tableBatch = new BatchTable(this);
    public final BatchItemTable tableBatchItem = new BatchItemTable(this);
    public final UserTable tableUser = new UserTable(this);
    public final UserGroupTable tableUserGroup = new UserGroupTable(this);
    public final GroupMemberTable tableGroupMember = new GroupMemberTable(this);
    public final GroupPermissionTable tableGroupPermission = new GroupPermissionTable(this);
    public final TomcatUserTable tableTomcatUser = new TomcatUserTable(this);
    public final TomcatRoleTable tableTomcatRole = new TomcatRoleTable(this);

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
        private final DBTableColumn username;
        private final DBTableColumn userpass;

        public TomcatUserTable(DBDatabase db) {
            super("TOMCAT_USERS", db);
            username = addColumn("USERNAME", DataType.TEXT, 255, true);
            userpass = addColumn("USERPASS", DataType.TEXT, 255, true);
            setPrimaryKey(username);
        }

    }

    public static final class TomcatRoleTable extends EnhancedDBTable {

        private static final long serialVersionUID = 1L;
        private final DBTableColumn username;
        private final DBTableColumn rolename;

        public TomcatRoleTable(DBDatabase db) {
            super("TOMCAT_ROLES", db);
            username = addColumn("USERNAME", DataType.TEXT, 255, true);
            rolename = addColumn("ROLENAME", DataType.TEXT, 255, true);
            setPrimaryKey(username, rolename);
        }

    }

    public ProarcDatabase() {
        addRelation(tableBatch.userId.referenceOn(tableUser.id));
        addRelation(tableBatchItem.batchId.referenceOn(tableBatch.id));
        addRelation(tableUser.username.referenceOn(tableTomcatUser.username));
        addRelation(tableGroupMember.groupid.referenceOn(tableUserGroup.id));
        addRelation(tableGroupMember.userid.referenceOn(tableUser.id));
        addRelation(tableGroupPermission.groupid.referenceOn(tableUserGroup.id));
        addRelation(tableTomcatRole.username.referenceOn(tableTomcatUser.username));
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

    static boolean schemaExists(ProarcDatabase db, Connection c) {
        try {
            DBCommand cmd = db.createCommand();
            cmd.select(db.tableProarcVersion.count());
            int count = db.querySingleInt(cmd, -1, c);
            return count >= 0;
        } catch (QueryFailedException ex) {
            return false;
        }
    }

    private static void createSchema(ProarcDatabase db, Connection conn) throws SQLException {
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

    private static void initProarcVersion(ProarcDatabase db, Connection conn) {
        DBRecord dbRecord = new DBRecord();
        dbRecord.create(db.tableProarcVersion);
        dbRecord.setValue(db.tableProarcVersion.id, 0);
        dbRecord.setValue(db.tableProarcVersion.schemaVersion, 1);
        dbRecord.update(conn);
    }

    private static Options toOptions(Object[] options) {
        Options result = new Options();
        for (Object option : options) {
            result.add(new OptionEntry(option, String.valueOf(option)));
        }
        return result;
    }

    private static abstract class EnhancedDBTable extends DBTable {

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
            DBTableColumn col = addColumn(columnName, DataType.DATETIME, 0, true);
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

        private static String postgreSequenceName(DBTable table, String columnName) {
            String sqnName = String.format("%s_%s_SEQ", table.getName(), columnName);
            return sqnName;
        }

    }

}
