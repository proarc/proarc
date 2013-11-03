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

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import javax.sql.DataSource;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBCmdType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBDatabase;
import org.apache.empire.db.DBDatabaseDriver;
import org.apache.empire.db.DBObject;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.DBTableColumn;
import org.apache.empire.db.postgresql.DBDatabaseDriverPostgreSQL;
import org.apache.empire.db.postgresql.PostgreDDLGenerator;

/**
 *
 * @author Jan Pokorsky
 */
public final class EmpireConfiguration {

    private final ProarcDatabase schema;
    private String jdbcClass;
    private String jdbcURL;
    private String jdbcUser;
    private String jdbcPwd;
    private String empireDBDriverClass;
    private String databaseName;
    private DataSource dataSource;

    public EmpireConfiguration(
            String jdbcClass,
            String jdbcURL,
            String jdbcUser,
            String jdbcPwd,
            String empireDBDriverClass,
            String databaseName
            ) {

        this(databaseName, empireDBDriverClass);
        this.jdbcClass = jdbcClass;
        this.jdbcURL = jdbcURL;
        this.jdbcUser = jdbcUser;
        this.jdbcPwd = jdbcPwd;
    }

    public EmpireConfiguration(String databaseName, String empireDBDriverClass, DataSource dataSource) {
        this(databaseName, empireDBDriverClass);
        this.dataSource = dataSource;
    }

    protected EmpireConfiguration(String databaseName, String empireDBDriverClass) {
        this.empireDBDriverClass = empireDBDriverClass;
        this.databaseName = databaseName;
        schema = new ProarcDatabase();
    }

    public ProarcDatabase getSchema() {
        return schema;
    }
    
    public DBDatabaseDriver getDriver() {
        if (DBDatabaseDriverPostgreSQL.class.getName().equals(empireDBDriverClass)) {
            if (databaseName == null || databaseName.isEmpty()) {
//                throw new IllegalStateException("databaseName\n" + toString());
            }
//            DBDatabaseDriverPostgreSQL drv = new DBDatabaseDriverPostgreSQL();
            DBDatabaseDriverPostgreSQL drv = new FixedDBDatabaseDriverPostgreSQL();
//            drv.setDatabaseName(databaseName);
            return drv;
        } else {
            throw new UnsupportedOperationException("empireDBDriverClass\n" + toString());
        }
    }

    public Connection getConnection() throws SQLException {
        if (dataSource != null) {
            return dataSource.getConnection();
        } else {
            return getPostgresConnection();
        }
    }

    private Connection getPostgresConnection() throws SQLException {
        try {
            Class.forName(jdbcClass);
        } catch (ClassNotFoundException ex) {
            throw new IllegalStateException(jdbcClass, ex);
        }
        Connection c = DriverManager.getConnection(jdbcURL, jdbcUser, jdbcPwd);
        c.setAutoCommit(false);
        return c;
    }

    public static EmpireConfiguration postgres(DataSource ds) {
        return new EmpireConfiguration(null, DBDatabaseDriverPostgreSQL.class.getName(), ds);
    }
    
    @Override
    public String toString() {
        return "EmpireConfiguration{"
                + "jdbcClass=" + jdbcClass
                + ", jdbcURL=" + jdbcURL
                + ", jdbcUser=" + jdbcUser
//                + ", jdbcPwd=" + jdbcPwd
                + ", empireDBDriverClass=" + empireDBDriverClass
                + ", databaseName=" + databaseName
                + ", dataSource=" + dataSource
                + '}';
    }

    private static final class FixedDBDatabaseDriverPostgreSQL extends DBDatabaseDriverPostgreSQL {

        private static final long serialVersionUID = 1L;

        private PostgreDDLGenerator ddlGenerator;

        public FixedDBDatabaseDriverPostgreSQL() {
            // e.g. for DATETIME:
            //   addColumn("CREATED", DataType.DATETIME, 0, true, SYSDATE);
            //   generate DEFAULT NOW()
            // see DBDDLGenerator.appendColumnDesc
            setDDLColumnDefaults(true);
        }

        @Override
        public String getSQLPhrase(int phrase) {
            switch (phrase) {
                // store milliseconds within timestamp
                // see http://empire-db.15390.n3.nabble.com/DBSequence-Table-and-PostGre-td925674.html
                case SQL_DATETIME_PATTERN :
                    return "yyyy-MM-dd HH:mm:ss.SSS";
            }
            return super.getSQLPhrase(phrase); //To change body of generated methods, choose Tools | Templates.
        }

        @Override
        public void getDDLScript(DBCmdType type, DBObject dbo, DBSQLScript script) {
            if (ddlGenerator == null) {
                ddlGenerator = new FixedPostgreDDLGenerator(this);
            }
            // forward request
            ddlGenerator.getDDLScript(type, dbo, script);
        }

    }

    /**
     * The generator does not create a sequence for newly added table to existing database.
     * {@code createDatabase} and {@code createTable} fix it.
     *
     * <p>For now the generator creates no sequence as they are produced by PostgreSql.
     */
    private static final class FixedPostgreDDLGenerator extends PostgreDDLGenerator {

        private boolean isCreateDatabase;

        public FixedPostgreDDLGenerator(DBDatabaseDriverPostgreSQL driver) {
            super(driver);
        }

        @Override
        protected void createDatabase(DBDatabase db, DBSQLScript script) {
            isCreateDatabase = true;
            try {
                super.createDatabase(db, script);
            } finally {
                isCreateDatabase = false;
            }
        }

        @Override
        protected void createTable(DBTable t, DBSQLScript script) {
            if (!isCreateDatabase) {
                for (DBColumn c : t.getColumns()) {
                    if (c.getDataType() == DataType.AUTOINC) {
                        createSequence(t.getDatabase(), (DBTableColumn) c, script);
                    }
                }
            }
            super.createTable(t, script);
        }

        @Override
        protected void createSequence(DBDatabase db, DBTableColumn c, DBSQLScript script) {
            // PostgreSql creates sequences itself for SERIAL type.
        }

    }

}
