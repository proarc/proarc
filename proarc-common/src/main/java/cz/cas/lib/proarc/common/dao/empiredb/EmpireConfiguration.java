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
import java.sql.Timestamp;
import javax.sql.DataSource;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBDDLGenerator;
import org.apache.empire.db.DBDatabase;
import org.apache.empire.db.DBObject;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.DBTableColumn;
import org.apache.empire.db.context.DBContextStatic;
import org.apache.empire.dbms.DBSqlPhrase;
import org.apache.empire.dbms.postgresql.DBMSHandlerPostgreSQL;
import org.apache.empire.dbms.postgresql.PostgresDDLGenerator;

/**
 *
 * @author Jan Pokorsky
 */
public final class EmpireConfiguration {

    private final ProarcDatabase schema;
    private String jdbcURL;
    private String jdbcUser;
    private String jdbcPwd;
    private String databaseName;
    private DataSource dataSource;

    public EmpireConfiguration(
            String jdbcURL,
            String jdbcUser,
            String jdbcPwd,
            String databaseName
    ) {

        this(databaseName);
        this.jdbcURL = jdbcURL;
        this.jdbcUser = jdbcUser;
        this.jdbcPwd = jdbcPwd;
    }

    public EmpireConfiguration(String databaseName, DataSource dataSource) {
        this(databaseName);
        this.dataSource = dataSource;
    }

    protected EmpireConfiguration(String databaseName) {
        this.databaseName = databaseName;
        schema = new ProarcDatabase();
    }

    public ProarcDatabase getSchema() {
        return schema;
    }

    public DBContextStatic getContext() throws SQLException {
        DBContextStatic context = new DBContextStatic(new FixedDBMSHandlerPostgreSQL(), getConnection());
        context.setRollbackHandlingEnabled(true);
        return context;
    }

    private Connection getConnection() throws SQLException {
        if (dataSource != null) {
            return dataSource.getConnection();
        } else {
            return getPostgresConnection();
        }
    }

    private Connection getPostgresConnection() throws SQLException {
        Connection c = DriverManager.getConnection(jdbcURL, jdbcUser, jdbcPwd);
        c.setAutoCommit(false);
        return c;
    }

    public static EmpireConfiguration postgres(DataSource ds) {
        return new EmpireConfiguration(null, ds);
    }

    @Override
    public String toString() {
        return "EmpireConfiguration{"
                + ", jdbcURL=" + jdbcURL
                + ", jdbcUser=" + jdbcUser
//                + ", jdbcPwd=" + jdbcPwd
                + ", databaseName=" + databaseName
                + ", dataSource=" + dataSource
                + '}';
    }

    private static final class FixedDBMSHandlerPostgreSQL extends DBMSHandlerPostgreSQL {

        private static final long serialVersionUID = 1L;

        private PostgresDDLGenerator ddlGenerator;

        @Override
        public String getSQLPhrase(DBSqlPhrase phrase) {
            switch (phrase) {
                case SQL_DATETIME_PATTERN:
                    return "yyyy-MM-dd HH:mm:ss.SSS";
            }
            return super.getSQLPhrase(phrase);
        }

        @Override
        public void getDDLScript(DBDDLGenerator.DDLActionType type, DBObject dbo, DBSQLScript script) {
            if (ddlGenerator == null) {
                ddlGenerator = new FixedPostgresDDLGenerator(this);
            }
            ddlGenerator.getDDLScript(type, dbo, script);
        }

        // todo
//        @Override
//        protected String getSQLDateTimeString(Object value, int sqlTemplate, int sqlPattern, int sqlCurrentDate) {
//            if (value instanceof Timestamp && sqlPattern == SQL_DATETIME_PATTERN) {
//                // gets timestamp in full precision
//                // Postgres default timestamp precision is microseconds!
//                return '\'' + ((Timestamp) value).toString() + '\'';
//            }
//            return super.getSQLDateTimeString(value, sqlTemplate, sqlPattern, sqlCurrentDate);
//        }

        @Override
        public Timestamp getUpdateTimestamp(Connection conn) {
            return new Timestamp(System.currentTimeMillis());
        }

    }

    /**
     * The generator does not create a sequence for newly added table to existing database.
     * {@code createDatabase} and {@code createTable} fix it.
     *
     * <p>For now the generator creates no sequence as they are produced by PostgreSql.
     */
    private static final class FixedPostgresDDLGenerator extends PostgresDDLGenerator {

        private boolean isCreateDatabase;

        public FixedPostgresDDLGenerator(FixedDBMSHandlerPostgreSQL driver) {
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
