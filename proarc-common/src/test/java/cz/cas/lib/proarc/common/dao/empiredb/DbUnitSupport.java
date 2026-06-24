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

import cz.cas.lib.proarc.common.dao.Transaction;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.ServerSocket;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Duration;
import java.util.Arrays;
import java.util.List;
import org.apache.empire.db.DBCmdType;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBDatabaseDriver;
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.dbunit.DatabaseUnitException;
import org.dbunit.database.DatabaseConfig;
import org.dbunit.database.DatabaseConnection;
import org.dbunit.database.IDatabaseConnection;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.xml.FlatDtdDataSet;
import org.dbunit.dataset.xml.FlatXmlDataSet;
import org.dbunit.dataset.xml.FlatXmlDataSetBuilder;
import org.dbunit.ext.postgresql.PostgresqlDataTypeFactory;
import org.dbunit.operation.DatabaseOperation;
import org.opentest4j.TestAbortedException;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 *
 * @author Jan Pokorsky
 */
public class DbUnitSupport {

    private static final String JDBC_DRIVER = "org.postgresql.Driver";
    private static final String EMPIRE_DRIVER = "org.apache.empire.db.postgresql.DBDatabaseDriverPostgreSQL";
    private static final String POSTGRES_IMAGE = "postgres:16-alpine";
    private static final String POSTGRES_DATABASE = "proarc";
    private static final String POSTGRES_USER = "proarcAdmin";
    private static final String POSTGRES_PASSWORD = "proarc";
    private static final DockerPostgres POSTGRES = new DockerPostgres();

    private final EmpireConfiguration emireCfg;
    private static String dtdSchema;

    public DbUnitSupport() {
        POSTGRES.start();
        emireCfg = new EmpireConfiguration(
                JDBC_DRIVER,
                POSTGRES.getJdbcUrl(),
                POSTGRES_USER,
                POSTGRES_PASSWORD,
                EMPIRE_DRIVER,
                null
        );
        try {
            resetDatabase();
        } catch (Exception ex) {
            throw abort("Cannot reset Docker PostgreSQL test database.", ex);
        }
    }

    public EmpireConfiguration getEmireCfg() {
        return emireCfg;
    }

    public IDatabaseConnection getConnection(Transaction tx) throws DatabaseUnitException, SQLException {
        return getConnection(getSqlConnection(tx));
    }

    public IDatabaseConnection getConnection() throws DatabaseUnitException, SQLException {
        return getConnection(emireCfg.getConnection());
    }

    public IDatabaseConnection getConnection(Connection c) throws DatabaseUnitException, SQLException {
        return createProgresConnection(c);
    }

    public Connection getSqlConnection(Transaction tx) {
        return ((SqlTransaction) tx).getConnection();
    }

    private IDatabaseConnection createProgresConnection(Connection c) throws DatabaseUnitException {
        DatabaseConnection dbc = new DatabaseConnection(c);
        DatabaseConfig config = dbc.getConfig();
        config.setProperty(DatabaseConfig.PROPERTY_DATATYPE_FACTORY, new PostgresqlDataTypeFactory());
        // Progress cannot handle columns names like XML thus we have to escape them.
        config.setProperty(DatabaseConfig.PROPERTY_ESCAPE_PATTERN, "\"?\"");
        config.setProperty(DatabaseConfig.FEATURE_CASE_SENSITIVE_TABLE_NAMES, false);
        return dbc;
    }

    public IDataSet loadFlatXmlDataStream(Class<?> c, String resource) throws Exception {
        return loadFlatXmlDataStream(c, resource, false);
    }

    public IDataSet loadFlatXmlDataStream(Class<?> c, String resource, boolean resetDtdSchema) throws Exception {
        FlatXmlDataSetBuilder builder = new FlatXmlDataSetBuilder();
        builder.setMetaDataSetFromDtd(getDtdSchema(resetDtdSchema));
//        builder.setMetaDataSet(getConnection().createDataSet());
        FlatXmlDataSet fds = builder.build(getResourceStream(c, resource));
        return fds;
    }

    /**
     * initializes sequences after DBUnit inserts
     */
    public void initSequences(Transaction tx, int startWith, String... sqnName) throws Exception {
        initSequences(getSqlConnection(tx), startWith, sqnName);
    }

    public void initSequences(Connection c, int startWith, String... sqnName) throws Exception {
        Statement stmt = c.createStatement();
        for (String name : sqnName) {
            String sql = String.format("ALTER SEQUENCE %s RESTART WITH %s", name, startWith);
            System.out.println(sql);
            stmt.execute(sql);
        }
    }

    public void cleanInsert(IDatabaseConnection c, IDataSet dataset) throws Exception {
        prepareForDelete(c.getConnection());
        DatabaseOperation.CLEAN_INSERT.execute(c, dataset);
    }

    /**
     * Removes all constrained values that cannot be resolved with proper delete table order.
     */
    public void prepareForDelete(Connection c) {
        ProarcDatabase schema = getEmireCfg().getSchema();
        DBCommand cmd = schema.createCommand();
        cmd.set(schema.tableUser.defaultGroup.to(null));
        cmd.set(schema.tableUser.userGroup.to(null));

        schema.executeUpdate(cmd, c);
    }

    private InputStream getResourceStream(Class<?> c, String resource) {
        InputStream stream = c.getResourceAsStream(resource);
        assertNotNull(stream, "stream.name: " + resource + ", class: " + c);
        return stream;
    }

    public void clearDtdSchema() {
        dtdSchema = null;
    }

    private void resetDatabase() throws Exception {
        dtdSchema = null;
        ProarcDatabase schema = getEmireCfg().getSchema();
        DBDatabaseDriver driver = getEmireCfg().getDriver();
        Connection conn = getEmireCfg().getConnection();
        try {
            schema.open(driver, conn);
            DBSQLScript script = new DBSQLScript();
            for (DBRelation relation : schema.getRelations()) {
                EmpireUtils.dropRelation(relation, driver, script);
            }
            for (DBTable table : schema.getTables()) {
                driver.getDDLScript(DBCmdType.DROP, table, script);
            }
            conn.setAutoCommit(true);
            try {
                script.executeAll(driver, conn, true);
            } finally {
                conn.setAutoCommit(false);
            }
        } finally {
            conn.close();
        }
        schema.init(getEmireCfg());
    }

    private Reader getDtdSchema(boolean resetDtdSchema) throws Exception {
        if (resetDtdSchema || dtdSchema == null) {
            Connection c = getEmireCfg().getConnection();
            try {

                IDatabaseConnection dc = createProgresConnection(c);
                StringWriter sw = new StringWriter();
                FlatDtdDataSet.write(dc.createDataSet(), sw);
                dtdSchema = sw.toString();
//                System.out.println(dtdSchema);
            } finally {
                c.close();
            }
        }
        return new StringReader(dtdSchema);
    }

    private static final class DockerPostgres {

        private String containerName;
        private int port;
        private boolean started;
        private TestAbortedException unavailable;

        synchronized void start() {
            if (started) {
                return;
            }
            if (unavailable != null) {
                throw unavailable;
            }
            port = findFreePort();
            containerName = "proarc-dbunit-" + ProcessHandle.current().pid() + "-" + port;
            try {
                runDocker(Arrays.asList(
                        "docker", "run", "--rm", "--name", containerName,
                        "-e", "POSTGRES_DB=" + POSTGRES_DATABASE,
                        "-e", "POSTGRES_USER=" + POSTGRES_USER,
                        "-e", "POSTGRES_PASSWORD=" + POSTGRES_PASSWORD,
                        "-p", "127.0.0.1:" + port + ":5432",
                        "-d", POSTGRES_IMAGE
                ), Duration.ofMinutes(3));
                Runtime.getRuntime().addShutdownHook(new Thread(() ->
                        runDocker(Arrays.asList("docker", "rm", "-f", containerName), Duration.ofSeconds(30))));
                waitForDatabase();
                started = true;
            } catch (TestAbortedException ex) {
                unavailable = ex;
                throw ex;
            }
        }

        String getJdbcUrl() {
            return "jdbc:postgresql://127.0.0.1:" + port + "/" + POSTGRES_DATABASE;
        }

        private void waitForDatabase() {
            long deadline = System.currentTimeMillis() + Duration.ofSeconds(60).toMillis();
            while (System.currentTimeMillis() < deadline) {
                try (Connection ignored = DriverManager.getConnection(getJdbcUrl(), POSTGRES_USER, POSTGRES_PASSWORD)) {
                    return;
                } catch (SQLException ex) {
                    try {
                        Thread.sleep(500);
                    } catch (InterruptedException interrupted) {
                        Thread.currentThread().interrupt();
                        throw abort("Interrupted while waiting for Docker PostgreSQL.", interrupted);
                    }
                }
            }
            throw abort("Docker PostgreSQL did not start in time.", null);
        }

        private static int findFreePort() {
            try (ServerSocket socket = new ServerSocket(0)) {
                socket.setReuseAddress(false);
                return socket.getLocalPort();
            } catch (Exception ex) {
                throw abort("Cannot allocate local port for Docker PostgreSQL.", ex);
            }
        }

        private static void runDocker(List<String> command, Duration timeout) {
            try {
                Process process = new ProcessBuilder(command).redirectErrorStream(true).start();
                boolean finished = process.waitFor(timeout.toMillis(), java.util.concurrent.TimeUnit.MILLISECONDS);
                if (!finished) {
                    process.destroyForcibly();
                    throw abort("Docker command timed out: " + command, null);
                }
                if (process.exitValue() != 0) {
                    String output = new String(process.getInputStream().readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
                    throw abort("Docker command failed: " + command + "\n" + output, null);
                }
            } catch (Exception ex) {
                throw abort("DbUnit Docker PostgreSQL tests require runnable Docker CLI: " + command, ex);
            }
        }

    }

    private static TestAbortedException abort(String message, Throwable cause) {
        return cause == null
                ? new TestAbortedException(message)
                : new TestAbortedException(message, cause);
    }

}
