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
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import org.dbunit.DatabaseUnitException;
import org.dbunit.database.DatabaseConfig;
import org.dbunit.database.DatabaseConnection;
import org.dbunit.database.IDatabaseConnection;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.xml.FlatDtdDataSet;
import org.dbunit.dataset.xml.FlatXmlDataSet;
import org.dbunit.dataset.xml.FlatXmlDataSetBuilder;
import org.dbunit.ext.postgresql.PostgresqlDataTypeFactory;
import org.junit.Assert;
import org.junit.Assume;

/**
 *
 * @author Jan Pokorsky
 */
public class DbUnitSupport {

    private final EmpireConfiguration emireCfg;
    private IDatabaseConnection dbuConnection;
    private static String dtdSchema;

    public DbUnitSupport() {
        Assume.assumeNotNull(System.getProperty("proarc-common.DbUnitSupport.jdbc.user"));
        emireCfg = new EmpireConfiguration(
                System.getProperty("proarc-common.DbUnitSupport.jdbc.driver"),
                System.getProperty("proarc-common.DbUnitSupport.jdbc.url"),
                System.getProperty("proarc-common.DbUnitSupport.jdbc.user"),
                System.getProperty("proarc-common.DbUnitSupport.jdbc.passwd"),
                System.getProperty("proarc-common.DbUnitSupport.empiredb.driver"),
                null
                );
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
        if (dbuConnection == null) {
            dbuConnection = createProgresConnection(c);
        }
        return dbuConnection;
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
        return loadFlatXmlDataStream(c, resource, true);
    }

    public IDataSet loadFlatXmlDataStream(Class<?> c, String resource, boolean usedtd) throws Exception {
        FlatXmlDataSetBuilder builder = new FlatXmlDataSetBuilder();
        if (usedtd) {
            builder.setMetaDataSetFromDtd(getDtdSchema());
        }
//        builder.setMetaDataSet(getConnection().createDataSet());
        FlatXmlDataSet fds = builder.build(getResourceStream(c, resource));
        return fds;
    }

    /** initializes sequences after DBUnit inserts */
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

    private InputStream getResourceStream(Class<?> c, String resource) {
        InputStream stream = c.getResourceAsStream(resource);
        Assert.assertNotNull("stream.name: " + resource + ", class: " + c, stream);
        return stream;
    }

    private Reader getDtdSchema() throws Exception {
        if (dtdSchema != null) {
            return new StringReader(dtdSchema);
        }
        Connection c = getEmireCfg().getConnection();
        try {

            IDatabaseConnection dc = createProgresConnection(c);
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            StringWriter sw = new StringWriter();
            FlatDtdDataSet.write(dc.createDataSet(), sw);
            dtdSchema = sw.toString();
//            System.out.println(dtdSchema);
            return new StringReader(dtdSchema);
        } finally {
            c.close();
        }
    }

}
