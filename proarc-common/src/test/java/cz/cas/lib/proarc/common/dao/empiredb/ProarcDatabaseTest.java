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
import org.apache.empire.db.DBCmdType;
import org.apache.empire.db.DBDatabaseDriver;
import org.apache.empire.db.DBExpr;
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class ProarcDatabaseTest {

    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireConfiguration emireCfg;
    private DBDatabaseDriver driver;

    public ProarcDatabaseTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        support = new DbUnitSupport();
        emireCfg = support.getEmireCfg();
        schema = emireCfg.getSchema();
        driver = emireCfg.getDriver();
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testCreateSchemaScript() throws Exception {
        Connection conn = emireCfg.getConnection();
        try {
            schema.open(driver, conn);
            DBSQLScript script = new DBSQLScript();
            schema.getCreateDDLScript(schema.getDriver(), script);
            System.out.println("### create script:\n" + script);
        } finally {
            conn.close();
        }
    }

    @Test
    public void testDropSchemaScript() throws Exception {
        Connection conn = emireCfg.getConnection();
        try {
            schema.open(driver, conn);
            DBSQLScript script = new DBSQLScript();
            dropConstraint(schema, script);
            dropTables(schema, script);
            System.out.println("### drop script:\n" + script);
        } finally {
            conn.close();
        }
    }
    
    @Test
    public void testInit() throws Exception {
        // clear DB
        dropSchema();
        schema.init(emireCfg);
    }

    @Test
    public void testSchemaExists() throws Exception {
        schema.init(emireCfg);
        Connection c = emireCfg.getConnection();
        try {
            boolean result = ProarcDatabase.schemaExists(schema, c);
            assertTrue(result);
        } finally {
            c.close();
        }
    }

    @Test
    public void testSchemaNotExists() throws Exception {
        dropSchema();
        Connection c = emireCfg.getConnection();
        try {
            boolean result = ProarcDatabase.schemaExists(schema, c);
            assertFalse(result);
        } finally {
            c.close();
        }
    }

    private void dropSchema() throws Exception {
        Connection conn = emireCfg.getConnection();
        try {
            schema.open(driver, conn);
            DBSQLScript script = new DBSQLScript();
            dropConstraint(schema, script);
            dropTables(schema, script);
//            System.out.println("### drop script:\n" + script);
            conn.setAutoCommit(true);
            try {
                script.run(driver, conn, true);
            } finally {
                conn.setAutoCommit(false);
            }
        } finally {
            conn.close();
        }
    }

    private void dropTables(ProarcDatabase schema, DBSQLScript script) {
        for (DBTable table : schema.getTables()) {
            driver.getDDLScript(DBCmdType.DROP, table, script);
        }
    }

    private void dropConstraint(ProarcDatabase schema, DBSQLScript script) {
        StringBuilder sql = new StringBuilder();
        DBSQLScript helper = new DBSQLScript();
        for (DBRelation relation : schema.getRelations()) {
            DBTable sourceTable = (DBTable) relation.getReferences()[0].getSourceColumn().getRowSet();
            sql.append("-- creating foreign key constraint ");
            sql.append(relation.getName());
            sql.append(" --\r\n");
            sql.append("ALTER TABLE ");
            sourceTable.addSQL(sql, DBExpr.CTX_FULLNAME);

            driver.getDDLScript(DBCmdType.DROP, relation, helper);
            sql.append(' ');
            sql.append(helper.getStmt(0));
            script.addStmt(sql);
            helper.clear();
        }
    }

}