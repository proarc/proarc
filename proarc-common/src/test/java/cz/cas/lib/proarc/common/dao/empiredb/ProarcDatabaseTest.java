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

import org.apache.empire.db.DBContext;
import org.apache.empire.db.DBDDLGenerator;
import org.apache.empire.db.DBDatabase;
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.dbunit.database.IDatabaseConnection;
import org.dbunit.dataset.IDataSet;
import org.dbunit.operation.DatabaseOperation;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class ProarcDatabaseTest {

    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireConfiguration emireCfg;

    public ProarcDatabaseTest() {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() {
        support = new DbUnitSupport();
        emireCfg = support.getEmireCfg();
        schema = emireCfg.getSchema();
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testCreateSchemaScript() throws Exception {
        DBContext context = emireCfg.getContext();
        try {
            schema.open(context);
            DBSQLScript script = new DBSQLScript(context);
            schema.getCreateDDLScript(script);
            System.out.println("### create script:\n" + script);
        } finally {
            context.discard();
        }
    }

    @Test
    public void testDropSchemaScript() throws Exception {
        DBContext context = emireCfg.getContext();
        try {
            schema.open(context);
            DBSQLScript script = new DBSQLScript(context);
            dropConstraints(schema, context, script);
            dropTables(schema, context, script);
            System.out.println("### drop script:\n" + script);
        } finally {
            context.discard();
        }
    }

    @Test
    public void testInit() throws Exception {
        // clear DB
        dropSchema(schema);
        schema.init(emireCfg);
    }

    @Test
    public void testUpgrade() throws Exception {
        ProarcDatabaseV1 v1 = new ProarcDatabaseV1();
        ProarcDatabaseV2 v2 = new ProarcDatabaseV2();
        ProarcDatabaseV3 v3 = new ProarcDatabaseV3();
        ProarcDatabaseV4 v4 = new ProarcDatabaseV4();
        final DBContext context = emireCfg.getContext();
        try {
            // clear DB
            dropSchema(schema);
            dropSchema(v4);
            dropSchema(v3);
            dropSchema(v2);
            dropSchema(v1);
            v1.init(emireCfg);
            assertEquals(1, ProarcDatabase.schemaExists(schema, context));

            IDataSet db = support.loadFlatXmlDataStream(getClass(), "proarc_v1.xml", true);
            try {
                DatabaseOperation.INSERT.execute((IDatabaseConnection) context.getConnection(), db);
                context.commit();
            } finally {
                support.clearDtdSchema();
            }
            schema.init(emireCfg);
            assertEquals(ProarcDatabase.VERSION, ProarcDatabase.schemaExists(schema, context));
        } finally {
            context.discard();
            dropSchema(schema);
            dropSchema(v1);
        }
    }

    @Test
    public void testSchemaExists() throws Exception {
        schema.init(emireCfg);
        DBContext context = emireCfg.getContext();
        try {
            boolean result = ProarcDatabase.schemaExists(schema, context) > 0;
            assertTrue(result);
        } finally {
            context.discard();
        }
    }

    @Test
    public void testSchemaNotExists() throws Exception {
        dropSchema(schema);
        DBContext context = emireCfg.getContext();
        try {
            boolean result = ProarcDatabase.schemaExists(schema, context) > 0;
            assertFalse(result);
        } finally {
            context.discard();
            ;
        }
    }

    private void dropSchema(DBDatabase schema) throws Exception {
        DBContext context = emireCfg.getContext();
        try {
            schema.open(context);
            DBSQLScript script = new DBSQLScript(context);
            dropConstraints(schema, context, script);
            dropTables(schema, context, script);
            context.getConnection().setAutoCommit(true);
            try {
                script.executeAll();
            } finally {
                context.getConnection().setAutoCommit(false);
            }
        } finally {
            context.discard();
        }
    }

    private void dropTables(DBDatabase schema, DBContext context, DBSQLScript script) {
        for (DBTable table : schema.getTables()) {
            context.getDbms().getDDLScript(DBDDLGenerator.DDLActionType.DROP, table, script);
        }
    }

    private void dropConstraints(DBDatabase schema, DBContext context, DBSQLScript script) {
        for (DBRelation relation : schema.getRelations()) {
            EmpireUtils.dropRelation(relation, context, script);
        }
    }

}