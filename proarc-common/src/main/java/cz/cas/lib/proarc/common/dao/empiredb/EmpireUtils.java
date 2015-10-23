/*
 * Copyright (C) 2015 Jan Pokorsky
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

import org.apache.empire.db.DBCmdType;
import org.apache.empire.db.DBDatabaseDriver;
import org.apache.empire.db.DBExpr;
import org.apache.empire.db.DBIndex;
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;

/**
 * EmpireDb helpers.
 *
 * @author Jan Pokorsky
 */
class EmpireUtils {

    /**
     * Adds DDL statements for a new table. In addition to {@link DBDatabaseDriver#getDDLScript}
     * it adds also DDL statements for references and indices .
     */
    public static void addTable(DBTable table, DBDatabaseDriver driver, DBSQLScript script) {
        driver.getDDLScript(DBCmdType.CREATE, table, script);
        for (DBRelation relation : table.getForeignKeyRelations()) {
            driver.getDDLScript(DBCmdType.CREATE, relation, script);
        }
        for (DBIndex index : table.getIndexes()) {
            driver.getDDLScript(DBCmdType.CREATE, index, script);
        }
    }

    /**
     * Adds DDL statement to the script to remove a table constraint.
     */
    public static void dropRelation(DBRelation relation, DBDatabaseDriver driver, DBSQLScript script) {
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

}
