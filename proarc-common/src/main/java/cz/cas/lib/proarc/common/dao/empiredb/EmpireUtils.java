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

import java.util.Arrays;
import java.util.List;
import org.apache.empire.commons.OptionEntry;
import org.apache.empire.commons.Options;
import org.apache.empire.data.DataMode;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBCmdType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBColumnExpr;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBDatabase;
import org.apache.empire.db.DBDatabaseDriver;
import org.apache.empire.db.DBExpr;
import org.apache.empire.db.DBIndex;
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.DBTableColumn;

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

    /**
     * Adds a column to the {@code order by} clause of the command.
     * @param cmd SQL command
     * @param columnBeanPropertyName a property name of the sort column,
     *          possibly prefixed with {@code '-'} to make the sorting descending.
     * @param defaultSortByColumn {@code null} or a column to use in case of
     *          missing {@code columnBeanPropertyName}
     * @param defaultDescending {@code true} to sort {@code defaultSortByColumn} top down
     */
    public static void addOrderBy(DBCommand cmd,
            String columnBeanPropertyName, DBTableColumn defaultSortByColumn,
            boolean defaultDescending
    ) {
        DBColumnExpr[] selectExprList = cmd.getSelectExprList();
        List<? extends DBColumnExpr> selections = Arrays.asList(selectExprList);
        addOrderBy(cmd, selections, columnBeanPropertyName, defaultSortByColumn, defaultDescending);
    }

    private static void addOrderBy(DBCommand cmd, List<? extends DBColumnExpr> selections,
            String columnBeanPropertyName, DBTableColumn defaultSortByColumn,
            boolean defaultDescending
    ) {
        DBColumnExpr sortByCol = findSelection(selections, columnBeanPropertyName);
        boolean descending;
        if (sortByCol != null) {
            descending = isDescendingSort(columnBeanPropertyName);
        } else if (defaultSortByColumn != null) {
            sortByCol = defaultSortByColumn;
            descending = defaultDescending;
        } else {
            return ;
        }
        cmd.orderBy(sortByCol, descending);
    }

    private static boolean isDescendingSort(String prefixedBeanPropertyName) {
        return prefixedBeanPropertyName.charAt(0) == '-';
    }

    private static DBColumnExpr findSelection(List<? extends DBColumnExpr> selections, String prefixedBeanPropertyName) {
        if (prefixedBeanPropertyName == null) {
            return null;
        }
        String beanPropertyName = prefixedBeanPropertyName.charAt(0) == '-'
                ? prefixedBeanPropertyName.substring(1) : prefixedBeanPropertyName;
        for (DBColumnExpr selection : selections) {
            if (beanPropertyName.equals(selection.getBeanPropertyName())) {
                return selection;
            }
        }
        return null;
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
