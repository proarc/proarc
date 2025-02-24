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

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
import org.apache.empire.db.DBRelation;
import org.apache.empire.db.DBSQLScript;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.DBTableColumn;
import org.apache.empire.db.expr.compare.DBCompareColExpr;
import org.apache.empire.db.expr.compare.DBCompareExpr;

/**
 * EmpireDb helpers.
 *
 * @author Jan Pokorsky
 */
class EmpireUtils {

    /**
     * Adds DDL statements for a new table. In addition to {@link DBDatabaseDriver#getDDLScript}
     * it adds also DDL statements for references.
     */
    public static void addTable(DBTable table, DBDatabaseDriver driver, DBSQLScript script) {
        driver.getDDLScript(DBCmdType.CREATE, table, script);
        for (DBRelation relation : table.getForeignKeyRelations()) {
            driver.getDDLScript(DBCmdType.CREATE, relation, script);
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
     * Adds datetime filters.to the where statement.
     *
     * @param column a DB to filter
     * @param dateFilter list of date filters as (operator, datetime) or single datetime
     */
    public static void addWhereDate(DBCommand cmd, DBTableColumn column, List<String> dateFilter) {
        List<DBCompareExpr> exprs = createWhereDateExpr(column, parseDateFilter(dateFilter));
        if (!exprs.isEmpty()) {
            cmd.addWhereConstraints(exprs);
        }
    }

    /**
     * Adds an LIKE statement to the where clause.
     * @param cmd the SQL command
     * @param column the column to compare
     * @param likePattern the compared value without wildcards. The supplied value may be {@code null}.
     * @return the modified command
     */
    public static void addWhereLike(DBCommand cmd, DBColumnExpr column, String likePattern) {
        if (likePattern != null) {
            String pattern = likePattern.trim().replace("%", "\\%");
            if (!pattern.isEmpty()) {
                cmd.where(column.like('%' + pattern + '%'));
            }
        }
    }

    public static void addWhereLikeIgnoreCase(DBCommand cmd, DBColumnExpr column, String likePattern) {
        if (likePattern != null) {
            String pattern = likePattern.toUpperCase().trim().replace("%", "\\%");
            if (!pattern.isEmpty()) {
                cmd.where(column.upper().like('%' + pattern + '%'));
            }
        }
    }

    /**
     * Adds an equals statement to the where clause.
     * @param cmd the SQL command
     * @param column the column to compare
     * @param isValue the compared value. The supplied value may be {@code null}.
     * @return the modified command
     */
    public static DBCommand addWhereIs(DBCommand cmd, DBColumnExpr column, Object isValue) {
        if (isValue != null) {
            cmd.where(column.is(isValue));
        }
        return cmd;
    }

    public static DBCommand addWhereIsIn(DBCommand cmd, DBColumnExpr column, List range) {
        if (range != null && range.size() > 0) {
            cmd.where(column.in(range));
        }
        return cmd;
    }

    /**
     *
     * @param column a DB to filter
     * @param dateEntries list of entries with date (key) and optional operator (value).
     */
    static List<DBCompareExpr> createWhereDateExpr(DBTableColumn column, List<Entry<String, String>> dateEntries) {
        ArrayList<DBCompareExpr> exprs = new ArrayList<DBCompareExpr>(dateEntries.size());
        for (Entry<String, String> entry : dateEntries) {
            String operator = entry.getValue();
            String date = entry.getKey();
            DBCompareColExpr expr = null;
            if (operator == null || "=".equals(operator)) {
                expr = column.is(date);
            } else if (">".equals(operator)) {
                expr = column.isGreaterThan(date);
            } else if ("<".equals(operator)) {
                expr = column.isSmallerThan(date);
            } else if (">=".equals(operator)) {
                expr = column.isMoreOrEqual(date);
            } else if ("<=".equals(operator)) {
                expr = column.isLessOrEqual(date);
            } else {
                throw new IllegalStateException(String.format("date: '%s', operand: '%s'", date, operator));
            }
            if (expr != null) {
                exprs.add(expr);
            }
        }
        return exprs;
    }

    static List<Entry<String, String>> parseDateFilter(List<String> dateFilter) {
        ArrayList<Entry<String, String>> dExpressions = new ArrayList<Map.Entry<String, String>>();
        if (dateFilter == null) {
            return dExpressions;
        }
        for (Iterator<String> dfit = dateFilter.iterator(); dfit.hasNext();) {
            String dateOrOperand = dfit.next();
            if (dfit.hasNext()) {
                String operand = dateOrOperand;
                String date = dfit.next();
                dExpressions.add(new SimpleImmutableEntry<String, String>(date, operand));
            } else {
                String date = dateOrOperand;
                dExpressions.add(new SimpleImmutableEntry<String, String>(date, null));
            }
        }
        return dExpressions;
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
